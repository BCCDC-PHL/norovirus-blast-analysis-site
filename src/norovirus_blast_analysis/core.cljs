(ns ^:figwheel-hooks norovirus-blast-analysis.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [clojure.set]
            [clojure.string :as str]
            [reagent.core :as r] 
            [reagent.dom :as rdom]
            [reagent.dom.server]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<! timeout chan put!]]
            [ag-grid-react :as ag-grid]
            [ag-charts-react :as ag-chart]
            [cljs.pprint :refer [pprint]]
            [norovirus-blast-analysis.fasta :as fasta]))

(def app-version "v0.1.0")

(def base-url "http://localhost:8000/")
(def endpoints {:submit #(identity "analysis/submission")
                :status #(str "analysis/" % "/status")
                :results #(str "analysis/" % "/results")})

(defonce db (r/atom {:sequences []
                     :selected-sequence-ids #{}
                     :pending-analyses {}
                     :results {}
                     :selected-result-ids #{}}))

(def refresh-rate-ms 4000)
(def min-sequence-length 425)
(def max-sequence-length 600)
(def max-ambiguous-bases 10)
(def max-n-bases 10)

(def files-chan (chan))
(def seqs-chan (chan))

(defn in? 
  "true if coll contains elem"
  [coll elem]  
  (some #(= elem %) coll))


(defn get-selected-rows
  "Takes an event e from ag-grid and returns the selected rows as a seq of maps"
  [e]
  (map #(js->clj (.-data %) :keywordize-keys true)
       (-> e
           .-api
           .getSelectedNodes)))


(defn sequences-selection-changed [e]
  "Takes an event e from ag-grid and updates the selected-sequence-ids in the db"
  (let [currently-selected-sequence-ids (->> e
                                             get-selected-rows
                                             (map :id)
                                             set)]
    (swap! db (fn [db] (assoc db :selected-sequence-ids currently-selected-sequence-ids)))))


(defn header
  "Header component"
  []
  [:header {:style {:display "grid"
                    :grid-template-columns "repeat(2, 1fr)"
                    :align-items "center"
                    :height "48px"}}
   [:div {:style {:display "grid"
                  :grid-template-columns "repeat(2, 1fr)"
                  :align-items "center"}}
    [:h1 {:style {:font-family "Arial" :color "#004a87" :margin "0px"}} "Norovirus Genotyping"][:p {:style {:font-family "Arial" :color "grey" :justify-self "start"}} app-version]]
   [:div {:style {:display "grid" :align-self "center" :justify-self "end"}}
    [:img {:src (str "images/bccdc_logo.svg") :height "48px"}]]])


(defn sequences-table
  "Sequences table component"
  []
  (let [grid-ref (clj->js {:current nil})
        sequences (:sequences @db)
        add-seq-length (fn [s] (assoc s :sequence_length (count (:sequence s))))
        add-num-n-bases (fn [s] (assoc s :num_n_bases (count (re-seq #"N" (:sequence s)))))
        add-num-ambiguous-bases (fn [s] (assoc s :num_ambiguous_bases (count (re-seq #"[^ATGCatgc]" (:sequence s)))))
        add-qc-status (fn [s] (if (and
                                   (> (:sequence_length s) min-sequence-length)
                                   (< (:sequence_length s) max-sequence-length)
                                   (< (:num_n_bases s) max-n-bases)
                                   (< (:num_ambiguous_bases s) max-ambiguous-bases))
                                (assoc s :qc_status "PASS")
                                (assoc s :qc_status "FAIL")))
        sequence-length-style (fn [params] (if (not (and (> (. params -value) min-sequence-length)
                                                         (< (. params -value) max-sequence-length)))
                                             (clj->js {:backgroundColor "#e6675e"})
                                             (clj->js {:backgroundColor "#6ade8a"})))
        num-ambiguous-bases-style (fn [params] (if (> (. params -value) max-ambiguous-bases)
                                                 (clj->js {:backgroundColor "#e6675e"})
                                                 (clj->js {:backgroundColor "#6ade8a"})))
        num-n-bases-style (fn [params] (if (> (. params -value) max-n-bases)
                                                 (clj->js {:backgroundColor "#e6675e"})
                                                 (clj->js {:backgroundColor "#6ade8a"})))
        qc-status-style (fn [params] (if (not (= "PASS" (. params -value)))
                                       (clj->js {:backgroundColor "#e6675e"})
                                       (clj->js {:backgroundColor "#6ade8a"})))
        row-data (->> sequences
                      (map add-seq-length)
                      (map add-num-n-bases)
                      (map add-num-ambiguous-bases)
                      (map add-qc-status))
        _ (js/console.log "sequences-table row-data")
        _ (js/console.log (clj->js row-data))]
    [:div {:class "ag-theme-balham"
           :style {}}
     [:> ag-grid/AgGridReact
      {:ref grid-ref
       :rowData row-data
       :pagination false
       :rowSelection "multiple"
       :enableCellTextSelection true
       :onFirstDataRendered #((do
                                (-> % .-api .sizeColumnsToFit)
                                (-> % .-api .selectAll)))
       :onSelectionChanged sequences-selection-changed
       }
      [:> ag-grid/AgGridColumn {:field "id" :headerName "Sequence ID" :minWidth 180 :resizable true :filter "agTextColumnFilter" :floatingFilter true :checkboxSelection true :headerCheckboxSelection true :headerCheckboxSelectionFilteredOnly true :sortable true}]
      [:> ag-grid/AgGridColumn {:field "sequence_length" :headerName "Sequence Length" :minWidth 180 :resizable true :filter "agNumberColumnFilter" :floatingFilter true :sortable true}]
      [:> ag-grid/AgGridColumn {:field "num_ambiguous_bases" :headerName "Num. Ambiguous Bases" :minWidth 180 :resizable true :filter "agNumberColumnFilter" :floatingFilter true :sortable true}]
      [:> ag-grid/AgGridColumn {:field "num_n_bases" :headerName "Num. N Bases" :minWidth 180 :resizable true :filter "agNumberColumnFilter" :floatingFilter true :sortable true}]
      [:> ag-grid/AgGridColumn {:field "qc_status" :headerName "QC Status" :minWidth 120 :resizable true :filter "agTextColumnFilter" :floatingFilter true :sortable true :cellStyle qc-status-style}]
      ]]))


(defn result-selected
  "Takes an event e from ag-grid and updates the selected-result-ids in the db"
  [e]
  (let [currently-selected-result-ids (->> e
                                           get-selected-rows
                                           (map :id)
                                           set)]
    (swap! db (fn [db] (assoc db :selected-result-ids currently-selected-result-ids)))))


(defn results-selection-table
  "Results selection table component"
  []
  (let [pending-analyses (:pending-analyses @db)
        pending-sequences (->> pending-analyses
                              vals
                              (mapcat :sequences))
        analysis-status-style (fn [params]
                                (let [status (. params -value)]
                                  (cond (= status "QUEUED") (clj->js {:backgroundColor "#e6675e"})
                                        (= status "RUNNING") (clj->js {:backgroundColor "#fae950"})
                                        (= status "COMPLETED") (clj->js {:backgroundColor "#6ade8a"}))))
        completed-sequences (map #(assoc {} :id % :analysis_status "COMPLETED") (keys (:results @db)))
        row-data (concat pending-sequences completed-sequences)]
    [:div {:class "ag-theme-balham"
           :style {}}
     [:> ag-grid/AgGridReact
      {:rowData row-data
       :pagination false
       :rowSelection "multiple"
       :enableCellTextSelection true
       :onFirstDataRendered #(-> % .-api .sizeColumnsToFit)
       :onSelectionChanged result-selected
       }
      [:> ag-grid/AgGridColumn {:field "id" :headerName "Sequence ID" :minWidth 100 :resizable true :filter "agTextColumnFilter" :floatingFilter true :checkboxSelection true :headerCheckboxSelection true :headerCheckboxSelectionFilteredOnly true :sortable true}]
      [:> ag-grid/AgGridColumn {:field "analysis_status" :headerName "Analysis Status" :minWidth 80 :resizable true :filter "agTextColumnFilter" :floatingFilter true :checkboxSelection false :sortable true :cellStyle analysis-status-style}]
      ]]))


(defn results-table
  "Results table component"
  []
  (let [grid-ref (clj->js {:current nil})
        results-by-seq-id (:results @db)
        selected-result-ids (:selected-result-ids @db)
        row-data (->> selected-result-ids
                     (select-keys results-by-seq-id)
                     (vals)
                     (flatten))
        _ (js/console.log "results-table row-data")
        _ (js/console.log (clj->js row-data))]
    [:div {:style {:display "grid"
                   :grid-template-columns "1fr"
                   :grid-template-rows "11fr 1fr"}}
     [:div {:class "ag-theme-balham"
            :style {}}
      [:> ag-grid/AgGridReact
       {:ref grid-ref
        :rowData row-data
        :pagination false
        :enableCellTextSelection true
        :onFirstDataRendered #((do
                                (-> % .-api .sizeColumnsToFit)
                                #_(-> grid-ref .-current js/console.log)
                                (-> grid-ref .-current .-columnApi
                                    (.applyColumnState (clj->js {:state [{:colId "query_seq_id" :sort "asc"}]})))))
       }
       [:> ag-grid/AgGridColumn {:field "query_seq_id" :headerName "Query Sequence ID" :minWidth 100 :resizable true :filter "agTextColumnFilter" :floatingFilter true :checkboxSelection false :sortable true}]
       [:> ag-grid/AgGridColumn {:field "subject_seq_id" :headerName "Subject Sequence ID" :minWidth 180 :resizable true :filter "agTextColumnFilter" :floatingFilter true :checkboxSelection false :sortable true}]
       [:> ag-grid/AgGridColumn {:field "region" :headerName "Region" :minWidth 80 :resizable true :filter "agTextColumnFilter" :floatingFilter true :checkboxSelection false :sortable true}]
       [:> ag-grid/AgGridColumn {:field "genotype" :headerName "Genotype" :minWidth 100 :resizable true :filter "agTextColumnFilter" :floatingFilter true :checkboxSelection false :sortable true}]
       [:> ag-grid/AgGridColumn {:field "subject_start" :headerName "Subject Start Pos." :minWidth 60 :resizable true :filter "agNumberColumnFilter" :floatingFilter true :sortable true}]
       [:> ag-grid/AgGridColumn {:field "subject_end" :headerName "Subject End Pos." :minWidth 60 :resizable true :filter "agNumberColumnFilter" :floatingFilter true :sortable true}]
       [:> ag-grid/AgGridColumn {:field "query_start" :headerName "Query Start Pos." :minWidth 60 :resizable true :filter "agNumberColumnFilter" :floatingFilter true :sortable true}]
       [:> ag-grid/AgGridColumn {:field "query_end" :headerName "Query End Pos." :minWidth 60 :resizable true :filter "agNumberColumnFilter" :floatingFilter true :sortable true}]
       [:> ag-grid/AgGridColumn {:field "num_mismatch" :headerName "Num. Mismatches" :minWidth 60 :resizable true :filter "agNumberColumnFilter" :floatingFilter true :sortable true}]
       [:> ag-grid/AgGridColumn {:field "alignment_length" :headerName "Alignment Length" :minWidth 60 :resizable true :filter "agNumberColumnFilter" :floatingFilter true :sortable true}]
       [:> ag-grid/AgGridColumn {:field "subject_length" :headerName "Subject Length" :minWidth 60 :resizable true :filter "agNumberColumnFilter" :floatingFilter true :sortable true}]
       [:> ag-grid/AgGridColumn {:field "percent_coverage" :headerName "Coverage (%)" :minWidth 60 :resizable true :filter "agNumberColumnFilter" :floatingFilter true :sortable true}]
       [:> ag-grid/AgGridColumn {:field "percent_identity" :headerName "Identity (%)" :minWidth 60 :resizable true :filter "agNumberColumnFilter" :floatingFilter true :sortable true}]
       [:> ag-grid/AgGridColumn {:field "bitscore" :headerName "Bit Score" :minWidth 100 :resizable true :filter "agNumberColumnFilter" :floatingFilter true :sortable true}]
       [:> ag-grid/AgGridColumn {:field "database_name" :headerName "DB Name" :minWidth 120 :resizable true :filter "agTextColumnFilter" :floatingFilter true :checkboxSelection false :sortable true}]
       [:> ag-grid/AgGridColumn {:field "database_version" :headerName "DB Version" :minWidth 80 :resizable true :filter "agTextColumnFilter" :floatingFilter true :checkboxSelection false :sortable true}]
       [:> ag-grid/AgGridColumn {:field "database_date" :headerName "DB Date" :minWidth 80 :resizable true :filter "agTextColumnFilter" :floatingFilter true :checkboxSelection false :sortable true}]]
      [:div {:style {:grid-row "2"}}
       [:button {:onClick #(.exportDataAsCsv (.-api (.-current grid-ref)) (clj->js {:fileName "norovirus_genotypes.csv"}))} "Export CSV"]]]]))



;; take files from files-chan, read them, parse them, and put them on seqs-chan
(go (loop []
      (let [file (-> (<! files-chan) .-target .-files first)
            reader (js/FileReader.)]
        (set! (.-onload reader)
              #(put! seqs-chan (-> % .-target .-result fasta/parse-fasta)))
        (.readAsText reader file)
        (recur))))

;; take seqs from seqs-chan and add them to the db
(go (loop []
      (let [seqs (<! seqs-chan)]
        (doseq [s seqs]
          (when s
            (swap! db assoc-in [:sequences] (conj (:sequences @db) s))))
        (recur))))


(defn submit-selected-sequences [e]
  ""
  (let [sequences (:sequences @db)
        selected-sequence-ids (:selected-sequence-ids @db)
        selected-sequences (into [] (filter #(in? selected-sequence-ids (:id %)) sequences))
        post-body (clj->js {:sequences selected-sequences})]
    (go
      (let [response (<! (http/post (str base-url ((:submit endpoints)))
                                    {:body (.stringify js/JSON post-body)
                                     :with-credentials? false
                                     :headers {"Content-Type" "application/json"}}))
            response-body (:body response)
            analysis-id (:analysis_uuid response-body)]
        (do
          (swap! db assoc :pending-analyses (assoc (:pending-analyses @db) analysis-id {:analysis_status "QUEUED"
                                                                                        :sequences (map #(assoc % :analysis_status "QUEUED") selected-sequences)}))
          (swap! db assoc :sequences (into [] (filter #(not (in? selected-sequence-ids (:id %))) sequences)))
          (swap! db assoc :selected-sequence-ids #{}))))))


(defn delete-selected-sequences [e]
  ""
  (let [sequences (:sequences @db)
        selected-sequence-ids (:selected-sequence-ids @db)]
    (swap! db assoc :sequences (into [] (filter #(not (in? selected-sequence-ids (:id %))) sequences)))))


(defn load-results []
  ""
  (go
    (while true
      (doseq [analysis-id (keys (:pending-analyses @db))]
        (js/console.log (str "checking status for " analysis-id))
        (let [status-response (<! (http/get (str base-url ((:status endpoints) analysis-id))
                                            {:with-credentials? false
                                             :headers {"Content-Type" "application/json"}}))
              status (:status (js->clj (:body status-response) :keywordize-keys true))]
          (js/console.log status)
          (swap! db assoc-in [:pending-analyses analysis-id :status] status)
          (swap! db update-in [:pending-analyses analysis-id :sequences] (fn [seqs] (into [] (map #(assoc % :analysis_status status) seqs))))
          (when (= status "COMPLETED")
            (let [results-response (<! (http/get (str base-url ((:results endpoints) analysis-id))
                                                 {:with-credentials? false
                                                  :headers {"Content-Type" "application/json"}}))
                  results (:body (js->clj results-response :keywordize-keys true))
                  results-by-seq-id (group-by :query_seq_id results)]
              (do
                #_(js/console.log (clj->js results-by-seq-id))
                (swap! db update :pending-analyses #(dissoc % analysis-id))
                (swap! db update :results #(merge % results-by-seq-id)))
            ))))
      (<! (timeout refresh-rate-ms)))))


(defn root []
  [:div {:style {:display "grid"
                 :grid-template-columns "1fr"
                 :grid-gap "4px 4px"
                 :height "100%"}} 
   [header]
   [:div {:style {:display "grid"
                  :grid-template-columns "1fr"
                  :grid-template-rows "1fr 2fr"
                  :gap "4px"
                  :height "800px"}}
    [:div {:style {:display "grid"
                   :gap "4px"
                   :grid-template-rows "6fr 1fr"
                   :grid-template-columns "1fr"}}
     [:div {:style {:display "grid"
                    :grid-row "1"}}
      [sequences-table]]
     [:div {:style {:display "grid"
                    :grid-row "2"}}
      [:div {:style {:display "grid"
                     :grid-template-columns "8fr 1fr 1fr"}}
       [:input {:type "file" :name "file" :style {:display "inline"} :on-change #(put! files-chan %)}]
       [:button {:type "submit" :style {:display "inline" :background-color "#6ade8a" :margin "2px" :border "2px solid #0d8c2f" :border-radius "4px" :width "80%"} :on-click #(submit-selected-sequences %)} "Submit"]
       [:button {:type "submit" :style {:display "inline" :background-color "#e6675e" :margin "2px" :border "2px solid #ad0e26" :border-radius "4px" :width "80%"} :on-click #(delete-selected-sequences %)} "Delete"]]]]
    [:div {:style {:display "grid"
                   :grid-row "2"}}
     [:div {:style {:display "grid"
                    :gap "4px"
                    :grid-template-columns "2fr 10fr"
                    :grid-template-rows "1fr"}}
      [:div {:style {:display "grid"
                     :grid-column "1"
                     :grid-row "1"}}
       [results-selection-table]]
      [:div {:style {:display "grid"
                     :grid-column "2"
                     :grid-row "1"}}
       [results-table]]]]]])


(defn render
  ""
  []
  (rdom/render [root] (js/document.getElementById "app")))


(defn ^:after-load re-render []
  (render))


(defn main
  ""
  []
  (render)
  (load-results))


(set! (.-onload js/window) main)

(comment

  )
