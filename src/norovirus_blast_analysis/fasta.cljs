(ns norovirus-blast-analysis.fasta
  (:require [clojure.string :as str]))

(defn add-fasta-line [seqs line]
  "Given a vector of maps with keys :id and :sequence,
   and a line from a fasta file, return a new vector of
   maps with the new sequence added to the last map"
  (cond
    (re-matches #"^>.*" line)
    (let [seq-id (str/replace-first (first (str/split line #"\s+")) #">" "")
          new-seqs (conj seqs {:id seq-id :sequence ""})]
      new-seqs)
    (= (count seqs) 1)
    (let [last-seq (last seqs)
          new-seqs (conj [] (assoc last-seq :sequence line))]
      new-seqs)
    :else (let [last-seq (last seqs)
                seq (str/join (:sequence last-seq) line)
                new-seqs (conj (into [] (butlast seqs)) (assoc last-seq :sequence seq))]
            new-seqs)))

(defn parse-fasta [fasta-str]
  "Given a string in fasta format, return a
   vector of maps with keys :id and :sequence"
  (let [lines (str/split-lines fasta-str)
        seqs []]
    (reduce add-fasta-line seqs lines)))
