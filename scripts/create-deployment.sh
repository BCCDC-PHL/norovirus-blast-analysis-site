#!/bin/bash

rm -r target/deploy/norovirus-blast-analysis 2> /dev/null
mkdir -p target/deploy/norovirus-blast-analysis
mkdir -p target/deploy/norovirus-blast-analysis/js
cp -r resources/public/css target/deploy/norovirus-blast-analysis
cp -r resources/public/images target/deploy/norovirus-blast-analysis
cp target/public/cljs-out/prod/main_bundle.js target/deploy/norovirus-blast-analysis/js/main.js
cp resources/public/index_prod.html target/deploy/norovirus-blast-analysis/index.html
pushd target/deploy > /dev/null
tar -czf norovirus-blast-analysis.tar.gz norovirus-blast-analysis
rm -r norovirus-blast-analysis
popd > /dev/null
