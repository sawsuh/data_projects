#!/bin/bash
x=$(date '+%H:%M:%S')
sleep 5
R -e "rmarkdown::render('~/data_projects/afl_model/analysis.rmd')"
git add . && git commit -m "auto-update ${x}" && git push
