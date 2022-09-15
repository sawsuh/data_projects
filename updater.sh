#!/bin/bash
while inotifywait -e close_write ~/R_indic;
do 
    break
done
bash hook.sh
