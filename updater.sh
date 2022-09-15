#!/bin/bash
while inotifywait -e close_write ~/R_indic;
do 
    bash hook.sh;
done
