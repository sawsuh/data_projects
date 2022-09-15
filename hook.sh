#!/bin/bash
x=$(date '+%H:%M:%S')
sleep 5
git add . && git commit -m "auto-update ${x}" && git push
