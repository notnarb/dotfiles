#!/bin/bash
destDir=~
currentDir=$(pwd)
fileList=".bash_profile .tmux.conf"

for file in $fileList
do
ln -s $currentDir/$file $destDir/$file
done
