#!/bin/bash  

echo $(date)

echo "Start"  

cd //home//ubuntu//AppDeputes//

/bin/rm -f outl/*
 
####################### LISTERIA

echo "Fetch Twitters"  
R CMD BATCH --no-save --no-restore fetch.R outl/fetch.txt

echo "Done"  

rsync -aEim --log-file="outl/data.txt" //home//ubuntu//AppDeputes//deputweets//data //srv//shiny-server//deputweets --delete-before 
rsync -aEim --log-file="outl/www.txt" //home//ubuntu//AppDeputes//deputsweets//www //srv//shiny-server//deputweets --delete-before 
