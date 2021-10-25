library("rtweet")
library("tidyverse")
library("utils")
library("RCurl")
library("magrittr")
library("png")
library("jpeg")
library("gmp")
library("qdapRegex")
library("tm")

print(Sys.time())
options(timeout=600)
options("timeout")

###################################################################################
# Se desabonne puis s'abonne aux twitos de deputweets

if(Sys.info()[1] == "Windows") {
  setwd("C:/Users/rpoui/OneDrive/ShinyApps/Twitter")
  
} else { #Linux
  setwd("//home//ubuntu//AppDeputes")
}
  # compte Deputweets
token <- readRDS("tokenDeputweets.rds")

Twitos <- readRDS("deputweets//data//Twitos.rds")

Twitos %<>% filter(screen_name!="")

TwitosFull <- lookup_users(Twitos$screen_name, token=token)%>%
  select(screen_name,user_id) %>%
  mutate(inApp=1)

myFlw <- get_friends("deputweets", token = token) %>% 
  mutate(inBase=1) %>% 
  right_join(TwitosFull, by="user_id") %>%
  mutate(inBase=ifelse(is.na(inBase),0,1),
         inApp=ifelse(is.na(inApp),0,1))  

table(myFlw$inBase,myFlw$inApp)  
  
out <- myFlw %>% filter(inBase == 1 & inApp ==0)
if(nrow(out) > 0){
for(i in out$user_id){
  print(i)
  post_unfollow_user(i, token = token)
  }
}

new <- myFlw %>% filter(inBase == 0 & inApp ==1)

for(i in new$screen_name){
  print(i)
  resp <- post_follow(i, token = token)
  print(resp)
}

