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
library("lubridate")

print(Sys.time())
options(timeout=600)
options("timeout")

###################################################################################
# Deal with the token

if(Sys.info()[1] == "Linux") {

  setwd("//home//ubuntu//AppDeputes")
  token <- readRDS("tokenDeputweets.rds")

} else { token <- readRDS("tokenRP.rds") }
  
  token <- bearer_token(token)

  #get_timeline("EmmanuelMacron", n = 10, token=token)

getMinStatus <- function(x) as.character(min(as.bigz(x$status_id)))
getMaxStatus <- function(x) as.character(max(as.bigz(x$status_id)))

###################################################################################
# Read the Excel files 
Deputes <- readxl::read_excel("Deputes 23-10-2021.xlsx", 
                              sheet="Data")

Deputes %<>% 
  filter(Keep!=0) %>% # remove old government
  rename(screen_name = twitter) %>%
  mutate(screen_name = ifelse(is.na(screen_name),"",screen_name)) 

###################################################################################
#Find data on users with Twitter

TwitosFull <- lookup_users(Deputes$screen_name, token = token) 

Twitos <- left_join(Deputes, TwitosFull, by="screen_name")
(nTwitos <- nrow(Twitos))

# Get the various adresses (remove twitter, get Facebook, get url)
sites <- gsub("\\|"," ",Deputes$sites_web)
sites <- ex_url(sites)

Twitos$Facebook <- lapply(sites, function(x) grep("facebook",x,value=TRUE)) 
table(sapply(Twitos$Facebook, length))

Twitos$url <- lapply(sites, function(x) x[grep("facebook|twitter", x, invert=TRUE)])
table(sapply(Twitos$url, length))


###################################################################################
# Check photos once a week (Friday)
if(weekdays(Sys.Date()) == "Friday" && substr(Sys.time(),12,13) == 12) {
  photos <- TRUE
} else {
  photos <- FALSE}

if(weekdays(Sys.Date()) == "Saturday") doneToday <- FALSE 

# Deal with the Pictures

###################################################################################
# Check photos once a week (Friday)

if(photos){
  
  Twitos$profile_image_url_bigger <- sub("normal","bigger",Twitos$profile_image_url)
  Twitos$profile_image_url_bigbig <- sub("_normal","",Twitos$profile_image_url)
  
  Error <- NULL
  for(i in 1:nTwitos){

    #    for(i in 96){
    # Errors photos: 96 377 464 466 523 547 569 
    # [1] "Alain_Bruneel"   "AudeLuquet2017"  "ValeriePetit_EM" "B_Peyrol"        "Fabien_Rssl"    
    # [6] "sonjoachim"      "HuguetteLREM46" 

    print(i)
    if(Twitos$screen_name[i] == "") next
    file <- paste0("deputweets//www//", Twitos$screen_name[i],".jpg")
    x <- try({
      x <- download.file(Twitos$profile_image_url_bigger[i],
                  file,
                  mode="wb",
                  cacheOK = FALSE)
      if(x != 0) download.file(Twitos$profile_image_url_bigbig[i],
                           file,
                           mode="wb",
                           cacheOK = FALSE)
      img <- readJPEG(file)
      writeJPEG(img, target = file, quality = 1)
    })
  
    if(class(x) == "try-error") {
      x <- try({
        img <- readPNG(file)
        writeJPEG(img, target = file, quality = 1)
      })
    }
    if(class(x) == "try-error") Error <- c(Error,i)
  }
  
  cat("Errors photos:", Error, "\n") 
  print(Twitos$screen_name[Error])
}
###################################################################################


Twitos$index <- 1:nrow(Twitos) 
Twitos$picture <- paste0('<img src="',Twitos$screen_name,'.jpg" height="73px" alt="Pas de Photo"></img>')

Twitos$last_textLink <- {
  link <- ex_twitter_url(Twitos$text)
  textLink <- rm_twitter_url(Twitos$text)
  link <- lapply(link, function(x) ifelse(is.na(x), "", paste0('<a href="',x,'" target="_blank">plus</a>')))
  link <- sapply(link, paste, collapse= " ")
  paste(textLink, link)
}

Twitos$screenNameLink <- paste0('<a href="https://twitter.com/',Twitos$screen_name,'" target="_blank">',Twitos$prenom_nom,'</a>')
Twitos$screenNameLink[Twitos$screen_name == ""] <- ""

Twitos$FBLink <- sapply(Twitos$Facebook, 
                          function(x) if(length(x) == 0 || is.na(x)) "" else
                                             paste(sapply(x, function(x) paste0('<a href="',x,'" target="_blank"><img src="FB.png" alt="FB" width="25px"/></a>')), collapse=""))

Twitos$Facebook <- sapply(Twitos$Facebook, paste, collapse= " ")


Twitos$URLLink <- sapply(Twitos$url, 
                     function(x) if(length(x) == 0 || is.na(x)) "" else 
                                        paste(sapply(x, function(x) paste0('<a href="',x,'" target="_blank"> site </a>')), collapse=""))
Twitos$url <- sapply(Twitos$url, paste, collapse= " ")

Twitos$circo <- ifelse(is.na(Twitos$nom_circo), "",
                       paste0(Twitos$nom_circo," (",Twitos$num_circo,"&deg;)"))

Twitos$name <- ifelse(is.na(Twitos$name),Twitos$Nom , Twitos$name)

Twitos %<>% 
  arrange(desc(DepuGouv), Nom) %>%
  select(
         index,
         picture,
         DepuGouv,
         screen_name,
         user_id,
         Nom= prenom_nom,
         name,
         sexe,
         FBLink,
         URLLink,
         screenNameLink,
         last_textLink,
         num_deptmt,
         nom_circo,
         num_circo,
         circo,
         region,
         groupe_sigle,
         commission_permanente,
         description,
         location,
         account_created_at,

         last_text=text,
         last_created_at=created_at,

         followers_count,
         friends_count,
         listed_count,
         user_favourites_count=favourites_count,
         retweet_count,
         statuses_count) 

saveRDS(Twitos, file="deputweets//data//Twitos.rds")

###################################################################################
# Update Old tweets 
###################################################################################

# from https://github.com/gadenbuie/gathertweet

lookup_status_ratelimit <- function(status_id, ...) { # Note: limited to 10000 because 90000 doesn't work on AWS
  tweets <- NULL
  rate_limit <- rtweet::rate_limits(query = "statuses/lookup", token = token)
  print(rate_limit)
  fetch_count <- 0
  n_status <- length(status_id)
  n_status_large <- n_status > 10000
  for (idx_group in seq(1, ceiling(n_status/10000))) {
    # Rate limit ----
    # Track rate limit and wait it out if needed
    if (Sys.time() > rate_limit$reset_at) {
      cat("Updating out-of-date rate limit\n")
      rate_limit <- rtweet::rate_limits(query = "statuses/lookup", token = token)
    }
    if (rate_limit$remaining - fetch_count < 1) {
      # wait until rate limit resets
      wait_s <- difftime(Sys.time(), rate_limit$reset_at, units = "sec")
      cat("Waiting for rate limit to reset at ",rate_limit$reset_at, "\n")
      Sys.sleep(ceiling(as.numeric(wait_s)))
    }
    if (fetch_count > 0 && fetch_count %% 50 == 0) {
      rate_limit <- rtweet::rate_limits(query = "statuses/lookup", token = token)
    }
    
    # Get Statuses ----
    if (n_status_large) {
      idx_start <- (idx_group - 1) * 10000 + 1
      idx_end   <- min(idx_group * 10000, n_status)
      cat("Getting tweets",idx_start,"to", idx_end," of ", n_status,"\n")
    } else {
      idx_start <- 1
      idx_end <- n_status
      cat("Getting", n_status," tweets\n")
    }
    tweets <- bind_rows(
      tweets,
      rtweet::lookup_statuses(status_id[idx_start:idx_end], token = token, ...)
    )
  }
  
  return(tweets)
}

TweetsBase <- readRDS("deputweets//data//Tweets.rds")

#remove tweets less than 7 days ago and twittos that are out
dim(TweetsBase)
(tmin <- Sys.time()-30*24*60*60)
TweetsBase %<>% filter(TweetsBase$created_at >= tmin,
                       TweetsBase$screen_name %in% Deputes$screen_name)
dim(TweetsBase)

tmp <- try(lookup_status_ratelimit(TweetsBase$status_id))
if(!inherits(tmp, "try-error")){
  lost <- TweetsBase %>% filter(!(status_id %in% tmp$status_id))
  TweetsBase <- bind_rows(tmp, lost) 
}

###################################################################################
# # Load New tWeets
###################################################################################

minStatusBase <- getMinStatus(TweetsBase)
# TweetsBase %>% filter(status_id == minStatusBase) %>% select(screen_name,created_at)

#fmeunier blocked
users <- Twitos$screen_name[!(Twitos$screen_name %in% c("","fmeunier19"))] 
nUsers <- length(users)

# users <- c("NBelloubet", "Ccastaner", "GDarmanin", "MFesneau")
# nUsers <- 4

for(i in 1:nUsers){
  
  cat(i,": ",users[i],"\n")
  
  minStatus <- getMaxStatus(TweetsBase %>% filter(screen_name == users[i]))
  # For those with 0 Tweet since 17/04
  if(length(minStatus) == 0) minStatus <- minStatusBase
  
  options(warn=2)
  tmls <- try(get_timeline(users[i], n = 3200, 
                           since_id =  minStatus,
                           check = FALSE, 
                           token = token))
  options(warn=0)
  
  if(is(tmls,"try-error")){
    print(tmls)
    print(Sys.time())
    print(x <- rtweet::rate_limits(query = "get_timeline", token = token))
    if(x$remaining == 0){
      cat("Waiting\n")
      Sys.sleep(16*60)
      options(warn=2)
      tmls <- try(get_timeline(users[i], n = 3200, 
                             since_id =  minStatus,
                             check = FALSE, token = token))
    options(warn=0)
    } #if other problem: pass over....
  } 
  if(!is(tmls,"try-error") && nrow(tmls) > 0){
    cat("Nb Tweets: ",nrow(tmls),"\n\n")
    TweetsBase <- add_row(TweetsBase, tmls)
  }
} 

dup <- duplicated(TweetsBase$status_id)
TweetsBase %<>% filter(!dup, 
                       !is.na(status_id))

saveRDS(TweetsBase, file="deputweets//data//Tweets.rds")


###################################################################################
# # Process Tweets
###################################################################################

  TweetsBase <- readRDS("deputweets//data//Tweets.rds")
  Twitos <- readRDS(Twitos, file="deputweets//data//Twitos.rds")

  source("deputweets//data//functions.R")
  
  #newTweetsBase %>% slice(1) %>% as.list
  #Twitos <- readRDS(file="deputweets//data//Twitos.rds")
  #newTweetsBase <- readRDS(file="deputweets//data//Tweets.rds")
  
  # Add/remove tweetos characterstics    
  joinTwitos <-  Twitos %>%
    select(DepuGouv,
           screen_name, 
           Nom, 
           name, 
           sexe, 
           num_deptmt, 
           nom_circo,
           num_circo, 
           circo,
           region,
           groupe_sigle, 
           commission_permanente)
  
  # Add tweets characteristics    
  
  TweetsBase$textLink <- {
    link <- ex_twitter_url(TweetsBase$text)
    textLink <- rm_twitter_url(TweetsBase$text)
    link <- lapply(link, function(x) ifelse(is.na(x), "", paste0('<a href="',x,'" target="_blank">plus</a>')))
    link <- sapply(link, paste, collapse= " ")
    paste(textLink, link)
  }
  
  process <- processWords(TweetsBase)
  #names(sort(table(unlist(strsplit(process$word," "))),TRUE))[1:500]
  
  processedTweets <-  TweetsBase %>% 
    select(status_id,
           status_url,
           created_at,
           screen_name,
           textLink,
           is_retweet,
           favorite_count,
           retweet_count,
           reply_count,
           retweet_name,
           retweet_status_id,
           retweet_followers_count,
           retweet_created_at,
           location,
           retweet_screen_name) %>%
    left_join(process,  by="status_id")  %>%
    inner_join(joinTwitos,  by="screen_name")
  
  # Remove Duplicated
  dup <- duplicated(processedTweets$status_id)
  cat("Duplicated:",sum(dup),"\n")
  processedTweets %<>% filter(!dup) %>%
    arrange(desc(status_id))

  saveRDS(processedTweets, file="deputweets//data//processedTweets.rds")
 

cat("Done\n")

