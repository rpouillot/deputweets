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
library("shiny")
library("kableExtra")
library("webshot")


if(Sys.info()[1] == "Windows") {
  setwd("C:/Users/PouillotRegis/OneDrive/ShinyApps/Twitter/deputweets/")
} else { #Linux
  setwd("//home//ubuntu//AppDeputes")
  token <- readRDS("tokenDeputweets.rds")
  
}

#sink("checkPost",append = TRUE)


cat("newHour\n")
print(hour(with_tz(Sys.time(), tzone = "Europe/Paris")))
print(hour(with_tz(Sys.time(), tzone = "Europe/Paris")) == 0)


# Tweet the best of after midnight (Paris)

if(hour(with_tz(Sys.time(), tzone = "Europe/Paris")) == 0){
  
  ##################################
  # Functions
  ##################################
  
  # trick. UTC is always before
  duJour <- {
    x <- lubridate:::today()
    paste(substr(x,9,10),substr(x,6,7),substr(x,1,4), sep="/")
  }
  
  
  tweets_in_last <- function(tweets, d = 0, h = 0, m = 0, s = 0) {
    tweets %>%
      filter(created_at >= lubridate::now() - lubridate::hours(h + d * 24) -
               lubridate::minutes(m) - lubridate::seconds(s))
  }
  
  BootstrapColorBasic <- c(Primary = "#0275d8", Success = "#5cb85c",
                           Info = "#5bc0de", Warning = "#f0ad4e", Danger="#d9534f") 
  
  linkSVG <- function(val, height=15, width=200, max = 100, col = "blue") {
    col <- rep(col, length = length(val))
    val <- val*width/max
    #browser()  
    paste0(
      '<svg width="',width,'" height="',height,'">
      <rect x = "0" width="', val,'" height="',height,'" style="fill:',col,'" /> #;stroke-width:3;stroke:rgb(0,0,0)
      <rect x = "', val,'" width="', width,'" height="',height,'" style="fill: #F5F5F5" /> #;stroke-width:3;stroke:rgb(0,0,0)
      Sorry, your browser does not support inline SVG.  
    </svg>'
    )
  }

  ##################################
  # Data
  ##################################
  processedTweets <- readRDS("deputweets//data//processedTweets.rds")
  
  selectedTweets <- processedTweets %>% 
    filter(!is_retweet) %>% 
    tweets_in_last(h=24) 
  
  Twitos <- readRDS("deputweets//data//Twitos.rds")

  ##################################
  # Tweet 1
  ##################################
  
  best <- selectedTweets %>%
    mutate(engagement = retweet_count * 2 + favorite_count) %>%
    arrange(desc(engagement)) %>%
    slice(1)
  
  txt <- paste0("Le tweet original le plus populaire du ",duJour," a \u00E9t\u00E9 \u00E9crit par ",
                best$Nom,
                " (@",best$screen_name,"), avec un score de ",best$engagement, " (Stat: nb favs + 2 x nb retweets). #President #Gouvernement #DirectAN")

  ##################################
  # Tweet 2
  ##################################
  
  baseGraph <- selectedTweets %>%
  group_by(screen_name) %>%
  summarize(engagement = (sum(retweet_count) * 2 + sum(favorite_count)) / n(),
            nn=n(),
            .groups = 'drop') %>%
  arrange(desc(engagement)) %>%
  slice(1:10) %>%
  left_join(Twitos %>% select(screen_name, Nom, screenNameLink, picture), by="screen_name") %>%
  mutate(
    stat = engagement / max(engagement) * 100,
    profile_image = paste0('<img src=\"deputweets//www//',screen_name,'.jpg\" height=\"40px\" alt=\"Pas de Photo\"></img>'),
    bar = linkSVG(stat, col= BootstrapColorBasic),
    engagement = paste0(nn*round(engagement)," / ",nn," = ",round(engagement)),
  ) 

  theBest <- baseGraph %>% slice(1) %>% pull(screen_name)
  theBestTweets <- selectedTweets %>%
    filter(screen_name == theBest) %>%
    mutate(engagement = retweet_count * 2 + favorite_count) %>%
    arrange(desc(engagement))
  
  graph <- baseGraph %>%
    select(profile_image, Nom, engagement, bar) %>%
    kbl(
      format = "html",
      escape = FALSE,
      align = "cll",
      col.names = c("", "Nom", "", "Score/Tweet"),
      table.attr = 'class = "table"'
    ) %>%
    kable_styling(bootstrap_options = c("condensed"), full_width = FALSE) %>%
    save_kable(file = "webshot.html", self_contained = T)

  screen_name <- paste0("@", baseGraph$screen_name[1:10], collapse = " ")
  
  txt2 <- paste0("Top ten du ",duJour, " (Stat: nb favs + 2 x nb retweets)/(nb tweets) pour les tweets originaux. ",
                screen_name)
  if(nchar(txt2) < 245) txt2 <- paste0(txt2," #President #Gouvernement #DirectAN")
  
#  con <- file("webshot.html", open = "w+", encoding = "latin1")
#  writeLines(graph, con = con)
#  close(con)
  
  webshot("webshot.html", file = "webshot.png", vwidth=500, vheight=500)
  
  ##################################
  # Post
  ##################################
  # token <- create_token(
  #   app = appD,
  #   consumer_key = api_keyD,
  #   consumer_secret = api_secret_keyD,
  #   access_token = access_tokenD,
  #   access_secret = access_token_secretD,
  #   set_renv = FALSE)
  
  # token <- get_token()

  # Best Of
  post_tweet(status= paste(txt, best$status_url), token = token)
  
  # 10 best of
  post_tweet(status= txt2, media = "webshot.png", token = token) 
  # And the tweets
  n <- nrow(theBestTweets)
  for(i in 1:n){
    Sys.sleep(10) # Just in case...
    my_timeline <- get_timeline(rtweet:::home_user())
    reply_id <- my_timeline$status_id[1]
    post_tweet(paste0("Tweets du #1 (",i,"/",n,") : ",
                      theBestTweets$status_url[i]),
             in_reply_to_status_id = reply_id)
  }

  } else cat("It is not the right Time/n")

sink()
