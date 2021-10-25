library("lubridate")
library("tm")
library("DT")
library("rtweet")
library("utils")
library("RCurl")
library("qdapRegex")
library("magrittr")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("visNetwork")
library("plotly")
library("tidyverse")
library("shiny")
library("shinyWidgets")
library("shinydashboard")
library("scales")
library("shinycssloaders")
library("kableExtra")

if(Sys.info()[1] == "Windows") {
  setwd("C:/Users/rpoui/OneDrive/ShinyApps/Twitter/deputweets")
  verbose <- FALSE
  Sys.setenv(TZ='Europe/Paris')
} else { #Linux
  verbose <- FALSE
  
  #  dirData <- "//home//rpouillot//autoLoad//data//"
}

# To be updated

listGroup <- list("Tous"= "Tous",
                  "Agir ensemble" = "AE",
                  #                  "Écologie démocratie solidarité" = "EDS",
                  "Gauche démocrate et républicaine"= "GDR",
                  "La France insoumise" = "LFI",
                  "Les Républicains" = "LR",
                  "La République en marche" = "LREM",
                  "Libertés et territoires" = "LT",
                  "Mouvement démocrate et apparentés" = "MODEM",
                  "Non-inscrits" = "NI",
                  "Socialistes et apparentés" = "SOC",   
                  "UDI et Indépendants" = "UDI"
)

refShape <- data.frame(label =  c("AE",
                                  #"EDS",
                                  "GDR","LFI","LR","LREM","LT","MODEM","NI","SOC","UDI","GVT","PDT","Autre"),
                       color = c("rgba(173,193,253)",
                                 #"rgba(110,208,134)",
                                 "rgba(221,0,0)","rgba(204,36,67)","rgba(0,102,204)",
                                 "rgba(255,235,0)","rgba(245,178,255)","rgba(255,128,28)",
                                 "rgba(170,177,183)","rgba(255,128,28)","rgba(0,255,255)","brown","brown","rgba(0,0,0)"),
                       font.color=c("black"),
                       shape=c(rep("dot",10),"triangle","square","star"),
                       value=10,
                       stringsAsFactors = FALSE)


BASIC_COLORS <- c("primary", "info", "success", "danger", "warning")

mypalette10 <-brewer.pal(10,"Paired")
BootstrapColorBasic <- c(Primary = "#0275d8", Success = "#5cb85c",
                          Info = "#5bc0de", Warning = "#f0ad4e", Danger="#d9534f") 


formatDateTimeFr <- function(x, what) {
  # what is "dt" fro date/time, "d" for date and "t" for time
  x <- as.POSIXlt(x, "Europe/Paris")
  date <- paste(substr(x,9,10),substr(x,6,7),substr(x,1,4), sep="/")
  time <- substr(x,12,16)
  switch(what, dt = paste(date,"à",time),
               d = date,
               t = time)
}

tweets_in_last <- function(tweets, d = 0, h = 0, m = 0, s = 0) {
  tweets %>%
    filter(created_at >= lubridate::now() - lubridate::hours(h + d * 24) -
             lubridate::minutes(m) - lubridate::seconds(s))
}

tweets_period <- function(tweets, 
                          from = today(tzone = "Europe/Paris"), 
                          to =   today(tzone = "Europe/Paris"),
                          tz = "Europe/Paris") {
  # tweets during a given period of time in "Europe". By default: today
  from <- with_tz(ymd_hms(paste(from, "00:00:00"), tz = tz ), tzone = "UTC")  
  to <-   with_tz(ymd_hms(paste(to, "23:59:59"),   tz = tz ), tzone = "UTC")  
  tweets %>%
    filter(created_at >= from, created_at <= to)
}

blockQuote <- function(tweet, null_on_error = TRUE, theme = "light", numbered = FALSE) {
  oembed_url <- glue::glue("https://publish.twitter.com/oembed?url=https://twitter.com/{tweet$screen_name}/status/{tweet$status_id}&omit_script=0&dnt=1&theme={theme}") 
  bq <- possibly(httr::GET, list(status_code = 999))(URLencode(oembed_url))
  if (bq$status_code >= 400) {
    if (null_on_error) return(NULL)
    '<blockquote style="font-size: 90%">Sorry, unable to get tweet ¯\\_(ツ)_/¯</blockquote>'
  } else {
    res <- if(numbered) paste0("#",tweet$nn," ") else ""
    res <- if(tweet$is_retweet) paste0(res, " (Retweeté par ", tweet$name,")<br>") else paste0(res,"<br>")
    paste0(res, httr::content(bq, "parsed")$html)
  }
}


linkSVG <- function(val, height=20, width=200, max = 100, col = "blue") {
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

makeLink <- function(text,url) paste0('<a href="',url,'" target="_blank">',text,'</a>')

makeLinkTweet <- function(liens){
  x <- paste0(
        '<blockquote class = "twitter-tweet" id = "tweet" data-lang="fr">',
        liens,
        '</blockquote>'
      )
  x[length(x)] <- paste0(x[length(x)], '<script>twttr.widgets.load(document.getElementById("tweet2"));</script>')
  return(x)
}


urlFrance <- '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'

Twitos <- readRDS("data//Twitos.rds") 

listRegions <- Twitos %>% filter(!is.na(region)) %>% distinct(region) %>% arrange(region) %>% pull
listCommission <- c("Toutes", sort(unique(Twitos$commission_permanente)))
listDeputes <- sort(unique(Twitos$screen_name))

#Tweets <- readRDS("data//Tweets.rds")


refShapeGraph <- data.frame(label =  c("Député","Président","Gouvernement","Autre"),
                       shape=c("dot","square","triangle","star"),
                       value=10,
                       stringsAsFactors = FALSE)

unaccent <- function(text) {
  text <- gsub("[éèê]", "e", text)
  text <- gsub("[ùúû]", "u", text)
  text <- gsub("[î]", "i", text)
  text <- gsub("ç", "c", text)
  return(text)
}

buildFreq <- function(x, case = TRUE, accent=FALSE){
   x <- paste(x, collapse=" ") %>%
      strsplit(split = " ") %>%
      .[[1]] %>%
      .[!(. %in% c(""," "))] 
   if(!case) x <- tolower(x)
   if(!accent) x <- unaccent(x)
  x <- table(x)
  as_tibble(list(word = names(x), freq = as.vector(x))) %>%
    arrange(desc(freq))
}

# Build the data for an history of hashtag
historyHashtag <- function(selectedHashtag, selected) {   
  
  selected %<>% select(status_id, screen_name, Nom, hashtag, created_at, 
                       retweet_status_id,  retweet_created_at,
                       status_url) %>%
    mutate(hashtag = strsplit(hashtag, split = " "))
  # Tweets with the selected hashtag
  quelTweets <- as.vector(sapply(selectedHashtag, match, x=selected$hashtag, nomatch=0) > 0)
  
  selected %<>% slice(which(quelTweets)) %>% 
    arrange(created_at) 
  
  newSelected <- x <- xend <- y <- yend <- col <- NULL  
  n <- 1
  
  while(nrow(selected) > 0){
    if(is.na(selected$retweet_status_id[1])){ #Original tweet, might be retweeted
      quel <- which(selected$retweet_status_id == selected$status_id[1])
      newSelected <- rbind(newSelected, slice(selected, c(1, quel)))
      lquel <- length(quel)
      
      if(lquel > 0){
        x <- c(x, rep(selected$created_at[1], 1 + lquel))
        xend <- c(xend, selected$created_at[1], selected$created_at[quel])
        y <- c(y, n, (n+1):(n+lquel))
        yend <- c(yend, n+lquel, (n+1):(n+lquel))
        col <- c(col, rep("Interne à la base Deputweets",1+lquel))
      }
      
      n <- n + 1 + lquel
      selected <- slice(selected, -c(1, quel))
    } else { #retweet, maybe from outside
      quel <- which(selected$retweet_status_id == selected$retweet_status_id[1])
      newSelected <- rbind(newSelected, slice(selected, quel))
      lquel <- length(quel)
      
      if(lquel > 0){
        x <- c(x, selected$retweet_created_at[1], selected$retweet_created_at[quel])
        xend <- c(xend, selected$retweet_created_at[1], selected$created_at[quel])
        y <- c(y, n, (n):(n+lquel-1))
        yend <- c(yend, (n+lquel-1), (n):(n+lquel-1))
        col <- c(col, rep("Externe de la base Deputweets",1 + lquel))
      }
      
      n <- n + lquel
      selected <- slice(selected, -quel)
      
    }
  }
  
  forArrow <- tibble(x=as_datetime(x),xend=as_datetime(xend),y=y,yend=yend, col=col)
  newSelected %<>% mutate(id=row_number(),
                          image=paste0(".//www//", screen_name, ".jpg")) 
  
  return(list(forArrow=forArrow,newSelected=newSelected))
}

