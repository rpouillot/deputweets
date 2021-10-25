


processWords <- function(tweet){
  tmp <- gsub("aujourd'hui", "aujourdhui", tweet$text)
  #Remove \n et '
  tmp <- gsub("\n|’|\u0092", " ", tmp)
  tmp <- gsub("[’\"]", " ", tmp)
  tmp <- gsub("\\\\", " ", tmp)
  tmp <- gsub("aujourdhui", "aujourd'hui", tmp)
  # strsplit             
  tmp <- (strsplit(tmp," ")) 
  # links
  links <- lapply(tmp, grep, pattern="https://t.co", value=TRUE)
  #  tmp <- lapply(tmp, function(x) x[grep(pattern="https://t.co",x,invert=TRUE)])
  # @
  refer <- sapply(tweet$mentions_screen_name, paste, collapse = " ")
  refer[refer == "NA"] <- ""
  #  tmp <- lapply(tmp, function(x) x[grep(pattern="@",x,invert=TRUE)])
  # #
  hashtag <- sapply(tweet$hashtags, paste, collapse = " ")
  hashtag[hashtag == "NA"] <- ""
  
  tmp <- lapply(tmp, function(x) gsub(pattern="#",replace="", x=x))
  
  tmp <-   lapply(tmp, gsub, pattern='[[:cntrl:]]',replacement='') ## Remove Controls and special characters
  regexp <- "\\?|!|:|'|\\\\|,|&amp|«|»|;"
  #  gsub(x="un, es+sai \ avec des - ? avec ! : '", regexp, "",perl=TRUE)
  tmp <-   lapply(tmp, gsub, pattern=regexp,replacement= '') ## Remove Punctuations
  tmp <-   lapply(tmp, gsub, pattern="-",replacement= '',fixed=TRUE) ## Remove Punctuations
  tmp <-   lapply(tmp, gsub, pattern="+",replacement= '',fixed=TRUE) ## Remove Punctuations
  tmp <-   lapply(tmp, gsub, pattern=".",replacement= '',fixed=TRUE) ## Remove Punctuations
  tmp <-   lapply(tmp, gsub, pattern="|",replacement= '',fixed=TRUE) ## Remove Punctuations
  tmp <-   lapply(tmp, tolower) ## Remove Punctuations
  
  stop <- c(stopwords("french"), "a")
  tmp <- lapply(tmp, gsub, pattern = paste0("\\b(",paste(stop, collapse="|"),")\\b"), 
                replacement = "")
  #  tmp <- lapply(tmp, stemDocument, language = "french")
  tmp <- sapply(tmp, paste, collapse=" ")
  
  return(data.frame(status_id=tweet$status_id, word=tmp, refer=refer, hashtag=hashtag,
                    stringsAsFactors = FALSE))
}


# Textprocessing <- function(x)
# {gsub("http[[:alnum:]]*",'', x)
#   gsub('http\\S+\\s*', '', x) ## Remove URLs
#   gsub('\\b+RT', '', x) ## Remove RT
#   gsub('#\\S+', '', x) ## Remove Hashtags
#   gsub('@\\S+', '', x) ## Remove Mentions
#   gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
#   gsub("\\d", '', x) ## Remove Controls and special characters
#   gsub('[[:punct:]]', '', x) ## Remove Punctuations
#   gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
#   gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
#   gsub(' +',' ',x) ## Remove extra whitespaces
# }

# wc <- function(tweets = tmls, TwitosNames = Twitos$screen_name){
#   tweetText <- tweets %>% 
#     filter(screen_name %in% TwitosNames) %>%
#     select(text)
#   docs <- Corpus(VectorSource(tweetText))
#   #cleaning here
#   # toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
#   # docs <- tm_map(docs, toSpace, "/")
#   # docs <- tm_map(docs, toSpace, "@")
#   # docs <- tm_map(docs, toSpace, "\\|")
#   
#   # Convert the text to lower case
#   docs <- tm_map(docs, content_transformer(tolower))
#   # Remove french common stopwords
#   docs <- tm_map(docs, removeWords, stopwords("french"))
#   # specify your stopwords as a character vector
#   docs <- tm_map(docs, removeWords, toBeRemoved) 
#   # Remove punctuations
#   docs <- tm_map(docs, removePunctuation)
#   # Eliminate extra white spaces
#   docs <- tm_map(docs, stripWhitespace)
#   # Text stemming (garde les racines)
#   # docs <- tm_map(docs, stemDocument,language = "french" )
#   dtm <- TermDocumentMatrix(docs)
#   m <- as.matrix(dtm)
#   v <- sort(rowSums(m),decreasing=TRUE)
#   d <- data.frame(word = names(v),freq=v)
#   head(d, 10)
#   set.seed(1234)
#   wordcloud(words = d$word, freq = d$freq, min.freq = 1,
#             max.words=200, random.order=FALSE, rot.per=0.35, 
#             colors=brewer.pal(8, "Dark2"))
# }

