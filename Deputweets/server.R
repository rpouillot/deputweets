


shinyServer(function(input, output) {
  
  is_mobile_device <- reactive(isTRUE(input$is_mobile_device))
  
  #Check every 10 minutes
  Twitos <- reactiveFileReader(1000 * 60 * 10, NULL, "data//Twitos.rds", readRDS)
  processedTweets <- reactiveFileReader(1000 * 60 * 10, NULL, "data//processedTweets.rds", readRDS)
  #processedTweets <- function() readRDS("data//processedTweets.rds")
  
  
  maxDate <- reactive({max(date(with_tz(processedTweets()$created_at,"Europe/Paris")))})
  minDate <- reactive({min(date(with_tz(processedTweets()$created_at,"Europe/Paris")))})
  
  
  # DashBoard
  
  output$total_today <- renderValueBox({
    valueBox(
             value = format(nrow(selectedTweets()), big.mark = " "), 
             subtitle = "Tweets (ou reTweets) séléctionnés",
             color = "purple",
             icon = icon("comments")
    )
  })
    
  output$total_today_ori <- renderValueBox({
    valueBox(
      value = selectedTweets() %>% filter(!is_retweet) %>% nrow %>% format(big.mark = " "), 
      subtitle = "Tweets originaux séléctionnés",
      color = "orange",
      icon = icon("comment")
    )
  })
  
  output$tweeters_today <- renderValueBox({
    valueBox(
      value = length(unique(selectedTweets()$screen_name)), 
      subtitle = "Nombre Tweeters séléctionnés ayant écrit durant la période",
      color = "red",
      icon = icon("user-circle")
    )
  })
  

  output$dash_most_liked <- renderUI({
    shiny:::validate(need(nrow(selectedTweets()) > 0, "Pas de tweet séléctionné"))

    selectedTweets() %>%
      arrange(desc(favorite_count)) %>%
      slice(1) %>%
      blockQuote %>% HTML
  })

  output$dash_most_eng <- renderUI({
    shiny:::validate(need(nrow(selectedTweets()) > 0, "Pas de tweet séléctionné"))
    selectedTweets() %>%
      filter(!is_retweet) %>%
      mutate(engagement = 2*retweet_count + favorite_count) %>%
      arrange(desc(engagement)) %>%
      slice(1) %>%
      blockQuote %>% HTML
  })
  
  output$dash_most_rt_ori <- renderUI({
    shiny:::validate(need(nrow(selectedTweets()) > 0, "Pas de tweet séléctionné"))
    selectedTweets() %>%
      filter(!is_retweet) %>%
      arrange(desc(retweet_count)) %>%
      slice(1) %>%
      blockQuote %>% HTML
  })
  
  
  output$top_tweeters_quant <- renderUI({
    shiny:::validate(need(nrow(selectedTweets()) > 0, "Pas de tweet séléctionné"))
    
    selectedTweets() %>%
      group_by(screen_name,Nom) %>%
      summarize(Nb = n()) %>%
      arrange(desc(Nb)) %>%
      ungroup() %>%
      slice(1:10) %>%
      left_join(Twitos() %>% select(screen_name, screenNameLink, picture), by="screen_name") %>%
      mutate(
        stat = Nb / max(Nb) * 100,
        profile_image = gsub("73px","20px",picture),
        bar = linkSVG(stat, col= BootstrapColorBasic)
      ) %>%
      select(profile_image, screenNameLink, Nb, bar) %>%
      knitr::kable(
        format = "html",
        escape = FALSE,
        align = "cll",
        col.names = c("", "Nom", "","Nombre de Tweets/reTweets "),
        table.attr = 'class = "table"'
      ) %>%
      HTML()
  })

  output$top_tweeters_quant_ori <- renderUI({
    shiny:::validate(need(nrow(selectedTweets()) > 0, "Pas de tweet séléctionné"))
    
    selectedTweets() %>%
      filter(!is_retweet) %>%
      group_by(screen_name,Nom) %>%
      summarize(Nb = n()) %>%
      arrange(desc(Nb)) %>%
      ungroup() %>%
      slice(1:10) %>%
      left_join(Twitos() %>% select(screen_name, screenNameLink, picture), by="screen_name") %>%
      mutate(
        stat = Nb / max(Nb) * 100,
        profile_image = gsub("73px","20px",picture),
        bar = linkSVG(stat, col= BootstrapColorBasic)
      ) %>%
      select(profile_image, screenNameLink, Nb, bar) %>%
      knitr::kable(
        format = "html",
        escape = FALSE,
        align = "cll",
        col.names = c("", "Nom", "","Nombre de Tweets "),
        table.attr = 'class = "table"'
      ) %>%
      HTML()
  })
  
  
  
  output$top_tweeters_qual <- renderUI({
    shiny:::validate(need(nrow(selectedTweets()) > 0, "Pas de tweet séléctionné"))
   
    selectedTweets() %>%
      filter(!is_retweet) %>%
      group_by(screen_name,Nom) %>%
      summarize(engagement = (sum(retweet_count) * 2 + sum(favorite_count)) / n()) %>%
      arrange(desc(engagement)) %>%
      ungroup() %>%
      slice(1:10) %>%
      left_join(Twitos() %>% select(screen_name,screenNameLink, picture), by="screen_name") %>%
      mutate(
        stat = engagement / max(engagement) * 100,
        profile_image = gsub("73px","20px", picture),
        bar = linkSVG(stat, col= BootstrapColorBasic),
        engagement = round(engagement)
      ) %>%
      select(profile_image, screenNameLink, engagement, bar) %>%
      knitr::kable(
        format = "html",
        escape = FALSE,
        align = "cll",
        col.names = c("", "Nom", "", "Engagement/Tweet "),
        table.attr = 'class = "table"'
      ) %>%
      HTML()
  })
  
  output$top_words <- renderUI({
    shiny:::validate(need(nrow(selectedTweets()) > 0, "Pas de tweet séléctionné"))
  
    selectedTweets()$word %>%
        buildFreq(input$caseSens,input$unaccent) %>% 
        arrange(desc(freq)) %>%
        slice(1:10) %>%
        mutate(
          stat = freq  / max(freq) * 100,
          bar = linkSVG(stat, col= BootstrapColorBasic, width = 150)
      ) %>%
      select(word, freq, bar) %>%
      knitr::kable(
        format = "html",
        escape = FALSE,
        align = "cll",
        col.names = c("Mot", "Fréq.", ""),
        table.attr = 'class = "table"'
      ) %>%
      HTML()
  })

  output$top_hashtags <- renderUI({
    shiny:::validate(need(nrow(selectedTweets()) > 0, "Pas de tweet séléctionné"))
    
    selectedTweets()$hashtag %>%
      buildFreq(input$caseSens,input$unaccent) %>% 
      arrange(desc(freq)) %>%
      slice(1:10) %>%
      mutate(
        word = paste0("#",word),
        stat = freq  / max(freq) * 100,
        bar = linkSVG(stat, col= BootstrapColorBasic, width = 150)
      ) %>%
      select(word, freq, bar) %>%
      knitr::kable(
        format = "html",
        escape = FALSE,
        align = "cll",
        col.names = c("Hashtag", "Fréq.", ""),
        table.attr = 'class = "table"'
      ) %>%
      HTML()
  })
  
  output$top_mentions <- renderUI({
    shiny:::validate(need(nrow(selectedTweets()) > 0, "Pas de tweet séléctionné"))
    
    selectedTweets()$refer %>%
      buildFreq(input$caseSens,input$unaccent) %>% 
      arrange(desc(freq)) %>%
      slice(1:10) %>%
      mutate(
        word = paste0("@",word),
        stat = freq  / max(freq) * 100,
        bar = linkSVG(stat, col= BootstrapColorBasic, width = 150)
      ) %>%
      select(word, freq, bar) %>%
      knitr::kable(
        format = "html",
        escape = FALSE,
        align = "cll",
        col.names = c("Mention", "Fréq.", ""),
        table.attr = 'class = "table"'
      ) %>%
      HTML()
  })

  
    #################### SELECTIONS
  
  # Build Dynamiccaly the UI for the calendar
  output$calendar <- renderUI({
    validate(need(maxDate(),"wait"))
    dateRangeInput("minDateGraph", "Date considérées", 
                 start = maxDate(), #-60*60*24,
                 end = maxDate(),
                 min = minDate(), 
                 max = maxDate(),
                 format = "dd-mm-yyyy", startview = "month", weekstart = 0,
                 language = "fr", width = NULL)
  })
  
  # Build Dynamiccaly the UI for the deputes
  output$pickOneDynamic <- renderUI({
    res <- Twitos()
#    browser()
    #Seulement Gvt
    if(input$selectGouv == "Gvt") res %<>% filter(DepuGouv %in% c("Président","Gouvernement")) else { 
      # Tous ou Dep
      if(input$selectRegion != "Toutes") res %<>% filter(region %in% input$selectRegion)
      if(input$selectGroup != "Tous") res %<>% filter(groupe_sigle %in% input$selectGroup)
      if(input$selectCommission != "Toutes") res %<>% filter(commission_permanente %in% input$selectCommission)
        #Dep
        if(input$selectGouv == "Dep") { res %<>% filter(DepuGouv == "Député")
        }
    }
         
    if(input$selectSex != "Tous") res %<>% filter(sexe %in% input$selectSex)

    noms <- res$name
    
    pickerInput(
      inputId = "pickOne", 
      label = "Sélectionnez/Désélectionnez", 
      choices = noms, 
      selected = noms,
      options = pickerOptions(
        actionsBox = TRUE, 
        size = 10,
        selectedTextFormat = "count > 3",
        deselectAllText = "Désélectionnez tout",
        selectAllText = "Sélectionnez tout",
        countSelectedText = "{0} Tweeter(s)"
      ),
      choicesOpt = list(
        style = rep("color: black; background: white",620)
        ),
      multiple = TRUE
    )
  })
  
  #Select Deputes
  selectedDeputes <- reactive({
    if(verbose) cat("I re select the Deputes\n")
    if(is.null(input$pickOne)) return(NULL)
    Twitos() %>% filter(name %in% input$pickOne)
  })
  
  #Select Tweets from Selected Deputes / selected time
  selectedTweets <- reactive({
    if(verbose) cat("I re select the Tweets\n")
    tmp <- processedTweets() %>% filter(screen_name %in% selectedDeputes()$screen_name)
    if(input$excludeRetweet) tmp %<>% filter(!is_retweet)     
    #cat(input$minDateGraph)

    if(input$depuisQuand == "today") { tmp %<>% tweets_period() 
    } else if(!is.na(as.integer(input$depuisQuand))) { tmp %<>% tweets_in_last(h = as.numeric(input$depuisQuand)) 
    } else if(input$depuisQuand == "Choix") {     tmp  %<>% tweets_period(input$minDateGraph[1], 
                                                                          input$minDateGraph[2])
        # filter(floor_date(created_at,"day") >=   as.Date(input$minDateGraph[1], origin = "1970-01-01"),
        #        floor_date(created_at,"day") <=   as.Date(input$minDateGraph[2], origin = "1970-01-01")) 
    }
    
    tmp  %>% arrange(desc(created_at))
    
  })

  
  #################### PRINT

  output$rangeTweets <- output$rangeTweets2 <- renderText({
    paste0("La base de données complète avant sélection inclut les Tweets et reTweets du président, des membres du gouvernement et des députés émis du ",
           formatDateTimeFr(min(processedTweets()$created_at),"dt")," au ",  formatDateTimeFr(max(processedTweets()$created_at),"dt"), " (heure Paris), soit ", 
           format(nrow(processedTweets()), big.mark = " ")," Tweets. La dernière vérification a été effectuée le ",
           formatDateTimeFr(file.info(file.path("data//processedTweets.rds"))$mtime,"dt"), " (heure Paris). L'application est actualisée toutes les heures. ",
           "Tous les jours, entre minuit et une heure, l'application reTweet le tweet le plus populaire ainsi que le top 10 sur le compte <a href='https://twitter.com/deputweets'>@deputweets</a>.")  
  })

  output$nbTweets <- renderText({
    paste0(nrow(selectedTweets()), " Tweets.")  
  })
  
  #################### TABLE DEPUTES
  
  output$tableDeputes <- DT::renderDataTable({
    shiny:::validate(
      need(nrow(selectedDeputes()) > 0, "Sélectionnez au moins un député.")
    )
#browser()
    forTable <- selectedDeputes() %>%
      mutate(DepuGouv=substr(DepuGouv,1,1),
             screenNameLink = ifelse(screenNameLink == "",Nom,screenNameLink)) %>%
      select(
        picture,
        screenNameLink,
#        Nom,
        DepuGouv,
#        URLLink,
#        FBLink,
        groupe_sigle,
        circo,
        region,
        description,
#        location,
        account_created_at,
        
        last_textLink,
        last_created_at,
        
        followers_count,
        friends_count,
        listed_count,
        user_favourites_count,
        statuses_count) %>%
      mutate(account_created_at = formatDateTimeFr(account_created_at,"d"),
             last_created_at = formatDateTimeFr(last_created_at,"dt")) %>%
      mutate_at(c("followers_count", "friends_count", "listed_count",
                  "user_favourites_count", "statuses_count"), format, big.mark="&#160;")
    
    datatable(forTable,
              rownames = NULL,
              colnames= c("",
                          "Nom",
                          "P/G/D",
#                          "Compte Twitter",
#                          "Site",
#                          "FB",
                          "Groupe",
                          "Circ.",
                          "Région",
                          "Description Twitter",
#                          "Ville",
                          "Compte crée le",
                          "Dernier tweet/reTweet",
                          "Ecrit le",
                          "Suivi par",
                          "Suit",
                          "Listes suivies",
                          "Nombre de Tweets favoris",
                          "Nombre de Tweets (inclus reTweets)" ),
              options = list(pageLength = min(nrow(forTable),25),
                             language = list(url = urlFrance),
                             autoWidth = TRUE
                             ),
              escape = FALSE
    ) 
  })
  
  
  #################### TABLE LEXIQUE
  # Selectionne data panel
  
  terms1 <- reactive({
    shiny:::validate(need(nrow(selectedTweets()) > 0,"Aucun Tweet sélectionné"))
    selectedTweets()$word %>% buildFreq(input$caseSens,input$unaccent)  })

  output$tableWords1 <- DT::renderDataTable(
    terms1() %>% rename("Fréquence"=freq),
    options = list(pageLength = 50,language = list(
      url = urlFrance))
  )
  
  output$hashtag1 <- DT::renderDataTable({
    shiny:::validate(need(nrow(selectedTweets()) > 0,"Aucun Tweet sélectionné"))
    selectedTweets()$hashtag %>%  buildFreq(input$caseSens,input$unaccent) %>%
      mutate(word=paste0("#",word)) %>% rename("Hashtag"=word, "Fréquence"=freq)
    },
    options = list(pageLength = 50,language = list(
                                   url = urlFrance))
  )
  
  output$nametag1 <- DT::renderDataTable({
    shiny:::validate(need(nrow(selectedTweets()) > 0,"Aucun Tweet sélectionné"))
    selectedTweets()$refer %>% buildFreq  %>%
      mutate(word=paste0("@",word)) %>% rename("Mention"=word, "Fréquence"=freq)
    },
    options = list(pageLength = 50,language = list(
                                   url = urlFrance))
  )
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$wordCloud1 <- renderPlot({
    
    wordcloud_rep(words = terms1()$word, freq = terms1()$freq, 
                  max.words=input$max,
                  random.order=FALSE, rot.per=0.35, 
                  colors=brewer.pal(8, "Dark2"),scale=c(4,.5))
  }, width=500, height=500)
  
  
  #################### TABLE LEXIQUE

  output$plot_history <- renderPlot({
    
    shiny:::validate(need(nrow(selectedTweets()) > 0,"Aucun Tweet sélectionné"))
    selectedHashtag <- "Bridgestone"
    
    baseHistory <- historyHashtag(selectedHashtag, selectedTweets())
    
    newSelected <- baseHistory$newSelected
    forArrow <- baseHistory$forArrow
    shiny:::validate(need(nrow(newSelected) > 0,"Aucun Tweet sélectionné"))
    
    scaleX <- diff(range(as.numeric(newSelected$created_at)))
    
    newSelected$onclick <- sprintf('window.open(\"%s\")',
                                   as.character(newSelected$status_url) ) 
    browser()
    p <- ggplot(newSelected, aes(y=id*5,x=created_at, label=Nom)) +
      geom_point(size=3) +
      geom_segment(data=forArrow,
                   aes(x=x,xend=xend,y=y*5,yend=yend*5,
                       label=NULL, colour = col),
                   size=1, linetype="twodash") +
      geom_text_interactive(aes(data_id = status_url,
                                onclick = onclick),
                            hjust="left",check_overlap = TRUE,nudge_x = .08*scaleX) +
      # geom_text() +
      # geom_image(aes(x=created_at+.04*scaleX,image=image)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
            legend.position = "top") +
      scale_x_datetime(timezone = "Europe/Paris", date_labels = "%d-%m-%y à %H:%m",
                       limits = range(newSelected$created_at)+c(0,.20*scaleX)) +
      scale_y_reverse() +
      ggtitle(paste0("#",selectedHashtag)) +
      scale_colour_discrete(type=c("blue","red"),name="Origine reTweet")
    
    girafe(ggobj = p)
    p
    })
  
  
  
  ##################################################### Tweets
  ##################################################### Tweets
  ##################################################### Tweets
  
  
  
  dataTweets <- reactive({
    shiny:::validate(need(nrow(selectedTweets()) > 0, "Aucun Tweet sélectionné"))
    selectedTweets() %>%
      dplyr:::arrange(desc(created_at)) %>%
      mutate(is_retweet = ifelse(is_retweet,"Oui","Non"),
             lien=makeLink("lien", status_url),
             created_at = formatDateTimeFr(created_at,"dt")) %>%
      select(Nom = name,
             Tweet=textLink,
             Lien=lien,
             Retweeté=retweet_count,
             Répondu=reply_count,
             "Est ce un Retweet"=is_retweet,
             "Si oui, de"= retweet_name,
#             "Lieu"= location,
             "Crée le"= created_at)
    }) 
    
  
    output$unTwitos <- DT::renderDataTable({
      shiny:::validate(need(nrow(dataTweets()) > 0,"Aucun Tweet sélectionné"))
      datatable(dataTweets(), 
                escape=FALSE,
                selection = "none",
                options = list(pageLength = 100,
                               language = list(url = urlFrance))
      )
  })
  
  observeEvent(input$unTwitos_cell_clicked, { #list(row = row_index, col = column_index, value = cell_value)

    info <- input$unTwitos_cell_clicked
    if (is.null(info$value)) return() # || info$col != 3
    data <- selectedTweets() %>%
      arrange(desc(created_at)) %>%
      slice(info$row)
    showModal(
      modalDialog(
      HTML(
        paste0(
#          "Cliquez en dehors de cette fenêtre ou sur la porte pour revenir à l'application",
          # '<blockquote class = "twitter-tweet" id = "tweet" data-lang="fr">',
          # findTweet,
          # '</blockquote>
          #  <script>twttr.widgets.load(document.getElementById("tweet2"));</script>'
          blockQuote(data)
      )
    ),
      footer = modalButton(label = "", icon = icon("door-open")),
      easyClose=TRUE))
  })
  
  ##################################################### Mur de Tweets
  ##################################################### Mur de Tweets
  ##################################################### Mur de Tweets
  
  #Create multiple button
  vals <- reactiveValues(data = "favorite_count") # Initialize vals$data
  
  observeEvent(input$MFav, vals$data <- "favorite_count")
  observeEvent(input$MRetweet, vals$data <- "retweet_count")
  observeEvent(input$MRecent, vals$data <- "created_at")

  liensTweets <- reactive({
    
    shiny:::validate(need(nrow(selectedTweets()) > 0, "Aucun Tweet sélectionné"))
    
    selection <- selectedTweets()
    if(vals$data == "favorite_count") selection %<>% arrange(desc(favorite_count)) else
      if(vals$data == "retweet_count") selection %<>% arrange(desc(retweet_count)) else
        selection %<>% arrange(desc(created_at)) 

    # selection %<>% slice(1:24) %>% blockQuote(numbered=TRUE)

    selection %<>% slice(1:24) %>% mutate(nn=row_number()) 
    sapply(1:24, function(x) blockQuote(slice(selection, x),numbered=TRUE)) 
  })
  
  # if mobile: 1:24 else 1;24 by step of 3
  output$b1 <- renderUI({
    #cat("mobile",is_mobile_device())
      liens <- if(is_mobile_device()) liensTweets()[seq(1,8)] else liensTweets()[seq(1,24,3)] 
      shiny:::validate(need(length(liens) > 0, ""))
      HTML(liens[!is.na(liens)])
  })
  
  output$b2 <- renderUI({
    liens <- if(is_mobile_device()) liensTweets()[seq(9,16)] else liensTweets()[seq(2,24,3)] 
    shiny:::validate(need(length(liens) > 0, ""))
    HTML(liens[!is.na(liens)])
  })
  
  output$b3 <- renderUI({
    liens <- if(is_mobile_device()) liensTweets()[seq(17,24)] else liensTweets()[seq(3,24,3)] 
    shiny:::validate(need(length(liens) > 0, ""))
    HTML(liens[!is.na(liens)])
  })
  
  
  
  
  
  ##################################################### Stat
  output$tableStat <- DT::renderDataTable({
    shiny:::validate(need(nrow(selectedTweets()) > 0,"Aucun Tweet sélectionné"))
    
    Data <- selectedTweets()
    # Trick
    Data$type <- if(input$gatherStat == "Tous") { "Tous" } else Data[[input$gatherStat]] 
    
    DataStat <- Data %>%
      inner_join(selectedDeputes() %>% select(screen_name, followers_count), by="screen_name") %>%
      group_by(type) %>%
      summarise(n = n(),
                nRet = sum(is_retweet),
                nFol = sum(followers_count),
                nFav = sum(favorite_count),
                nRetweete = sum(retweet_count)) %>%
      dplyr:::arrange(desc(n)) %>%
      mutate(wFav = round(nFav/nFol*100,2),
             wRet = round(nRetweete/nFol*100,2)) %>%
      select(type, n, nRet, nFol, nFav, wFav, nRetweete, wRet) %>%
      ungroup  
    
    DataStat %>%
      datatable(rownames=FALSE,
                colnames=c("Groupe/Individus",
                           "Nombre de Tweets",
                           "(dont nombre reTweets)",
                           "Nombre de Destinataires",
                           "Nombre de Favoris",
                           "% Favoris",
                           "Nombre de fois reTweetés",
                           "% reTweetés"
                ),
                escape=FALSE,
                options = list(pageLength = 100,
                               language = list(url = urlFrance))
      ) %>%
      formatCurrency(c(2,3,4,5,7),currency = "", interval = 3, mark = ".", digit=0)

  })
  
  ##################################################### Ligne de Temps
  output$tweetPerDay <- renderPlot({
    
    shiny:::validate(need(nrow(selectedTweets()) > 0,"Aucun Tweet sélectionné"))
    Data <- selectedTweets()
    Data$type <- if(input$gather == "Tous") { 1 } else Data[[input$gather]] 
 
    Data %<>%
      group_by(date = floor_date(created_at, input$parTime), type) %>%
      summarise(nbTweets=n(),
                nbRet = sum(is_retweet),
                nbFav = sum(favorite_count),
                nbRetweet = sum(retweet_count)) %>%
      ungroup
    Data$stat <- Data[[input$stat]] 
    
    x <- ggplot(Data, aes(x=date, y = stat, color=type)) +
      geom_line(size=1) +
      geom_point(size=2) +
      xlab('') + 
      ylab("Tweets") +
      theme_light() +
      theme(legend.title =element_blank(),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"),
            legend.position="top") 
    
      if(input$parTime == "hours") x <- x + scale_x_datetime(date_labels = "%d/%m %H:00")
      if(input$parTime == "days") x <- x + scale_x_datetime(date_labels = "%d/%m")
    
    if(input$gather == "Tous") x <- x + theme(legend.position = "none")
    
    x
  })
  
  ############################################## NETWORK Retweet
  output$Network <- renderVisNetwork({

    retweet <- selectedTweets() %>% filter(is_retweet)
    # if solo Deputes, filter only deputes
    if(input$soloDep == "Dep") retweet %<>% filter(retweet_screen_name %in% listDeputes) 
    else if(input$soloDep == "DepSel") retweet %<>% filter(retweet_screen_name %in% selectedDeputes()$screen_name) 
    
    shiny:::validate(need(nrow(retweet) > 0,'Aucun Tweet sélectionné. Décochez "Exclu reTweet"?'))
    
    showModal(modalDialog(title = "En construction...", "Patientez",footer = NULL))
    
    ID <- sort(unique(c(retweet$screen_name,retweet$retweet_screen_name)))
    
    # All ID retweeted
    fromRetweet <- retweet %>% 
      select(retweet_screen_name, retweet_name, retweet_followers_count) %>%
      distinct(retweet_screen_name, .keep_all = TRUE) %>%
      filter(!(retweet_screen_name %in% Twitos()$screen_name)) %>%
      mutate(DepuGouv = "Autre") %>%
      rename(screen_name = retweet_screen_name, name = retweet_name, followers_count = retweet_followers_count) %>%
      mutate(color = "black")
    
    # All ID retweeter
    fromDeputes <- Twitos() %>% 
      select(DepuGouv, screen_name, name, followers_count, groupe_sigle) %>%
      inner_join(tibble(ID),  by=c("screen_name" = "ID")) %>% 
      mutate(color = refShape$color[match(groupe_sigle, refShape$label)]) %>%
      select(-groupe_sigle)
    
    nodes <- rbind(fromDeputes,fromRetweet)  
    
    shape <- refShapeGraph$shape[match(nodes$DepuGouv, refShapeGraph$label)]
    
    nodes <- data.frame(id = 1:nrow(nodes), 
                        screen_name = nodes$screen_name,
                        label = nodes$name,
                        group = nodes$DepuGouv,
                        shape = shape,
                        color = nodes$color,
                        value = rescale(log(nodes$followers_count),to=c(10,30)),
                        stringsAsFactors = FALSE)

    from <- retweet %>% select(screen_name) %>%
      left_join(nodes, by="screen_name") %>%
      select(id) %>% pull
    to <- retweet %>% select(retweet_screen_name) %>%
      left_join(nodes, by=c("retweet_screen_name" = "screen_name")) %>%
      select(id) %>% pull
    edges <- data.frame(from = from, to = to )
    
    edges %<>% group_by(from, to) %>%
      summarize(title = n())
    
    graph <- visNetwork(nodes, edges) %>%
      visNodes(scaling=list(min=10, max=30)) %>%
      visEdges(arrows  = "to", smooth=TRUE) %>%
      visIgraphLayout(layout=input$Layout, randomSeed = 123, physics=input$dynRet) %>%
      visOptions(selectedBy = list(variable = "label", main="Sélectionnez"), 
                 nodesIdSelection = list(enabled = TRUE, values=sort(nodes$id[nodes$group != "Autre"]),
                                         main="Sélectionnez"),
                 highlightNearest = list(enabled = TRUE, degree=1, hover=FALSE)) %>%
      visLegend(width=.2, ncol=2, useGroups = FALSE, addNodes = refShape,zoom=FALSE)

        removeModal()
    graph
  })   
  
  ############################################## NETWORK Retweet
  output$NetworkMention <- renderVisNetwork({
    
    
    referTweet <- selectedTweets() %>% filter(refer != "") %>%
      select(screen_name, refer)
    
    if(input$soloDep2 == "Dep") referTweet %<>% filter(refer %in% listDeputes) 
      else if(input$soloDep2 == "DepSel") referTweet %<>% filter(screen_name %in% selectedDeputes()$screen_name) 
    
    # Trick for multiple mention
    lrefer <- strsplit(referTweet$refer, " ")
    nbRefer <- sapply(lrefer,length)
    lrefer <- unlist(lrefer)
    lscreen_name <- mapply(rep, referTweet$screen_name, nbRefer) %>% unlist
    names(lscreen_name) <- NULL
    
    shiny:::validate(need(length(lrefer) > 0,"Aucun Tweet sélectionné"))
    showModal(modalDialog(title = "En construction...", "Patientez",footer = NULL))
    on.exit(removeModal())
    
    ID <- sort(unique(c(lscreen_name, lrefer)))
    
    edges <-  data.frame(from = lscreen_name, to = lrefer ) %>%
      group_by(from, to) %>%
      summarize(title = n())
    
    nodes <- data.frame("screen_name" = ID, stringsAsFactors = FALSE) %>% 
      left_join(Twitos(), by="screen_name") %>%
      select(screen_name, name, groupe_sigle, followers_count, DepuGouv) %>%
      mutate(color = ifelse(is.na(name),"black", refShape$color[match(groupe_sigle, refShape$label)]),
             name = ifelse(is.na(name),screen_name, name),
             followers_count = ifelse(is.na(followers_count),1,followers_count),
             DepuGouv = ifelse(is.na(DepuGouv),"Autre",DepuGouv)
      )
   
   shape <- refShapeGraph$shape[match(nodes$DepuGouv, refShapeGraph$label)]
   
   nodes <- data.frame(id = nodes$screen_name, 
                        screen_name = nodes$screen_name,
                        label = nodes$name,
                        group = nodes$DepuGouv,
                        shape = shape,
                        color = nodes$color,
                        value = rescale(log(nodes$followers_count),to=c(10,30)),
                        stringsAsFactors = FALSE)
    
   visNetwork(nodes, edges) %>%
      visNodes(scaling=list(min=10, max=30)) %>%
      visEdges(arrows  = "to", smooth=TRUE) %>%
      visPhysics(stabilization = TRUE) %>%
      visIgraphLayout(layout=input$Layout, randomSeed = 123, physics=input$dynMen) %>%
      visOptions(selectedBy = list(variable = "label", main="Sélectionnez"), 
                 nodesIdSelection = list(enabled = TRUE, values=sort(nodes$id[!is.na(nodes$group)]),
                                         main="Sélectionnez"),
                 highlightNearest = list(enabled = TRUE, degree=1, hover=FALSE)) %>%
      visLegend(width=.2, ncol=2, useGroups = FALSE, addNodes = refShape,zoom=FALSE)
  })  
  
})
