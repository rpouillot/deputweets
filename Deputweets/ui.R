

# Define UI for application that draws a histogram
dashboardPage(
    # Dashboard Page Setup ----------------------------------------------------
    title = "Deputweets",
    skin  = "blue",
    #    theme = c(META$theme_css, "custom.css"),
    #    sidebar_mini = TRUE,
    dashboardHeader(
        title = "Deputweets"
    ),
    dashboardSidebar(
        tags$head(
            # Twitter
            tags$script('window.twttr = (function(d, s, id) {var js, fjs = d.getElementsByTagName(s)[0],t = window.twttr || {};if (d.getElementById(id)) return t;js = d.createElement(s);js.id = id;js.src = "https://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js, fjs);t._e = [];t.ready = function(f) {t._e.push(f);};return t;}(document, "script", "twitter-wjs"));'),
            HTML('<!-- Global site tag (gtag.js) - Google Analytics -->'),
            HTML('<script async src="https://www.googletagmanager.com/gtag/js?id=UA-117763910-1"></script>' ),
            includeScript("data//GoogleScript.js"),
            includeScript("data//mobile.js")
        ),
        
        #        width = "25%",
        #        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        h2("Sélection"),
        selectInput("depuisQuand", "Période",
                    choices = list("Aujourd'hui"="today",
                                   "Dernières 24 heures"= 24,
                                   "Dernières 48 h" = 48,
                                   "7 derniers jours" = 168,
                                   "Choisissez la date" = "Choix"),
                    selected = "24"
        ),
        conditionalPanel(
            condition = 'input.depuisQuand == "Choix"',
            uiOutput("calendar")
        ),
        selectInput("selectGouv",
                    "Députés / Président/Gouvernement:",
                    choices=list("Tous"="Tous", "Députés"="Dep", "Président/Gouvernement"="Gvt")
        ),
        conditionalPanel(
            condition = 'input.selectGouv == "Tous" || input.selectGouv == "Dep"',
            selectInput("selectGroup",
                        "Groupe parlementaire:",
                        choices=listGroup
            ),
            selectInput("selectCommission",
                        "Commission parlementaire:",
                        choices=listCommission
            ),
            selectInput("selectRegion",
                        "Région / COM:",
                        choices = c("Toutes", listRegions)
            )
        ),
        selectInput("selectSex",
                    "Genre:",
                    choices=list( "Tous"="Tous",
                                  "Masculin"="H", 
                                  "Féminin" = "F")
        ),
        uiOutput("pickOneDynamic"),
        checkboxInput("excludeRetweet","Exclu reTweet?"),
        conditionalPanel(
            condition = "input.tabs == 'Lexique'",
            checkboxInput("caseSens","Sensible à la casse?",TRUE),
            checkboxInput("unaccent","Sensible accent/cédille, ...?",TRUE)
        ),
        textOutput("nbTweets"),
        conditionalPanel(
            condition = "input.tabs == 'GraphRet' || input.tabs == 'GraphMen'",
            selectInput("Layout", "Disposition",
                        choices = list("Default"="layout_nicely",
                                       "Kamada-Kawai layout"="layout_with_kk",
                                       "Fruchterman-Reingold layout" = "layout_with_fr",
                                       "Large Graph layout" = "layout_with_lgl",
                                       #"Davidson-Harel layout" = "layout_with_dh",
                                       "graphopt layout" = "layout_with_graphopt",
                                       "Layout by multidimensional scaling" = "layout_with_mds",
                                       "Circle"="layout_in_circle")
            )
        )
    ), # End dashboardSidebar
    
    dashboardBody(
        fluidRow(box('Notre Président / Notre Gouvernement / Nos Deputés sur Twitter', 
                     # HTML("Cliquez sur&nbsp;"), icon("align-justify"), 
                     # HTML("&nbsp;dans le bandeau pour sélectionner."),
                     
                     width = 12, 
                     background="blue")),
        
        fluidRow(
            tabBox(id = "tabs",
                   width=NULL,
                   # Tableau de Bord - Start --------------------------------------
                   tabPanel(
                       title="Tableau de bord",
                       fluidRow(
                           # Frontpage - boxes - start -----------------------------------------------
                           valueBoxOutput("total_today",
                                          width = 4) ,
                           valueBoxOutput("total_today_ori",
                                          width = 4) ,
                           valueBoxOutput("tweeters_today",
                                          width = 4) 
                       ),
                       fluidRow(
                           # Frontpage - Most XX Tweets - start --------------------------------------
                           box(
                               width = 4,
                               offset = 2,
                               class = "text-center",
                               title = span(tagList("Tweet original avec le plus de ", icon("heart"))),
                               status="warning",
                               solidHeader = TRUE,
                               withSpinner(uiOutput("dash_most_liked"))
                               
                           ) ,
                           box(
                               width = 4,
                               offset = 2,
                               title = span(tagList("Tweet original avec le plus de ", icon('retweet'))),
                               status="primary",
                               solidHeader = TRUE,
                               withSpinner(uiOutput("dash_most_rt_ori"))
                           ) ,
                           box(
                               width = 4,
                               offset = 2,
                               title = span(tagList("Tweet original avec le plus d'influence directe")),
                               status="success",
                               solidHeader = TRUE,
                               helpText("Statistique : (2 x nombre de retweets + nombre de favs)."),
                               withSpinner(uiOutput("dash_most_eng"))
                           )
                       ),
                       fluidRow(
                           box(width=12,
                               title = "Données", 
                               background = "black",
                               htmlOutput("rangeTweets2")
                           )
                       )
                   ),
                   # Tableau de Bord - End --------------------------------------
                   
                   # Tableau d'honneur - Début --------------------------------------
                   tabPanel(
                       title="Tableau d'honneur",
                       # Frontpage - Most XX Tweets - end --------------------------------------
                       # Frontpage - Top - Start --------------------------------------
                       fluidRow(
                           box(
                               width = 4,
                               status = "primary",
                               title = "Tweeters ayant le plus d'influence directe*",
                               solidHeader = TRUE,
                               withSpinner(uiOutput("top_tweeters_qual")),
                               helpText("*Moyenne pondérée des RT (2x) et favoris (1x) par tweet original.")
                           ),
                           box(
                               width = 4,
                               status = "info",
                               title = 'Tweeters les plus prolifiques',
                               solidHeader = TRUE,
                               withSpinner(uiOutput("top_tweeters_quant")),
                               helpText("Nombre de Tweets ou reTweets durant la période.")
                           ),
                           box(
                               width = 4,
                               status = "warning",
                               title = 'Tweeters les plus prolifiques (mais originaux)',
                               solidHeader = TRUE,
                               withSpinner(uiOutput("top_tweeters_quant_ori")),
                               helpText("Nombre de Tweets originaux durant la période.")
                           )
                       )
                       # Frontpage - Top - End --------------------------------------
                       
                       
                   ),
                   # Tableau d'honneur - Fin --------------------------------------
                   
                   # Lexique - Début --------------------------------------
                   tabPanel("Lexique",
                            
                            fluidRow(          
                                box(
                                    width = 4,
                                    status = "success",
                                    title = "Hashtags",
                                    withSpinner(uiOutput("top_hashtags")),
                                    solidHeader = TRUE,
                                    helpText("Hashtags les plus employés parmi les tweets séléctionnés.")
                                ),
                                box(
                                    width = 4,
                                    status = "primary",
                                    title = "Mentions",
                                    solidHeader = TRUE,
                                    withSpinner(uiOutput("top_mentions")),
                                    helpText("Mentions les plus employées parmi les tweets séléctionnés.")
                                ),
                                box(
                                    width = 4,
                                    status = "warning",
                                    title = "Mots",
                                    solidHeader = TRUE,
                                    withSpinner(uiOutput("top_words")),
                                    helpText("Mots les plus employés parmi les tweets séléctionnés.")
                                )
                            ),
                            
                            fluidRow(
                                box(width=4,
                                    title = "Hashtag",
                                    dataTableOutput("hashtag1")
                                ),
                                box(width=4,
                                    title = "Mentions",
                                    dataTableOutput("nametag1")
                                ),
                                box(width=4,
                                    title = "Mots",
                                    dataTableOutput("tableWords1")
                                )
                            ),
                            fluidRow(
                                box(width=12,
                                    sliderInput("max",
                                                "Nombre maximum de mots dans le nuage:",
                                                min = 1,  max = 300,  value = 100),
                                    plotOutput("wordCloud1"),
                                    height  = 600
                                )
                            )
                   ),
                   # Lexique - Fin --------------------------------------
                   
                   # Test history hashtag - Début --------------------------------------
                   # tabPanel("Hashtag",
                   # 
                   #          fluidRow(width=12,
                   #                   plotOutput("plot_history",height = "1000px")
                   #          )
                   # ),
                   # Test history hashtag - Fin --------------------------------------

                   # Twitters - Début --------------------------------------
                   tabPanel(title = "Députés/Gouvernement", 
                            fluidRow(
                                # status = "primary",
                                # title = "Tweeters",
                                # solidHeader = TRUE,
                                 
                                #column(width=12,
                                    box(width=12,
                                        #withSpinner(DT::dataTableOutput("tableDeputes")))
                                    div(style = 'overflow-x: scroll', DT::dataTableOutput('tableDeputes'))
                                    )
                                #)
                           )
                   ),
                   # Twitters - Fin --------------------------------------
                   
                   
                   # Mur de Tweets - Début --------------------------------------
                   tabPanel("Mur de Tweets",
                            
                                     HTML("Tri:&nbsp;&nbsp;"),
                                     actionButton("MFav","+ de fav (défaut)",icon('heart')),
                                     actionButton("MRetweet","+ de retweet",icon('retweet')),
                                     actionButton("MRecent","+ récent",icon('clock')),
                            
                            
                            fluidRow(width=12,
                                     column(width=4,box(width=NULL,withSpinner(uiOutput("b1")))),
                                     column(width=4,box(width=NULL,withSpinner(uiOutput("b2")))),
                                     column(width=4,box(width=NULL,withSpinner(uiOutput("b3"))))
                            )
                   ),
                   # Mur de Tweets - Fin --------------------------------------
                   
                                      
                   # Tweets - Début --------------------------------------
                   tabPanel("Tableau de Tweets",
                             helpText("Classés par défaut du plus récent au plus ancien. Cliquez sur une colonne pour un autre ordre de tri. Cliquez sur une ligne pour voir le tweet correspondant"),
                            fluidRow(
                                box(width=12,
                                    div(style = 'overflow-x: scroll', DT::dataTableOutput('unTwitos'))
                                )
                            )
                   ),
                   # Mur de Tweets - Fin --------------------------------------
                   
                   # Graph Stats - Début ----------------------------------
                   tabPanel("Stats",
                            fluidRow(
                                column(width=3,
                                       selectInput("gatherStat", "Grouper par ...",
                                                   choices=list("Tous"="Tous",
                                                                "Députés/membres du Gouvernement"="name",
                                                                "Groupe parlementaire"="groupe_sigle",
                                                                "Commission" = "commission_permanente"),
                                                   selected="name"
                                       )
                                       
                                )
                            ),
                            fluidRow(
                                box(width=12,
                                    #withSpinner(DT::dataTableOutput("tableDeputes")))
                                    div(style = 'overflow-x: scroll', DT::dataTableOutput('tableStat'))
                                    )
                                )
                   ),
                   # Graph Stats - Fin ----------------------------------
                   
                   # Graph Ligne de temps - Début ----------------------------------
                   tabPanel("Ligne de temps",
                            fluidRow(
                                column(width=3,
                                       selectInput("stat", "Statistique",
                                                   choices=list("Nombre de Tweets"= "nbTweets",
                                                                "Nombre de fois reTweeté"= "nbRet",
                                                                "Nombre de Fav"= "nbFav"),
                                                   selected="nbTweets"
                                       )
                                ),
                                column(width=3,
                                       selectInput("parTime", "groupées par ...",
                                                   choices=list("Heure"="hours",
                                                                "Jour"="days"),
                                                   selected="hours"
                                       )
                                ),
                                column(width=3,
                                       selectInput("gather", "et par ...",
                                                   choices=list("Tous"="Tous",
                                                                "Individu"="name",
                                                                "Groupe parlementaire/Gouvernement"="groupe_sigle",
                                                                "Commission" = "commission_permanente"),
                                                   selected="Tous"
                                       )
                                       
                                )
                            ),
                            fluidRow(
                                box(width=12,
                                    withSpinner(plotOutput("tweetPerDay", height = "800px"))
                                )
                            )
                   ),
                   # Graph Ligne de temps - Fin ----------------------------------
                   
                   # Graph Retweets - Début ----------------------------------
                   tabPanel(
                       title="Graphe des reTweets",
                       value="GraphRet",
                       fluidRow(
                           column(width=3,
                                  selectInput("soloDep","Retweets de: ",
                                              choices=list("Tout le monde"= "All",
                                                           "Tous les députés/membres du gouvernement"= "Dep",
                                                           "Tous les députés/membres du gouvernement sélectionnés" = "DepSel"))
                           ),
                           column(width=3,
                                  checkboxInput("dynRet","Dynamique?",value=FALSE)
                           )
                       ),
                       
                       # selectInput("visSelect", "Selection Criteria", 
                       #             choices = list("Country"="Country",
                       #                            "Year"="Year",
                       #                            "Project center"="Center",
                       #                            "Serovar"="Serovar",
                       #                            "SNP Cluster"="Cluster",
                       #                            "Source" = "Source",
                       #                            "CC" = "CC_number")
                       # ),
                       # ), #End Side Bar Panel
                       #mainPanel(
                       #fluidRow(
                           HTML('<h4>Réseau des reTweets. Une flêche de A vers B signifie "A a retweeté B (président &#9632;, membre du gouvernement &#9650;, député &#9679; ou autre &#9733;)".<h4>'),
                           
                           width = 10,
                           h5(" Agrandissez le graphe pour voir les noms. Si vous obtenez un réseau illisible, limitez la période d'étude ou les députés/membres du gouvernement  choisis. 
                                         Vous pouvez isoler le graphe d'un député / membre du gouvernement en le selectionnant dans le cadre 'Sélectionnez'."),
                           h5("Attention: il ne s'agit pas des tweets des députés/membres du gouvernement retweetés, mais bien des tweets retweetés par les députés/membres du gouvernement"),
                           visNetworkOutput("Network", width = "auto", height="800px")
                       #) # End Row
                   ), 
                   # Graph Retweets - Fin ----------------------------------
                   
                   # Graph Mentions - Début ----------------------------------
                   tabPanel(
                       title="Graphe des Mentions",
                       value="GraphMen",
                       # fluidRow(
                       #     column(width=3,
                       selectInput("soloDep2","Mentionnés: ",
                                   choices=list("Tout le monde"= "All",
                                                "Tous les députés/membres du gouvernement"= "Dep",
                                                "Tous les députés/membres du gouvernement sélectionnés" = "DepSel")
                       ),
                       # ),
                       # column(width=3,
                       checkboxInput("dynMen","Dynamique?",value=FALSE),
                       # )
                       #),
                       #fluidRow(
                       HTML('<h4>Réseau des mentions. Une flêche de A vers B signifie "A a mentionné B (président &#9632;, membre du gouvernement &#9650;, député &#9679; ou autre &#9733;)".<h4>'),
                       
                       width = 10,
                       h5(" Aggrandissez le graphe pour voir les noms. Si vous obtenez un réseau illisible, limitez la période d'étude ou les députés / membres du gouvernement choisis. 
                                         Vous pouvez isoler le graphe d'un député / membre du gouvernement en le selectionnant dans le cadre 'Select by id'."),
                       h5("Attention: il ne s'agit pas des députés/membres du gouvernement mentionnés sur Twitter, mais bien des mentions utilisées par les députés/membres du gouvernement"),
                       
                       visNetworkOutput("NetworkMention", width = "auto", height="800px")
                       #) # End Row
                   ), 
                   # Graph Mentions - Fin ----------------------------------
                   
                   # A propos - Start --------------------------------------
                   tabPanel(
                       title="A propos",
                       fluidRow(column(width=12,
                       h3("Introduction"),
                       HTML("Cette application charge les tweets du président, des 43 membres du gouvernement et des 577 députés Français 
                       (tout au moins, ceux qui avaient un compte accessible au 24 Octobre 2021.)<br>"),
                       h3("Mode d'emploi"),
                       HTML("Effectuez une sélection dans la colonne de gauche et allez d'onglet en onglet pour voir
                                         statistiques, listes et réseaux correspondant aux personnes sélectionnées."),
                       
                       h3("Période"),
                       htmlOutput("rangeTweets"),
                       HTML("Toutes les dates/heures correspondent à la date/heure en France métroploitaine.<p/>"),
                       h3("Source données"),
                       HTML('Source Tweets : <a "https://twitter.com" target="_blank">Twitter</a><br>'),
                       HTML('Source Données Députés : <a "https://www.voxpublic.org/spip.php?page=annuaire&cat=deputes&lang=fr" target="_blank">VoxPublic</a> au 1<sup>er</sup> Mai 2020 corrigée au 24 Octobre 2021 (source : <a href="http://www2.assemblee-nationale.fr">Assemblée nationale</a>)<br>'),
                       HTML('Source Données Membres du Gouvernement : <a "https://fr.wikipedia.org/wiki/Gouvernement_Jean_Castex" target="_blank">Wikipedia</a> au 5 Octobre 2020'),
                       h3("A propos"),
                       HTML("Application réalisée par Régis Pouillot. Suggestions et erreurs : <a href='mailto: Owner@thesearemyapps.com'>Ecrivez-moi</a><br>
                            J'ai utilisé largement du <a href='https://github.com/gadenbuie/tweet-conf-dash/'>code et des idées de design</a> proposés par <a href='https://www.garrickadenbuie.com/'>Garrick Aden-Buie</a>. ")
                       
                   )))
                   # A propos - Fin --------------------------------------
                   
                   
                   
            ) #End tabBox
        )# End Fluid Row
        
    ) #End dashboardBody
)
