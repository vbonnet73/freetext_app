#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

############ Required library ############

library(shiny)
require(openxlsx)
require(rio)
require(ggplot2)
require(dplyr)
require(tidytext)
require(janeaustenr)
require(tidyr)
require(igraph)
require(ggraph)
require(lazyeval)
require(gridExtra)
require(tm)
require(visNetwork)
require(shinyWidgets)
require(DT)
require(shinyjs)
require(stringr)

############ Options ############
options(shiny.maxRequestSize=100*1024^2) 

############ variables declaration ############

# Special characters
special_characters = data.frame(word =c(',', '!', '"', '#', '$', '%', '&', "'", '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=', '>', '?', '@', '[', '\\', ']', '^', '_', '`', '{', '|', '}', '~' ))
special_characters$word <- as.character(special_characters$word)



############ functions declaration ############

# suppress accent in a text
Unaccent <- function(text) {
  
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}

# tokenize col_name colum in n words
# tokenize Description colum in n words
tokenize <- function(mes_df, col_name, n_words, threshold){
  if(missing(threshold)){threshold = 0}
  n_grams <- data.frame(mes_df) %>%
    unnest_tokens_("ngram", col_name, token = "ngrams", n=n_words)
  print(n_grams)
  return(n_grams %>%
           count(ngram, sort = TRUE) %>%
           dplyr::filter(n > threshold) %>%
           mutate(ngram = reorder(ngram, n))
         )
}

# Facet a filter ngram by a column name
tokenize_by <- function(mes_df, col_name, n_words, filter, criteria){

  expr1 = interp(~x, x = as.name(criteria))
  expr2 = interp(~y, y = as.name("ngram"))
  n_grams <- data.frame(mes_df) %>%
    unnest_tokens_("ngram", col_name, token = "ngrams", n=n_words)
  ngram_tf_idf <- n_grams %>%
    count_(c(expr1,expr2), sort = TRUE)
  return(ngram_tf_idf %>%
           dplyr::filter(ngram %in% c(filter)) %>%
           dplyr::filter (n > 10) %>%
           mutate(ngram = reorder(ngram, n)) %>%
           ggplot(aes(ngram, n)) +
           geom_col() +
           xlab(NULL) +
           coord_flip()+
           facet_wrap(expr1, scales = "free_y"))
}#End tokenize_by

# Print a cloud of word
cloudwords <- function(mes_df, col_name, n_words, recurrence){
  if(missing(recurrence)){recurrence = 1}
  word_list <- list()
  wl = c()
  for (i in 1:n_words) {
    word_list[[i]] = paste(c("word", i), collapse = "")
    wl[[i]]=interp(~x, x = as.name(paste(c("word", i), collapse = "")))
  }
  n_grams <- data.frame(mes_df) %>%
    unnest_tokens_("ngram", col_name, token = "ngrams", n=n_words)
  ngrams_separated <- n_grams %>%
    separate(ngram, unlist(word_list), sep = " ")
  ngram_counts <- ngrams_separated %>% 
    group_by_( .dots = wl) %>%
    summarise(n = n()) %>%
    arrange(desc(n))
  ngram_graph <- ngram_counts %>%
    dplyr::filter(n > recurrence) %>%
    graph_from_data_frame()
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.075, "inches"))
  return(
    ggraph(ngram_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                   arrow = a, end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "red", size = 1) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 2.5)+
    theme_void()
  )
}#End cloudwords

# Print a zoomable cloud of word
cloudwords_bis <- function(mes_df, col_name, n_words, recurrence){
  if(missing(recurrence)){recurrence = 1}
  word_list <- list()
  wl = c()
  for (i in 1:n_words) {
    word_list[[i]] = paste(c("word", i), collapse = "")
    wl[[i]]=interp(~x, x = as.name(paste(c("word", i), collapse = "")))
  }
  
  n_grams <- data.frame(mes_df) %>%
    unnest_tokens_("ngram", col_name, token = "ngrams", n=n_words)
  
  ngrams_separated <- n_grams %>%
    separate(ngram, unlist(word_list), sep = " ")
  
  ngram_counts <- ngrams_separated %>% 
    group_by_( .dots = wl) %>%
    summarise(n = n()) %>%
    arrange(desc(n))
  
  # Network creation net
  net <- ngram_counts %>%
    dplyr::filter(n > recurrence) %>%
    graph_from_data_frame()
  
  #Network edges parameter set-up
  V(net)$shape <- "dot"
  V(net)$color.background <- "blue"
  V(net)$color.border <- "darkblue"
  V(net)$font.size <- 35
  V(net)$shadow <- TRUE # Nodes will drop shadow
  
  #Network links parameter set-up
  E(net)$color <- "black"
  E(net)$color.opacity <- (E(net)$n-min(E(net)$n))/(max(E(net)$n)-min(E(net)$n))
  E(net)$width <- 8*(E(net)$n-min(E(net)$n))/(max(E(net)$n)-min(E(net)$n))
  E(net)$arrows <- "middle"
  return(net)
 
}#End cloudwords_bis

  

############ Define UI for the app ############

ui <- fluidPage(
  
  useShinyjs(),
  inlineCSS(list("table" = "font-size: 9px")),
   # Application title
   titlePanel("Free text analytics app"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        fileInput("file1", "select excel file to be analyzed",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv",
                    ".xslx")
        ),
        uiOutput("dropDown"),
        uiOutput("toCol"),
        sliderInput("nb_words",
                    "Number of words for analysis:",
                    min = 1,  max = 10, value = 2),
        sliderInput("freq",
                    "Minimum frequency of results:",
                    min = 1,  max = 50,  value = 10),
        h5(strong("Remove words in the analysis")),
        #materialSwitch(inputId = "toto_stp", label = "Remove french stopwords", status = "primary", right = TRUE),
        checkboxInput("french_stp", "french stopwords"),
        checkboxInput("english_stp", "english stopwords"),
        checkboxInput("special_characters", "special characters"),
        checkboxInput("other_stp", "other words", NULL),
        conditionalPanel(
          condition = "input.other_stp == true",
          textAreaInput("text",
                        label = h6("Enter each words separated by / (with no space)"),
                        value = NULL)
          ),
        actionButton("update", "Run analysis",
                     style="color: #8B0000; border-color:#8B0000")
      ),#End sidebar Panel
      
      
      
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Word recurrence",plotOutput("freqPlot")),        
          tabPanel("Word cloud",visNetworkOutput("cloudPlot")),
          tabPanel("Facet analysis",
                   plotOutput("tokenizeby"),
                   uiOutput("toCol2"),
                   textAreaInput("text2",
                                 label = h6("Enter sequence of word to be analysed"),
                                 value = NULL)
                   ),#End tabPanel
          tabPanel("Dataset",
                   dataTableOutput("Dataset"),
                   downloadButton("downloadData", "Download"))#End DatasetPanel
        )#End tabsetPanel
      )#End MainPanel
   )#End SidebarLayout
)#End fluidPage



############ Define the server logic to execute the app ############

server <- function(input, output) {
  
  df <- reactive({
    req(input$file1)
    inFile <- input$file1
    df <- import(inFile$datapath)
  })#End reactive
  
  output$toCol <- renderUI({
    data <- df()
    items <- names(data)
    flowLayout(
      selectInput('text_col', 'Pick column for analysis', items)
    )
  })#End output$toCol
  
  output$dropDown <- renderUI({
    data <- df()
    dropdownButton(
      uiOutput("subsetbox1"),
      uiOutput("subsetbox1text"),
      uiOutput("subsetbox2"),
      uiOutput("subsetbox2text"),
      uiOutput("subsetbox3"),
      uiOutput("subsetbox3text"),
      circle = FALSE, status = "primary", size = "sm", icon = icon("filter"),
      label = "Filtering options", width = "300px"
    )
  })#End output$dropDown
  
  output$subsetbox1 <- renderUI({
    data <- df()
    items_1 <- names(data)
    #items2 <- unique(data$Imputation)
    selectInput(
      "subsetbox1sel","Choose 1st column to filter:",
      choices=items_1
      )
    })#output$subsetbox1
  
  output$subsetbox1text <- renderUI({
    data <- df()
    items_11 <- data[[input$subsetbox1sel]]
    selectInput(
      "subsetbox1textsel","Choose values to keep:",
      choices=items_11,
      multiple = T
      )
    })#output$subsetboxtext1
  
  output$subsetbox2 <- renderUI({
    data <- df()
    items_1 <- names(data)
    selectInput(
      "subsetbox2sel","Choose 2nd column to filter:",
      choices=items_1
    )
  })#output$subsetbox2
  output$subsetbox2text <- renderUI({
    data <- df()
    items_11 <- data[[input$subsetbox2sel]]
    selectInput(
      "subsetbox2textsel","Choose values to keep:",
      choices=items_11,
      multiple = T
    )
  })#output$subsetboxtext2
  
  output$subsetbox3 <- renderUI({
    data <- df()
    items_1 <- names(data)
    selectInput(
      "subsetbox3sel","Choose 3rd column to filter:",
      choices=items_1
    )
  })#output$subsetbox3
  output$subsetbox3text <- renderUI({
    data <- df()
    items_11 <- data[[input$subsetbox3sel]]
    selectInput(
      "subsetbox3textsel","Choose values to keep:",
      choices=items_11,
      multiple = T
    )
  })#output$subsetboxtext3
    

  output$toCol2 <- renderUI({
    data <- df()
    items <- names(data)
    selectInput('text_col2', 'Choose column to facet your analysis', items)
  })#End output$toCol2
  
  

  observeEvent(input$update, {
    
    # Return input file from browser selection
    # If inFile is null return NULL
    require(input$text_col)
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    withProgress(message = 'Analyzing data', value = 0.5, {
      
      # Read selected file inFile
      df <- read.xlsx(inFile$datapath)
      
      var1 <- reactive({input$subsetbox1textsel})
      var2 <- reactive({input$subsetbox2textsel})
      var3 <- reactive({input$subsetbox3textsel})
      if (is.null(var1()) && is.null(var2()) && is.null(var3())){
        df_subset<- reactive({
          a <- df()
          return(a)
          })
        }else if (!is.null(var1()) && is.null(var2()) && is.null(var3())){
          df_subset<- reactive({
            a <- subset(df(), df[[input$subsetbox1sel]] == var1())
            return(a)
          })
        } else if (!is.null(var1()) && !is.null(var2()) && is.null(var3())){
          df_subset<- reactive({
            a <- subset(df(), (df[[input$subsetbox1sel]] == var1() &
                                 df[[input$subsetbox2sel]] == var2()))
            return(a)
          })
        } else if (!is.null(var1()) && !is.null(var2()) && !is.null(var3())){
          df_subset<- reactive({
            a <- subset(df(), (df[[input$subsetbox1sel]] == var1() &
                                 df[[input$subsetbox2sel]] == var2() &
                                 df[[input$subsetbox3sel]] == var3()
            ))
            return(a)
          })
        } else if (is.null(var1()) && !is.null(var2()) && is.null(var3())){
          df_subset<- reactive({
            a <- subset(df(), (df[[input$subsetbox2sel]] == var2() 
            ))
            return(a)
          })
        } else if (is.null(var1()) && is.null(var2()) && !is.null(var3())){
          df_subset<- reactive({
            a <- subset(df(), (df[[input$subsetbox3sel]] == var3() 
            ))
            return(a)
          })
        } else if (!is.null(var1()) && is.null(var2()) && !is.null(var3())){
          df_subset<- reactive({
            a <- subset(df(), (df[[input$subsetbox1sel]] == var1() &
                                 df[[input$subsetbox3sel]] == var3()
            ))
            return(a)
          })
        } else if (is.null(var1()) && !is.null(var2()) && !is.null(var3())){
          df_subset<- reactive({
            a <- subset(df(), (df[[input$subsetbox2sel]] == var2() &
                                 df[[input$subsetbox3sel]] == var3()
            ))
            return(a)
          })
        }
        df <- df_subset()
      
      # Suppress accent
      sapply(df[[input$text_col]], Unaccent)
    
      # Lower case
      sapply(df[[input$text_col]], tolower)
      
      # Remove stop words if check box value is TRUE
      # french stopwords
      if(input$french_stp){
        i = 1
        for (i in 1:length(stopwords("fr"))) {
          df[[input$text_col]] <-str_replace_all(df[[input$text_col]], paste(" ",stopwords("fr")[i]," ", sep=""),"")
          df[[input$text_col]] <-str_replace_all(df[[input$text_col]], paste("^", stopwords("fr")[i], sep=""),"")
          df[[input$text_col]] <-str_replace_all(df[[input$text_col]], paste(" ", stopwords("fr")[i],"$", sep=""),"")
          i = i+1
        }#End for
      }#End if
      
      # English stopwords
      if(input$english_stp){
        i = 1
        for (i in 1:length(stopwords("en"))) {
          df[[input$text_col]] <-gsub(paste(" ",stopwords("en")[i],"$", sep = ""),
                                " ", df[[input$text_col]])
          df[[input$text_col]] <-gsub(paste(" ",stopwords("en")[i], " ",sep = ""), 
                                " ", df[[input$text_col]])
          df[[input$text_col]] <-gsub(paste("^",stopwords("en")[i], " ",sep = ""), 
                                "", df[[input$text_col]])
          i = i+1
        }#End for
      }#End if
      
      # Specific words
      if(input$other_stp){
        other_list <- strsplit(input$text, "/")[[1]]
        i = 1
        for (i in 1:length(other_list)) {
          df[[input$text_col]] <-str_replace_all(df[[input$text_col]], paste(other_list[i]," ", sep=""),"")
          df[[input$text_col]] <-str_replace_all(df[[input$text_col]], paste(" ", other_list[i], sep=""),"")
          i = i+1
          i = i+1
        }#End for
      }#End if
      
      # Tokenizing the Description column
      tk <-tokenize(df,input$text_col, input$nb_words, input$freq)
    })#End withProgress
    
    
    # Output n°1 = freqPlot
    output$freqPlot <- renderPlot({
      
      withProgress(message = 'Making plot', value = 0.1, {
        ggplot(tk, aes(ngram, n)) +
          geom_col() +
          xlab(NULL) +
          coord_flip()
      })#End withProgress
    })#End output$freqPlot
    
    # Output n°2 = Cloudword
    output$cloudPlot <- renderVisNetwork({
      withProgress(message = 'Analyzing data', value = 0.1, {
        #Network plot
        net <- cloudwords_bis(df, input$text_col, input$nb_words, input$freq)
        visIgraph(net, physics = TRUE, smooth = TRUE)
      }) #End withProgress
    }) #End output$cloudPlot
      
    
    # Output n°3 = Tokenizeby
    output$tokenizeby <- renderPlot({
      tokenize_by(df, input$text_col, input$nb_words, input$text2, input$text_col2)
    })#End output$tokenizeby
    
    # Output Dataset = datatset
    output$Dataset <- renderDataTable({
      datatable(df_subset(),
      options = list(lengthMenu = c(5, 10),
                     pageLength = 5,
                     scrollX = TRUE),
      escape = FALSE)
      })#End output$Dataset
    
    output$filtered_row <- 
      renderPrint({
        input[["Dataset_rows_all"]]
      })
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$dataset, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(#dataTableOutput("Dataset")
          df[input[["Dataset_rows_all"]], ] ,file, row.names = FALSE)
      }
    )
    
  })#End observeEvent
    
 }#End server

# Run the application 
shinyApp(ui = ui, server = server)

