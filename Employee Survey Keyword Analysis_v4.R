#########################################################
#   Changes in v4:                                      #
#       - completed and/or functionality for keywords   #
#       - added exception handling for no instances     #
#         of keywords/logic in survey verbatims         #
#########################################################


package.list <- c("shiny"
                  ,"dplyr"
                  ,"stringr"
                  ,"reshape"
                  ,"formattable"
                  ,"ggplot2"
                  ,"kableExtra"
                  ,"lubridate"
                  ,"shinythemes")

for( i in package.list ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
        #  If package was not able to be loaded then re-install
        install.packages( i , dependencies = TRUE )
        #  Load package after installing
        require( i , character.only = TRUE )
    }
}

# library(shiny)
# library(dplyr)
# library(stringr)
# library(reshape)
# library(formattable)
# library(ggplot2)
# library(kableExtra)
# library(lubridate)
# library(shinythemes)
# library(shinyjs)


# Define UI 
ui <- fluidPage(theme = shinytheme("flatly"),
     
     # App title ----
     titlePanel("Employee Survey Keyword Analysis"),
     
     # Sidebar layout with input and output definitions ----
     sidebarLayout(
          
          # Sidebar panel for inputs ----
          sidebarPanel(width=2,
               
               # Inputs ----
               textInput(inputId = "keywords",
                         label = "Enter keywords separated by commas:"),
               selectInput(inputId = "keywordLogic",
                           label = "Keyword logic:",
                           choices = c("OR","AND")),
               tags$br(),
               fileInput(inputId = "datafile", 
                         label = "Survey data file:"),
               tags$br(),
               uiOutput("analysisType.ui"),
               uiOutput("dateRange1.ui"),
               uiOutput("dateRange2.ui"),
               actionButton(inputId = "analyzeButton",
                            label = "Analyze!")
          ),
          
          # Main panel for displaying outputs ----
          mainPanel(
              tabsetPanel(
                  tabPanel("Analysis",
                    fluidRow(
                        column(6, 
                            tags$br(),
                            tags$br(),
                            tags$b(textOutput("table.1.text")),
                            tags$br(),
                            tableOutput("table.1"),
                            tags$br(),
                            tags$br(),
                            tags$br(),
                            tags$br(),
                            tags$b(textOutput("comp.chart.text")),
                            tags$br(),
                            plotOutput("comp.chart")
                        ),
                        column(6, 
                            tags$br(),
                            tags$br(),
                            tags$b(textOutput("table.2.text")),
                            tags$br(),
                            tableOutput("table.2"),
                            tags$br(),
                            tags$br(),
                            tags$br(),
                            tags$br(),
                            tags$b(textOutput("table.3.header")),
                            tags$br(),
                            tableOutput("table.3"),
                            tags$br(),
                            textOutput("chi.text"),
                            tags$br(),
                            textOutput("t.test.text")
                        ))),
                  tabPanel("Statistical Tests",
                    column(6,
                        tags$br(),
                        tags$br(),
                        tags$b(textOutput("chi.header")),
                        tags$br(),
                        verbatimTextOutput("chi.sq.test")),
                    column(6,
                        tags$br(),
                        tags$br(),
                        tags$b(textOutput("t.test.header")),
                        tags$br(),
                        verbatimTextOutput("t.test.results"))),
                  tabPanel("Verbatim Examples",
                    br(),
                    br(),
                    textOutput("verb1"),
                    hr(),
                    textOutput("verb2"),
                    hr(),
                    textOutput("verb3"),
                    hr(),
                    textOutput("verb4"),
                    hr(),
                    textOutput("verb5"),
                    hr(),
                    textOutput("verb6"),
                    hr(),
                    textOutput("verb7"),
                    hr(),
                    textOutput("verb8"),
                    hr(),
                    textOutput("verb9"),
                    hr(),
                    textOutput("verb10")),
                    br(),
                    br()
              )
          )
     )
)



# Define server logic
server <- function(input, output) {
    
    surveys <- reactive({
        infile <- input$datafile
        req(infile)
        read.csv(infile$datapath,header=TRUE)
    })

    output$analysisType.ui <- renderUI({
        req(keywords())
        radioButtons(inputId = "analysisType", 
                     label = "What type of analysis?", 
                     choices = c("Keyword vs no keyword",
                                 "Period vs period"))
    })
    
    analysis_type <- reactive({
        req(input$analysisType)
        if_else(input$analysisType == "Keyword vs no keyword", 0, 1 )
    })
    
    keywords_in <- reactive({
        req(input$keywords)
        list <- str_split(input$keywords, ",", simplify=TRUE)
        for (i in 1:length(list)) {
            list[1,i] <- str_trim(list[1,i], side="both")
        }
        return(list)
    })
    
    key_flag <- reactive({
        req(keywords_in())
        req(surveys())
        key_flag <- rep(0, dim(surveys())[1])

        for (i in 1:length(key_flag)) {
            if (input$keywordLogic == "OR") {
                key_flag[i] <- if_else(any(str_detect(surveys()$verbatim[i], keywords_in())),
                                       1,
                                       0,
                                       missing=0)
            } else {
                key_flag[i] <- if_else(all(str_detect(surveys()$verbatim[i], keywords_in())),
                                       1,
                                       0,
                                       missing=0)
            }
        }

        key_flag <- sum(as.integer(key_flag))
    })

    keywords_val <- reactive({
        req(key_flag())
        # key_flag <- as.integer(key_flag())
        validate(
            need(key_flag() > 0, "There are no instances of those keywords in the data.")
        )
        return("Keywords validated!")

    })

    keywords <- reactive({
        req(keywords_val())
        if (keywords_val()=="Keywords validated!") {
            return(keywords_in())
        }
        return(NULL)
    })
    
    daterange1 <- reactive({
        req(surveys())
        dates <- c(min(as.Date(surveys()[,1], "%m/%d/%Y")), max(as.Date(surveys()[,1], "%m/%d/%Y")))
        return(dates)
    })
    
    daterange2 <- reactive({
        req(input$dateRange1)
        if (as.Date(input$dateRange1[2]) < max(as.Date(surveys()[,1], "%m/%d/%Y"))) {
            dates <- c(as.character(as.Date(input$dateRange1[2])+1), as.character(max(as.Date(surveys()[,1], "%m/%d/%Y"))))
            return(dates)
        } else {
            dates <- c(as.character(input$dateRange1[2]), as.character(max(as.Date(surveys()[,1], "%m/%d/%Y"))))
            return(dates)
        }
    })
    
    output$dateRange1.ui <- renderUI({
        req(analysis_type())
        if (analysis_type()==0) {
            dateRangeInput(inputId = "dateRange1",
                           label = "Date range:",
                           start = daterange1()[1],
                           end = daterange1()[2])
        }
        else {
            dateRangeInput(inputId = "dateRange1",
                           label = "Period 1 date range:",
                           start = daterange1()[1],
                           end = daterange1()[2])
        }
    })
    
    output$dateRange2.ui <- renderUI({
        req(analysis_type())
        if (analysis_type()==0) {
            return(NULL)
        }
        dateRangeInput(inputId = "dateRange2",
                       label = "Period 2 date range:",
                       start = daterange2()[1],
                       end = daterange2()[2])
    })
    
    
    observeEvent(input$analyzeButton, {

        surveys <- surveys()
        keywords <- keywords()
        
        period1 <- interval(ymd(as.character(input$dateRange1[1])), ymd(as.character(input$dateRange1[2])))
        period2 <- interval(ymd(as.character(input$dateRange2[1])), ymd(as.character(input$dateRange2[2])))
        original_verbatims <- as.character(surveys[,3])
        
        ### format data
        surveys[,1] <- as.Date(surveys[,1], "%m/%d/%Y")
        surveys[,2] <- as.integer(surveys[,2])
        surveys[,3] <- as.character(str_to_lower(surveys[,3]))
        
        ### calculate NPS segments
        nps_segment <- rep(NA, dim(surveys)[1])
        for (i in (1:length(nps_segment))) {
            nps_segment[i] <- as.character(if_else(surveys[i,2] > 8, 
                                                   "Promoter", 
                                                   if_else(surveys[i,2] > 6, 
                                                           "Passive", 
                                                           "Detractor")))
        }
        
        ### combine NPS segment with other data
        surveys <- data.frame(cbind(surveys,nps_segment))
        names(surveys) <- c("date","ltr","verbatim","nps_segment")
        
        farmcols <- c("#0072CE","#323232","#E01933","#B30032","#003087")
        
        #-------------------------#
        #   Create keyword flag   #
        #-------------------------#
        
        key_flag <- rep(NA, dim(surveys)[1])
        
        for (i in 1:length(key_flag)) {
            if (input$keywordLogic == "OR") {
                key_flag[i] <- if_else(any(str_detect(surveys$verbatim[i], keywords)),
                                        1, 
                                        0, 
                                        missing=0)
            } else {
                key_flag[i] <- if_else(all(str_detect(surveys$verbatim[i], keywords)),
                                        1, 
                                        0, 
                                        missing=0)
            }
        }
        
        key_flag <- as.factor(key_flag)
        ### combine keyword flag with other data
        surveys <- data.frame(cbind(surveys,key_flag))
        
        #-------------------------#
        #   Create period flag    #
        #-------------------------#
        
        period_flag <- rep(NA, dim(surveys)[1])
        multi_period_analysis <- analysis_type()   # {NULL, 0, 1}
        
        if (multi_period_analysis==1) {
            
            for (i in 1:length(period_flag)) {
                period_flag[i] <- if_else(surveys$date[i] %within% period1,
                                          1,
                                          if_else(surveys$date[i] %within% period2,
                                                  2,
                                                  NULL))
            }
        } else {
            for (i in 1:length(period_flag)) {
                period_flag[i] <- if_else(surveys$date[i] %within% period1,
                                          1,
                                          NULL)
            }
        }
        
        period_flag <- as.factor(period_flag)
        surveys$period_flag <- period_flag
        
        if (multi_period_analysis==1) {
            #----------------------------------#
            #   Period vs. Period Summaries    #
            #----------------------------------#
            
            table.data <- surveys[which(surveys$key_flag==1),]
            ### table with counts
            pivot1 <- melt(table.data[,c(2,4,6)],
                           id.vars=c("nps_segment","period_flag"),
                           value="ltr")
            table <- cast(pivot1, nps_segment~period_flag, fun.aggregate = length, margins=TRUE)
            names(table) <- c("NPS Segment","Period 1","Period 2","Total")
            
            table.1 <- table
            names(table.1)[1] <- ""
            output$table.1 <- function() {
                kable(table.1, booktabs=T, align=c("l","c","c","c")) %>%
                    kable_styling(position="left", full_width = FALSE) %>%
                    column_spec(1, width="10em", bold=T, color="black", background = "lightgrey") %>%
                    column_spec(2, width="8em", color="black") %>%
                    column_spec(3, width="8em", color="black") %>%
                    column_spec(4, width="8em", color="black") %>%
                    row_spec(0, bold=T, background = "darkgrey") %>%
                    row_spec(4, bold=T, background = "lightblue", color="black")}
            
        } else {
            #----------------------------------#
            #   Keyword/No Keyword Summaries   #
            #----------------------------------#
            
            table.data <- surveys[which(surveys$period_flag==1),]
            ### table with counts
            pivot1 <- melt(table.data[,c(2,4,5)], id.vars=c("nps_segment","key_flag"), value="ltr")
            table <- cast(pivot1, nps_segment~key_flag, fun.aggregate = length, margins=TRUE)
            names(table) <- c("NPS Segment","No keywords","Keywords","Total")
            
            table.1 <- table
            names(table.1)[1] <- ""
            output$table.1 <- function() {
                kable(table.1, booktabs=T, align=c("l","c","c","c")) %>%
                    kable_styling(position="left", full_width = FALSE) %>%
                    column_spec(1, width="10em", bold=T, color="black", background = "lightgrey") %>%
                    column_spec(2, width="8em", color="black") %>%
                    column_spec(3, width="8em", color="black") %>%
                    column_spec(4, width="8em", color="black") %>%
                    row_spec(0, bold=T, background = "darkgrey") %>%
                    row_spec(4, bold=T, background = "lightblue", color="black")}
        }
        
        output$table.1.text <- renderText("Count Table")
        
        # perform chi-squared test
        chi.test.results <- chisq.test(table[,2:3], correct=TRUE)
        
        ### table with percentages
        table_per.1 <- table
        table_per.2 <- table
        
        for (i in 1:4) {
            for (j in 2:4) {
                table_per.1[i,j] <- round((table[i,j]/table[4,j])*100,2)
            }
        }
        
        for (i in 1:4) {
            for (j in 2:4) {
                table_per.2[i,j] <- paste(round((table[i,j]/table[4,j])*100,2)," %")
            }
        }
        
        names(table_per.2)[1] <- ""
        output$table.2 <- function() {
            kable(table_per.2, booktabs=T, align=c("l","c","c","c")) %>%
                kable_styling(position="left", full_width = FALSE) %>%
                column_spec(1, width="10em", bold=T, color="black", background = "lightgrey") %>%
                column_spec(2, width="8em", color="black") %>%
                column_spec(3, width="8em", color="black") %>%
                column_spec(4, width="8em", color="black") %>%
                row_spec(0, bold=T, background = "darkgrey") %>%
                row_spec(4, bold=T, background = "lightblue", color="black")
        }
        
        output$table.2.text <- renderText("Proportion Table")
        
        output$chi.header <- renderText("Chi-squared Test for eNPS")
        
        output$chi.sq.test <- renderPrint(chi.test.results)
        
        chi.text <- paste("There ",if_else(chi.test.results$p.value <= 0.05,
                                                               "is",
                                                               "is not"),sep="")
        chi.text <- paste(chi.text,
                          " a significant difference between the distribution across NPS segments for ",
                          sep="")
        
        chi.text <- paste(chi.text,
                          if_else(multi_period_analysis==1,
                                  "keyword surveys from Period 1 and those from Period 2.",
                                  "surveys with and those without keywords."),
                          sep="")
        
        output$chi.text <- renderText(chi.text)
        
        if (multi_period_analysis==1) {
            chart.data <- data.frame(table_per.1['NPS Segment'],table_per.1['Period 1'],table_per.1['Period 2'])
            chart.data <- chart.data[-4,]
            
            chart.data.m <- melt(chart.data, id.vars = 'NPS.Segment')
            
            
            g1 <- ggplot(chart.data.m, aes(NPS.Segment, value)) + 
                geom_bar(aes(fill=variable), position="dodge", stat="identity", width=0.55) +
                scale_fill_manual(values=c(farmcols[1],farmcols[3]),
                                  name="", 
                                  labels=c("Period 1","Period 2")) +
                labs(x="NPS Segment", y="Percentage") 
            #theme(legend.position = "bottom")
            output$comp.chart <- renderPlot(g1,width=600)
            
        } else {
            ### grouped/stacked bar chart
            chart.data <- data.frame(table_per.1['NPS Segment'],table_per.1['No keywords'],table_per.1['Keywords'])
            chart.data <- chart.data[-4,]
            
            chart.data.m <- melt(chart.data, id.vars = 'NPS.Segment')
            
            
            g1 <- ggplot(chart.data.m, aes(NPS.Segment, value)) + 
                geom_bar(aes(fill=variable), position="dodge", stat="identity", width=0.55) +
                scale_fill_manual(values=c(farmcols[1],farmcols[3]),
                                  name="", 
                                  labels=c("No keywords","Keywords")) +
                labs(x="NPS Segment", y="Percentage") 
            #theme(legend.position = "bottom")
            output$comp.chart <- renderPlot(g1,width=600)
        }   
        
        output$comp.chart.text <- renderText("Comparison Chart")
        
        if (multi_period_analysis==1) {
            # table with eNPS and eMPS
            table_nps <- data.frame(c("Period 1 with keywords","Period 2 with keywords","Overall"), 
                                    c(round(table_per.1[3,2]-table_per.1[1,2],1), 
                                      round(table_per.1[3,3]-table_per.1[1,3],1), 
                                      round(table_per.1[3,4]-table_per.1[1,4],1)),
                                    c(round(mean(table.data$ltr[which(table.data$period_flag==1)]),2),
                                      round(mean(table.data$ltr[which(table.data$period_flag==2)]),2), 
                                      round(mean(table.data$ltr),2)))
            names(table_nps) <- c(" ","eNPS","eMPS")
            
            output$table.3 <- function() {
                kable(table_nps, booktabs=T, align=c("l","c","c")) %>%
                    kable_styling(position="left", full_width = FALSE) %>%
                    column_spec(1, width="10em", bold=T, color="black", background = "lightgrey") %>%
                    column_spec(2, width="8em", color="black") %>%
                    column_spec(3, width="8em", color="black") %>%
                    row_spec(0, bold=T, background = "darkgrey") %>%
                    row_spec(3, bold=T, background = "lightblue", color="black")
            }
        } else {
            # table with eNPS and eMPS
            table_nps <- data.frame(c("Surveys with no keywords","Surveys with keywords","Overall"), 
                                    c(round(table_per.1[3,2]-table_per.1[1,2],1), 
                                      round(table_per.1[3,3]-table_per.1[1,3],1), 
                                      round(table_per.1[3,4]-table_per.1[1,4],1)),
                                    c(round(mean(table.data$ltr[which(table.data$key_flag==0)]),2),
                                      round(mean(table.data$ltr[which(table.data$key_flag==1)]),2), 
                                      round(mean(table.data$ltr),2)))
            names(table_nps) <- c(" ","eNPS","eMPS")
            
            output$table.3 <- function() {
                kable(table_nps, booktabs=T, align=c("l","c","c")) %>%
                    kable_styling(position="left", full_width = FALSE) %>%
                    column_spec(1, width="10em", bold=T, color="black", background = "lightgrey") %>%
                    column_spec(2, width="8em", color="black") %>%
                    column_spec(3, width="8em", color="black") %>%
                    row_spec(0, bold=T, background = "darkgrey") %>%
                    row_spec(3, bold=T, background = "lightblue", color="black")    
            }
        }
        
        output$table.3.header <- renderText("eNPS and eMPS")
        
        if (multi_period_analysis) {
            ### test difference in eNPS for significance
            t.test.results <- t.test(table.data$ltr[which(table.data$period_flag==1)], 
                                     table.data$ltr[which(table.data$period_flag==2)], 
                                     var.equal = FALSE, 
                                     conf.level = 0.95, 
                                     alternative= "two.sided")  
            change1 <- round(table_nps[2,2]-table_nps[1,2],2)
            change2 <- round(table_nps[2,3]-table_nps[1,3],2)
        } else {
            ### test difference in eMPS for significance
            t.test.results <- t.test(table.data$ltr[which(table.data$key_flag==0)], 
                                     table.data$ltr[which(table.data$key_flag==1)], 
                                     var.equal = FALSE, 
                                     conf.level = 0.95, 
                                     alternative= "two.sided")
            change1 <- round(table_nps[3,2]-table_nps[1,2],2)
            change2 <- round(table_nps[3,3]-table_nps[1,3],2)
        }
        
        t.test.text <- str_c(if_else(multi_period_analysis==1, 
                                     "eNPS for surveys with keywords changed by ", 
                                     "Surveys with keywords changed overall eNPS by "),
                             {change1},
                             " points ",
                             if_else(multi_period_analysis==1, 
                                     "from Period 1 to Period 2 and eMPS changed by ",
                                     "and overall eMPS by "),
                             {change2},
                             " points.  ",
                             "The difference in eMPS ",
                             if_else(t.test.results$p.value <= 0.05, 
                                     "is ", 
                                     "is not "),
                             "significant.")
        
        output$t.test.results <- renderPrint(t.test.results)
        output$t.test.text <- renderText(as.character(t.test.text))
        
        output$t.test.header <- renderText("t Test for eMPS")
        
        key_verbatims <- original_verbatims[which(surveys$key_flag==1)]
        
        n <- ifelse(length(key_verbatims)<10,length(key_verbatims),10)
        
        rand_samp <- key_verbatims[sample.int(length(key_verbatims),n)]
        
        output$verb1 <- renderText(rand_samp[1])
        output$verb2 <- renderText(rand_samp[2])
        output$verb3 <- renderText(rand_samp[3])
        output$verb4 <- renderText(rand_samp[4])
        output$verb5 <- renderText(rand_samp[5])
        output$verb6 <- renderText(rand_samp[6])
        output$verb7 <- renderText(rand_samp[7])
        output$verb8 <- renderText(rand_samp[8])
        output$verb9 <- renderText(rand_samp[9])
        output$verb10 <- renderText(rand_samp[10])
        
        output$verbatim.header <- renderText("Keyword Verbatim Examples")
           
    })
    
}


shinyApp(ui, server)


