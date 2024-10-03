library(shiny)
library(tidyverse)
library(semantic.dashboard)
library(DT)
library(plotly)
library(sparklyr)
library(readr)
library(shinyWidgets)
library(PerformanceAnalytics)
acp <- read.csv2("https://stooq.pl/q/d/l/?s=acp&d1=20230101&d2=20230531&i=d&o=1111111", header = TRUE, sep = ",",dec = ".")
ale <- read.csv2("https://stooq.pl/q/d/l/?s=ale&d1=20230101&d2=20230531&i=d&o=1111111", header = TRUE, sep = ",",dec = ".")
alr <- read.csv2("https://stooq.pl/q/d/l/?s=alr&d1=20230101&d2=20230531&i=d&o=1111111", header = TRUE, sep = ",",dec = ".")
cdr <- read.csv2("https://stooq.pl/q/d/l/?s=cdr&d1=20230101&d2=20230531&i=d&o=1111111", header = TRUE, sep = ",",dec = ".")
cps <- read.csv2("https://stooq.pl/q/d/l/?s=cps&d1=20230101&d2=20230531&i=d&o=1111111", header = TRUE, sep = ",",dec = ".")
dnp <- read.csv2("https://stooq.pl/q/d/l/?s=dnp&d1=20230101&d2=20230531&i=d&o=1111111", header = TRUE, sep = ",",dec = ".")
jsw <- read.csv2("https://stooq.pl/q/d/l/?s=jsw&d1=20230101&d2=20230531&i=d&o=1111111", header = TRUE, sep = ",",dec = ".")
kgh <- read.csv2("https://stooq.pl/q/d/l/?s=kgh&d1=20230101&d2=20230531&i=d&o=1111111", header = TRUE, sep = ",",dec = ".")
kru <- read.csv2("https://stooq.pl/q/d/l/?s=kru&d1=20230101&d2=20230531&i=d&o=1111111", header = TRUE, sep = ",",dec = ".")
kty <- read.csv2("https://stooq.pl/q/d/l/?s=kty&d1=20230101&d2=20230531&i=d&o=1111111", header = TRUE, sep = ",",dec = ".")
lpp <- read.csv2("https://stooq.pl/q/d/l/?s=lpp&d1=20230101&d2=20230531&i=d&o=1111111", header = TRUE, sep = ",",dec = ".")
mbk <- read.csv2("https://stooq.pl/q/d/l/?s=mbk&d1=20230101&d2=20230531&i=d&o=1111111", header = TRUE, sep = ",",dec = ".")
opl <- read.csv2("https://stooq.pl/q/d/l/?s=opl&d1=20230101&d2=20230531&i=d&o=1111111", header = TRUE, sep = ",",dec = ".")
peo <- read.csv2("https://stooq.pl/q/d/l/?s=peo&d1=20230101&d2=20230531&i=d&o=1111111", header = TRUE, sep = ",",dec = ".")
pge <- read.csv2("https://stooq.pl/q/d/l/?s=pge&d1=20230101&d2=20230531&i=d&o=1111111", header = TRUE, sep = ",",dec = ".")
pkn <- read.csv2("https://stooq.pl/q/d/l/?s=pkn&d1=20230101&d2=20230531&i=d&o=1111111", header = TRUE, sep = ",",dec = ".")
pko <- read.csv2("https://stooq.pl/q/d/l/?s=pko&d1=20230101&d2=20230531&i=d&o=1111111", header = TRUE, sep = ",",dec = ".")
pzu <- read.csv2("https://stooq.pl/q/d/l/?s=pzu&d1=20230101&d2=20230531&i=d&o=1111111", header = TRUE, sep = ",",dec = ".")
spl <- read.csv2("https://stooq.pl/q/d/l/?s=spl&d1=20230101&d2=20230531&i=d&o=1111111", header = TRUE, sep = ",",dec = ".")
WIG20 <- read.csv2("https://stooq.pl/q/d/l/?s=wig20&d1=20230101&d2=20230531&i=d&o=1111111", header = TRUE, sep = ",",dec = ".")
colnames(acp)[5]<-"acp"
colnames(ale)[5]<-"ale"
colnames(alr)[5]<-"alr"
colnames(cdr)[5]<-"cdr"
colnames(cps)[5]<-"cps"
colnames(dnp)[5]<-"dnp"
colnames(jsw)[5]<-"jsw"
colnames(kgh)[5]<-"kgh"
colnames(kru)[5]<-"kru"
colnames(kty)[5]<-"kty"
colnames(lpp)[5]<-"lpp"
colnames(mbk)[5]<-"mbk"
colnames(opl)[5]<-"opl"
colnames(peo)[5]<-"peo"
colnames(pge)[5]<-"pge"
colnames(pkn)[5]<-"pkn"
colnames(pko)[5]<-"pko"
colnames(pzu)[5]<-"pzu"
colnames(spl)[5]<-"spl"
colnames(WIG20)[5]<-"WIG20"
dataset<-acp[, c("Data", "acp")]
dataset<- dataset%>% bind_cols(ale= ale$ale)
dataset<- dataset%>% bind_cols(alr= alr$alr)
dataset<- dataset%>% bind_cols(cdr= cdr$cdr)
dataset<- dataset%>% bind_cols(cps= cps$cps)
dataset<- dataset%>% bind_cols(dnp= dnp$dnp)
dataset<- dataset%>% bind_cols(jsw= jsw$jsw)
dataset<- dataset%>% bind_cols(kgh= kgh$kgh)
dataset<- dataset%>% bind_cols(kru= kru$kru)
dataset<- dataset%>% bind_cols(kty= kty$kty)
dataset<- dataset%>% bind_cols(lpp= lpp$lpp)
dataset<- dataset%>% bind_cols(mbk= mbk$mbk)
dataset<- dataset%>% bind_cols(opl= opl$opl)
dataset<- dataset%>% bind_cols(peo= peo$peo)
dataset<- dataset%>% bind_cols(pge= pge$pge)
dataset<- dataset%>% bind_cols(pkn= pkn$pkn)
dataset<- dataset%>% bind_cols(pko= pko$pko)
dataset<- dataset%>% bind_cols(pzu= pzu$pzu)
dataset<- dataset%>% bind_cols(spl= spl$spl)
dataset<- dataset%>% bind_cols(WIG20= WIG20$WIG20)
possibilities<-c("acp", "ale","alr", "cdr","cps","dnp","jsw", "kgh", "kru", "kty", "lpp","mbk", "opl", "peo","pge", "pkn", "pko", "pzu", "spl")
dataset<-round(dataset %>% select (all_of(possibilities),"WIG20"), 2)%>% bind_cols("Data"= dataset$Data)


dataset$Data<-as.Date(dataset$Data)

returns<-Return.calculate(dataset)

returns<-dataset[,"Data"]%>% bind_cols(returns)
colnames(returns)[colnames(returns)=="...1"]<- "Data"
calculateMultiplierVector <- function(money, cost) {
  multiplier <- 1
  result <- c()
  
  while (TRUE) {
    value <- multiplier * cost
    if (value <= money) {
      result <- c(result, value)
    } else {
      break
    }
    multiplier <- multiplier + 1
  }
  
  result
}

# Define UI for application that draws a histogram
ui <- dashboardPage( theme ="spacelab",
                     dashboard_header(),
                     dashboardSidebar(
                       sidebarMenu(
                         menuItem("All Stock table", tabName="StockTable"),
                         menuItem("Compare stock plots", tabName="StockPlots"),
                         menuItem("Check one action", tabName="CheckAction"),
                         menuItem("Create first portfolio", tabName="CheckPortfolio"),
                         menuItem("Create second portfolio", tabName="CheckPortfolio2"),
                         menuItem("Comparison!", tabName="Comparison")
                         
                         
                       )
                     ),
                     dashboardBody(
                       
                       tabItems(
                         ####----------------------------------page 1---------------------------------------------------------------------------------------------------------------------------
                         tabItem("StockTable",
                                 fluidRow(
                                   column(width = 4,
                                          sidebarPanel(
                                            h1("Stock Data Table"),
                                            dateRangeInput("date_range_table", "Choose date range:", start = min(dataset$Data), end = max(dataset$Data), format = "yyyy-mm-dd"),
                                            pickerInput(inputId = "stocks", label = "Choose stocks", choices = possibilities, selected = possibilities, options = list(`actions-box` = TRUE), multiple = T),
                                            selectInput("benchmark1", "Want benchmarks?", selected= "no", c("yes", "no"))
                                          ),
                                          
                                   ),
                                   column(width = 9,
                                          mainPanel(
                                            dataTableOutput("StockTable")
                                          )
                                   )
                                 )
                         ),
                         ####----------------------------------page 2---------------------------------------------------------------------------------------------------------------------------
                         
                         tabItem("StockPlots",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("action1", "Choose action:", selected= "ale", possibilities),
                                     selectInput("action2", "Choose action:", selected= "alr", possibilities),
                                     dateRangeInput("date_range", "Choose date range:", start = min(dataset$Data), end = max(dataset$Data), format = "yyyy-mm-dd"),
                                     selectInput("benchmarks", "Want benchmarks?", selected= "no", c("yes", "no"))
                                     #checkboxInput("benchmarks", "Want benchmarks?", FALSE),
                                     
                                     
                                   ),
                                   mainPanel(
                                     plotlyOutput("plot1", "120%", "150%")
                                   )
                                 )),#tabitem
                         ####----------------------------------page 3---------------------------------------------------------------------------------------------------------------------------
                         
                         tabItem("CheckAction",
                                 fluidRow(
                                   column(width = 4,
                                          sidebarPanel(
                                            selectInput("checkAction", "Choose action:", selected = "ale", choices = possibilities),
                                            dateRangeInput("check_date_range", "Choose date range:", start = min(dataset$Data), end = max(dataset$Data), format = "yyyy-mm-dd"),
                                            selectInput("fit", "Add line of best fit", selected= "no", c("yes", "no")),
                                            
                                          )
                                   ),
                                   column(width = 9,
                                          mainPanel(
                                            h1(textOutput("nameinfo")),
                                            h3("Informations"),
                                            verbatimTextOutput("printinfo"),
                                            h3("Plot"),
                                            plotOutput("return_plot"),
                                            h3("Return rate"),
                                            dataTableOutput("ReturnTable", "50%")
                                          )
                                   )
                                 )
                         ),
                         ####----------------------------------page 4---------------------------------------------------------------------------------------------------------------------------
                         tabItem(
                           "CheckPortfolio",
                           fluidRow(
                             column(width = 4,
                                    sidebarPanel(
                                      ##------------------------------PODAJ DATE------------------------------------------------
                                      dateInput("single_date", "Choose a date:",  min = min(dataset$Data), max = max(dataset$Data), value = max(dataset$Data), format = "yyyy-mm-dd"),
                                      ##------------------------------PODAJ PIENIĄDZE-----------------------------------------------
                                      numericInput("money", "Enter money ", value = 2000),
                                      ##------------------------------WYBOR AKCJI PIERWSZEJ-----------------------------------
                                      tags$hr(),
                                      selectInput("action1_4", "Choose action 1 :", selected = "ale", possibilities),
                                      textOutput("action1_cost"),
                                      h5("Posibilities:"),
                                      tableOutput("multiplier_vector1"),
                                      numericInput("action1_value", "Choose amount of money:", value = 213.12),
                                      tags$hr(),
                                      ##------------------------------WYBOR AKCJI DRUGIEJ-----------------------------------
                                      selectInput("action2_4", "Choose action 2 :", selected = "cps", possibilities),
                                      textOutput("action2_cost"),
                                      h5("Posibilities:"),
                                      tableOutput("multiplier_vector2"),
                                      numericInput("action2_value", "Choose amount of money:", value = 309.00),
                                      tags$hr(),
                                      ##------------------------------WYBOR AKCJI TRZECIEJ-----------------------------------
                                      selectInput("action3_4", "Choose action 3:", selected = "kgh", possibilities),
                                      textOutput("action3_cost"),
                                      h5("Posibilities:"),
                                      tableOutput("multiplier_vector3"),
                                      numericInput("action3_value", "Choose amount of money:", value = 418.40),
                                      tags$hr(),
                                      ##------------------------------WYBOR AKCJI CZWARTEJ-----------------------------------
                                      selectInput("action4_4", "Choose action 4:", selected = "pko", possibilities),
                                      textOutput("action4_cost"),
                                      h5("Posibilities:"),
                                      tableOutput("multiplier_vector4"),
                                      numericInput("action4_value", "Choose amount of money:", value = 673.68),
                                      tags$hr(),
                                      ##------------------------------WYBOR AKCJI PIĄTEJ-----------------------------------
                                      selectInput("action5_4", "Choose action5:", selected = "opl", possibilities),
                                      textOutput("action5_cost"),
                                      h5("Posibilities:"),
                                      tableOutput("multiplier_vector5"),
                                      numericInput("action5_value", "Choose amount of money:", value = 375.44),
                                      tags$hr(),
                                      ##------------------------------KONIEC WYBORÓW AKCJI-----------------------------------
                                      actionButton("CreatePortfolio", "Create Portfolio!")
                                      
                                      
                                      
                                    )),
                             column(width = 9,
                                    mainPanel(
                                      h1("Investment Portfolio"),
                                      h5(verbatimTextOutput("remain_money")),
                                      h5(textOutput("Return")),
                                      h5(textOutput("Variance")),
                                      h5(textOutput("WIG20")),
                                      h5(textOutput("Corr")),
                                      dataTableOutput("correlation"),
                                      h5(textOutput("PSummary")),
                                      dataTableOutput("portfolioTable"),
                                      h5(textOutput("PReturn")),
                                      dataTableOutput("portfolioreturns"),
                                      h5(textOutput("PDataset")),
                                      dataTableOutput("portfoliodataset"),
                                    )
                             ))),
                         ####----------------------------------page 5---------------------------------------------------------------------------------------------------------------------------
                         tabItem(
                           "CheckPortfolio2",
                           fluidRow(
                             column(width = 4,
                                    sidebarPanel(
                                      ##------------------------------PODAJ DATE------------------------------------------------
                                      dateInput("single_date_2", "Choose a date:",  min = min(dataset$Data), max = max(dataset$Data), value = max(dataset$Data), format = "yyyy-mm-dd"),
                                      ##------------------------------PODAJ PIENIĄDZE-----------------------------------------------
                                      numericInput("money_2", "Enter money ", value = 2000),
                                      ##------------------------------WYBOR AKCJI PIERWSZEJ-----------------------------------
                                      tags$hr(),
                                      selectInput("action1_5", "Choose action 1 :", selected = "acp", possibilities),
                                      textOutput("action1_cost_2"),
                                      h5("Posibilities:"),
                                      tableOutput("multiplier_vector1_2"),
                                      numericInput("action1_value_2", "Choose amount of money:", value = 247.05),
                                      tags$hr(),
                                      ##------------------------------WYBOR AKCJI DRUGIEJ-----------------------------------
                                      selectInput("action2_5", "Choose action 2 :", selected = "cdr", possibilities),
                                      textOutput("action2_cost_2"),
                                      h5("Posibilities:"),
                                      tableOutput("multiplier_vector2_2"),
                                      numericInput("action2_value_2", "Choose amount of money:", value = 121.20),
                                      tags$hr(),
                                      ##------------------------------WYBOR AKCJI TRZECIEJ-----------------------------------
                                      selectInput("action3_5", "Choose action 3:", selected = "dnp", possibilities),
                                      textOutput("action3_cost_2"),
                                      h5("Posibilities:"),
                                      tableOutput("multiplier_vector3_2"),
                                      numericInput("action3_value_2", "Choose amount of money:", value = 416.10),
                                      tags$hr(),
                                      ##------------------------------WYBOR AKCJI CZWARTEJ-----------------------------------
                                      selectInput("action4_5", "Choose action 4:", selected = "pzu", possibilities),
                                      textOutput("action4_cost_2"),
                                      h5("Posibilities:"),
                                      tableOutput("multiplier_vector4_2"),
                                      numericInput("action4_value_2", "Choose amount of money:", value = 540.12),
                                      tags$hr(),
                                      ##------------------------------WYBOR AKCJI PIĄTEJ-----------------------------------
                                      selectInput("action5_5", "Choose action5:", selected = "mbk", possibilities),
                                      textOutput("action5_cost_2"),
                                      h5("Posibilities:"),
                                      tableOutput("multiplier_vector5_2"),
                                      numericInput("action5_value_2", "Choose amount of money:", value = 352.80),
                                      tags$hr(),
                                      ##------------------------------KONIEC WYBORÓW AKCJI-----------------------------------
                                      actionButton("CreatePortfolio_2", "Create Portfolio!")
                                      
                                      
                                      
                                    )),
                             column(width = 9,
                                    mainPanel(
                                      h1("Investment Portfolio"),
                                      h5(verbatimTextOutput("remain_money_2")),
                                      h5(textOutput("Return_2")),
                                      h5(textOutput("Variance_2")),
                                      h5(textOutput("WIG20_2")),
                                      h5(textOutput("Corr_2")),
                                      dataTableOutput("correlation_2"),
                                      h5(textOutput("PSummary_2")),
                                      dataTableOutput("portfolioTable_2"),
                                      #h5(textOutput("PReturn_2")),
                                      dataTableOutput("portfolioreturns_2"),
                                      h5(textOutput("PDataset_2")),
                                      dataTableOutput("portfoliodataset_2"),
                                    )
                             ))),
                         ####----------------------------------page 6---------------------------------------------------------------------------------------------------------------------------
                         tabItem(
                           "Comparison",
                           fluidRow(
                             column(width = 12,
                                    align = "center",
                                    h1("Compare created portfolios!")
                             )
                           ),
                           fluidRow(
                             column(width = 6,
                                    align = "left",
                                    # Left section content
                                    actionButton("ShowPortfolio1", "Show first portfolio! "),
                                    verbatimTextOutput("markowitz_1_output"),
                                    verbatimTextOutput("sharp_1_output")
                             ),
                             column(width = 6,
                                    align = "right",
                                    # Right section content
                                    actionButton("ShowPortfolio2", "Show second portfolio! "),
                                    verbatimTextOutput("markowitz_2_output"),
                                    verbatimTextOutput("sharp_2_output")
                             )
                           ),
                           fluidRow(
                             column(width = 10,
                                    align = "center",
                                    actionButton("Compare", "Compare them!"),
                                    h4( textOutput("markowitz_comparison")),
                                    h4(textOutput("sharp_comparison"))
                             )
                           )
                         )
                         
                         ##-------------------------------------------------------------------------------------------------------------------------------------
                       )#dbitems
                     )#dbbody
)#dbpage

# Define server logic required to draw a histogram
server <- function(input, output) {
  ####----------------------------------page 1---------------------------------------------------------------------------------------------------------------------------
  
  
  tabledata <-reactive({
    table<-dataset[as.Date(dataset$Data) >= as.Date(input$date_range_table[1]) &
                     as.Date(dataset$Data) <= as.Date(input$date_range_table[2]), ]%>% select("Data",input$stocks)
    tableWIG<-dataset[as.Date(dataset$Data) >= as.Date(input$date_range_table[1]) &
                        as.Date(dataset$Data) <= as.Date(input$date_range_table[2]), ]%>% select("Data","WIG20")
    if(input$benchmark1 == "yes")
    {
      table<- table%>% bind_cols(WIG20= tableWIG$WIG20)
    }
    table
  })
  
  output$StockTable<- renderDataTable(tabledata())
  
  ####----------------------------------page 2---------------------------------------------------------------------------------------------------------------------------
  
  output$plot1<- renderPlotly({
    filtered_data <- dataset[as.Date(dataset$Data) >= as.Date(input$date_range[1]) &
                               as.Date(dataset$Data) <= as.Date(input$date_range[2]), ]
    plot <- plot_ly(data = filtered_data, x = ~Data)
    plot <- add_lines(plot, y = ~get(input$action1),  name = input$action1)
    plot <- add_lines(plot, y = ~get(input$action2), name = input$action2)
    
    if(input$benchmarks == "yes")
    {
      
      plot <- add_lines(plot, y = filtered_data$WIG20, name = "WIG20")
    }
    
    plot
    
  })
  
  ####----------------------------------page 3---------------------------------------------------------------------------------------------------------------------------
  
  output$nameinfo<-renderText({paste0("    Choosen action - ", input$checkAction)})
  
  x<-reactive(summary(dataset[[input$checkAction]]))
  x_dataset<-reactive({
    dataset[as.Date(dataset$Data) >= as.Date(input$check_date_range[1]) &
              as.Date(dataset$Data) <= as.Date(input$check_date_range[2]), ]%>% select(Data, input$checkAction )
    
  })
  x_return<- reactive({
    
    return<-returns[as.Date(returns$Data) >= as.Date(input$check_date_range[1]) &
                      as.Date(returns$Data) <= as.Date(input$check_date_range[2]), ]%>% select(Data, input$checkAction )
    return[,2]<-round(return[,2], 4)
    return
  })
  
  output$printinfo <- renderPrint({
    x() #obliczenie podstawowych statystyk kursu zamknięcia
    
    
  })
  output$return_plot <- renderPlot({
    
    
    plot <- ggplot(x_dataset(), aes(Data, get(input$checkAction))) + geom_line()
    
    if (input$fit == "yes") {
      plot <- plot + geom_smooth(method = "lm")
    }
    
    plot
  })
  output$ReturnTable<- renderDataTable(x_return()%>% select(input$checkAction))
  ####----------------------------------page 4---------------------------------------------------------------------------------------------------------------------------
  
  
  # Reactive vector
  reactive_vector <- reactive({
    selected_row <- dataset[as.Date(dataset$Data) == as.Date(input$single_date), ]
    colnames(selected_row)<-colnames(dataset)
    selected_row
  })
  money<- reactive(input$money)
  used_money<- reactive(money()-input$action1_value-input$action2_value-input$action3_value-input$action4_value-input$action5_value)
  
  
  
  ##--------------------AKCJA 1--------------------------
  cost1<-reactive(reactive_vector()[[input$action1_4]])
  output$action1_cost<-renderText({paste0("Cost of one action is :", cost1())})
  output$multiplier_vector1 <- renderTable({
    if(used_money()<cost1())
    {
      df1 <- data.frame()
    }
    else{
      multiplier_vector <- calculateMultiplierVector(used_money(), cost1())
      df1 <- data.frame(Numbers = 1:length(multiplier_vector), Vector = multiplier_vector)
    }
    
    df1
  })
  ##--------------------AKCJA 2--------------------------
  cost2<-reactive(reactive_vector()[[input$action2_4]])
  output$action2_cost<-renderText({paste0("Cost of one action is :", cost2())})
  output$multiplier_vector2 <- renderTable({
    if(used_money()<cost2())
    {
      df2 <- data.frame()
    }
    else{
      multiplier_vector <- calculateMultiplierVector(used_money(), cost2())
      df2 <- data.frame(Numbers = 1:length(multiplier_vector), Vector = multiplier_vector)
    }
    df2
  })
  ##--------------------AKCJA 3--------------------------
  cost3<-reactive(reactive_vector()[[input$action3_4]])
  output$action3_cost<-renderText({paste0("Cost of one action is :", cost3())})
  output$multiplier_vector3 <- renderTable({
    if(used_money()<cost3())
    {
      df3 <- data.frame()
    }
    else{
      multiplier_vector <- calculateMultiplierVector(used_money(), cost3())
      df3 <- data.frame(Numbers = 1:length(multiplier_vector), Vector = multiplier_vector)
    }
    df3
  })
  ##--------------------AKCJA 4--------------------------
  cost4<-reactive(reactive_vector()[[input$action4_4]])
  output$action4_cost<-renderText({paste0("Cost of one action is :", cost4())})
  output$multiplier_vector4 <- renderTable({
    if(used_money()<cost4())
    {
      df4 <- data.frame()
    }
    else{
      multiplier_vector <- calculateMultiplierVector(used_money(), cost4())
      df4<- data.frame(Numbers = 1:length(multiplier_vector), Vector = multiplier_vector)
    }
    df4
  })
  ##--------------------AKCJA 5--------------------------
  cost5<-reactive(reactive_vector()[[input$action5_4]])
  output$action5_cost<-renderText({paste0("Cost of one action is :", cost5())})
  output$multiplier_vector5 <- renderTable({
    if(used_money()<cost5())
    {
      df5 <- data.frame()
    }
    else{
      multiplier_vector <- calculateMultiplierVector(used_money(), cost5())
      df5 <- data.frame(Numbers = 1:length(multiplier_vector), Vector = multiplier_vector)
    }
    df5
  })
  ##----------------------Creating data frame---------------------
  
  reactive_dataset <- eventReactive(input$CreatePortfolio,{
    filtered_data <- dataset[as.Date(dataset$Data) <= as.Date(input$single_date), ]
    filtered_data<-filtered_data%>% select("Data",input$action1_4,input$action2_4,input$action3_4,input$action4_4,input$action5_4)
    filtered_data
  })
  WIG_return<-eventReactive(input$CreatePortfolio,{
    filtered_data <- returns[as.Date(returns$Data) <= as.Date(input$single_date), ]
    filtered_data<-filtered_data%>% select("WIG20")
    filtered_data<-filtered_data[-1,]
    filtered_data
  })
  reactive_return<- eventReactive(input$CreatePortfolio,{
    return<-round(Return.calculate(reactive_dataset()),4)
    returns<-reactive_dataset()[,"Data"]%>% bind_cols(return)
    returns<- returns[-1,]
    returns
    
  })
  
  r_c1 <- eventReactive(input$CreatePortfolio, {
    result_cov <- cov(reactive_return() %>% select(input$action1_4),WIG_return())
    result_cov
  })
  r_c2 <- eventReactive(input$CreatePortfolio, {
    result_cov <- cov(reactive_return() %>% select(input$action2_4),WIG_return())
    result_cov
  })
  r_c3 <- eventReactive(input$CreatePortfolio, {
    result_cov <- cov(reactive_return() %>% select(input$action3_4),WIG_return())
    result_cov
  })
  r_c4 <- eventReactive(input$CreatePortfolio, {
    result_cov <- cov(reactive_return() %>% select(input$action4_4),WIG_return())
    result_cov
  })
  r_c5 <- eventReactive(input$CreatePortfolio, {
    result_cov <- cov(reactive_return() %>% select(input$action5_4),WIG_return())
    result_cov
  })
  dataframe<- eventReactive(input$CreatePortfolio,{
    
    row1 <- c(input$action1_value, input$action2_value, input$action3_value, input$action4_value, input$action5_value)
    row2 <- c(input$action1_value/ cost1(), input$action2_value/ cost2(), input$action3_value/ cost3(), input$action4_value/ cost4(), input$action5_value/ cost5())
    weights<- row1/sum(row1)
    weights<- round(weights, 5)
    return<- c(sum(reactive_return()[,input$action1_4])/nrow(reactive_return()),sum(reactive_return()[,input$action2_4])/nrow(reactive_return()),sum(reactive_return()[,input$action3_4])/nrow(reactive_return()),sum(reactive_return()[,input$action4_4])/nrow(reactive_return()),sum(reactive_return()[,input$action5_4])/nrow(reactive_return()))
    return<- round(return, 5)  
    variance<- c(var(reactive_return()[,input$action1_4]),var(reactive_return()[,input$action2_4]),var(reactive_return()[,input$action3_4]),var(reactive_return()[,input$action4_4]),var(reactive_return()[,input$action5_4]))
    variance<- round(variance, 5)
    sd<- c(sd(reactive_return()[,input$action1_4]),sd(reactive_return()[,input$action2_4]),sd(reactive_return()[,input$action3_4]),sd(reactive_return()[,input$action4_4]),sd(reactive_return()[,input$action5_4]))
    sd<- round(sd, 5)
    result_cov<-c(r_c1(),r_c2(),r_c3(),r_c4(),r_c5())
    result_cov<- round(result_cov,5)
    Beta_value<- round(result_cov/variance,5)
    df <- data.frame( ActionCost= row1, ActionAmount = row2, Weights= weights, Returns= return, Variance= variance, Stand.Dev= sd, Covariance.WIth.WIG20= result_cov, Beta= Beta_value)
    tdf<- t(df)
    colnames(tdf) <-c(input$action1_4,input$action2_4,input$action3_4,input$action4_4,input$action5_4)
    tdf
  })
  Return<-eventReactive(input$CreatePortfolio,{
    rt<- sum(dataframe()[4,]*dataframe()[3,])
    rt
  })
  Variance<-
    eventReactive(input$CreatePortfolio,{
      vr<- mean(dataframe()[5,])
      vr
    })
  Correlation <-eventReactive(input$CreatePortfolio,{
    correlation<-"Correlation between actions:"
    correlation
  } )
  PortfolioReturn<-eventReactive(input$CreatePortfolio,{
    portfolioReturn<-"Action returns table:"
    portfolioReturn
  })
  
  PortfolioDataset<-eventReactive(input$CreatePortfolio,{
    portfolioDataset<-"Action returns table:"
    portfolioDataset
  })
  PortfolioSummary<- eventReactive(input$CreatePortfolio,{
    portfolioSummary<- "Summary of Portfolio"
    portfolioSummary
  })
  
  output$Corr<- renderText({paste0(Correlation())})
  output$PReturn<- renderText({paste0(PortfolioReturn())})
  output$PDataset<- renderText({paste0(PortfolioDataset())})
  output$PSummary<- renderText({paste0(PortfolioSummary())})
  output$Return<-renderText({paste0("Portfolio return rate = ", round(Return(),5))})
  output$Variance<-renderText({paste0("Portfolio Variance = ", round(Variance(),5))})
  output$WIG20<-renderText({paste0("WIG20 return rate = ", round(mean(WIG_return()),5))})
  
  ##----------showing correlation--------
  output$correlation<- renderDataTable({
    round(cor(reactive_return()%>% select(input$action1_4,input$action2_4,input$action3_4,input$action4_4,input$action5_4)),4)
  })
  ##----------showing dataframe--------
  output$portfolioTable<- renderDataTable({
    dataframe()
  })
  ###--------Showing money------------
  output$remain_money <- renderPrint({
    # Display the used_money value
    paste("Amount of remaining money:", used_money())
  })
  ##------- Showing returns------------
  output$portfolioreturns<- renderDataTable({
    reactive_return()
  })
  ##------- Showing dataframe------------
  output$portfoliodataset<- renderDataTable({
    reactive_dataset()
  })
  
  ####----------------------------------page 5---------------------------------------------------------------------------------------------------------------------------
  
  
  # Reactive vector
  reactive_vector_2 <- reactive({
    selected_row <- dataset[as.Date(dataset$Data) == as.Date(input$single_date_2), ]
    colnames(selected_row)<-colnames(dataset)
    selected_row
  })
  money_2<- reactive(input$money_2)
  used_money_2<- reactive(money_2()-input$action1_value_2-input$action2_value_2-input$action3_value_2-input$action4_value_2-input$action5_value_2)
  
  
  
  ##--------------------AKCJA 1--------------------------
  cost1_2<-reactive(reactive_vector()[[input$action1_5]])
  output$action1_cost_2<-renderText({paste0("Cost of one action is :", cost1_2())})
  output$multiplier_vector1_2 <- renderTable({
    if(used_money_2()<cost1_2())
    {
      df1 <- data.frame()
    }
    else{
      multiplier_vector <- calculateMultiplierVector(used_money_2(), cost1_2())
      df1 <- data.frame(Numbers = 1:length(multiplier_vector), Vector = multiplier_vector)
    }
    
    df1
  })
  ##--------------------AKCJA 2--------------------------
  cost2_2<-reactive(reactive_vector()[[input$action2_5]])
  output$action2_cost_2<-renderText({paste0("Cost of one action is :", cost2_2())})
  output$multiplier_vector2_2 <- renderTable({
    if(used_money_2()<cost2_2())
    {
      df2 <- data.frame()
    }
    else{
      multiplier_vector <- calculateMultiplierVector(used_money_2(), cost2_2())
      df2 <- data.frame(Numbers = 1:length(multiplier_vector), Vector = multiplier_vector)
    }
    df2
  })
  ##--------------------AKCJA 3--------------------------
  cost3_2<-reactive(reactive_vector()[[input$action3_5]])
  output$action3_cost_2<-renderText({paste0("Cost of one action is :", cost3_2())})
  output$multiplier_vector3_2 <- renderTable({
    if(used_money_2()<cost3_2())
    {
      df3 <- data.frame()
    }
    else{
      multiplier_vector <- calculateMultiplierVector(used_money_2(), cost3_2())
      df3 <- data.frame(Numbers = 1:length(multiplier_vector), Vector = multiplier_vector)
    }
    df3
  })
  ##--------------------AKCJA 4--------------------------
  cost4_2<-reactive(reactive_vector()[[input$action4_5]])
  output$action4_cost_2<-renderText({paste0("Cost of one action is :", cost4_2())})
  output$multiplier_vector4_2 <- renderTable({
    if(used_money_2()<cost4_2())
    {
      df4 <- data.frame()
    }
    else{
      multiplier_vector <- calculateMultiplierVector(used_money_2(), cost4_2())
      df4<- data.frame(Numbers = 1:length(multiplier_vector), Vector = multiplier_vector)
    }
    df4
  })
  ##--------------------AKCJA 5--------------------------
  cost5_2<-reactive(reactive_vector()[[input$action5_5]])
  output$action5_cost_2<-renderText({paste0("Cost of one action is :", cost5_2())})
  output$multiplier_vector5_2 <- renderTable({
    if(used_money_2()<cost5_2())
    {
      df5 <- data.frame()
    }
    else{
      multiplier_vector <- calculateMultiplierVector(used_money_2(), cost5_2())
      df5 <- data.frame(Numbers = 1:length(multiplier_vector), Vector = multiplier_vector)
    }
    df5
  })
  ##----------------------Creating data frame---------------------
  
  reactive_dataset_2 <- eventReactive(input$CreatePortfolio_2,{
    filtered_data <- dataset[as.Date(dataset$Data) <= as.Date(input$single_date_2), ]
    filtered_data<-filtered_data%>% select("Data",input$action1_5,input$action2_5,input$action3_5,input$action4_5,input$action5_5)
    filtered_data
  })
  WIG_return_2<-eventReactive(input$CreatePortfolio_2,{
    filtered_data <- returns[as.Date(returns$Data) <= as.Date(input$single_date_2), ]
    filtered_data<-filtered_data%>% select("WIG20")
    filtered_data<-filtered_data[-1,]
    filtered_data
  })
  reactive_return_2<- eventReactive(input$CreatePortfolio_2,{
    return_2<-round(Return.calculate(reactive_dataset_2()),4)
    returns<-reactive_dataset_2()[,"Data"]%>% bind_cols(return_2)
    returns<- returns[-1,]
    returns
    
  })
  r_c1_2 <- eventReactive(input$CreatePortfolio, {
    result_cov <- cov(reactive_return_2() %>% select(input$action1_5),WIG_return_2())
    result_cov
  })
  r_c2_2 <- eventReactive(input$CreatePortfolio, {
    result_cov <- cov(reactive_return_2() %>% select(input$action2_5),WIG_return_2())
    result_cov
  })
  r_c3_2 <- eventReactive(input$CreatePortfolio, {
    result_cov <- cov(reactive_return_2() %>% select(input$action3_5),WIG_return_2())
    result_cov
  })
  r_c4_2 <- eventReactive(input$CreatePortfolio, {
    result_cov <- cov(reactive_return_2() %>% select(input$action4_5),WIG_return_2())
    result_cov
  })
  r_c5_2 <- eventReactive(input$CreatePortfolio, {
    result_cov <- cov(reactive_return_2() %>% select(input$action5_5),WIG_return_2())
    result_cov
  })
  
  dataframe_2<- eventReactive(input$CreatePortfolio_2,{
    
    row1_2 <- c(input$action1_value_2, input$action2_value_2, input$action3_value_2, input$action4_value_2, input$action5_value_2)
    row2_2 <- c(input$action1_value_2/ cost1_2(), input$action2_value_2/ cost2_2(), input$action3_value_2/ cost3_2(), input$action4_value_2/ cost4_2(), input$action5_value_2/ cost5_2())
    weights_2<- row1_2/sum(row1_2)
    weights_2<- round(weights_2, 5)
    return_2<- c(sum(reactive_return_2()[,input$action1_5])/nrow(reactive_return_2()),sum(reactive_return_2()[,input$action2_5])/nrow(reactive_return_2()),sum(reactive_return_2()[,input$action3_5])/nrow(reactive_return_2()),sum(reactive_return_2()[,input$action4_5])/nrow(reactive_return_2()),sum(reactive_return_2()[,input$action5_5])/nrow(reactive_return_2()))
    return_2<- round(return_2, 5)  
    variance_2<- c(var(reactive_return_2()[,input$action1_5]),var(reactive_return_2()[,input$action2_5]),var(reactive_return_2()[,input$action3_5]),var(reactive_return_2()[,input$action4_5]),var(reactive_return_2()[,input$action5_5]))
    variance_2<- round(variance_2, 5)
    sd_2<- c(sd(reactive_return_2()[,input$action1_5]),sd(reactive_return_2()[,input$action2_5]),sd(reactive_return_2()[,input$action3_5]),sd(reactive_return_2()[,input$action4_5]),sd(reactive_return_2()[,input$action5_5]))
    sd_2<- round(sd_2, 5)
    result_cov_2<-c(r_c1_2(),r_c2_2(),r_c3_2(),r_c4_2(),r_c5_2())
    result_cov_2<- round(result_cov_2,5)
    Beta_value_2<- round(result_cov_2/variance_2,5)
    df_2 <- data.frame( ActionCost= row1_2, ActionAmount = row2_2, Weights= weights_2, Returns= return_2, Variance= variance_2, Stand.Dev= sd_2, Covariance.WIth.WIG20= result_cov_2, Beta= Beta_value_2)
    tdf_2<- t(df_2)
    colnames(tdf_2) <-c(input$action1_5,input$action2_5,input$action3_5,input$action4_5,input$action5_5)
    tdf_2
  })
  Return_2<-eventReactive(input$CreatePortfolio_2,{
    rt<- sum(dataframe_2()[4,]*dataframe_2()[3,])
    rt
  })
  Variance_2<-eventReactive(input$CreatePortfolio_2,{
    vr<- mean(dataframe_2()[5,])
    vr
  })
  Correlation_2 <-eventReactive(input$CreatePortfolio_2,{
    correlation<-"Correlation between actions:"
    correlation
  } )
  PortfolioReturn_2 <-eventReactive(input$CreatePortfolio_2,{
    preturn<-"Portfolio return rate:"
    preturn
  } )
  
  PortfolioDataset_2<-eventReactive(input$CreatePortfolio_2,{
    portfolioDataset<-"Action returns table:"
    portfolioDataset
  })
  PortfolioSummary_2<- eventReactive(input$CreatePortfolio_2,{
    portfolioSummary<- "Summary of Portfolio"
    portfolioSummary
  })
  
  output$Corr_2<- renderText({paste0(Correlation_2())})
  output$PReturn_2<- renderText({paste0(PortfolioReturn_2())})
  output$PDataset_2<- renderText({paste0(PortfolioDataset_2())})
  output$PSummary_2<- renderText({paste0(PortfolioSummary_2())})
  output$Return_2<-renderText({paste0("Portfolio return rate = ", round(Return_2(),5))})
  output$Variance_2<-renderText({paste0("Portfolio Variance = ", round(Variance_2(),5))})
  output$WIG20_2<-renderText({paste0("WIG20 return rate = ", round(mean(WIG_return_2()),5))})
  
  ##----------showing correlation--------
  output$correlation_2<- renderDataTable({
    round(cor(reactive_return_2()%>% select(input$action1_5,input$action2_5,input$action3_5,input$action4_5,input$action5_5)),4)
  })
  ##----------showing dataframe--------
  output$portfolioTable_2<- renderDataTable({
    dataframe_2()
  })
  ###--------Showing money------------
  output$remain_money_2 <- renderPrint({
    paste("Amount of remaining money:", used_money_2())
  })
  ##------- Showing returns------------
  output$portfolioreturns_2<- renderDataTable({
    reactive_return_2()
  })
  ##------- Showing dataframe------------
  output$portfoliodataset_2<- renderDataTable({
    reactive_dataset_2()
  })
  ##---------------------------------------page 6 ------------------------------------------------------------------------------------------
  
  markowitz_1<- eventReactive(input$ShowPortfolio1,
                              {
                                markowitz<-round(Return()/Variance(),5)
                                markowitz
                              })
  
  markowitz_2<- eventReactive(input$ShowPortfolio2,
                              {
                                markowitz<-round(Return_2()/Variance_2(),5)
                                markowitz
                              })
  Sharp_1<- eventReactive(input$ShowPortfolio1,
                          {
                            Sharp<-round(sum(dataframe()[8,]*dataframe()[3,]),5)
                            Sharp
                          })
  Sharp_2<- eventReactive(input$ShowPortfolio2,
                          {
                            Sharp<-round(sum(dataframe_2()[8,]*dataframe_2()[3,]),5)
                            Sharp
                          })
  output$markowitz_1_output <- renderPrint({
    markowitz_text <- paste("Markowitz Ratio of first portfolio is:", markowitz_1())
    markowitz_text
  })
  
  output$markowitz_2_output <- renderPrint({
    markowitz_text <- paste("Markowitz Ratio of second portfolio is:", markowitz_2())
    markowitz_text
  })
  
  output$sharp_1_output <- renderText({
    sharp_text <- paste("Sharp Ratio of first portfolio is:", Sharp_1())
    sharp_text
  })
  
  output$sharp_2_output <- renderText({
    sharp_text <- paste("Sharp Ratio of second portfolio is:", Sharp_2())
    sharp_text
  })
  
  
  answear_Markowitz<- eventReactive(input$Compare, {
    if(markowitz_1()>markowitz_2())
    {
      ans<- "Comparing stocks in terms of the Markowitz ratio, first portfolio is better."
    }
    else if(markowitz_1()<markowitz_2())
    {
      ans<- "Comparing stocks in terms of the Markowitz ratio, second portfolio is better."
    }
    else
    {
      ans<- "Comparing stocks in terms of the Markowitz ratio, both are the same."
    }
    ans
  })
  answear_Sharpe<- eventReactive(input$Compare, {
    if(Sharp_1()>Sharp_2())
    {
      ans<- "Comparing stocks in terms of the Sharp'e ratio, with the increase in the rate of return from the WIG20, the rate of return of the first portfolio increases more than the rate of return of the second portfolio ."
    }
    else if(Sharp_1()<Sharp_2())
    {
      ans<- "Comparing stocks in terms of the Sharp'e ratio, with the increase in the rate of return from the WIG20, the rate of return of the second portfolio increases more than the rate of return of the first portfolio."
    }
    else
    {
      ans<- "Comparing stocks in terms of the Sharp'e ratio, both are the same."
    }
    ans
  })
  output$markowitz_comparison <- renderPrint({
    paste0(answear_Markowitz())
    
  })
  
  output$sharp_comparison <- renderPrint({
    paste0(answear_Sharpe())
  })
}


# Run the application
shinyApp(ui = ui, server = server)