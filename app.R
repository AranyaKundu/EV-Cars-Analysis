source("cbproj.R", local=T)
source("tsa.R", local = T)
source("dashboard.R", local = T)


library(shiny)
library(shinyAce)
library(shinyBS)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(DT)


ui <- shinyUI(
  dashboardPage(
    header = dashboardHeader(title = "Atliq Motors"), 
    skin = "purple",
    sidebar = dashboardSidebar(
      collapsed = T,
      sidebarMenu(
        menuItem("Home", icon = icon("house", verify_fa = F), tabName = "homeTab"),
        menuItem("DataSet", # Create DataSet Tab
                 icon = icon("table", verify_fa = F), tabName = "dataSets"),
        menuItem("Data Visualization", icon = icon("table", verify_fa = F), tabName = "VizCraft",
                 newtab = F, startExpanded = F,
                 
                 menuSubItem(text = "Top-Bottom Data", tabName = "dataViz", icon = icon("database")),
                 
                 menuSubItem(text = "Advanced Bar Charts", tabName = "advBar", icon = icon("fa-solid fa-chart-bar")),
                 
                 menuSubItem("Time Series Plots", tabName = "tsplots", icon = icon("fa-solid fa-chart-line"))
                 ),
        menuItem("Compare Models", icon = icon("fa-duotone fa-magnifying-glass", verify_fa = F),
                 tabName = "compare"),
        menuItem("Dashboard", icon = icon("fa-solid fa-tachometer-alt", verify_Fa = F),
                 tabName = "dashboard")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          # code for home tab
          tabName = "homeTab", 
          div(class = "container", tags$img(src = "evcar.jpg"))
        ), 
        tabItem(
          # Code for Data Sets Tab
          tabName = "dataSets",
          fluidRow(
            column(7, DTOutput("stateData", width = "90%")),
            column(5, DTOutput("makerData", width = "100%"))
          )
        ),
        tabItem(
          tabName = "dataViz",
          fluidRow(
            column(2, selectInput("selectDF", label = "Choose Dataset",
                                  choices = c("EV Sales by Maker", "EV Sales by State"))),
            column(2, selectInput("selectCategory", label = "Choose Vehicle Type", 
                                  choices = c("2-Wheelers", "4-Wheelers"))),
            column(2, selectInput("selectYear", label = "Choose Year",
                                  choices = year_list)),
            column(2, numericInput("topN", "Choose Top N", min = 1, max = 10, step = 1, value = 1)),
            column(2, numericInput("bottomN", "Choose Bottom N", min = 1, max = 10, step = 1, value = 1)),
            column(1, HTML("<br><br>"), actionButton("showPlot1", class = "btn1", label = "Display"))
          ),
          fluidRow(withSpinner(plotOutput("coolplot1", width = "95%", height = "700px")))
        ),
        tabItem(
          tabName = "advBar", 
          fluidRow(
            column(2, 
                   selectInput("datatouse", label = "By Maker or State", choices = c("Makers", "States"))
            ),
            column(2, 
                   selectInput("vehicleCategory", label = "Choose Vehicle Type", 
                               choices = c("2-Wheelers", "4-Wheelers"))
            ),
            column(2, 
                   conditionalPanel(
                     condition = "input.datatouse == 'Makers'",
                      pickerInput(inputId = "selectMakers", label = "Choose Maker(s)",
                                 choices = maker_list, selected = NULL, 
                                 options = list(`actions-box` = T, `live-search` = TRUE),
                                 multiple = TRUE)
                   ),
                   conditionalPanel(
                     condition = "input.datatouse == 'States'",
                     pickerInput("selectStates", label = "Choose State(s)", 
                                 choices = state_list, selected = state_list, 
                                 options = list(`actions-box` = TRUE, `live-search` = T), 
                                 multiple = TRUE)
                   )
            ),
            column(2, 
                   pickerInput("selectYears", label = "Select Year(s)", 
                               choices = year_list, selected = year_list, 
                               options = list(`actions-box` = TRUE, `live-search` = T), 
                               multiple = TRUE)
            ),
            column(2, 
                   pickerInput("selectMonths", label = "Select Month(s)", 
                               choices = month_list, selected = NULL, 
                               options = list(`actions-box` = TRUE, `live-search` = T), 
                               multiple = TRUE)
            ),
            column(1, 
                   HTML("<br><br>"),
                   actionButton("showPlot2", class = "btn1", label = "Display")),
            column(1)
          ),
          fluidRow(
            div(class = "datashow", 
                div(style = "width: 95%;",
                  withSpinner(plotOutput("coolplot2", width = "100%", height = "77vh"), 
                               type = 4, color = "purple"))
            )
          )
        ),
        tabItem(tabName = "tsplots", 
          fluidRow(
            column(2, selectInput(inputId = "maker_or_state", label = "Choose Maker or State",
                                  choices = c("Maker", "State"))),
            column(2, conditionalPanel(
              condition = "input.maker_or_state == 'Maker'",
              pickerInput(inputId = "selectmakers", label = "Select Maker(s)",
                          choices = maker_list, selected = NULL,
                          options = list(`actions-box` = T, `live-search` = T),
                          multiple = T)
              ),
              conditionalPanel(
                condition = "input.maker_or_state == 'State'",
                pickerInput("selectstates", label = "Choose State(s)", 
                            choices = state_list, selected = state_list, 
                            options = list(`actions-box` = T, `live-search` = T), 
                            multiple = T)
              )
              ),
            column(2, HTML("<br><br>"), actionButton("showPlot3", class = "btn1", label = "Display"))
            ),
          fluidRow(
            div(class = "datashow", 
                div(style = "width: 95%;",
                    withSpinner(plotOutput("coolplot3", width = "100%", height = "77vh"), 
                                type = 4, color = "violetred"))
            )
          )
        ),
        tabItem(
          tabName = "compare",
          fluidRow(
            column(2, selectInput(inputId = "maker_state_ts", label = "Choose Maker or State",
                                  choices = c("Maker", "State"))),
            column(2, conditionalPanel(
              condition = "input.maker_state_ts == 'Maker'",
              selectInput(inputId = "selectmaker", label = "Select Maker", choices = maker_list)
            ),
            conditionalPanel(
              condition = "input.maker_state_ts == 'State'",
              selectInput("selectstate", label = "Choose State", choices = state_list)
            )
          ),
          column(2,
            selectInput("nTrain", label = "Train Period", choices = c(18:30), selected = 24)
          ),
          column(2, HTML("<br><br>"), actionButton("compute", class = "btn1", label = "Compute"))
          ),
          div(class="datashow", DTOutput("ts_accuracy") %>% 
                withSpinner(type = 2, color = "tomato4", color.background = "aliceblue"))
        ),
        tabItem(
          tabName = "dashboard",
          fluidRow(
            column(2, selectInput(inputId = "maker_or_state", label = "Choose Maker/State", 
                                  choices = c("Maker", "State"), multiple = F)),
            column(2, HTML('<br><br>'), actionButton("showPlot4", class = "btn1", label = "Plot")),
            column(4, HTML("<br><br>"), actionButton("decline", class = "btn1", label = "Decline Table by State")),
            column(4, HTML('<br><br>'), actionButton("makerdecline", class = "btn1", 
                                                     label = "Decline Table by Maker"))
          ),
          fluidRow(
            column(4, 
                   withSpinner(plotOutput("coolplot4", width = "100%", height = "35vh"), 
                                type = 4, color = "seagreen4")),
            column(4,
              div(class="datashow", DTOutput("decline_dt", width = "100%", height = "35vh") %>% 
                withSpinner(type = 2, color = "tomato4", color.background = "tomato4")
              )
            ), 
            column(4,
                   div(class="datashow", DTOutput("decline_maker", width = "100%", height = "35vh") %>% 
                         withSpinner(type = 2, color = "tomato4", color.background = "tomato4")
                   )
            )
          ),
          fluidRow(
            column(2, selectInput(inputId = "state1", label = "Select State 1", choices = state_list, 
                                  selected = "Delhi")),
            column(2, selectInput(inputId = "state2", label = "Select State 2", choices = state_list, 
                                  selected = "Karnataka")), 
            column(2, pickerInput(inputId = "makers", label = "Select Makers", choices = maker_list, 
                                  selected = maker_list, multiple = T, 
                                  options = list(`actions-box` = T, `live-search` = T))),
            column(2, selectInput(inputId = "groups_maker", label = "Group By", multiple = T, 
                                  selected = c("year", "month"), choices = c("year", "month"))),
            column(2, pickerInput(inputId = "states", label = "Select State", choices = state_list, 
                                  selected = c("Delhi", "Karnataka", "Maharashtra", "Andhra Pradesh"), multiple = T, 
                                  options = list(`actions-box` = T, `live-search` = T))),
            column(2, selectInput(inputId = "groups_state", label = "Group By", multiple = T, 
                                  selected = c("year", "month"), choices = c("year", "month")))
          ),
          fluidRow(
            column(4, plotOutput("coolplot5", width = "100%", height = "35vh") %>% 
                   withSpinner(type = 3, color = "navy", color.background = "aliceblue")),
            column(4, DTOutput("coolDT1", width = "100%") %>% withSpinner(type = 6)),
            column(4, DTOutput("coolDT2", width = "100%") %>% withSpinner(type = 7))
          )
        )
        # New TabItem to be added here
      ),
      # Place it within the body tag, not within page tag
      tags$head(
        tags$script(src = "sidebar.js"),
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        tags$link(rel="stylesheet", type = "text/css",
                  href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.3.0/css/fontawesome.min.css"),
        tags$link(rel = "shortcut icon", href = "logo.ico")
      ),
      tags$style("#resultTable{font-size: 20px}") # increase font-size of textOutput
    )
  )
)


server <- (
  shinyServer(function(input, output, session){
    data <- eventReactive(input$display, {
      if (input$selectData == "EV Sales by Maker"){
        evsales_bymaker
      } else {
        evsales_bystate
      }
    })
    
    output$stateData <- renderDataTable({
      req(evsales_bystate)
      datatable(evsales_bystate, options = list(scrollY = "77vh", pageLength = 16))
    })
    
    output$makerData <- renderDT({
      req(evsales_bymaker)
      datatable(evsales_bymaker,options = list(scrollY = "77vh", pageLength = 12))
    })
    
    cool_plot1 <- eventReactive(input$showPlot1, {
      if (input$selectDF == "EV Sales by Maker"){
        top_and_bottom_makers_by_wheels(top_n = input$topN, 
                                        bottom_n = input$bottomN, 
                                        wheels_count = input$selectCategory, 
                                        year=input$selectYear)
      }
      else{
        top_and_bottom_states_by_wheels(top_n = input$topN, 
                                        bottom_n = input$bottomN, 
                                        wheels_count = input$selectCategory, 
                                        year=input$selectYear)
      }
    })
    
    output$coolplot1 <- renderPlot(cool_plot1())
    
    
    cool_plot2 <- eventReactive(input$showPlot2, {
      if(input$datatouse == "Makers"){
        selected_makers_plot(makers = input$selectMakers, 
                             wheels_count = input$vehicleCategory, 
                             year = input$selectYears, 
                             month = input$selectMonths)
      }
      else{
        selected_states_plot(states = input$selectStates, 
                             wheels_count = input$vehicleCategory, 
                             year = input$selectYears, 
                             month = input$selectMonths)
      }
    })
    
    output$coolplot2 <- renderPlot(cool_plot2())
    
    cool_plot3 <- eventReactive(input$showPlot3, {
      if (input$maker_or_state == 'Maker'){
        ts_bymaker_plot(makers = input$selectmakers)
      }else {
        ts_bystate_plot(states = input$selectstates)
      }
    })
    
    output$coolplot3 <- renderPlot(cool_plot3())
    
    accr_df <- eventReactive(input$compute, {
      if(input$maker_state_ts == "Maker"){
        ts_models_maker(maker = input$selectmaker, train = input$nTrain)
      }else{
        ts_models_state(state = input$selectstate, train = input$nTrain)
      }
    })
    
    output$ts_accuracy <- renderDT(datatable(accr_df()) %>% 
                          formatRound(columns = colnames(accr_df())))
    
    # c("ME", "RMSE", "MAE", "MPE", "MAPE", "ACF1", "Theil's U")
    cool_plot4 <- eventReactive(input$showPlot4, {
      if (input$maker_or_state == "Maker") {
        fwhlr_qtrend_bymaker()
      } else {fwhlr_qtrend_bystate()}
    })
    
    output$coolplot4 <- renderPlot(cool_plot4())
    
    state_df <- eventReactive(input$decline, {
      sales_decline()
    })
    
    output$decline_dt <- renderDT(
      datatable(state_df(), options = list(pageLength = 2)) %>% 
        formatRound(columns = "Decline percentage", digits = 2))
    
    maker_df <- eventReactive(input$makerdecline, {
      maker_decline()
    })
    
    output$decline_maker <- renderDataTable({
      datatable(data = maker_df(), options = list(pageLength = 2)) %>% 
        formatRound(columns = "Decline percentage", digits = 2)
    })
    
    cool_plot5 <- reactive({
      req(input$state1, input$state2)
      states_compare(state1 = input$state1, state2 = input$state2)
    })
    
    output$coolplot5 <- renderPlot(cool_plot5())
    
    
    cool_Dt1 <- reactive({
      req(input$groups_maker, input$makers)
      filter_maker(makers = input$makers, groups = input$groups_maker)
    })
    
    output$coolDT1 <- renderDataTable({
      datatable(data = cool_Dt1(), options = list(pageLength = 3))
    })
    
    cool_Dt2 <- reactive({
      req(input$groups_state, input$states)
      filter_state(states = input$states, groups = input$groups_state)
    })
    
    output$coolDT2 <- renderDT({
      datatable(data = cool_Dt2(), options = list(pageLength = 3))
    })
    
  })
)

shinyApp(ui = ui, server = server)