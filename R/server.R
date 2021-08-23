server <- function(input, output, session) {

  library(dplyr)
  library(lubridate)
  library(purrr)
  library(readr)
  library(readxl)
  library(shiny)
  library(composr)
  library(stringr)
  library(stringi)
  library(shinyjs)
  library(rlang)
  library(writexl)
  library(openxlsx)

  shinyjs::disable("dwclog")
  shinyjs::disable("dwclean")
  shinyjs::disable("dwconv")

  shinyjs::disable("runchecks")
  shinyjs::disable("runclean")
  shinyjs::disable("runconv")

  output$ui.db<-renderUI({
    if(input$action%!in%c(NULL,"")){
      shiny::fileInput("data", "DATASET (.csv or .xlsx)", accept = c(".csv",".xlsx",".xls"))
    }
  })
  output$ui.questionnaire<-renderUI({
    if(input$action%!in%c(NULL,"")){
      shiny::fileInput("questionnaire", "Questionnaire EXCEL workbook", accept = c(".xlsx",".xls"))
    }
  })
  output$ui.choiceslabel<-renderUI({
    req(input$questionnaire)
    selectInput("choiceslabel","Label column for choices tab", choices = names(choices()))
  })
  output$ui.surveylabel<-renderUI({
    req(input$questionnaire)
    selectInput("surveylabel","Label column for survey tab", choices = names(survey()))

  })
  output$ui.clog<-renderUI({
    if(input$action=="clean"){
      shiny::fileInput("clog", "Cleaning LOG (.csv or .xlsx)", accept = c(".csv",".xlsx",".xls"))
    }
  })
  output$ui.cloglabel<-renderUI({
    if(input$action=="clog"){
      shiny::selectizeInput(
        'cloglabel', "Do you want to label questions/choices in the Cleaning LOG?", choices = setNames(c("no","yes"),c("No","Yes")),
        options = list(
          placeholder = 'Please choose of the options below',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    }
  })
  output$ui.f2f_phone<-renderUI({
    if(input$action=="clog"){
      shiny::selectizeInput(
        'f2f_phone', "Data Collection Method", choices = setNames(c("f2f","phone"),c("Face to face","Telephone")),
        options = list(
          placeholder = 'Please choose one of the options below',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    }
  })

  db <- shiny::reactive({
    shiny::req(input$data)
    load_file(input$data$name,input$data$datapath) %>% prepdata(.)
  })
  survey <- shiny::reactive({
    shiny::req(input$questionnaire)
    df<-readxl::read_excel(input$questionnaire$datapath,"survey",col_types = "text")
  })
  choices <- shiny::reactive({
    shiny::req(input$questionnaire)
    df<-readxl::read_excel(input$questionnaire$datapath,"choices",col_types = "text")
  })
  clog <- shiny::reactive({
    shiny::req(input$clog)
    load_file(input$clog$name,input$clog$datapath)
  })

  observe({
    stateaction<-input$action
    statedata<-input$data
    statequestionnaire<-input$questionnaire
    stateclog<-input$clog
    if(stateaction=="clog"&!is.null(statedata)&!is.null(statequestionnaire)){enable("runchecks")} else{disable("runchecks")}
  })
  observe({
    stateaction<-input$action
    statedata<-input$data
    statequestionnaire<-input$questionnaire
    stateclog<-input$clog
    if(stateaction=="clean"&!is.null(statedata)&!is.null(statequestionnaire)&!is.null(stateclog)){enable("runclean")} else{disable("runclean")}
  })
  observe({
    stateaction<-input$action
    statedata<-input$data
    statequestionnaire<-input$questionnaire
    if(stateaction%in%c("xml_tolabel")&!is.null(statedata)&!is.null(statequestionnaire)){enable("runconv")} else{disable("runconv")}
  })

  check<-reactive({
    oth_check<-other_check(db(),survey())
    logbook<-apply_checks(db(), dc_method = input$f2f_phone)
    clog<-bind_rows(logbook,oth_check)
    if(input$cloglabel=="yes"){clog<-label_clog(clog,survey(),choices(),input$surveylabel,input$choiceslabel)}
    shinyjs::enable("dwclog")
    if(input$cloglabel=="yes"){
      shinyjs::html("dwclog", "Download labeled Cleaning LOG")
    } else{shinyjs::html("dwclog", "Download Cleaning LOG")}
    clog

  })

  forout_clog <- reactiveValues()
  observeEvent(input$runchecks, {
    x<-check()
    forout_clog$x=x
  })
  clean<-reactive({
    db<-db()[which(!is.na(db()$uuid)),]
    db<- cleaning_data(db(),clog(),survey(),choices())
    shinyjs::enable("dwclean")
    shinyjs::html("dwclean", "Download Clean data")
    db

  })
  forout_clean <- reactiveValues()
  observeEvent(input$runclean, {
    x<-clean()
    forout_clean$x=x
  })

  conv<-reactive({
    db<-from_xml_tolabel(db(),choices(),survey(),input$choiceslabel,input$surveylabel)
    shinyjs::enable("dwconv")
    shinyjs::html("dwconv", "Download Converted DATA")
    db
  })
  forout_conv <- reactiveValues()
  observeEvent(input$runconv, {
    x<-conv()
    forout_conv$x=x
  })

  output$dwclog <- shiny::downloadHandler(
    filename = function() {
      if(input$cloglabel=="no"){
        paste0("cleaninglog-",humanTime(),".csv")
      }else {paste0("labeled-cleaninglog-",humanTime(),".csv")}
    },
    content = function(file) {
      xout<-forout_clog$x
      write_excel_csv(xout, file)
    }
  )
  output$dwclean <- shiny::downloadHandler(
    filename = function() {
      paste0("cleandata-",humanTime(),".csv")
    },
    content = function(file) {
      xout<-forout_clean$x
      write_excel_csv(xout, file)
    }
  )
  output$dwconv <- shiny::downloadHandler(
    filename = function() {
      paste0("Converted_data-",humanTime(),".csv")
    },
    content = function(file) {
      xout<-forout_conv$x
      write_excel_csv(xout, file)
    }
  )
}
