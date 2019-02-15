

credentials <- list("test" = "123")

shinyServer(function(input, output) {
  shinyURL.server()
  
  USER <- reactiveValues(Logged = FALSE)
  
  observeEvent(input$.login, {
    if (isTRUE(credentials[[input$.username]]==input$.password)){
      USER$Logged <- TRUE
    } else {
      show("message")
      output$message = renderText("Invalid user name or password")
      delay(2000, hide("message", anim = TRUE, animType = "fade"))
    }
  })
  
  output$app = renderUI(
    if (!isTRUE(USER$Logged)) {
      fluidRow(column(width=4, offset = 4,
                      wellPanel(id = "login",
                                textInput(".username", "Username:"),
                                passwordInput(".password", "Password:"),
                                div(actionButton(".login", "Log in"), style="text-align: center;")
                      ),
                      textOutput("message")
      ))
    } else {
      # Sidebar with a slider input for number of bins
      # sidebarLayout(
      #   sidebarPanel(
      #     sliderInput("bins",
      #                 "Number of bins:",
      #                 min = 1,
      #                 max = 50,
      #                 value = 30),
      
      navbarPage("Dati di attività della struttuta complessa BG-SO-VA",
                 
                 
                 ######STRUTTURA COMPLESSA#################################             
                 tabPanel("Struttura complessa ",
                          fluidPage(
                            fluidRow(
                              
                              column(6,div(style="height:50px"),dygraphOutput('dygraph')),
                              
                              column(6,div(style="height:50px"),dygraphOutput('vargraf'))),
                            
                            br(),
                            
                            
                            fluidRow( 
                              
                              column(6,div(style="height:50px", align="center",sliderInput("mesi", "smoothing value", 0,48,1))),
                              
                              column(6,div(style="height:50px",align="center",
                                           selectInput("set", "settore",
                                                       c(unique(as.character(dati$settore))))))),
                            
                            br(),
                            br(),
                            hr()
                            
                            
                            
                          )
                 ),
                 #######REPARTO######################
                 tabPanel("Reparto",
                          fluidPage(
                            
                            fluidRow(
                              column(12,div(style="height:50px",align="center",
                                            selectInput("sez", "seleziona un reparto",
                                                        c(unique(as.character(dati$reparto))))))
                            ),
                            
                            fluidRow(
                              column(6,div(style="height:50px"),dygraphOutput('regraph')),
                              
                              column(6,div(style="height:50px"),dygraphOutput('revargraf'))),
                            
                            
                            br(),
                            
                            
                            fluidRow(
                              column(6,div(style="height:50px", align="center",sliderInput("mesi2", "smoothing value", 0,48,1))),
                              
                              
                              column(6,div(style="height:50px",align="center",
                                           selectInput("set2", "settore",
                                                       c(unique(as.character(dati$settore))))))
                            ),
                            
                            br(),
                            br(),
                            hr()      
                            
                            
                            
                            
                          )
                 ),
                 ##############LABORATORIO###############################
                 
                 tabPanel("Laboratorio", 
                          
                          fluidPage(
                            fluidRow(
                              column(6,div(style="height:50px",align="center",
                                           selectInput("sez2", "seleziona un reparto",
                                                       c(unique(as.character(dati$reparto)))))),
                              
                              column(6, div(style="height:50px",align="center",
                                            selectInput("laboratorio", "seleziona un laboratorio",
                                                        c(unique(as.character(dati$labs))))))
                              
                            ),
                            hr(),
                            br(),
                            fluidRow( 
                              column(12, div(style="height:5px",align="center",
                                             sliderInput("mesi3", "smoothing value", 0,48,1)))),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            fluidRow(
                              column(12, div(style="height:5px",align="center",
                                             dygraphOutput('labgraph')))
                              
                            )
                            
                            
                            
                          )),
                 ################DATA EXPLORER##########################
                 tabPanel("Data Explorer",
                          fluidRow(
                            column(3,
                                   selectInput("settore",
                                               "Settore:",
                                               c("All",
                                                 unique(as.character(dati$settore))))
                            ),
                            column(3,
                                   selectInput("reparto",
                                               "Sezione:",
                                               c("All",
                                                 unique(as.character(dati$reparto))))
                            ),
                            column(3,
                                   selectInput("labs",
                                               "Laboratorio:",
                                               c("All",
                                                 unique(as.character(dati$labs))))
                            ),
                            
                            column(3,
                                   selectInput("finalità",
                                               "Finalità della Prova:",
                                               c("All",
                                                 unique(as.character(dati$finalità))))
                                   
                                   
                                   
                            )),
                          
                          fluidRow(
                            column(12,div(align='center',dateRangeInput('dateRange',
                                                                        label = 'Seleziona un intervallo di tempo',
                                                                        start = Sys.Date() - 2, end = Sys.Date() + 2
                            )
                            ))),
                          # Create a new row for the table.
                          fluidRow(
                            DT::dataTableOutput("table")
                          )
                          # fluidPage(
                          #   downloadLink("downloadData", "Download")
                          # )
                 ),
                 
                 
                 ################PIVOT TABLE##########################
                 tabPanel("Tabelle Pivot", 
                          
                          fluidPage(
                            fluidRow(
                              
                              
                              
                              column(6,div(style="height:10px"),rpivotTableOutput("pivot") )
                              
                            ),
                            
                            br(),
                            
                            
                            
                            
                            # box(dataTableOutput('table'))),
                            
                            
                            
                            br(),
                            br()
                            
                            
                            
                            
                          )
                 )
                 # ,
                 # tabPanel("Prove", "This panel is intentionally left blank")
      )

          #shinyURL.ui()
      
    }
    
  )
  
  tot<-summarise(group_by(dati,mese), esami=sum(esami,na.rm=TRUE))
  set<-summarise(group_by(dati,settore, mese),esami=sum(esami,na.rm=TRUE))
  Alim<-set%>%
    filter(settore=='Alimenti Uomo')
  San<-set%>%
    filter(settore=='Sanità Animale')
  Contr<-set%>%
    filter(settore=='Controlli Interni Sistema Qualità')
  Zoo<-set%>%
    filter(settore=='Alimenti Zootecnici')
  totale<-xts(tot$esami, order.by=as.Date(tot$mese))
  alimenti<-xts(Alim$esami, order.by = as.Date(Alim$mese))
  sanità<-xts(San$esami, order.by = as.Date(San$mese))
  contr<-xts(Contr$esami, order.by=as.Date(Contr$mese))
  zoo<-xts(Zoo$esami, order.by=as.Date(Zoo$mese))
  graf<-cbind(totale,alimenti,sanità, contr, zoo)
  
  ###########GRAFICO E TABELLA DELLA STRUTTURA COMPLESSA###############
  grafico<-reactive({graf})
  tabella<-reactive({set})
  
  output$dygraph <- renderDygraph({
    
    dygraph(grafico(),ylab = "N.Esami / Mese")%>%
      dySeries("totale", label="Totale Attività", color='red')%>%
      dySeries("alimenti", label = "Settore Alimenti", color='green')%>%
      dySeries("sanità", label="Sanità Animale", color='blue')%>%
      dySeries("contr", label="Controllo Qualità", color='black')%>%
      dySeries("zoo", label="Alimenti Zootecnici", color='brown')%>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 1.5))%>%
      dyRoller(rollPeriod = input$mesi)%>%
      dyRangeSelector()
    
  })
  #output$table <- renderDataTable(tabella())
  
  ###########GRAFICO E TABELLA PER REPARTO ############################ 
  output$regraph <- renderDygraph({
    rep<-filter(dati, reparto==input$sez)
    rtot<-summarise(group_by(rep, mese), esami=sum(esami,na.rm=TRUE))
    rsett<-summarise(group_by(rep,settore, mese),esami=sum(esami,na.rm=TRUE))
    rAlim<-rsett%>%
      filter(settore=='Alimenti Uomo')
    rSan<-rsett%>%
      filter(settore=='Sanità Animale')
    rContr<-rsett%>%
      filter(settore=='Controlli Interni Sistema Qualità')
    rZoo<-rsett%>%
      filter(settore=='Alimenti Zootecnici')
    rtotale<-xts(rtot$esami, order.by=as.Date(rtot$mese))
    ralimenti<-xts(rAlim$esami, order.by = as.Date(rAlim$mese))
    rsanità<-xts(rSan$esami, order.by = as.Date(rSan$mese))
    rcontr<-xts(rContr$esami, order.by=as.Date(rContr$mese))
    rzoo<-xts(rZoo$esami, order.by=as.Date(rZoo$mese))
    rgraf<-cbind(rtotale,ralimenti,rsanità, rcontr, rzoo)
    
    dygraph(rgraf,ylab = "N.Esami / Mese")%>%
      dySeries("rtotale", label="Totale Attività", color='red')%>%
      dySeries("ralimenti", label = "Settore Alimenti", color='green')%>%
      dySeries("rsanità", label="Sanità Animale", color='blue')%>%
      dySeries("rcontr", label="Controllo Qualità", color='black')%>%
      dySeries("rzoo", label="Alimenti Zootecnici", color='brown')%>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 1.5))%>%
      dyRoller(rollPeriod = input$mesi2)%>%
      dyRangeSelector()
  })
  ###########################GRAFICO PER LABORATORI###################
  output$labgraph <- renderDygraph({
    rep<-filter(dati,reparto==input$sez2)
    labb<-summarise(group_by(rep,labs, mese),esami=sum(esami,na.rm=TRUE))
    lab<-labb%>%
      filter(labs==input$laboratorio)
    graf<-xts(lab$esami, order.by = as.Date(lab$mese))
    dygraph(graf,ylab = "N.Esami / Mese")%>%
      dySeries("V1", label = input$laboratorio)%>%
      #dyHighlight(highlightSeriesOpts = list(strokeWidth = 1.5))%>%
      dyRoller(rollPeriod = input$mesi3)%>%
      dyRangeSelector()
  })
  ###########################GRAFICO VARIAZIONI % STRUTTURA ##################
  
  output$vargraf<-renderDygraph({
    setvar<-summarise(group_by(dati,settore, mese),esami=sum(esami, na.rm=TRUE))%>%
      mutate(var_change = (esami/lag(esami) - 1) * 100)
    sector<-setvar%>%
      filter(settore==input$set)
    sectorts<-xts(sector$var_change, order.by = as.Date(sector$mese))
    dygraph(sectorts,ylab = "variazione % ")%>%
      dySeries("V1", label = input$set)%>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 1.5))%>%
      dyRoller(rollPeriod = input$mesi)%>%
      dyRangeSelector()
    
  })
  
  
  ##############GRAFICO VARIAZIONE % REPARTO#######################
  output$revargraf<-renderDygraph({
    
    repvar<-filter(dati,reparto==input$sez)
    reptvar2<-summarise(group_by(repvar,settore, mese),esami=sum(esami, na.rm=TRUE))%>%
      mutate(var_change = (esami/lag(esami) - 1) * 100)
    
    varsett<-filter(reptvar2, settore==input$set2)
    
    variazsettrep<-xts(varsett$var_change, order.by = as.Date(varsett$mese))
    dygraph(variazsettrep,ylab = "variazione % ")%>%
      dySeries("V1", label = input$set2)%>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 1.5))%>%
      dyRoller(rollPeriod = input$mesi2)%>%
      dyRangeSelector()
    
  })
  
  
  ##############GRAFICO VARIAZIONE % LABORATORIO###################
  output$labvargraf<-renderDygraph({
    repvar<-filter(dati,reparto==input$sez2)
    labvar<-summarise(group_by(repvar,labs, mese),esami=sum(esami, na.rm=TRUE))%>%
      mutate(var_change = (esami/lag(esami) - 1) * 100)
    varlab<-filter(labvar, settore==input$laboratori)
    variazlab<-xts(varlab$var_change, order.by = as.Date(varlab$mese))
    
    dygraph(variazlab,ylab = "variazione % ")%>%
      dySeries("V1", label = input$laboratorio)%>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 1.5))%>%
      dyRoller(rollPeriod = input$mesi2)%>%
      dyRangeSelector()
    
    
  })
  
  
  ##########################DATA EXPLORER############################
  
  output$table <- DT::renderDataTable({
    dati$datainizio<-as.Date(dati$datainizio)
    data <- na.omit(dati)
    if (input$settore != "All") {
      data <- data[data$settore == input$settore,]
    }
    if (input$reparto != "All") {
      data <- data[data$reparto == input$reparto,]
    }
    if (input$labs != "All") {
      data <- data[data$labs == input$labs,]
    }
    if (input$finalità != "All") {
      data <- data[data$finalità == input$finalità,]
    }
    data<-data[data$datainizio>=input$dateRange[1]&data$datainizio<=input$dateRange[2], ]
    
    data[,-c(1,2,5,8,10,11,12,14)]
    
    
  },server= FALSE,filter = 'top',extensions = 'Buttons',class = 'cell-border stripe',
  options = list(dom = 'Bfrtip',searching = FALSE,paging = TRUE,autoWidth = TRUE,
                 pageLength = 10,buttons = c("csv",'excel'))
  
  )
  
  ##################PIVOT TABLE#################
  output$pivot <- renderRpivotTable({
    dx<-dati %>% 
      dplyr::select(settore,prova, reparto, labs, esami, anno,tecnica)
    rpivotTable(dx,aggregatorName="Sum", vals="esami")
  })
  
})
  
