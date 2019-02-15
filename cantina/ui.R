#############INTERFACCIA GRAFICA###################################################
ui<-navbarPage("Dati di attività della struttuta complessa BG-SO-VA",
               
               
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
                        ),
                        fluidPage(
                          downloadLink("downloadData", "Download")
                        )
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
               ),
               tabPanel("Prove", "This panel is intentionally left blank")
)
#)





