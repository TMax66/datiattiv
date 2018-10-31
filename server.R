#######################FUNZIONI DI R#############################################

server<-function(input, output){
  
  #dati<-read.csv("dati.csv", header=T, sep=",")
  # dati<-na.omit(dati)
  # dati<-as.tibble(dati)
  # dati$data<-as.Date(dati$datainizio)
  # dati<- mutate(dati, mese=paste(year(datainizio),'-',month(datainizio),sep=''))
  # dati$mese<-as.Date((paste(dati$mese,"-01",sep="")))
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
      dySeries("..1", label="Totale Attività", color='red')%>%
      dySeries("..2", label = "Settore Alimenti", color='green')%>%
      dySeries("..3", label="Sanità Animale", color='blue')%>%
      dySeries("..4", label="Controllo Qualità", color='black')%>%
      dySeries("..5", label="Alimenti Zootecnici", color='brown')%>%
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
      dySeries("..1", label="Totale Attività", color='red')%>%
      dySeries("..2", label = "Settore Alimenti", color='green')%>%
      dySeries("..3", label="Sanità Animale", color='blue')%>%
      dySeries("..4", label="Controllo Qualità", color='black')%>%
      dySeries("..5", label="Alimenti Zootecnici", color='brown')%>%
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
  
  output$table <- DT::renderDataTable(DT::datatable({
    #dati$datainizio<-as.Date(dati$datainizio)
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
    
    data[,-c(1,2,5,8,10,11,12,14,15)]
    
    
  },filter = 'top',extensions = 'Buttons',options = list(dom = 'Bfrtip',
                                                         buttons = c('csv', 'excel'))
  
  
  
  
  ))
  
##################PIVOT TABLE#################
  output$pivot <- renderRpivotTable({
    rpivotTable(dati)
  })
  
}



