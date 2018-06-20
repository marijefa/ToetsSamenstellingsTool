library(shiny)
library(ggplot2)
library(tinytex)

debug <- T

function(input, output) {
  
  ###reactive###
  
  itemoverzicht <- reactive({
    if(debug){cat("itemoverzicht\n")}
    itemoverzicht <- read.csv("R:\\10VDL\\Toetsverwerking\\analyse_10vdl\\Apps\\Toetssamenstelling\\itemoverzicht.csv", header=T, sep=";")
    itemoverzicht <- itemoverzicht[itemoverzicht$vak==input$vakkeuze,]
    itemoverzicht <- itemoverzicht[order(itemoverzicht$toets),]
    
    acceptatievragen <- read.csv("R:\\10VDL\\Toetsverwerking\\analyse_10vdl\\Apps\\Toetssamenstelling\\inacceptatie.csv", header=F, sep=";")
    vraagcodein2 <- strsplit(as.character(acceptatievragen[,1]),"\\$")
    #NT-010208-16a-v01$Mak1-2302-Tz03 (356) mist dollarteken
    inacceptatie <- data.frame(vak=substr(as.character(acceptatievragen[,1]),1,2),
                               vraagcode=acceptatievragen[,1],
                               vraagcode_deel1=unlist(vraagcodein2)[2*(1:nrow(acceptatievragen))-1],
                               vraagcode_deel2=unlist(vraagcodein2)[2*(1:nrow(acceptatievragen))])
    inacceptatie <- inacceptatie[inacceptatie$vak==input$vakkeuze,]
  
    vraagcodein2 <- strsplit(as.character(itemoverzicht$vraagcode),"\\$")
    itemoverzicht$vraagcode_deel1 <- unlist(vraagcodein2)[2*(1:nrow(itemoverzicht))-1]
    itemoverzicht$vraagcode_deel2 <- unlist(vraagcodein2)[2*(1:nrow(itemoverzicht))]
    itemoverzicht$vraagcode <- as.character(itemoverzicht$vraagcode)
    itemoverzicht$vraagcode_deel1 <- as.character(itemoverzicht$vraagcode_deel1)
    itemoverzicht$vraagcode_deel2 <- as.character(itemoverzicht$vraagcode_deel2)

    itemoverzicht <- itemoverzicht[itemoverzicht$vraagcode_deel1 %in% inacceptatie$vraagcode_deel1,]
    nognietafgenomen <- inacceptatie[!inacceptatie$vraagcode_deel1 %in% itemoverzicht$vraagcode_deel1,]
    nieuw <- data.frame(vak=input$vakkeuze, toets=NA, n=NA, alfa=NA,
                        vraagcode=nognietafgenomen$vraagcode,
                        topic=NA, p_schat=NA, tijd_schat=NA, p=NA, rir=NA, tijd=NA,
                        vraagcode_deel1=nognietafgenomen$vraagcode_deel1,
                        vraagcode_deel2=nognietafgenomen$vraagcode_deel2)
    itemoverzicht <- rbind(itemoverzicht,nieuw)
    
    itemoverzicht$topic <- unlist(strsplit(itemoverzicht$vraagcode_deel1,"-"))[4*(1:nrow(itemoverzicht))-2]
    itemoverzicht$p_schat <- substr(itemoverzicht$vraagcode_deel2,1,2)
    itemoverzicht$tijd_schat <- substr(itemoverzicht$vraagcode_deel2,4,4)
    
    selvragen <<- NULL
    
    itemoverzicht
  })
  
  geselvragen <- reactive({
    if(debug){cat("geselvragen\n")}
    huidigtopicuitsel <- data.frame(selvragen=selvragen,
                                    topic=unlist(strsplit(as.character(selvragen),"-"))[5*(1:length(selvragen))-3])
    selvragen <<- as.character(huidigtopicuitsel[huidigtopicuitsel$topic!=input$topickeuze,"selvragen"])
    
    geselvragen <- c(selvragen,input$vraagselectie)
    selvragen <<- unique(geselvragen)
      
    selvragen
    })
  
  toetssel <- reactive({
    if(debug){cat("toetssel\n")}
    toetssel <- itemoverzicht()[itemoverzicht()$vraagcode_deel1 %in%
                                  unlist(strsplit(as.character(geselvragen()),"\\$"))[2*(1:length(geselvragen()))-1],
                                c("vraagcode","toets","n","p_schat","tijd_schat","p","rir","tijd")]
    toetssel[order(toetssel$vraagcode),]
  })
  
  toetssel_gem <- reactive({
    if(debug){cat("toetssel_gem\n")}
    toetssel_gem <- data.frame(vraagcode=geselvragen(), toets=NA, n=NA, p_schat=NA, tijd_schat=NA, p=NA, rir=NA, tijd=NA, tijd_comb=NA)
    
    for(r in 1:nrow(toetssel_gem)){
      sel <- itemoverzicht()[itemoverzicht()$vraagcode_deel1 %in%
                               unlist(strsplit(as.character(geselvragen()),"\\$"))[2*(1:length(geselvragen()))-1][r],]
      toetssel_gem$toets[r] <- paste0(unique(as.character(sel$toets)),collapse=",")
      toetssel_gem$n[r] <- sum(sel$n)
      toetssel_gem$p_schat[r] <- unique(sel$p_schat)[length(unique(sel$p_schat))] #bij verschil, pak de nieuwste schatting
      toetssel_gem$tijd_schat[r] <- unique(sel$tijd_schat)[length(unique(sel$tijd_schat))]
      toetssel_gem$p[r] <- sum(sel$n*sel$p)/sum(sel$n)
      toetssel_gem$rir[r] <- sum(sel$n*sel$rir)/sum(sel$n)
      toetssel_gem$tijd[r] <- sum(sel[!is.na(sel$tijd),"n"]*sel[!is.na(sel$tijd),"tijd"])/sum(sel[!is.na(sel$tijd),"n"])
    }
    toetssel_gem$tijd_comb <- toetssel_gem$tijd
    toetssel_gem[is.na(toetssel_gem$tijd_comb),"tijd_comb"] <- toetssel_gem[is.na(toetssel_gem$tijd_comb),"tijd_schat"]
    
    toetssel_gem[order(toetssel_gem$vraagcode),]
  })
  
  laatstetoets <- reactive({
    if(debug){cat("laatstetoets\n")}
    paste0("T",max(as.numeric(substr(unique(itemoverzicht()$toets)[!is.na(unique(itemoverzicht()$toets))],2,3))))

  })
  
  ###nonreactive###
  
  output$topiclijst = renderUI({
    if(debug){cat("topickeuze\n")}
    selectInput("topickeuze", width=500, label="Kies een (sub)topic om vragen uit te selecteren voor de toets", 
                sort(unique(itemoverzicht()$topic)))
  })
  
  output$tekst_nvragenpertopic <- renderText({
    if(debug){cat("nvragenpertopic\n")}
    toetsen <- unique(itemoverzicht()$toets)
    pertoets <- NA
    for(t in 1:length(toetsen)){
      pertoets[t] <- sum(itemoverzicht()[itemoverzicht()$toets==toetsen[t],"topic"]==input$topickeuze)}
    n <- sort(unique(pertoets)) 

    if(length(n)==1){
      paste0("Aantal vragen in (sub)topic ",input$topickeuze," per toets: ",n)}else{
        paste0("Aantal vragen in (sub)topic ",input$topickeuze," per toets: ",paste0(n,collapse=", "),
               " (",sum(itemoverzicht()[itemoverzicht()$toets==laatstetoets(),"topic"]==input$topickeuze)," in de afgelopen toets)")}
  })
  
  output$tab_vragenintopic <- renderTable({
    if(debug){cat("tab_vragenintopic\n")}
    vragenintopic <- itemoverzicht()[itemoverzicht()$topic==input$topickeuze,c("vraagcode","toets","p_schat","tijd_schat","p","rir","tijd")]
    vragenintopic <- vragenintopic[order(vragenintopic[,input$vraagsortering]),]
    if(input$weergave_p){vragenintopic <- vragenintopic[is.na(vragenintopic$p)|(vragenintopic$p>=.4&vragenintopic$p<=.9),]}
    if(input$weergave_rir){vragenintopic <- vragenintopic[is.na(vragenintopic$rir)|vragenintopic$rir>=.1,]}
    if(input$weergave_tijd){vragenintopic <- vragenintopic[is.na(vragenintopic$tijd)|vragenintopic$tijd<=3,]}
    vragenintopic
    })
  
  output$vraagselectie = renderUI({
    if(debug){cat("vraagselectie\n")}
    vragenintopic <- itemoverzicht()[itemoverzicht()$topic==input$topickeuze,"vraagcode"]
    vragendeel1 <- unlist(strsplit(as.character(vragenintopic),"\\$"))[2*(1:length(vragenintopic))-1]
    if(debug){cat("vraagselectie_ontdubbelen\n")}
    ontdubbelen <- data.frame(vragenintopic, vragendeel1, uniek=NA)
    for(r in 1:nrow(ontdubbelen)){
      sel <- ontdubbelen[ontdubbelen$vragendeel1==ontdubbelen$vragendeel1[r],]
      langste <- sel$vragenintopic[which(nchar(as.character(sel$vragenintopic))==max(nchar(as.character(sel$vragenintopic))))]
      ontdubbelen[ontdubbelen$vragenintopic==langste,"uniek"]<-1}
    uniekevragen <- sort(ontdubbelen[!is.na(ontdubbelen$uniek),"vragenintopic"])

    if(debug){cat("vraagselectie_checkbox\n")}
    checkboxGroupInput("vraagselectie",
                       label = "Selecteer vragen voor de toets",
                       choices = uniekevragen)
  })
  
  output$tab_toetssel <- renderTable({
    if(length(geselvragen())>0){
      toetssel()}
    })
  
  output$tab_toetssel_gem <- renderTable({
    if(length(geselvragen())>0){
      toetssel_gem()[,1:(ncol(toetssel_gem())-1)]}
  })
  
  output$tekst_alle_ntoetsen <- renderText({
    paste0("Aantal toetsen (ingelezen): ",substr(laatstetoets(),2,3),
           " (",length(unique(itemoverzicht()$toets)[!is.na(unique(itemoverzicht()$toets))]),")")
  })
  
  output$tekst_alle_ndg <- renderText({
    ns <- unique(itemoverzicht()[,1:4])[!is.na(unique(itemoverzicht()[,1:4])$toets),]$n
    paste0("Aantal eerstekansers: gem. ",round(mean(ns),0)," (",
           round(min(ns),2),"-",round(max(ns),2),")")
  })
  
  output$tekst_alle_alfa <- renderText({
    alfas <- unique(itemoverzicht()[,1:4])[!is.na(unique(itemoverzicht()[,1:4])$toets),]$alfa
    paste0("Cronbachs alfa: gem. ",round(mean(alfas,na.rm=T),2)," (",
           round(min(alfas,na.rm=T),2),"-",round(max(alfas,na.rm=T),2),")")
  })
  
  output$tekst_alle_p <- renderText({
    ps <- by(itemoverzicht()$p,itemoverzicht()$toets,mean)
    paste0("Gemiddelde p-waarde: ",round(mean(ps),2)," (",
           round(min(ps),2),"-",round(max(ps),2),")")
  })
  
  output$tekst_alle_rir <- renderText({
    rirs <- by(itemoverzicht()$rir,itemoverzicht()$toets,function(x) mean(x,na.rm=T))
    paste0("Gemiddelde itemrestcorrelatie: ",round(mean(rirs),2)," (",
           round(min(rirs),2),"-",round(max(rirs),2),")")
  })

  output$tekst_alle_tijd <- renderText({
    tijden <- by(itemoverzicht()$tijd,itemoverzicht()$toets,function(x) mean(x,na.rm=T))
    paste0("Gemiddelde tijd per vraag (min): ",round(mean(tijden,na.rm=T),2)," (",
           round(min(tijden,na.rm=T),2),"-",round(max(tijden,na.rm=T),2),")")
  })
  
  output$tekst_laatste_volgnr <- renderText({
    paste0("Volgnummer: ",laatstetoets())
  })
  
  output$tekst_laatste_nvragen <- renderText({
    paste0("Aantal vragen: ",nrow(itemoverzicht()[!is.na(itemoverzicht()$toets)&itemoverzicht()$toets==laatstetoets(),]))
  })
  
  output$tekst_laatste_ndg <- renderText({
    paste0("Aantal eerstekansers: ",unique(itemoverzicht()[!is.na(itemoverzicht()$toets)&itemoverzicht()$toets==laatstetoets(),"n"]))
    })
  
  output$tekst_laatste_alfa <- renderText({
    alfas <- unique(itemoverzicht()[,1:4])$alfa
    paste0("Cronbachs alfa: ",unique(itemoverzicht()[!is.na(itemoverzicht()$toets)&itemoverzicht()$toets==laatstetoets(),"alfa"]))
  })
  
  output$tekst_laatste_p <- renderText({
    ps <- itemoverzicht()[!is.na(itemoverzicht()$toets)&itemoverzicht()$toets==laatstetoets(),"p"]
    paste0("p-waarde: gem. ",round(mean(ps),2)," (",
           round(min(ps),2),"-",round(max(ps),2),")")
  })
  
  output$tekst_laatste_rir <- renderText({
    rirs <- itemoverzicht()[!is.na(itemoverzicht()$toets)&itemoverzicht()$toets==laatstetoets(),"rir"]
    paste0("Itemrestcorrelatie: gem. ",round(mean(rirs),2)," (",
           round(min(rirs),2),"-",round(max(rirs),2),")")
  })
  
  output$tekst_laatste_tijd <- renderText({
    tijden <- itemoverzicht()[!is.na(itemoverzicht()$toets)&itemoverzicht()$toets==laatstetoets(),"tijd"]
    paste0("Tijd per vraag (min): gem. ",round(mean(tijden),2)," (",
           round(min(tijden),2),"-",round(max(tijden),2),")")
  })

  output$tekst_huidig_nvragen <- renderText({
    paste0("Aantal geselecteerde vragen: ",length(geselvragen()))
  })
  
  output$tekst_huidig_nvragenafgenomen <- renderText({
    if(length(geselvragen())>0){
      n <- nrow(toetssel_gem()[!is.na(toetssel_gem()$p),])
      paste0("Aantal met afnamegegevens: ",n," (",round(100*n/length(geselvragen()),0),"%)")}
  })
  
  output$tekst_huidig_p <- renderText({
    if(length(geselvragen())>0){
      ps <- toetssel_gem()[!is.na(toetssel_gem()$p),"p"]
      paste0("p-waarde afgenomen vragen: gem. ",round(mean(ps),2)," (",
             round(min(ps),2),"-",round(max(ps),2),")")}
  })
  
  output$tekst_huidig_mamemo <- renderText({
    if(length(geselvragen())>0){
      ma <- nrow(toetssel_gem()[toetssel_gem()$p_schat=="Ma",])
      me <- nrow(toetssel_gem()[toetssel_gem()$p_schat=="Me",])
      mo <- nrow(toetssel_gem()[toetssel_gem()$p_schat=="Mo",])
      paste0("Ingeschat als makkelijk / medium / moeilijk: ",
             ma," (",round(100*ma/length(geselvragen()),0),"%) / ",
             me," (",round(100*me/length(geselvragen()),0),"%) / ",
             mo," (",round(100*mo/length(geselvragen()),0),"%)"
             )}
  })
  
  output$tekst_huidig_rir <- renderText({
    if(length(geselvragen())>0){
      rirs <- toetssel_gem()[!is.na(toetssel_gem()$rir),"rir"]
      paste0("Itemrestcorrelatie afgenomen vragen: gem. ",round(mean(rirs),2)," (",
             round(min(rirs),2),"-",round(max(rirs),2),")")}
  })
  
  output$tekst_huidig_tijd <- renderText({
    if(length(geselvragen())>0){
      tijden <- toetssel_gem()[!is.na(toetssel_gem()$tijd),"tijd"]
      paste0("Tijd per afgenomen vraag (min): gem. ",round(mean(tijden),2)," (",
             round(min(tijden),2),"-",round(max(tijden),2),")")}
  })

  output$tekst_huidig_totaletijd <- renderText({
    if(length(geselvragen())>0){
      paste0("Verwachte totale toetstijd (min): ",round(sum(as.numeric(as.character(toetssel_gem()$tijd_comb)),na.rm=T),0))}
  })
  
  output$fileDownloadklaar <- reactive({#om in linkerpanel niets weer te geven als nog niks geupload
    if(debug){cat("output$fileDownloadklaar\n")}
    return(length(geselvragen())>0)
  })
  outputOptions(output, 'fileDownloadklaar', suspendWhenHidden=FALSE) #om in linkerpanel niets weer te geven als nog niks geupload
  
  output$download_xslx <- downloadHandler(
    filename = function() { paste("Samengestelde toets.xlsx")},
    
    content = function(file){
      Results_Workbook <- createWorkbook(type='xlsx')
      sheet.1 <- createSheet(Results_Workbook, sheetName = "Gecombineerde gegevens per vraag")
      addDataFrame(toetssel_gem(), sheet=sheet.1, startRow=1,startColumn=1,row.names=F)
      sheet.2 <- createSheet(Results_Workbook, sheetName = "Gegevens per vraag per afname")
      addDataFrame(toetssel(), sheet=sheet.2, startRow=1,startColumn=1,row.names=F)
      saveWorkbook(Results_Workbook,file)
    })
  
}
