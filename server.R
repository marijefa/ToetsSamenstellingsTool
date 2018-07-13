library(shiny)
library(ggplot2)
library(tinytex)

debug <- T

function(input, output) {
  
  ###reactive###
  
  itemoverzicht_upload <- reactive({
    if(!is.null(input$itemoverzicht)){
      read.csv(input$itemoverzicht$datapath, header=T, sep=";")}else{return(NULL)}
  })
  
  inacceptatie_upload <- reactive({
    if(!is.null(input$inacceptatie)){
      read.csv(input$inacceptatie$datapath, header=F, sep=";")}else{return(NULL)}
  })
  
  output$fileUploaded <- reactive({#om in linkerpanel niets weer te geven als nog niks geupload
    if(debug){cat("output$fileUploaded\n")}
    return(!is.null(input$itemoverzicht)&!is.null(input$inacceptatie))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE) #om in linkerpanel niets weer te geven als nog niks geupload
  
  itemoverzicht <- reactive({#inlezen beschikbare afnamegegevens en wat in acceptatie zit, metatags etc eruit halen
    if(debug){cat("itemoverzicht\n")}
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    #itemoverzicht <- read.csv("R:\\10VDL\\Toetsverwerking\\analyse_10vdl\\Apps\\Toetssamenstelling\\itemoverzicht.csv", header=T, sep=";")
    #acceptatievragen <- read.csv("R:\\10VDL\\Toetsverwerking\\analyse_10vdl\\Apps\\Toetssamenstelling\\inacceptatie.csv", header=F, sep=";")
    itemoverzicht <- itemoverzicht_upload()
    acceptatievragen <- inacceptatie_upload()
    
    vakkeuze <- input$vakkeuze
    
    itemoverzicht <- itemoverzicht[itemoverzicht$vak==vakkeuze,]
    itemoverzicht <- itemoverzicht[order(itemoverzicht$toets),]
    
    vraagcodein2 <- strsplit(as.character(acceptatievragen[,1]),"\\$")
    #NT-010208-16a-v01$Mak1-2302-Tz03 (356) mist dollarteken en AK-010102-55b-v01Mat0-TXX (2557), BI-030203-10a-v01Met2-01 (4598)
    #EC-030201-08a-v01$Mak1 zelfde vraag ook bij EC-030201-16a-v01$Mak1-Tt06-RT2 (6475)
    #Jump (7071 en7072)
    #Copy of GW-020102-32b-v01$Mai0-T09 (10690)
    #Gw-020103-02b-v01$Mat1 (10723)
    #VERPLAATSEN NAAR 050301 GW-050401-19a-v01$Mek1 (11054)
    inacceptatie <- data.frame(vak=substr(as.character(acceptatievragen[,1]),1,2),
                               vraagcode=acceptatievragen[,1],
                               vraagcode_deel1=unlist(vraagcodein2)[2*(1:nrow(acceptatievragen))-1],
                               vraagcode_deel2=unlist(vraagcodein2)[2*(1:nrow(acceptatievragen))])
    inacceptatie <- inacceptatie[inacceptatie$vak==vakkeuze,]
  
    vraagcodein2 <- strsplit(as.character(itemoverzicht$vraagcode),"\\$")
    itemoverzicht$vraagcode_deel1 <- unlist(vraagcodein2)[2*(1:nrow(itemoverzicht))-1]
    itemoverzicht$vraagcode_deel2 <- unlist(vraagcodein2)[2*(1:nrow(itemoverzicht))]
    itemoverzicht$vraagcode <- as.character(itemoverzicht$vraagcode)
    itemoverzicht$vraagcode_deel1 <- as.character(itemoverzicht$vraagcode_deel1)
    itemoverzicht$vraagcode_deel2 <- as.character(itemoverzicht$vraagcode_deel2)
    itemoverzicht$toetsen=NA; itemoverzicht$laatst <- 0

    itemoverzicht$inacceptatie <- as.numeric(itemoverzicht$vraagcode_deel1 %in% inacceptatie$vraagcode_deel1)
    nognietafgenomen <- inacceptatie[!inacceptatie$vraagcode_deel1 %in% itemoverzicht$vraagcode_deel1,]
    nieuw <- data.frame(vak=vakkeuze, toets=NA, n=NA, alfa=NA,
                        vraagcode=nognietafgenomen$vraagcode,
                        topic=NA, p_schat=NA, tijd_schat=NA, p=NA, rir=NA, tijd=NA, inacceptatie=1,
                        vraagcode_deel1=nognietafgenomen$vraagcode_deel1,
                        vraagcode_deel2=nognietafgenomen$vraagcode_deel2,
                        toetsen=NA,laatst=1)
    itemoverzicht <- rbind(itemoverzicht,nieuw)
    
    itemoverzicht$topic <- unlist(strsplit(itemoverzicht$vraagcode_deel1,"-"))[4*(1:nrow(itemoverzicht))-2]
    itemoverzicht$topic <- gsub(" ","",itemoverzicht$topic)
    itemoverzicht$p_schat <- substr(itemoverzicht$vraagcode_deel2,1,2)
    itemoverzicht$tijd_schat <- substr(itemoverzicht$vraagcode_deel2,4,4)

    for(r in 1:nrow(itemoverzicht)){
      itemoverzicht$toetsen[r] <- paste0(unique(itemoverzicht[itemoverzicht$vraagcode_deel1 %in% itemoverzicht$vraagcode_deel1[r],"toets"]),collapse=",")}
    uniekevragen <- unique(itemoverzicht$vraagcode_deel1)
    for(v in 1:length(uniekevragen)){
      sel <- itemoverzicht[itemoverzicht$vraagcode_deel1==as.character(uniekevragen)[v],]
      toetsnummers <- as.numeric(substr(as.character(sel$toets),2,3)) 
      laatste <- sel$vraagcode[which(toetsnummers==max(toetsnummers))]
      itemoverzicht[itemoverzicht$vraagcode==as.character(laatste),"laatst"] <- 1}
    
    selvragen <<- NULL
    
    itemoverzicht
  })
  
  itemoverzicht_gem <- reactive({#tabel met voor alle vragen een regel met op n gewogen combineerde statstieken
    if(debug){cat("itemoverzicht_gem\n")}
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    
    itemoverzicht <- itemoverzicht()
    itemoverzicht_uniek <- itemoverzicht[itemoverzicht$laatst==1,]
    itemoverzicht_gem <- data.frame(vraagcode=itemoverzicht_uniek$vraagcode,
                                    inacceptatie=itemoverzicht_uniek$inacceptatie,
                                    topic=itemoverzicht_uniek$topic,
                                    vraagcode_deel1=itemoverzicht_uniek$vraagcode_deel1,
                                    vraagcode_deel2=itemoverzicht_uniek$vraagcode_deel2,
                                    toetsen=itemoverzicht_uniek$toetsen,
                                    n=NA, p_schat=NA, tijd_schat=NA, p=NA, rir=NA, tijd=NA, tijd_comb=NA)
    itemoverzicht_gem <- itemoverzicht_gem[order(itemoverzicht_gem$vraagcode),]
    
    for(r in 1:nrow(itemoverzicht_gem)){
      sel <- itemoverzicht[itemoverzicht$vraagcode_deel1 %in% itemoverzicht_gem$vraagcode_deel1[r],]
      itemoverzicht_gem$p_schat[r] <- unique(sel$p_schat)[length(unique(sel$p_schat))] #bij verschil, pak de nieuwste schatting
      itemoverzicht_gem$tijd_schat[r] <- unique(sel$tijd_schat)[length(unique(sel$tijd_schat))]
      if(sum(!is.na(sel$n))>0){
        sel <- sel[!is.na(sel$p),]
        itemoverzicht_gem$n[r] <- sum(sel$n,na.rm=T)
        itemoverzicht_gem$p[r] <- sum(sel$n*sel$p)/sum(sel$n)
        itemoverzicht_gem$rir[r] <- sum(sel$n*sel$rir)/sum(sel$n)
        itemoverzicht_gem$tijd[r] <- sum(sel[!is.na(sel$tijd),"n"]*sel[!is.na(sel$tijd),"tijd"])/sum(sel[!is.na(sel$tijd),"n"])
      }else{
          itemoverzicht_gem$n[r] <- 0}
      }
    itemoverzicht_gem$tijd_comb <- itemoverzicht_gem$tijd
    itemoverzicht_gem[is.na(itemoverzicht_gem$tijd_comb),"tijd_comb"] <- itemoverzicht_gem[is.na(itemoverzicht_gem$tijd_comb),"tijd_schat"]
    
    itemoverzicht_gem
  })
  
  
  vragenintopic_metnietacc <- reactive({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    vragenintopic <- itemoverzicht()[itemoverzicht()$topic==input$topickeuze,c("vraagcode","toets","p_schat","tijd_schat","p","rir","tijd")]
    vragenintopic <- vragenintopic[order(vragenintopic[,input$vraagsortering]),]
    vragenintopic
  })
  
  vragenintopic_zondernietacc <- reactive({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    vrageninacc <- itemoverzicht()[itemoverzicht()$inacceptatie==1,"vraagcode"]
    vragenintopic <- vragenintopic_metnietacc()[vragenintopic_metnietacc()$vraagcode %in% vrageninacc,]
    vragenintopic
  })
  
  geselvragen <- reactive({#de door de gebruiker geselecteerde vragen (BUG: deselecteren na topicwissel nog onmogelijk)
    if(debug){cat("geselvragen\n")}
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    itemoverzicht_gem <- itemoverzicht_gem()
    
    huidigtopicuitsel <- data.frame(selvragen=selvragen,
                                    topic=unlist(strsplit(as.character(selvragen),"-"))[5*(1:length(selvragen))-3])
    selvragen <<- as.character(huidigtopicuitsel[huidigtopicuitsel$topic!=input$topickeuze,"selvragen"])
    
    if(length(input$vraagselectie)>0){#zodat vraagcodes in selectieweergave verkort kunnen worden weergegeven en niet gaan schuiven ten opzichte van rechterlijst
      selcodes <- NA
      for(v in 1:length(input$vraagselectie)){
        selcodes[v] <- as.character(itemoverzicht_gem$vraagcode
                                    [which(grepl(gsub("\\$",".",input$vraagselectie[v]),gsub("\\$",".",itemoverzicht_gem$vraagcode)))])}
    }else{
      selcodes <- NULL}
    
    geselvragen <- c(selvragen,selcodes)
    selvragen <<- unique(geselvragen)
      
    selvragen
    })
  
  toetssel <- reactive({#tabel met voor elke geselecteerde vraag voor elke afname een regel
    if(debug){cat("toetssel\n")}
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    toetssel <- itemoverzicht()[itemoverzicht()$vraagcode_deel1 %in%
                                  unlist(strsplit(as.character(geselvragen()),"\\$"))[2*(1:length(geselvragen()))-1],
                                c("vraagcode","toets","n","p_schat","tijd_schat","p","rir","tijd")]
    toetssel[order(toetssel$vraagcode),]
  })
  
  toetssel_gem <- reactive({#tabel met voor elke geselecteerde vraag een regel met op n gewogen combineerde statstieken
    if(debug){cat("toetssel_gem\n")}
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    toetssel_gem <- itemoverzicht_gem()[itemoverzicht_gem()$vraagcode %in% geselvragen(),
                                        c("vraagcode","toetsen","n","p_schat","tijd_schat","p","rir","tijd","tijd_comb")]
    toetssel_gem$n <- as.character(toetssel_gem$n)
    
    toetssel_gem
    })
  
  laatstetoets <- reactive({#bepalen wat de laatst afgenomen toets in de beschikbare data is
    if(debug){cat("laatstetoets\n")}
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    nr <- max(as.numeric(substr(unique(itemoverzicht()$toets)[!is.na(unique(itemoverzicht()$toets))],2,3)))
    if(nr<10){paste0("T0",nr)}else{paste0("T",nr)}
  })
  
  toetsoverzicht <- reactive({#dataframe met overzichtsgegevens huidige toets voor eerste tabblad Excel
    if(debug){cat("toetsoverzicht\n")}
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    if(length(geselvragen())>0){
      n1 <- nrow(toetssel_gem()[!is.na(toetssel_gem()$p),])
      n2 <- nrow(toetssel_gem()[!is.na(toetssel_gem()$p)&toetssel_gem()$p>=.40&toetssel_gem()$p<=.90&
                                 !is.na(toetssel_gem()$rir)&toetssel_gem()$rir>=.10&
                                 (is.na(toetssel_gem()$tijd)|toetssel_gem()$tijd<=3),])
      ps <- toetssel_gem()[!is.na(toetssel_gem()$p),"p"]
      ma <- nrow(toetssel_gem()[toetssel_gem()$p_schat=="Ma",])
      me <- nrow(toetssel_gem()[toetssel_gem()$p_schat=="Me",])
      mo <- nrow(toetssel_gem()[toetssel_gem()$p_schat=="Mo",])
      rirs <- toetssel_gem()[!is.na(toetssel_gem()$rir),"rir"]
      tijden <- toetssel_gem()[!is.na(toetssel_gem()$tijd),"tijd"]
      
    overzicht <- data.frame(onderdeel=c("Aantal geselecteerde vragen: ",
                                        "Aantal met afnamegegevens: ",
                                        "Aantal met goede statistieken: ",
                                        "p-waarde afgenomen vragen: ",
                                        "Ingeschat als makkelijk: ",
                                        "Ingeschat als medium : ",
                                        "Ingeschat als moeilijk: ",
                                        "Itemrestcorrelatie afgenomen vragen: ",
                                        "Tijd per afgenomen vraag (min): ",
                                        "Verwachte totale toetstijd (min): "),
                            waarde=c(length(geselvragen()),
                                     paste0(n1," (",round(100*n1/length(geselvragen()),0),"%)"),
                                     paste0(n2," (",round(100*n2/length(geselvragen()),0),"%)"),
                                     paste0("gem. ",round(mean(ps),2)," (",round(min(ps),2),"-",round(max(ps),2),")"),
                                     paste0(ma," (",round(100*ma/length(geselvragen()),0),"%)"),
                                     paste0(me," (",round(100*me/length(geselvragen()),0),"%)"),
                                     paste0(mo," (",round(100*mo/length(geselvragen()),0),"%)"),
                                     paste0("gem. ",round(mean(rirs),2)," (", round(min(rirs),2),"-",round(max(rirs),2),")"),
                                     paste0("gem. ",round(mean(tijden),2)," (",round(min(tijden),2),"-",round(max(tijden),2),")"),
                                     paste0(round(sum(as.numeric(as.character(toetssel_gem()$tijd_comb)),na.rm=T),0))
                                     ))
    }
    
    overzicht
  })
  
  ###nonreactive###
  
  output$vakkeuze = renderUI({#alle topics die voorkomen in de vraagcodes
    if(debug){cat("vakkeuze\n")}
    if(!is.null(input$itemoverzicht)&!is.null(input$inacceptatie)){
      selectInput("vakkeuze", width=500, label="Kies het vak voor toetssamenstelling",
                  sort(unique(itemoverzicht_upload()$vak)))}
  })
  
  output$topiclijst = renderUI({#alle topics die voorkomen in de vraagcodes
    if(debug){cat("topickeuze\n")}
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    selectInput("topickeuze", width=500, label="Kies een (sub)topic om vragen uit te selecteren voor de toets", 
                sort(unique(itemoverzicht()$topic)))
  })
  
  output$tab_nvragenpertopic <- renderTable({#weergeven hoeveel vragen in geselecteerd topic per toets
    if(debug){cat("nvragenpertopic\n")}
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    toetsen <- unique(itemoverzicht()$toets)[!is.na(unique(itemoverzicht()$toets))]
    pertoets <- data.frame(toets=toetsen,n=NA)
    for(r in 1:nrow(pertoets)){
      pertoets$n[r] <- nrow(vragenintopic_metnietacc()[!is.na(vragenintopic_metnietacc()$toets)&vragenintopic_metnietacc()$toets==as.character(pertoets$toets[r]),])}
    pertoets <- t(pertoets[,-1]) #transpose
    colnames(pertoets) <- toetsen
    pertoets
  })
  
  output$tab_vragenintopic <- renderTable({#tabel met beschikbare gegevens vragen in topic (selectie met condities)
    if(debug){cat("tab_vragenintopic\n")}
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    vragenintopic <- vragenintopic_zondernietacc()
    if(!input$weergave_welafgenomen){vragenintopic <- vragenintopic[is.na(vragenintopic$p),]}
    if(!input$weergave_nietafgenomen){vragenintopic <- vragenintopic[!is.na(vragenintopic$p),]}
    if(input$weergave_p){vragenintopic <- vragenintopic[is.na(vragenintopic$p)|(vragenintopic$p>=.4&vragenintopic$p<=.9),]}
    if(input$weergave_rir){vragenintopic <- vragenintopic[is.na(vragenintopic$rir)|vragenintopic$rir>=.1,]}
    if(input$weergave_tijd){vragenintopic <- vragenintopic[is.na(vragenintopic$tijd)|vragenintopic$tijd<=3,]}
    vragenintopic
    })
  
  output$vraagselectie = renderUI({#aanvinkbaar lijstje met alle vragen in acceptatie in topic
    if(debug){cat("vraagselectie\n")}
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    checkboxGroupInput("vraagselectie",
                       label = "Selecteer vragen voor de toets",
                       choices = substr(itemoverzicht_gem()[itemoverzicht_gem()$topic==input$topickeuze&itemoverzicht_gem()$inacceptatie==1,"vraagcode"],1,35))
  })
  
  output$vraagselectie_stats = renderUI({#lijstje met statistieken alle vragen in acceptatie in topic
    if(debug){cat("vraagselectie_stats\n")}
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    intopic <- itemoverzicht_gem()[itemoverzicht_gem()$topic==input$topickeuze&itemoverzicht_gem()$inacceptatie==1,]
    stats <- paste0("p = ",round(intopic$p,2),", rir = ",round(intopic$rir,2),", tijd = ",round(intopic$tijd,2)," min")
    stats[is.na(intopic$p)] <- "geen afnamegegevens"
    
    vinkje <- rep(1,length(stats))
    if(!input$weergave_welafgenomen){vinkje[!is.na(intopic$p)] <- 0}
    if(!input$weergave_nietafgenomen){vinkje[is.na(intopic$p)] <- 0}
    if(input$weergave_p){vinkje[!is.na(intopic$p)&(intopic$p<.4|intopic$p>.9)] <- 0}
    if(input$weergave_rir){vinkje[!is.na(intopic$rir)&intopic$rir<.1] <- 0}
    if(input$weergave_tijd){vinkje[!is.na(intopic$tijd)&intopic$tijd>3] <- 0}
    vinkje <- stats[vinkje==1]
    
    checkboxGroupInput("vraagselectie_stats",
                       label = "Gecombineerde statistieken",
                       choices = stats,
                       selected=vinkje)
  })
  
  output$tab_toetssel <- renderTable({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    if(length(geselvragen())>0){
      toetssel()}
    })
  
  output$tab_toetssel_gem <- renderTable({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    if(length(geselvragen())>0){
      toetssel_gem()[,1:(ncol(toetssel_gem())-1)]}
  })
  
  output$tekst_alle_ntoetsen <- renderText({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    paste0("Aantal toetsen (ingelezen): ",as.numeric(substr(laatstetoets(),2,3)),
           " (",length(unique(itemoverzicht()$toets)[!is.na(unique(itemoverzicht()$toets))]),")")
  })
  
  output$tekst_alle_ndg <- renderText({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    ns <- unique(itemoverzicht()[,1:4])[!is.na(unique(itemoverzicht()[,1:4])$toets),]$n
    paste0("Aantal eerstekansers: gem. ",round(mean(ns),0)," (",
           round(min(ns),2),"-",round(max(ns),2),")")
  })
  
  output$tekst_alle_alfa <- renderText({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    alfas <- unique(itemoverzicht()[,1:4])[!is.na(unique(itemoverzicht()[,1:4])$toets),]$alfa
    paste0("Cronbachs alfa: gem. ",round(mean(alfas,na.rm=T),2)," (",
           round(min(alfas,na.rm=T),2),"-",round(max(alfas,na.rm=T),2),")")
  })

  output$tekst_alle_p <- renderText({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    ps <- by(itemoverzicht()$p,itemoverzicht()$toets,mean)
    ps <- ps[!is.na(ps)]
    paste0("Gemiddelde p-waarde: ",round(mean(ps),2)," (",
           round(min(ps),2),"-",round(max(ps),2),")")
  })
  
  output$tekst_alle_rir <- renderText({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    rirs <- by(itemoverzicht()$rir,itemoverzicht()$toets,function(x) mean(x,na.rm=T))
    rirs <- rirs[!is.na(rirs)]
    paste0("Gemiddelde itemrestcorrelatie: ",round(mean(rirs),2)," (",
           round(min(rirs),2),"-",round(max(rirs),2),")")
  })

  output$tekst_alle_tijd <- renderText({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    tijden <- by(itemoverzicht()$tijd,itemoverzicht()$toets,function(x) mean(x,na.rm=T))
    paste0("Gemiddelde tijd per vraag (min): ",round(mean(tijden,na.rm=T),2)," (",
           round(min(tijden,na.rm=T),2),"-",round(max(tijden,na.rm=T),2),")")
  })
  
  
  output$tekst_alle_nvragenacc <- renderText({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    paste0("Aantal vragen in acceptatie: ",nrow(itemoverzicht_gem()[itemoverzicht_gem()$inacceptatie==1,]))
  })
  
  output$tekst_alle_nvragenaccafgenomen <- renderText({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    n <- nrow(itemoverzicht_gem()[!is.na(itemoverzicht_gem()$p)&itemoverzicht_gem()$inacceptatie==1,])
    n_tot <- nrow(itemoverzicht_gem()[itemoverzicht_gem()$inacceptatie==1,])
    paste0("Aantal acceptatie met afnamegegevens: ",n," (",round(100*n/n_tot,0),"%)")
  })
  
  output$tekst_laatste_volgnr <- renderText({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    paste0("Volgnummer: ",laatstetoets())
  })
  
  output$tekst_laatste_nvragen <- renderText({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    paste0("Aantal vragen: ",nrow(itemoverzicht()[!is.na(itemoverzicht()$toets)&itemoverzicht()$toets==laatstetoets(),]))
  })
  
  output$tekst_laatste_ndg <- renderText({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    paste0("Aantal eerstekansers: ",unique(itemoverzicht()[!is.na(itemoverzicht()$toets)&itemoverzicht()$toets==laatstetoets(),"n"]))
    })
  
  output$tekst_laatste_alfa <- renderText({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    alfas <- unique(itemoverzicht()[,1:4])$alfa
    paste0("Cronbachs alfa: ",unique(itemoverzicht()[!is.na(itemoverzicht()$toets)&itemoverzicht()$toets==laatstetoets(),"alfa"]))
  })
  
  output$tekst_laatste_p <- renderText({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    ps <- itemoverzicht()[!is.na(itemoverzicht()$toets)&itemoverzicht()$toets==laatstetoets(),"p"]
    paste0("p-waarde: gem. ",round(mean(ps),2)," (",
           round(min(ps),2),"-",round(max(ps),2),")")
  })
  
  output$tekst_laatste_mamemo <- renderText({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    laatstetoets <- itemoverzicht()[!is.na(itemoverzicht()$toets)&itemoverzicht()$toets==laatstetoets(),]
    ma <- nrow(laatstetoets[laatstetoets$p_schat=="Ma",])
    me <- nrow(laatstetoets[laatstetoets$p_schat=="Me",])
    mo <- nrow(laatstetoets[laatstetoets$p_schat=="Mo",])
    paste0("Ingeschat als makkelijk / medium / moeilijk: ",
           ma," (",round(100*ma/nrow(laatstetoets),0),"%) / ",
           me," (",round(100*me/nrow(laatstetoets),0),"%) / ",
           mo," (",round(100*mo/nrow(laatstetoets),0),"%)")
  })
  
  output$tekst_laatste_rir <- renderText({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    rirs <- itemoverzicht()[!is.na(itemoverzicht()$toets)&itemoverzicht()$toets==laatstetoets(),"rir"]
    paste0("Itemrestcorrelatie: gem. ",round(mean(rirs),2)," (",
           round(min(rirs),2),"-",round(max(rirs),2),")")
  })
  
  output$tekst_laatste_tijd <- renderText({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    tijden <- itemoverzicht()[!is.na(itemoverzicht()$toets)&itemoverzicht()$toets==laatstetoets(),"tijd"]
    paste0("Tijd per vraag (min): gem. ",round(mean(tijden),2)," (",
           round(min(tijden),2),"-",round(max(tijden),2),")")
  })

  output$tekst_huidig_nvragen <- renderText({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    paste0("Aantal geselecteerde vragen: ",length(geselvragen()))
  })
  
  output$tekst_huidig_nvragenafgenomen <- renderText({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    if(length(geselvragen())>0){
      paste0(toetsoverzicht()[2,1],toetsoverzicht()[2,2])}
  })
  
  output$tekst_huidig_ngoedestats <- renderText({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    if(length(geselvragen())>0){
      paste0(toetsoverzicht()[3,1],toetsoverzicht()[3,2])}
  })
  
  output$tekst_huidig_p <- renderText({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    if(nrow(toetssel_gem()[!is.na(toetssel_gem()$p),])>0){
      paste0(toetsoverzicht()[4,1],toetsoverzicht()[4,2])}
  })
  
  output$tekst_huidig_mamemo <- renderText({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    if(length(geselvragen())>0){
      paste0("Ingeschat als makkelijk / medium / moeilijk: ",
             toetsoverzicht()[5,2]," / ",toetsoverzicht()[6,2]," / ",toetsoverzicht()[7,2])}
  })
  
  output$tekst_huidig_rir <- renderText({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    if(nrow(toetssel_gem()[!is.na(toetssel_gem()$rir),])>0){
      paste0(toetsoverzicht()[8,1],toetsoverzicht()[8,2])}
  })
  
  output$tekst_huidig_tijd <- renderText({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    if(nrow(toetssel_gem()[!is.na(toetssel_gem()$tijd),])>0){
      paste0(toetsoverzicht()[9,1],toetsoverzicht()[9,2])}
  })

  output$tekst_huidig_totaletijd <- renderText({
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    if(length(geselvragen())>0){
      paste0(toetsoverzicht()[10,1],toetsoverzicht()[10,2])}
  })
  
  output$fileDownloadklaar <- reactive({#om in linkerpanel niets weer te geven als nog niks geupload
    if(debug){cat("output$fileDownloadklaar\n")}
    if(is.null(input$itemoverzicht)|is.null(input$inacceptatie)){return(NULL)}
    return(length(geselvragen())>0)
  })
  outputOptions(output, 'fileDownloadklaar', suspendWhenHidden=FALSE) #om in linkerpanel niets weer te geven als nog niks geupload
  
  output$download_xslx <- downloadHandler(
    filename = function() { paste("Samengestelde toets.xlsx")},
    
    content = function(file){
      Results_Workbook <- createWorkbook(type='xlsx')
      sheet.1 <- createSheet(Results_Workbook, sheetName = "Samenvatting")
      addDataFrame(toetsoverzicht(), sheet=sheet.1, startRow=1,startColumn=1,row.names=F,col.names=F)
      sheet.2 <- createSheet(Results_Workbook, sheetName = "Gegevens gecombineerd per vraag")
      addDataFrame(toetssel_gem(), sheet=sheet.2, startRow=1,startColumn=1,row.names=F)
      sheet.3 <- createSheet(Results_Workbook, sheetName = "Gegevens per vraag per afname")
      addDataFrame(toetssel(), sheet=sheet.3, startRow=1,startColumn=1,row.names=F)
      saveWorkbook(Results_Workbook,file)
    })
  
}
