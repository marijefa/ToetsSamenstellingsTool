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
  
    vraagcodein2 <- strsplit(as.character(itemoverzicht$vraagcode),"\\$")
    itemoverzicht$topic <- unlist(strsplit(unlist(vraagcodein2)[2*(1:nrow(itemoverzicht))-1],"-"))[4*(1:nrow(itemoverzicht))-2]
    itemoverzicht$p_schat <- substr(unlist(vraagcodein2)[2*(1:nrow(itemoverzicht))],1,2)
    itemoverzicht$tijd_schat <- substr(unlist(vraagcodein2)[2*(1:nrow(itemoverzicht))],4,4)
    
    itemoverzicht
  })
  
  laatstetoets <- reactive({
    if(debug){cat("laatstetoets\n")}
    paste0("T",max(as.numeric(substr(unique(itemoverzicht()$toets),2,3))))

  })
  
  ###nonreactive###
  
  output$topiclijst = renderUI({
    if(debug){cat("topickeuze\n")}
    selectInput("topickeuze", label="Kies een (sub)topic om vragen uit te selecteren voor de toets", 
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
  
  output$tekst_alle_ntoetsen <- renderText({
    paste0("Aantal toetsen (ingelezen): ",substr(laatstetoets(),2,3),
           " (",length(unique(itemoverzicht()$toets)),")")
  })
  
  output$tekst_alle_ndg <- renderText({
    ns <- unique(itemoverzicht()[,1:4])$n
    paste0("Aantal eerstekansers: gem. ",round(mean(ns),0)," (",
           round(min(ns),2),"-",round(max(ns),2),")")
  })
  
  output$tekst_alle_alfa <- renderText({
    alfas <- unique(itemoverzicht()[,1:4])$alfa
    paste0("Cronbachs alfa: gem. ",round(mean(alfas),2)," (",
           round(min(alfas),2),"-",round(max(alfas),2),")")
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
    tijden <- by(itemoverzicht()$tijd,itemoverzicht()$toets,mean)
    paste0("Gemiddelde tijd per vraag (min): ",round(mean(tijden),2)," (",
           round(min(tijden),2),"-",round(max(tijden),2),")")
  })
  
  output$tekst_laatste_volgnr <- renderText({
    paste0("Volgnummer: ",laatstetoets())
  })
  
  output$tekst_laatste_ndg <- renderText({
    paste0("Aantal eerstekansers: ",unique(itemoverzicht()[itemoverzicht()$toets==laatstetoets(),"n"]))
    })
  
  output$tekst_laatste_alfa <- renderText({
    alfas <- unique(itemoverzicht()[,1:4])$alfa
    paste0("Cronbachs alfa: ",unique(itemoverzicht()[itemoverzicht()$toets==laatstetoets(),"alfa"]))
  })
  
  output$tekst_laatste_p <- renderText({
    ps <- itemoverzicht()[itemoverzicht()$toets==laatstetoets(),"p"]
    paste0("p-waarde: gem. ",round(mean(ps),2)," (",
           round(min(ps),2),"-",round(max(ps),2),")")
  })
  
  output$tekst_laatste_rir <- renderText({
    rirs <- itemoverzicht()[itemoverzicht()$toets==laatstetoets(),"rir"]
    paste0("Itemrestcorrelatie: gem. ",round(mean(rirs),2)," (",
           round(min(rirs),2),"-",round(max(rirs),2),")")
  })
  
  output$tekst_laatste_tijd <- renderText({
    tijden <- itemoverzicht()[itemoverzicht()$toets==laatstetoets(),"tijd"]
    paste0("Tijd per vraag: gem. ",round(mean(tijden),2)," (",
           round(min(tijden),2),"-",round(max(tijden),2),")")
  })
  
}
