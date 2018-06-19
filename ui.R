library(shiny)
library(ggplot2)
library(scales)
library(directlabels)
library(plyr)
library(xlsx)
library(psy)
library(xtable) #om automatisch tabel te laten maken
library(reshape2) #voor omzetten correlatiematrix naar lang
library(knitr)

fluidPage(
  
  titlePanel("ToetsSamenstellingsTool"),
  
  sidebarPanel(
    "Stel een nieuwe toets samen met behulp van de statistische gegevens van vragen die al
    eerder zijn afgenomen.",
    br(),
    br(),
    selectInput("vakkeuze", 
                label = "Kies het vak voor toetssamenstelling",
                choices = c("NT", "RW", "AK", "BI","DU","EC","EN","FR","GS","GW",
                            "MA","NA","NE","OK","SK","WK"),
                selected = "RW"),
    h5(strong("Alle toetsen tot nu toe")),
    textOutput("tekst_alle_ntoetsen"),
    textOutput("tekst_alle_ndg"),
    textOutput("tekst_alle_alfa"),
    textOutput("tekst_alle_p"),
    textOutput("tekst_alle_rir"),
    textOutput("tekst_alle_tijd"),
    br(),
    h5(strong("Afgelopen toets")),
    textOutput("tekst_laatste_volgnr"),
    textOutput("tekst_laatste_ndg"),
    textOutput("tekst_laatste_alfa"),
    textOutput("tekst_laatste_p"),
    textOutput("tekst_laatste_rir"),
    textOutput("tekst_laatste_tijd"),
    br(),
    h5(strong("Toets die nu wordt samengesteld"))
    ),
  
  ####main panel####
  
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Vraagselectie per subtopic",
                         br(),
                         uiOutput("topiclijst"),
                         textOutput("tekst_nvragenpertopic"),
                         br(),
                         selectInput("vraagsortering", width=500, 
                                     label = "Kies de sortering van de vragen in het topic",
                                     choices = c("vraagcode","toets","p_schat","tijd_schat",
                                                 "p","rir","tijd"),
                                     selected = "vraagcode"),
                         checkboxInput("weergave_p", width=600,
                                       label = "alleen vragen met p-waarde van minstens .40 en hoogstens .90", 
                                       value = TRUE),
                         checkboxInput("weergave_rir", width=600, 
                                       label = "alleen vragen met itemrestcorrelatie van minstens .10", 
                                       value = TRUE),
                         checkboxInput("weergave_tijd", width=600, 
                                       label = "alleen vragen met gemiddelde tijd van hoogstens 3 minuten", 
                                       value = TRUE),
                         tableOutput("tab_vragenintopic"),
                         br(),
                         uiOutput("vraagselectie")
                         ),
                tabPanel("Alle tot nu toe geselecteerde varagen",
                         br())
                )
    )
)
