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
    "Upload eerst de door",em("10voordeleraar"),"verstrekte bestanden:",
    br(),
    br(),
    fileInput('itemoverzicht','itemoverzicht.csv',accept = c(".csv")),
    fileInput('inacceptatie','inacceptatie.csv',accept = c(".csv")),
    uiOutput("vakkeuze"),
    conditionalPanel(
      condition = "output.fileUploaded",
    h5(strong("Alle toetsen tot nu toe")),
    textOutput("tekst_alle_ntoetsen"),
    textOutput("tekst_alle_ndg"),
    textOutput("tekst_alle_alfa"),
    textOutput("tekst_alle_p"),
    textOutput("tekst_alle_rir"),
    textOutput("tekst_alle_tijd"),
    textOutput("tekst_alle_nvragenacc"),
    textOutput("tekst_alle_nvragenaccafgenomen"),
    br(),
    h5(strong("Laatst afgenomen toets")),
    textOutput("tekst_laatste_volgnr"),
    textOutput("tekst_laatste_nvragen"),
    textOutput("tekst_laatste_ndg"),
    textOutput("tekst_laatste_alfa"),
    textOutput("tekst_laatste_p"),
    textOutput("tekst_laatste_mamemo"),
    textOutput("tekst_laatste_rir"),
    textOutput("tekst_laatste_tijd"),
    br(),
    h5(strong("Toets die nu wordt samengesteld")),
    textOutput("tekst_huidig_nvragen"),
    textOutput("tekst_huidig_nvragenafgenomen"),
    textOutput("tekst_huidig_ngoedestats"),
    textOutput("tekst_huidig_p"),
    textOutput("tekst_huidig_mamemo"),
    textOutput("tekst_huidig_rir"),
    textOutput("tekst_huidig_tijd"),
    textOutput("tekst_huidig_totaletijd"),
    br(),
    conditionalPanel(
      condition = "output.fileDownloadklaar",
    downloadButton('download_xslx', 'Gegevens samengestelde toets in Excel'))
    )),
  
  ####main panel####
  
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Vraagselectie per (sub)topic",
                         conditionalPanel(
                           condition = "!output.fileUploaded",
                           br(),
                           "Upload eerst de door",em("10voordeleraar"),"verstrekte bestanden (itemoverzicht.csv en 
                           inacceptatie.csv) om te beginnen met de toetssamenstelling."),
                         conditionalPanel(
                           condition = "output.fileUploaded",
                         br(),
                         uiOutput("topiclijst"),
                         h5(strong("Aantal vragen in dit (sub)topic per toets")),
                         tableOutput("tab_nvragenpertopic"),
                         h5(strong("Beschikbare vragen in (sub)topic in acceptatie")),
                         "In de onderstaande lijst van vragen worden alle vragen in acceptatie in het (sub)topic 
                         weergegeven en daarnaast de bijbehorende statistieken. 
                         Wanneer een vraag meer dan een keer is afgenomen geven 'p', 'rir' en 'tijd' de gewogen gemiddelden
                         van de p-waardes, itemrestcorrelaties en gemiddelde tijd per vraag over de afnames.
                         De linkeraanvinklijst kan worden gebruikt om vragen te selecteren voor de toets.
                         De rechteraanvinklijst heeft vinkjes bij de vragen die voldoen aan de hieronder geselecteerde
                         criteria.", 
                         br(),
                         "Let op: wanneer het scherm onvoldoende breed is en vraagcodes of statistieken 
                         van de regel afvallen, kan het zijn  dat de linker- en rechterlijst ten opzichte van elkaar
                         verschoven worden weergeven.",
                         checkboxInput("weergave_welafgenomen", width=600,
                                       label = "vragen met afnamegegevens", 
                                       value = TRUE),
                         checkboxInput("weergave_nietafgenomen", width=600,
                                       label = "vragen zonder afnamegegevens", 
                                       value = TRUE),
                         checkboxInput("weergave_p", width=600,
                                       label = "geen vragen met p-waarde van minder dan .40 of meer dan .90", 
                                       value = TRUE),
                         checkboxInput("weergave_rir", width=600, 
                                       label = "geen vragen met itemrestcorrelatie van minder dan .10", 
                                       value = TRUE),
                         checkboxInput("weergave_tijd", width=600, 
                                       label = "geen vragen met gemiddelde tijd van meer dan 3 minuten", 
                                       value = TRUE),
                         column(6,
                                uiOutput("vraagselectie")),
                         column(6,
                                uiOutput("vraagselectie_stats")),
                         br(),
                         column(12, 
                                h5(strong("Gegevens per vraag in (sub)topic per afname")),
                          "In de onderstaande tabel worden de vragen in het (sub)topic 
                          die meerdere keren zijn afgenomen op meerdere regels 
                         weergegeven, zodat de statistieken per afname zichtbaar zijn.
                          Alleen vragen die voldoen aan de aangevinkte criteria worden weergegeven.",
                          br(),
                          br(),
                         selectInput("vraagsortering", width=500, 
                                     label = "Kies de sortering van de beschikbare vragen in acceptatie in het (sub)topic",
                                     choices = c("vraagcode","toets","p_schat","tijd_schat",
                                                 "p","rir","tijd"),
                                     selected = "vraagcode"),
                         tableOutput("tab_vragenintopic"))
                         )),
                tabPanel("Alle tot nu toe geselecteerde vragen",
                         conditionalPanel(
                           condition = "!output.fileUploaded",
                           br(),
                           "Upload eerst de door",em("10voordeleraar"),"verstrekte bestanden (itemoverzicht.csv en 
                           inacceptatie.csv) om te beginnen met de toetssamenstelling."),
                         conditionalPanel(
                           condition = "output.fileUploaded",
                         br(),
                         h5(strong("Gecombineerde gegevens per vraag")),
                         "In de onderstaande tabel worden statistieken van de voor de toets geselecteerde vragen 
                         weergegeven. Wanneer een vraag meer dan een keer is afgenomen geeft 'n' het totale aantal
                         eerstekansers waarbij de vraag is afgenomen en geven 'p', 'rir' en 'tijd' de gewogen gemiddelden
                         van de p-waardes, itemrestcorrelaties en gemiddelde tijd per vraag over de afnames.",
                         br(),
                         br(),
                         tableOutput("tab_toetssel_gem"),
                         br(),
                         h5(strong("Gegevens per vraag per afname")),
                         "In de onderstaande tabel worden voor de toets geselecteerde vragen 
                          die meerdere keren zijn afgenomen op meerdere regels 
                         weergegeven, zodat de statistieken per afname zichtbaar zijn.",
                         br(),
                         br(),
                         tableOutput("tab_toetssel")
                         ))
                )
    )
)
