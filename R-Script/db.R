#include libraries
library(shiny)
library(shinydashboard)
library(jsonlite)
library(httr)
library(lubridate)
library(sp)
library(opensensmapr)
library(dplyr)        
library(sf)
library(magrittr)
library(rgdal)
library(rgeos)
library(rintrojs)
library(ggplot2)
library(zoo)
library(shinyjs)
library(leaflet)


#set working direction
dirname <-  '~/Spatial-Dashboard/R-Script'
if (!dir.exists(dirname))dir.create(dirname,recursive=TRUE)

#setwd("//home/jasper/Spatial-Dashboard/R-Script")

#include help script
source("help.R")

#variables for phenomena
temps <- "Temperatur"
feuchte <- "rel.%20Luftfeuchte"
Luftdruck <- "Luftdruck"
PM10 <- "PM10"
PM25 <- "PM2.5"
UV <- "UV-Intensität"
Helligkeit <- "Beleuchtungsstärke"


#url <- "https://api.opensensemap.org"
#path <- paste('/boxes/data?boxId=570bad2b45fd40c8197f13a2&from-Datum=',now()-days(3),'&to-Datum=',now(),'&phenomenon=PM2.5&format=json', sep = "")
#path <- '/boxes/data?boxId=570bad2b45fd40c8197f13a2&from-Datum=2018-06-25 14:00:00 &to-Datum=2018-07-25T14:00:00Z&phenomenon=PM2.5&format=json'
#raw.result <- GET(url = url, path = path)
#this.raw.content <- rawToChar(raw.result$content)
#this.content <- fromJSON(this.raw.content)


#get current temperature
get_latest_temp <- function(id, phen){
  
  url <- "https://api.opensensemap.org"
  
  path <- paste('//boxes/data?boxId=',id,'&phenomenon=',phen,'&format=json', sep = "")
  #  path <- paste('/statistics/descriptive?boxId=',id,'&phenomenon=Temperatur&from-Datum=2017-06-25T14:00:00Z&to-Datum=2018-07-25T15:00:00Z&operation=max&window=604800000&format=json',sep="")
  
  raw.result <- GET(url = url, path = path)
  
  this.raw.content <- rawToChar(raw.result$content)
  
  this.content <- fromJSON(this.raw.content)
  
  a  <- which(this.content$createdAt == max(this.content$createdAt))
  
  return(this.content$value[[a]])
  
}

#include help file for intro.js content
steps <- read.csv("help.csv")

#dashboard header
header <- dashboardHeader(title = "A Multi-Spatial Dashboard",titleWidth= 450,
                   tags$li(a(href = 'https://github.com/jasperbuss/Spatial-Dashboard',
                             target = "_blank",
                             img(src = 'github-mark.png',
                                 title = "zum github repository", height = "50px"),
                             style = "padding-top:0px; padding-bottom:20px;"),
                           class = "dropdown"))


sidebar <-   dashboardSidebar(disable = TRUE,
                              sidebarMenu(
                                menuItem("dashboard", tabName = "dashboard", icon = icon("dashboard"))
                              ))

body <-   dashboardBody(
  includeCSS("introjs.min.css"),
  
  # Include styling for the app
  includeCSS("app.css"),
  
  # Include IntroJS library
  includeScript("intro.min.js"),
  
  # Include javascript code to make shiny communicate with introJS
  includeScript("app.js"),
  
  fluidRow(
    infoBoxOutput("oldest"),
    infoBoxOutput("newest"),
    infoBoxOutput("aktive")
    ),

  div(class="flexcontainer", 
      
      # action button
      actionButton(inputId="startHelp", label="Hilfe", class="btn-success")
  ),
  tabItems(
    
    
    # First tab content
   tabItem(tabName = "dashboard",
          selectInput('cntr', 'Wähle eine Stadt', c('Berlin', 'Braunschweig','Chemnitz', 
                                                    'Düsseldorf',
                                                    'Flensburg', 'Frankfurt','Hamburg', 'Hannover', 'Kaiserslautern','Karlsruhe','Kiel','Köln',
                                                     'Leipzig','Magdeburg','Mainz','München',  'Münster',
                                                     'Oldenburg', 'Osnabrück', 'Regensburg', 'Rostock', 'Saarbrücken', 
                                                    'Schwerin', 'Stuttgart',
                                                    'Wiesbaden'
                                                     ), selected = 'Münster') ,
            
            leafletOutput("senseMap"),
            br(),

            #     selectInput('data', 'Wähle ein Phenomen', c('Temperatur', 'Luftfeuchtigkeit', 'PM10', 'PM25')),
            #     radioButtons('tool', 'Wähle einen Plot', c("summary", 'plot')),
            #     actionButton('add', 'Add Result'),
            #      div(id = 'placeholder'),
            #     actionButton('rm', "remove result"),
            #     div(id = 'placeholder'),
            fluidRow(
              infoBoxOutput("aktuell_h"),
              infoBoxOutput("aktuell_t"),
              infoBoxOutput("aktuell_pm")
            ),
            
            fluidRow(
              #  box(plotOutput("da"), width = 3)
              box(plotOutput("pm3"), width = 4),
              box(plotOutput("th3"), width = 4),
              box(plotOutput("ld3"), width = 4)
              
            ),
          fluidRow(
            #  box(plotOutput("da"), width = 3)
            box(plotOutput("pm1"), width = 4),
            box(plotOutput("th1"), width = 4),
            box(plotOutput("ld1"), width = 4)
            
          ),
          tags$div(fluidRow(
            tags$footer(a(list(tags$style("img {display:inline-block;background-repeat:no-repeat;position:relative;left:20px;top:30px;z-index:3;}"),
                               tags$img(src="sensebox.svg",target = "_blank", height="65px",width="170px")),href="http://sensebox.de/"),
                        tags$style("footer {background-color: #333333;height:100px}"))))
      
    )
  )
)

ui <- dashboardPage(header,sidebar, body, skin = 'green')

server <- function(input, output, session) {
  
  
  boxes <- osem_boxes()
  
  #cache_info(boxes)
  
  duration = boxes %>%
    group_by(exposure) %>%
    filter(!is.na(updatedAt)) %>%
    mutate(duration = difftime(updatedAt, createdAt, units='days'))
  
  exposure_counts <-  boxes %>%
    group_by(exposure) %>%
    mutate(count = row_number(createdAt))
  
  smry <- exposure_counts %>%
    summarise(
      oldest = min(createdAt),
      newest = max(createdAt),
      count = max(count)
    ) %>%
    arrange(desc(count))
  
  # set help content
  session$sendCustomMessage(type = 'setHelpContent', message = list(steps = toJSON(steps) ))
  
  # listen to the action button
  observeEvent(input$startHelp,{
    
    #  on click, send custom message to start help
    session$sendCustomMessage(type = 'startHelp', message = list(""))
    
  })
  
  
  autoInvaliDatum <- reactiveTimer(40000)
  
  # autoInvaliDatum()
  
  cutemp <- reactive({
    autoInvaliDatum()
    
    if (input$cntr == "Bremen")
      values <- get_latest_temp(Bremen, temps)
    if (input$cntr == "Hamburg")
      values <- get_latest_temp(Hamburg, temps)
    if (input$cntr == "München")
      values <- get_latest_temp(München, temps)
    if (input$cntr == "Hannover")
      values <- get_latest_temp(Hannover, temps)
    if (input$cntr == "Frankfurt")
      values <- get_latest_temp(Frankfurt, temps)
    if (input$cntr == "Mainz")
      values <- get_latest_temp(Mainz, temps)
    if (input$cntr == "Wiesbaden")
      values <- get_latest_temp(Wiesbaden, temps)
    if (input$cntr == "Saarbrücken")
      values <- get_latest_temp(Saarbrücken, temps)
    if (input$cntr == "Berlin")
      values <- get_latest_temp(Berlin, temps)
    if (input$cntr == "Köln")
      values <- get_latest_temp(Köln, temps)
    if (input$cntr == "Kaiserslautern")
      values <- get_latest_temp(Kaiserslautern, temps)
    if (input$cntr == "Karlsruhe")
      values <- get_latest_temp(Karlsruhe, temps)
    if (input$cntr == "Stuttgart")
      values <- get_latest_temp(Stuttgart, temps)
    if (input$cntr == "Chemnitz")
      values <- get_latest_temp(Chemnitz, temps)
    if (input$cntr == "Leipzig")
      values <- get_latest_temp(Leipzig, temps)
    if (input$cntr == "Rostock")
      values <- get_latest_temp(Rostock, temps)
    if (input$cntr == "Schwerin")
      values <- get_latest_temp(Schwerin, temps)
    if (input$cntr == "Kiel")
      values <- get_latest_temp(Kiel, temps)
    if (input$cntr == "Flensburg")
      values <- get_latest_temp(Flensburg, temps)
    if (input$cntr == "Osnabrück")
      values <- get_latest_temp(Osnabrück, temps)
    if (input$cntr == "Münster")
      values <- get_latest_temp(Münster, temps)
    if (input$cntr == "Oldenburg")
      values <- get_latest_temp(Oldenburg, temps)
    if (input$cntr == "Braunschweig")
      values <- get_latest_temp(Braunschweig, temps)
    if (input$cntr == "Magdeburg")
      values <- get_latest_temp(Magdeburg, temps)
    if (input$cntr == "Regensburg")
      values <- get_latest_temp(Regensburg, temps)
    if (input$cntr == "Düsseldorf")
      values <- get_latest_temp(Duesseldorf, temps)

    
    return(values)
    
  })
  
  cuhu <- reactive({
    autoInvaliDatum()
    
    if (input$cntr == "Bremen")
      values <- get_latest_temp(Bremen, feuchte)
    if (input$cntr == "Hamburg")
      values <- get_latest_temp(Hamburg, feuchte)
    if (input$cntr == "München")
      values <- get_latest_temp(München, feuchte)
    if (input$cntr == "Hannover")
      values <- get_latest_temp(Hannover, feuchte)
    if (input$cntr == "Frankfurt")
      values <- get_latest_temp(Frankfurt, feuchte)
    if (input$cntr == "Mainz")
      values <- get_latest_temp(Mainz, feuchte)
    if (input$cntr == "Wiesbaden")
      values <- get_latest_temp(Wiesbaden, feuchte)
    if (input$cntr == "Saarbrücken")
      values <- get_latest_temp(Saarbrücken, feuchte)
    if (input$cntr == "Berlin")
      values <- get_latest_temp(Berlin, feuchte)
    if (input$cntr == "Köln")
      values <- get_latest_temp(Köln, feuchte)
    if (input$cntr == "Kaiserslautern")
      values <- get_latest_temp(Kaiserslautern, feuchte)
    if (input$cntr == "Karlsruhe")
      values <- get_latest_temp(Karlsruhe, feuchte)
    if (input$cntr == "Stuttgart")
      values <- get_latest_temp(Stuttgart, feuchte)
    if (input$cntr == "Chemnitz")
      values <- get_latest_temp(Chemnitz, feuchte)
    if (input$cntr == "Leipzig")
      values <- get_latest_temp(Leipzig, feuchte)
    if (input$cntr == "Rostock")
      values <- get_latest_temp(Rostock, feuchte)
    if (input$cntr == "Schwerin")
      values <- get_latest_temp(Schwerin, feuchte)
    if (input$cntr == "Kiel")
      values <- get_latest_temp(Kiel, feuchte)
    if (input$cntr == "Flensburg")
      values <- get_latest_temp(Flensburg, feuchte)
    if (input$cntr == "Osnabrück")
      values <- get_latest_temp(Osnabrück, feuchte)
    if (input$cntr == "Münster")
      values <- get_latest_temp(Münster, feuchte)
    if (input$cntr == "Oldenburg")
      values <- get_latest_temp(Oldenburg, feuchte)
    if (input$cntr == "Braunschweig")
      values <- get_latest_temp(Braunschweig, feuchte)
    if (input$cntr == "Magdeburg")
      values <- get_latest_temp(Magdeburg, feuchte)
    if (input$cntr == "Regensburg")
      values <- get_latest_temp(Regensburg, feuchte)
    if (input$cntr == "Düsseldorf")
      values <- get_latest_temp(Duesseldorf, feuchte)
    
    return(values)
    
  })
  cupm10 <- reactive({
    autoInvaliDatum()
    
    
    if (input$cntr == "Bremen")
      values <- get_latest_temp(Bremen, PM10)
    if (input$cntr == "Hamburg")
      values <- get_latest_temp(Hamburg, PM10)
    if (input$cntr == "München")
      values <- get_latest_temp(München, PM10)
    if (input$cntr == "Hannover")
      values <- get_latest_temp(Hannover, PM10)
    if (input$cntr == "Frankfurt")
      values <- get_latest_temp(Frankfurt, PM10)
    if (input$cntr == "Mainz")
      values <- get_latest_temp(Mainz, PM10)
    if (input$cntr == "Wiesbaden")
      values <- get_latest_temp(Wiesbaden, PM10)
    if (input$cntr == "Saarbrücken")
      values <- get_latest_temp(Saarbrücken, PM10)
    if (input$cntr == "Berlin")
      values <- get_latest_temp(Berlin, PM10)
    if (input$cntr == "Köln")
      values <- get_latest_temp(Köln, PM10)
    if (input$cntr == "Kaiserslautern")
      values <- get_latest_temp(Kaiserslautern, PM10)
    if (input$cntr == "Karlsruhe")
      values <- get_latest_temp(Karlsruhe, PM10)
    if (input$cntr == "Stuttgart")
      values <- get_latest_temp(Stuttgart, PM10)
    if (input$cntr == "Chemnitz")
      values <- get_latest_temp(Chemnitz, PM10)
    if (input$cntr == "Leipzig")
      values <- get_latest_temp(Leipzig, PM10)
    if (input$cntr == "Rostock")
      values <- get_latest_temp(Rostock, PM10)
    if (input$cntr == "Schwerin")
      values <- get_latest_temp(Schwerin, PM10)
    if (input$cntr == "Kiel")
      values <- get_latest_temp(Kiel, PM10)
    if (input$cntr == "Flensburg")
      values <- get_latest_temp(Flensburg, PM10)
    if (input$cntr == "Osnabrück")
      values <- get_latest_temp(Osnabrück, PM10)
    if (input$cntr == "Münster")
      values <- get_latest_temp(Münster, PM10)
    if (input$cntr == "Oldenburg")
      values <- get_latest_temp(Oldenburg, PM10)
    if (input$cntr == "Braunschweig")
      values <- get_latest_temp(Braunschweig, PM10)
    if (input$cntr == "Magdeburg")
      values <- get_latest_temp(Magdeburg, PM10)
    if (input$cntr == "Regensburg")
      values <- get_latest_temp(Regensburg, PM10)
    if (input$cntr == "Düsseldorf")
      values <- get_latest_temp(Duesseldorf, PM10)
 
    
    return(values)
    
  })
  
  cupm25 <- reactive({
    autoInvaliDatum()
    
    
    if (input$cntr == "Bremen")
      values <- get_latest_temp(Bremen, PM25)
    if (input$cntr == "Hamburg")
      values <- get_latest_temp(Hamburg, PM25)
    if (input$cntr == "München")
      values <- get_latest_temp(München, PM25)
    if (input$cntr == "Hannover")
      values <- get_latest_temp(Hannover, PM25)
    if (input$cntr == "Frankfurt")
      values <- get_latest_temp(Frankfurt, PM25)
    if (input$cntr == "Mainz")
      values <- get_latest_temp(Mainz, PM25)
    if (input$cntr == "Wiesbaden")
      values <- get_latest_temp(Wiesbaden, PM25)
    if (input$cntr == "Saarbrücken")
      values <- get_latest_temp(Saarbrücken, PM25)
    if (input$cntr == "Berlin")
      values <- get_latest_temp(Berlin, PM25)
    if (input$cntr == "Köln")
      values <- get_latest_temp(Köln, PM25)
    if (input$cntr == "Koblenz")
      values <- get_latest_temp(Koblenz, PM25)
    if (input$cntr == "Kaiserslautern")
      values <- get_latest_temp(Kaiserslautern, PM25)
    if (input$cntr == "Karlsruhe")
      values <- get_latest_temp(Karlsruhe, PM25)
    if (input$cntr == "Stuttgart")
      values <- get_latest_temp(Stuttgart, PM25)
    if (input$cntr == "Nürnberg")
      values <- get_latest_temp(Nürnberg, PM25)
    if (input$cntr == "Chemnitz")
      values <- get_latest_temp(Chemnitz, PM25)
    if (input$cntr == "Leipzig")
      values <- get_latest_temp(Leipzig, PM25)
    if (input$cntr == "Rostock")
      values <- get_latest_temp(Rostock, PM25)
    if (input$cntr == "Schwerin")
      values <- get_latest_temp(Schwerin, PM25)
    if (input$cntr == "Kiel")
      values <- get_latest_temp(Kiel, PM25)
    if (input$cntr == "Flensburg")
      values <- get_latest_temp(Flensburg, PM25)
    if (input$cntr == "Osnabrück")
      values <- get_latest_temp(Osnabrück, PM25)
    if (input$cntr == "Münster")
      values <- get_latest_temp(Münster, PM25)

    if (input$cntr == "Oldenburg")
      values <- get_latest_temp(Oldenburg, PM25)
    if (input$cntr == "Braunschweig")
      values <- get_latest_temp(Braunschweig, PM25)
    if (input$cntr == "Magdeburg")
      values <- get_latest_temp(Magdeburg, PM25)
    if (input$cntr == "Regensburg")
      values <- get_latest_temp(Regensburg, PM25)
    if (input$cntr == "Düsseldorf")
      values <- get_latest_temp(Duesseldorf, PM25)

    
    return(values)
    
  })
  
  
  
  output$oldest <- 
    
    renderInfoBox({
      autoInvaliDatum()
      infoBox(
        "Die erste senseBox wurde ",       
        paste(smry$oldest, "registriert"),
        icon = icon("battery-slash"),
        color = "black", fill = TRUE
      )
      
    })
  output$sensor <- 
    
    renderInfoBox({
      autoInvaliDatum()
      infoBox(
        "Nördlichste aktive Box",       
        paste("Name:", boxes$name[1778], "Koordinaten", round(boxes$lon[1778], digits = 4),round(boxes$lat, digits = 4)),
        icon = icon("battery-slash"),
        color = "black", fill = TRUE,
        width = 3
      )
      
    })
  output$aktive <- 
    
    renderInfoBox({
      autoInvaliDatum()
      infoBox(
        "Die am längsten aktive Box läuft seit",       
        paste(round(max(duration$duration)), "Tagen"),
        icon = icon("battery-slash"),
        color = "black", fill = TRUE,
        width = 3
      )
      
    })
  output$newest <- 
    
    renderInfoBox({
      autoInvaliDatum()
      infoBox(
        "neueste",       
        paste(smry$newest),
        icon = icon("sun"),
        color = "aqua", fill = TRUE,
        width = 3
      )
      
    })
  output$aktuell_t <- 
    
    renderInfoBox({
      autoInvaliDatum()
      infoBox(
        "Aktuelle Temperatur",
        paste(cutemp(), "°C"),
        icon = icon("sun"),
        color = "red", fill = TRUE,
        width = 3
      )
      
    })
  
  output$aktuell_h <- 
    
    renderInfoBox({
      autoInvaliDatum()
      infoBox(
        "Aktuelle relative Luftfeuchte",
        paste(cuhu(), "%"),
        icon = icon("sun"),
        color = "red", fill = TRUE,
        width = 3
      )
      
    })
  
  output$aktuell_pm <- 
    
    renderInfoBox({
      autoInvaliDatum()
      infoBox(
        "Aktuelle Feinstaubwerte",
        paste("PM10: ", cupm10(),"ug/m³",  "PM2.5: ", cupm25(), "ug/m³"),
        icon = icon("sun"),
        color = "red", fill = TRUE,
        width = 3
      )
      
    })
  

  output$count <- 
    
    renderInfoBox({
      autoInvaliDatum()
      infoBox(
        "Anzahl aller senseBoxen",       
        paste(smry$count),
        icon = icon("icon-sun", lib = "glyphicon"),
        color = "yellow", fill = TRUE,
        width = 3
      )
      
    })
  

  observeEvent(input$cntr, {
    
    output$ld3 <- 
      
      if (input$cntr == "Hamburg")
     renderPlot({   ggplot(ham_dr , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 8, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    
    
    else if (input$cntr == "München")
      renderPlot({   ggplot(Munch_dr , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 8, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
  
    else if (input$cntr == "Hannover")
      renderPlot({   ggplot(han_dr , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 8, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})

    else if (input$cntr == "Mainz")
      renderPlot({   ggplot(mai_dr , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 8, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Wiesbaden")
      renderPlot({   ggplot(wies_dr , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 8, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})

    else if (input$cntr == "Berlin")
      renderPlot({    ggplot(ber_dr , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 8, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Köln")
      renderPlot({     ggplot(kol_dr , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 8, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Kaiserslautern")
      renderPlot({      ggplot(klt_dr , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 8, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Karlsruhe")
      renderPlot({   ggplot(kr_dr , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 8, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Stuttgart")
      renderPlot({     ggplot(stu_dr , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 8, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Chemnitz")
      renderPlot({    ggplot(che_dr , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 8, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Leipzig")
      renderPlot({    ggplot(lpz_dr , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 8, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Rostock")
      renderPlot({     ggplot(ros_dr , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 8, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Schwerin")
      renderPlot({    ggplot(sw_dr , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 8, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Kiel")
      renderPlot({   ggplot(kl_dr , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 8, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Flensburg")
      renderPlot({     ggplot(fl_dr, aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 8, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Osnabrück")
      renderPlot({      ggplot(osna_dr , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 8, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Münster")
      renderPlot({      ggplot(muns_dr , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 8, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})

  
    else if (input$cntr == "Braunschweig")
      renderPlot({     ggplot(br_dr , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 8, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Magdeburg")
      renderPlot({   ggplot(mag_dr , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 8, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})


    
    output$ld1 <- 
      
      if (input$cntr == "Hamburg")
        renderPlot({     ggplot(ham_drs , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    
    
    else if (input$cntr == "München")
      renderPlot({    ggplot(Munch_drs , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Hannover")
      renderPlot({       ggplot(han_drs , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Frankfurt")
      renderPlot({    ggplot(fra_drs , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})

    else if (input$cntr == "Berlin")
      renderPlot({    ggplot(ber_drs , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Köln")
      renderPlot({    ggplot(kol_drs , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Kaiserslautern")
      renderPlot({   ggplot(klt_drs , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Karlsruhe")
      renderPlot({     ggplot(kr_drs , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Stuttgart")
      renderPlot({     ggplot(stu_drs , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Chemnitz")
      renderPlot({    ggplot(che_drs , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Leipzig")
      renderPlot({      ggplot(lpz_drs , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})

    else if (input$cntr == "Schwerin")
      renderPlot({      ggplot(sw_drs , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Kiel")
      renderPlot({     ggplot(kl_drs , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Flensburg")
      renderPlot({    ggplot(fl_drs , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Osnabrück")
      renderPlot({     ggplot(osna_drs , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Münster")
      renderPlot({    ggplot(muns_drs , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    
    else if (input$cntr == "Oldenburg")
      renderPlot({     ggplot(old_drs , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Braunschweig")
      renderPlot({     ggplot(br_drs , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Magdeburg")
      renderPlot({     ggplot(mag_drs , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Regensburg")
      renderPlot({  ggplot(rb_drs , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    else if (input$cntr == "Düsseldorf")
      renderPlot({   ggplot(dd_drs , aes(x = as.Date(Datum), colour = phenomena )) + 
      ggtitle("Luftdruck")+
      xlab('Datum') + ylab('hPa') +
      #    geom_line(aes(y = x))+
      geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
      scale_colour_manual(values = c("blue", "red"))  +
      labs(y = "Luftdruck [hPa]",
           x = "Datum",
           colour = "Legende")+
      theme(legend.position = c(0.9, 0.9))})
    
    
    
    
    output$senseMap <- 
      
      if(input$cntr == 'Münster')  renderLeaflet({  
        
        leaflet(mnstr) %>% addTiles() %>%
          addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                     radius = ~sqrt(Pop) * 800, popup = ~City
          ) %>% setView(7.62, 51.96, zoom = 7)
        
      })
    
    
    else  if(input$cntr == 'Hamburg')  renderLeaflet({  
      
      leaflet(hmbrg) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 800, popup = ~City
        ) %>% setView( 9.993682, 53.55109,zoom = 7)
      
    })
    
    
    else  if(input$cntr == 'Köln')  renderLeaflet({  
      
      leaflet(kln) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 800, popup = ~City
        ) %>% setView( 6.9602786
                       , 50.937531
                       ,zoom = 7)
      
    })
    
    
    else  if(input$cntr == 'Kiel')  renderLeaflet({  
      
      leaflet(kl) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 800, popup = ~City
        ) %>% setView( 10.1227652
                       , 54.3232927
                       ,zoom = 7)
      
    })
    
    
    else  if(input$cntr == 'München')  renderLeaflet({  
      
      leaflet(mnchn) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 800, popup = ~City
        ) %>% setView(11.58198,48.13513, zoom = 7)
      
    })
    
    else  if(input$cntr == 'Mönchen-Gladbach')  renderLeaflet({  
      
      leaflet(mngl) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 800, popup = ~City
        ) %>% setView(6.442804
                      ,51.180457
                      , zoom = 7)
      
    })
    
    else  if(input$cntr == 'Aachen')  renderLeaflet({  
      
      leaflet(aachn) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 800, popup = ~City
        ) %>% setView(6.0838868
                      ,50.7753455
                      , zoom = 7)
      
    })


    
    else  if(input$cntr == 'Berlin')  renderLeaflet({  
      
      leaflet(brln) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 800, popup = ~City
        ) %>% setView(13.404954
                      ,52.520007
                      , zoom = 7)
      
    })
    

    else  if(input$cntr == 'Braunschweig')  renderLeaflet({  
      
      leaflet(brnsc) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 800, popup = ~City
        ) %>% setView(10.5267696
                      ,52.2688736
                      , zoom = 7)
      
    })
    
    else  if(input$cntr == 'Chemnitz')  renderLeaflet({  
      
      leaflet(cmnz) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 800, popup = ~City
        ) %>% setView(8.5324708,52.0302285, zoom = 7)
      
    })
    


    
    else  if(input$cntr == 'Düsseldorf')  renderLeaflet({  
      
      leaflet(dsdrf) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 800, popup = ~City
        ) %>% setView(6.7734556
                      ,51.2277411
                      , zoom = 7)
      
    })

    else  if(input$cntr == 'Flensburg')  renderLeaflet({  
      
      leaflet(flnb) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 800, popup = ~City
        ) %>% setView(9.4469964
                      ,54.7937431
                      , zoom = 7)
      
    })
    
    
    else  if(input$cntr == 'Frankfurt')  renderLeaflet({  
      
      leaflet(blfld) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 800, popup = ~City
        ) %>% setView(8.682127
                      ,50.110922
                      , zoom = 7)
      
    })
    
    
    else  if(input$cntr == 'Hannover')  renderLeaflet({  
      
      leaflet(hnvr) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 800, popup = ~City
        ) %>% setView(9.7320104
                      ,52.3758916
                      , zoom = 7)
      
    })
    
    
   else  if(input$cntr == 'Karlsruhe')  renderLeaflet({  
      
      leaflet(krls) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 800, popup = ~City
        ) %>% setView( 8.4036527
                       , 49.0068901
                       ,zoom = 7)
      
    })  
    
    else  if(input$cntr == 'Kaiserslautern')  renderLeaflet({  
      
      leaflet(hmbrg) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 800, popup = ~City
        ) %>% setView( 7.7491265
                       , 49.4400657
                       ,zoom = 7)
      
    })    else  if(input$cntr == 'Leipzig')  renderLeaflet({  
      
      leaflet(lpz) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 800, popup = ~City
        ) %>% setView( 12.3730747
                       , 51.3396955
                       ,zoom = 7)
      
    })    else  if(input$cntr == 'Magdeburg')  renderLeaflet({  
      
      leaflet(mgdb) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 800, popup = ~City
        ) %>% setView( 11.6276237
                       , 52.1205333
                       ,zoom = 7)
      
    })    else  if(input$cntr == 'Mainz')  renderLeaflet({  
      
      leaflet(mnz) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 800, popup = ~City
        ) %>% setView( 8.2472526
                       , 49.9928617
                       ,zoom = 7)
      
    })    
    
    
    else  if(input$cntr == 'Oldenburg')  renderLeaflet({  
      
      leaflet(olbrg) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 800, popup = ~City
        ) %>% setView( 8.2145521
                       , 53.1434501
                       ,zoom = 7)
      
    })    else  if(input$cntr == 'Osnabrück')  renderLeaflet({  
      
      leaflet(osna) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 800, popup = ~City
        ) %>% setView( 8.0471788
                       , 52.2799112
                       ,zoom = 7)
      
    })    
    
    
    else  if(input$cntr == 'Regensburg')  renderLeaflet({  
      
      leaflet(rgnbrg) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 800, popup = ~City
        ) %>% setView( 12.1016236
                       , 49.0134297
                       ,zoom = 7)
      
    })    else  if(input$cntr == 'Rostock')  renderLeaflet({  
      
      leaflet(rstk) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 800, popup = ~City
        ) %>% setView( 12.099147
                       , 54.092441
                       ,zoom = 7)
      
    })
    
    
    
    else  if(input$cntr == 'Saarbrücken')  renderLeaflet({  
      
      leaflet(sbrk) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 800, popup = ~City
        ) %>% setView( 6.9969327
                       , 49.2401572
                       ,zoom = 7)
      
    })    
    else  if(input$cntr == 'Stuttgart')  renderLeaflet({  
      
      leaflet(stgrt) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 800, popup = ~City
        ) %>% setView( 9.1829321
                       , 48.7758459
                       ,zoom = 7)
      
    })    
    else  if(input$cntr == 'Schwerin')  renderLeaflet({  
      
      leaflet(swrn) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 800, popup = ~City
        ) %>% setView( 11.4012499
                       , 53.6355022
                       ,zoom = 7)
      
    })    

    else  if(input$cntr == 'Wiesbaden')  renderLeaflet({  
      
      leaflet(wsbn) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 800, popup = ~City
        ) %>% setView( 8.2397608
                       , 50.0782184
                       ,zoom = 7)
      
    })    
    
    
    
    output$pm3 <- 
      
      if(input$cntr == "Münster")
        renderPlot({   ggplot(muns_pm, aes(x = as.Date(Datum), colour = phenomena )) + 
            ggtitle("Feinstaub Konzentration")+
            xlab('Datum') + ylab('ug / m³') +
            geom_line(aes(y = rollmean(x, 5, fill = list(NA, NULL, NA))))+
            scale_y_continuous(sec.axis = dup_axis())+
            scale_colour_manual(values = c("blue", "red"))  +
            labs(y = "ug / m³",
                 x = "Datum",
                 colour = "Partikelgröße")+
            theme(legend.position = c(0.8, 0.9))})  
    
     else if(input$cntr == "München")
        renderPlot({   ggplot(Munch_pm, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
         xlab('Datum') + ylab('ug / m³') +
         geom_line(aes(y = rollmean(x, 5, fill = list(NA, NULL, NA))))+
         scale_y_continuous(sec.axis = dup_axis())+
         scale_colour_manual(values = c("blue", "red"))  +
         labs(y = "ug / m³",
              x = "Datum",
              colour = "Partikelgröße")+
         theme(legend.position = c(0.8, 0.9))})   
    
    else if(input$cntr == "Hamburg")
      renderPlot({   ggplot(ham_pm, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 5, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    
    else if(input$cntr == "Hannover")
      renderPlot({   ggplot(han_pm, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 5, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    
  else  if(input$cntr == "Frankfurt")
    renderPlot({   ggplot(fra_pm, aes(x = as.Date(Datum), colour = phenomena )) + 
        ggtitle("Feinstaub Konzentration")+
        xlab('Datum') + ylab('ug / m³') +
        geom_line(aes(y = rollmean(x, 5, fill = list(NA, NULL, NA))))+
        scale_y_continuous(sec.axis = dup_axis())+
        scale_colour_manual(values = c("blue", "red"))  +
        labs(y = "ug / m³",
             x = "Datum",
             colour = "Partikelgröße")+
        theme(legend.position = c(0.8, 0.9))})   
    
    else if(input$cntr == "Berlin")
      renderPlot({   ggplot(ber_pm, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 5, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    else  if(input$cntr == "Köln")
      renderPlot({   ggplot(kol_pm, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 5, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    else   if(input$cntr == "Stuttgart")
      renderPlot({   ggplot(stu_pm10, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 5, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    else  if(input$cntr == "Kaiserslautern")
      renderPlot({   ggplot(klt_pm10, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 5, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    else  if(input$cntr == "Oldenburg")
      renderPlot({   ggplot(old_pm, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 5, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})     
    else  if(input$cntr == "Leipzig")
      renderPlot({   ggplot(lpz_pm, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    else if(input$cntr == "Rostock")
      renderPlot({   ggplot(ros_pm10, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 5, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    else if(input$cntr == "Saarbrücken")
      renderPlot({   ggplot(sar_pm, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 5, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    else  if(input$cntr == "Chemnitz")
      renderPlot({   ggplot(che_pm, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 5, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})    
    else if(input$cntr == "Kiel")
      renderPlot({   ggplot(kl_pm10, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 5, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    else   if(input$cntr == "Osnabrück")
      renderPlot({   ggplot(osna_pm, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 5, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    else  if(input$cntr == "Magdeburg")
      renderPlot({   ggplot(mag_pm10, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 5, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})    
    else  if(input$cntr == "Wiesbaden")
      renderPlot({   ggplot(wies_pm, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 5, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    else  if(input$cntr == "Mainz")
      renderPlot({   ggplot(mai_pm, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 5, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})    
    else if(input$cntr == "Mönchen-Gladbach")
      renderPlot({   ggplot(mg_pm, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 5, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    else  if(input$cntr == "Karlsruhe")
      renderPlot({   ggplot(kr_pm10, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 5, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    else  if(input$cntr == "Schwerin")
      renderPlot({   ggplot(sw_pm, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 5, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    else if(input$cntr == "Regensburg")
      renderPlot({   ggplot(rb_pm, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 5, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    else  if(input$cntr == "Flensburg")
      renderPlot({   ggplot(fl_pm, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 5, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    
    
    else   if(input$cntr == "Braunschweig")
      renderPlot({   ggplot(br_pm10, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 5, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    
    else if(input$cntr == "Düsseldorf")
      renderPlot({   ggplot(dd_pm10, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 5, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
      
  
      
    output$pm1 <- 
      
      if(input$cntr == "Münster")
        renderPlot({   ggplot(muns_pms, aes(x = as.Date(Datum), colour = phenomena )) + 
            ggtitle("Feinstaub Konzentration")+
            xlab('Datum') + ylab('ug / m³') +
            geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
            scale_y_continuous(sec.axis = dup_axis())+
            scale_colour_manual(values = c("blue", "red"))  +
            labs(y = "ug / m³",
                 x = "Datum",
                 colour = "Partikelgröße")+
            theme(legend.position = c(0.8, 0.9))})  
    
    else if(input$cntr == "München")
      renderPlot({   ggplot(Munch_pms, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    
    else if(input$cntr == "Hamburg")
      renderPlot({   ggplot(ham_pms, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    
    else if(input$cntr == "Hannover")
      renderPlot({   ggplot(han_pms, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    
    else  if(input$cntr == "Frankfurt")
      renderPlot({   ggplot(fra_pms, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    
    else if(input$cntr == "Berlin")
      renderPlot({   ggplot(ber_pms, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    else  if(input$cntr == "Köln")
      renderPlot({   ggplot(kol_pms, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    else   if(input$cntr == "Stuttgart")
      renderPlot({   ggplot(stu_pm10s, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    else  if(input$cntr == "Kaiserslautern")
      renderPlot({   ggplot(klt_pm10s, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    else  if(input$cntr == "Oldenburg")
      renderPlot({   ggplot(old_pms, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})     
    else  if(input$cntr == "Leipzig")
      renderPlot({   ggplot(lpz_pms, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    else if(input$cntr == "Rostock")
      renderPlot({   ggplot(ros_pm10s, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    else if(input$cntr == "Saarbrücken")
      renderPlot({   ggplot(sar_pms, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    else  if(input$cntr == "Chemnitz")
      renderPlot({   ggplot(che_pms, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})    
    else if(input$cntr == "Kiel")
      renderPlot({   ggplot(kl_pm10s, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    else   if(input$cntr == "Osnabrück")
      renderPlot({   ggplot(osna_pms, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    else  if(input$cntr == "Magdeburg")
      renderPlot({   ggplot(mag_pm10s, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})    
    else  if(input$cntr == "Wiesbaden")
      renderPlot({   ggplot(wies_pms, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    else  if(input$cntr == "Mainz")
      renderPlot({   ggplot(mai_pms, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})    
    else  if(input$cntr == "Karlsruhe")
      renderPlot({   ggplot(kr_pm10s, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    else  if(input$cntr == "Schwerin")
      renderPlot({   ggplot(sw_pms, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    else if(input$cntr == "Regensburg")
      renderPlot({   ggplot(rb_pms, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    else  if(input$cntr == "Flensburg")
      renderPlot({   ggplot(fl_pms, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    
    
    else   if(input$cntr == "Braunschweig")
      renderPlot({   ggplot(br_pm10s, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    
    else if(input$cntr == "Düsseldorf")
      renderPlot({   ggplot(dd_pm10s, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Feinstaub Konzentration")+
          xlab('Datum') + ylab('ug / m³') +
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = dup_axis())+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "ug / m³",
               x = "Datum",
               colour = "Partikelgröße")+
          theme(legend.position = c(0.8, 0.9))})   
    
    
    
    
    output$th3 <- 

      if(input$cntr == "Münster")
        renderPlot({   ggplot(muns_th , aes(x = as.Date(Datum), colour = phenomena )) + 
            ggtitle("Luft-Temperatur und Luftfeuchte")+
            xlab('Datum') + ylab('Temperatur ') +
            #    geom_line(aes(y = x))+
            geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
            scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative humidity [%]"))+
            scale_colour_manual(values = c("blue", "red"))  +
            labs(y = "Luft-Temperatur [°C]",
                 x = "Datum",
                 colour = "Legende")+
            theme(legend.position = c(0.9, 0.9))})   
    
  
    else  if(input$cntr == "München")
      renderPlot({   ggplot(Munch_th , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative humidity [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Hamburg")
      renderPlot({   ggplot(ham_th , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative humidity [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Hannover")
      renderPlot({   ggplot(han_th , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative humidity [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Frankfurt")
      renderPlot({   ggplot(fra_th , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative humidity [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Berlin")
      renderPlot({   ggplot(ber_th , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative humidity [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Köln")
      renderPlot({   ggplot(kol_th , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative humidity [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else if(input$cntr == "Stuttgart")
      renderPlot({   ggplot(stu_th , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative humidity [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Kaiserslautern")
      renderPlot({   ggplot(klt_th, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative humidity [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else if(input$cntr == "Oldenburg")
      renderPlot({   ggplot(old_th , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative humidity [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Leipzig")
      renderPlot({   ggplot(lpz_th , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative humidity [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Rostock")
      renderPlot({   ggplot(ros_th , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative humidity [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Saarbrücken")
      renderPlot({   ggplot(sar_temp , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative humidity [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Chemnitz")
      renderPlot({   ggplot(che_th , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative Luftfeuchtigkeit [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Kiel")
      renderPlot({   ggplot(kl_th , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative Luftfeuchtigkeit [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Osnabrück")
      renderPlot({   ggplot(osna_th , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative Luftfeuchtigkeit [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else if(input$cntr == "Magdeburg")
      renderPlot({   ggplot(mag_th , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative Luftfeuchtigkeit [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Wiesbaden")
      renderPlot({   ggplot(wies_th , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative Luftfeuchtigkeit [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Mainz")
      renderPlot({   ggplot(mai_th , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative Luftfeuchtigkeit [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Mönchen-Gladbach")
      renderPlot({   ggplot(mg_th , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative Luftfeuchtigkeit [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else if(input$cntr == "Karlsruhe")
      renderPlot({   ggplot(kr_th , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative Luftfeuchtigkeit [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
  else  if(input$cntr == "Schwerin")
      renderPlot({   ggplot(sw_temp , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative Luftfeuchtigkeit [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Regensburg")
      renderPlot({   ggplot(rb_th , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative Luftfeuchtigkeit [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Flensburg")
      renderPlot({   ggplot(fl_th , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative Luftfeuchtigkeit [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    
    
    else if(input$cntr == "Braunschweig")
      renderPlot({   ggplot(br_th , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative Luftfeuchtigkeit [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})
    
    else if(input$cntr == "Düsseldorf")
      renderPlot({   ggplot(fl_th , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative Luftfeuchtigkeit [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})
    
    
    
    output$th1 <- 
      
      if(input$cntr == "Münster")
        renderPlot({   ggplot(muns_ths , aes(x = as.Date(Datum), colour = phenomena )) + 
            ggtitle("Luft-Temperatur und Luftfeuchte")+
            xlab('Datum') + ylab('Temperatur ') +
            #    geom_line(aes(y = x))+
            geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
            scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative humidity [%]"))+
            scale_colour_manual(values = c("blue", "red"))  +
            labs(y = "Luft-Temperatur [°C]",
                 x = "Datum",
                 colour = "Legende")+
            theme(legend.position = c(0.9, 0.9))})   
    
    
    else  if(input$cntr == "München")
      renderPlot({   ggplot(Munch_ths , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative humidity [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Hamburg")
      renderPlot({   ggplot(ham_ths , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative humidity [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Hannover")
      renderPlot({   ggplot(han_ths , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative humidity [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Frankfurt")
      renderPlot({   ggplot(fra_ths , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative humidity [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Berlin")
      renderPlot({   ggplot(ber_ths , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative humidity [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Köln")
      renderPlot({   ggplot(kol_ths , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative humidity [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else if(input$cntr == "Stuttgart")
      renderPlot({   ggplot(stu_ths , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative humidity [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Kaiserslautern")
      renderPlot({   ggplot(klt_ths, aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative humidity [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else if(input$cntr == "Oldenburg")
      renderPlot({   ggplot(old_ths , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative humidity [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Leipzig")
      renderPlot({   ggplot(lpz_ths , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative humidity [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Rostock")
      renderPlot({   ggplot(ros_ths , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative humidity [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Saarbrücken")
      renderPlot({   ggplot(sar_temps , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative humidity [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Chemnitz")
      renderPlot({   ggplot(che_ths , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative Luftfeuchtigkeit [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Kiel")
      renderPlot({   ggplot(kl_ths , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative Luftfeuchtigkeit [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Osnabrück")
      renderPlot({   ggplot(osna_ths , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative Luftfeuchtigkeit [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else if(input$cntr == "Magdeburg")
      renderPlot({   ggplot(mag_ths , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative Luftfeuchtigkeit [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Wiesbaden")
      renderPlot({   ggplot(wies_ths , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative Luftfeuchtigkeit [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Mainz")
      renderPlot({   ggplot(mai_ths , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative Luftfeuchtigkeit [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else if(input$cntr == "Karlsruhe")
      renderPlot({   ggplot(kr_ths , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative Luftfeuchtigkeit [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Schwerin")
      renderPlot({   ggplot(sw_temps , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative Luftfeuchtigkeit [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Regensburg")
      renderPlot({   ggplot(rb_ths , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative Luftfeuchtigkeit [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    else  if(input$cntr == "Flensburg")
      renderPlot({   ggplot(fl_ths , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative Luftfeuchtigkeit [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})   
    
    
    else if(input$cntr == "Braunschweig")
      renderPlot({   ggplot(br_ths , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative Luftfeuchtigkeit [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})
    
    else if(input$cntr == "Düsseldorf")
      renderPlot({   ggplot(fl_ths , aes(x = as.Date(Datum), colour = phenomena )) + 
          ggtitle("Luft-Temperatur und Luftfeuchte")+
          xlab('Datum') + ylab('Temperatur ') +
          #    geom_line(aes(y = x))+
          geom_line(aes(y = rollmean(x, 1, fill = list(NA, NULL, NA))))+
          scale_y_continuous(sec.axis = sec_axis(~.*1, breaks = c(20,40,60,80,100),labels = c("20", "40", "60", "80", "100"), name = "Relative Luftfeuchtigkeit [%]"))+
          scale_colour_manual(values = c("blue", "red"))  +
          labs(y = "Luft-Temperatur [°C]",
               x = "Datum",
               colour = "Legende")+
          theme(legend.position = c(0.9, 0.9))})
    
    
    
  
})
  
}


shinyApp(ui ,  server)





