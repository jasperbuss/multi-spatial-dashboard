library(shiny)
library(jsonlite)
library(httr)
library(lubridate)
library(sp)
library(opensensmapr)
library(dplyr)        
library(fdrtool)
library(sf)
library(units)
library(magrittr)
library(rgdal)
library(rgeos)
library(rintrojs) 
library(cowplot)
library(plotly)

NRW ="59763220fe1c740011719f1d,5b01b8f0223bd80019357439,570bad2b45fd40c8197f13a2,5b3e7f6f5dc1ec001be11cf1"

BREMEN ="5b2200151fef04001bc45eab,589e0a05ede4bd00109186d6"

NIEDERSACHSEN="5b2124a51fef04001ba24362,5b0a7aa04cd32e0019b64674,59342ee2ad0fa30011e85360,5ad21f1a223bd80019d41ccc"

Hamburg="59df62cfcec1f2000f263bbe,590e0b0a51d3460011c725c4,5b54905185381900195a1968"

Berlin="592ca4b851d3460011ea2635,59f8af62356823000fcc460c"

Brandenburg="597f4ff4e3b1fa001013c1d7,59592d0994f05200114428e8,5a915e10bc2d410019b19557"

Thüringen="58f0bfe3ba279000119bb1ee,5b28c9061fef04001bd6318f,5aa40396396417001b7ab4e5"

Sachsen="5a196da619991f00118fa186,5ac9e0c7c4edec0019093bc3,5a8de4babc2d41001940cc5c,597c4420fe1c740011cbfefb"

Bayern="5a47a3efdf5df2001a394b46,5aa6ea73396417001bd9c92f,59c9374cd67eb500119833d4,5aa1a53a396417001b2ba63f,5a6de0917cbee8000f707ad1"

BadenWürttemberg="597f3e3de3b1fa001012ba3c,5977bde9fe1c740011889f38,59eb178c49f6f80011a361e6,5a19b8d719991f0011962db6"

SachsenAnhalt="581112710d07460012f1b36b,59906d4a7e280a001028c955"

Saarland="59835538e3b1fa001051cad5,5a535fd5fa02ec000ffdcce4,5a63a6bb411a790019c44ce1"

RheinlandPfalz="5aaac64e396417001b5b1195,598b2f86e3b1fa0010f0f8d4,59726914fe1c740011396a83,5917501651d34600111d21d0"

SchleswigHolstein="5a25825e37b73700109c44c9,5a85f03fbc2d4100194852bb,5b5308bc853819001917f75c,5a94894fbc2d4100191a18dc,5ae89845223bd8001940df8b" 

Hessen="597a0939fe1c740011aaa080,591cb99a51d3460011508f29,5af2ed9c223bd80019e057a4,5b450e565dc1ec001bf7cd1d"

MeckPomm="5ac37b76850005001bface69,59206a3b51d3460011750d4e,5a9c5387bc2d41001924872b"

#url <- "https://api.opensensemap.org"
#path <- '/boxes/data?boxId=5a79937eb43907001b172bc2,5a44f07edf5df2001af6dd42,5b4d11485dc1ec001b5452c7&from-date=2018-02-25T14:00:00Z&to-date=2018-06-25T14:00:00Z&phenomenon=Temperatur&format=json'
#raw.result <- GET(url = url, path = path)
#this.raw.content <- rawToChar(raw.result$content)
#this.content <- fromJSON(this.raw.content)

temp <- "Temperatur"
Luftfeuchtigkeit <- "rel. Luftfeuchte"
Luftdruck <- "Luftdruck"
PM10 <- "PM10"
PM25 <- "PM2.5"
UV <- "UV-Intensität"
Helligkeit <- "Beleuchtungsstärke"


get_stat_measurements <- function(id, phen){

  url <- "https://api.opensensemap.org"
  
  path <- paste('//boxes/data?boxId=',id,'&from-date=2018-06-25T14:00:00Z&to-date=2018-07-25T15:00:00Z&phenomenon=',phen,'&format=json', sep = "")
  #  path <- paste('/statistics/descriptive?boxId=',id,'&phenomenon=Temperatur&from-date=2017-06-25T14:00:00Z&to-date=2018-07-25T15:00:00Z&operation=max&window=604800000&format=json',sep="")
  
  raw.result <- GET(url = url, path = path)
  
  this.raw.content <- rawToChar(raw.result$content)
  
  this.content <- fromJSON(this.raw.content)
  
  this.content$createdAt <- as.POSIXct(this.content$createdAt)
  
  this.content$value <- as.numeric(this.content$value)
  
  Hours<-cut(this.content$createdAt, "1 h",right = TRUE) 
  
  h_mean <- aggregate(this.content[,2],by=list(Hours),FUN=mean) 
  
  names(h_mean)[1] <- "TIMESTAMP"
  
  return(h_mean)
  
}

temp_nrw <- get_stat_measurements(NRW,temp)
tp <- temp_nrw$x[!temp_nrw$x %in% boxplot.stats(temp_nrw$x)$out]
PM10_nrw <- get_stat_measurements(NRW,PM10)
PM25_nrw <- get_stat_measurements(NRW,PM25)

plot_ly(temp_nrw, type=NULL, labels = )

temp_bremen <- get_stat_measurements(BREMEN, temp)
PM10_bremen <- get_stat_measurements(BREMEN,PM10)
PM25_bremen <- get_stat_measurements(BREMEN,PM25)

temp_Hamburg <- get_stat_measurements(Hamburg, temp)
PM10_Hamburg <- get_stat_measurements(Hamburg,PM10)
PM25_Hamburg <- get_stat_measurements(Hamburg,PM25)

temp_Berlin <- get_stat_measurements(Berlin, temp)
PM10_Berlin <- get_stat_measurements(Berlin,PM10)
PM25_Berlin <- get_stat_measurements(Berlin,PM25)

temp_nds <- get_stat_measurements(NIEDERSACHSEN, temp)
PM10_nds <- get_stat_measurements(NIEDERSACHSEN,PM10)
PM25_nds <- get_stat_measurements(NIEDERSACHSEN,PM25)

temp_thue <- get_stat_measurements(Thüringen, temp)
PM10_thue <- get_stat_measurements(Thüringen,PM10)
PM25_thue <- get_stat_measurements(Thüringen,PM25)

temp_baden <- get_stat_measurements(BadenWürttemberg, temp)
PM10_baden <- get_stat_measurements(BadenWürttemberg,PM10)
PM25_baden <- get_stat_measurements(BadenWürttemberg,PM25)

temp_bayern <- get_stat_measurements(Bayern, temp)
tep_bayern <- temp_bayern$x[!temp_bayern$x %in% boxplot.stats(temp_bayern$x)$out]

PM10_bayern <- get_stat_measurements(Bayern,PM10)
names(PM10_bayern)[1] <- 'Datum'
names(PM10_bayern)[2] <- 'anteil'

PM25_bayern <- get_stat_measurements(Bayern,PM25)

temp_saarland <- get_stat_measurements(Saarland, temp)
PM10_saarland <- get_stat_measurements(Saarland,PM10)
PM25_saarland <- get_stat_measurements(Saarland,PM25)

temp_sachsen <- get_stat_measurements(Sachsen, temp)
PM10_sachsen <- get_stat_measurements(Sachsen,PM10)
PM25_sachsen <- get_stat_measurements(Sachsen,PM25)

temp_sachsenanhalt <- get_stat_measurements(SachsenAnhalt, temp)
PM10_sachsenanhalt <- get_stat_measurements(SachsenAnhalt,PM10)
PM25_sachsenanhalt <- get_stat_measurements(SachsenAnhalt,PM25)

temp_branden <- get_stat_measurements(Brandenburg, temp)
PM10_branden <- get_stat_measurements(Brandenburg,PM10)
PM25_branden <- get_stat_measurements(Brandenburg,PM25)

temp_hessen <- get_stat_measurements(Hessen, temp)
PM10_hessen <- get_stat_measurements(Hessen,PM10)
PM25_hessen <- get_stat_measurements(Hessen,PM25)

temp_sh <- get_stat_measurements(SchleswigHolstein, temp)
PM10_sh <- get_stat_measurements(SchleswigHolstein,PM10)
PM25_sh <- get_stat_measurements(SchleswigHolstein,PM25)

temp_mp <- get_stat_measurements(MeckPomm,temp)
PM10_mp <- get_stat_measurements(MeckPomm,PM10)
PM25_mp <- get_stat_measurements(MeckPomm,PM25)

temp_rp <- get_stat_measurements(RheinlandPfalz,temp) 
temp_rp <- temp_rp[!temp_rp %in% boxplot.stats(temp_rp)$out]
PM10_rp <- get_stat_measurements(RheinlandPfalz,PM10)
PM25_rp <- get_stat_measurements(RheinlandPfalz,PM25)

theme_set(theme_cowplot(font_size=12)) # reduce default font size
plot.nrw <- ggplot(temp_nrw)
plot.bay <- ggplot(temp_bayern)

plot_grid(plot.nrw, plot.bay, labels = "AUTO")

ui <- shinyUI(fluidPage(
        headerPanel(title= "Spatial Dashboard"),
        
        sidebarLayout(
          sidebarPanel(
            selectInput('cntr', 'Wähle eine Stadt', c('Hamburg', 'Bayern', 'Bremen', 'Niedersachsen', 'Baden-Württemberg', 'Sachsen-Anhalt','Saarland',
                                                        'Sachsen', 'Schleswig-Holstein', 'Thüringen', 'Nordrhein-Westfalen', 'Hessen','Mecklenburg-Vorpommern',
                                                        'Brandenburg', 'Berlin', 'Rheinland-Pfalz'  ), selected = 'Hamburg'),  
              selectInput('data', 'Wähle ein Phenomen', c('Temperatur', 'Luftfeuchtigkeit', 'PM10', 'PM25')),
         #     radioButtons('tool', 'Wähle einen Plot', c("summary", 'plot')),
              actionButton('add', 'Add Result'),
              actionButton('removeBtn', "remove result")
                 ),
          
            mainPanel(
              fluidRow(
            column(8, align="left",  plotOutput("df")),
            column(4, align="right",  plotOutput("ds"))
              )
            )
          )
        )
      )
    


p <- plot_ly(data, labels = ~TIMESTAMP, values = ~x, type = 'pie',textposition = 'inside',
                          textinfo = 'label+percent',
                          insidetextfont = list(color = '#FFFFFF'),
                          hoverinfo = 'text',
                          text = ~paste( x, ' grad celsius'),
                        marker = list(colors = colors,
                        line = list(color = '#FFFFFF', width = 1)),showlegend = FALSE) %>%
     layout(title = 'United States Personal Expenditures by Categories in 1960',
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
server <- function(input, output, session) {

 # im server als df speichern und dann rendern lassen 

  datasetTemp <- reactive( {
    if(input$cntr == 'Bayern')
    df <- temp_bayern
    
    if(input$cntr =='Hamburg')
      df <- temp_Hamburg
    
    return(df)

  })
  
  datasetPM10 <- reactive({
    if(input$cntr == 'Bayern')
      df <- PM10_bayern
    
    if(input$cntr =='Hamburg')
      df <- PM10_Hamburg
    
    return(df)
  })
  
  output$df <- renderPlot({ plot(datasetTemp(), ylim=c(-10,40))}, height = 300, width = 500)   
  
  
  output$df <-  renderPlot({ p}, height = 300, width = 500)   
  
  
    
    #renderPrint({ summary(isolate(datasets()))}, width = 600)
  
  #observeEvent(input$add, {
  #  id <- input$add
  #  insertUI('#placeholder'
        #     ui = switch (input$tool,
         #                'summary' = verbatimTextOutput(id),
          #               'plot' = plotOutput(id),
           #              'head' = tableOutput(id))
  # )
    
   #output[[id]] <- 
  #                 renderPrint({ summary(isolate(dataset()))}, width = 600)
#                   renderPlot({ plot(dataset(), ylim=c(-10,40))}, height = 400, width = 600)    
                   

                
 # })

                       observeEvent(input$removeBtn, {
                         removeUI(
                           selector = 'div:has(> #sld13)'
                         )
                       })
}


shinyApp(ui ,  server)


if(input$cntr  == 'Nordrhein-Westfalen')
  
  df <-    switch (input$data, 'Temperatur' = temp_nrw, 'Luftfeuchtigkeit' = ft_ham$value, 'PM25' = PM25_nrw ,'PM10' = PM10_nrw)


if(input$cntr == 'Bayern')
  
  df <-    switch (input$data, 'Temperatur' = temp_bayern, 'Luftfeuchtigkeit' = ft_bay$value, 'PM25' = PM25_bayern,'PM10' = PM10_bayern)


if(input$cntr  == 'Bremen')
  
  df <-    switch (input$data, 'Temperatur' = temp_bremen, 'Luftfeuchtigkeit' = ft_ham$value, 'PM25' = PM25_bremen,'PM10' = PM10_bremen)


if(input$cntr == 'Niedersachsen')
  
  df <-    switch (input$data, 'Temperatur' = temp_ndsn, 'Luftfeuchtigkeit' = ft_bay$value, 'PM25' = PM25_nds,'PM10' = PM10_nds)


if(input$cntr  == 'Berlin')
  
  df <-    switch (input$data, 'Temperatur' = temp_Berlin, 'Luftfeuchtigkeit' = ft_ham$value, 'PM25' = PM25_Berlin,'PM10' = PM10_Berlin)


if(input$cntr == 'Brandenburg')
  
  df <-    switch (input$data, 'Temperatur' = temp_branden, 'Luftfeuchtigkeit' = ft_bay$value, 'PM25' = PM25_branden,'PM10' = PM10_branden)



if(input$cntr  == 'Sachsen')
  
  df <-    switch (input$data, 'Temperatur' = temp_sachsen, 'Luftfeuchtigkeit' = ft_$value, 'PM25' = PM25_sachsen,'PM10' = PM10_sachsen)


if(input$cntr == 'Sachsen-Anhalt')
  
  df <-    switch (input$data, 'Temperatur' = temp_sachsenanhalt, 'Luftfeuchtigkeit' = ft_bay$value, 'PM25' = PM25_sachsenanhalt,'PM10' = PM10_sachsenanhalt)


if(input$cntr  == 'Thüringen')
  
  df <-    switch (input$data, 'Temperatur' = temp_thue, 'Luftfeuchtigkeit' = ft_ham$value, 'PM25' = PM25_thue ,'PM10' = PM10_thue)


if(input$cntr == 'Baden-Württemberg')
  
  df <-    switch (input$data, 'Temperatur' = temp_baden, 'Luftfeuchtigkeit' = ft_bay$value, 'PM25' = PM25_baden,'PM10' = PM10_baden)


if(input$cntr  == 'Schleswig-Holstein')
  
  df <-    switch (input$data, 'Temperatur' = temp_sh, 'Luftfeuchtigkeit' = ft_ham$value, 'PM25' = PM25_sh,'PM10' = PM10_sh)


if(input$cntr == 'Mecklenburg-Vorpommern')
  
  df <-    switch (input$data, 'Temperatur' = temp_meckpomm, 'Luftfeuchtigkeit' = ft_bay$value, 'PM25' = PM25_meckpomm,'PM10' = PM10_meckpomm)



if(input$cntr  == 'Hamburg')
  
  df <-    switch (input$data, 'Temperatur' = temp_Hamburg, 'Luftfeuchtigkeit' = ft_ham$value, 'PM25' = PM25_Hamburg ,'PM10' = PM10_Hamburg)



if(input$cntr == 'Saarland')
  
  df <-    switch (input$data, 'Temperatur' = temp_saarland, 'Luftfeuchtigkeit' = ft_bay$value, 'PM25' = PM25_saarland,'PM10' = PM10_saarland)



if(input$cntr  == 'Hessen')
  
  df <-    switch (input$data, 'Temperatur' = temp_hessen, 'Luftfeuchtigkeit' = ft_ham$value, 'PM25' = PM25_hessen,'PM10' = PM10_hessen)


if(input$cntr == 'Rheinland-Pfalz')
  
  df <-    switch (input$data, 'Temperatur' = temp_rp, 'Luftfeuchtigkeit' = ft_r$value, 'PM25' = PM25_rp,'PM10' = PM10_rp)

return(df)



