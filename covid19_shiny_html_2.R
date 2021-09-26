---
title: "New York City Covid-19 Trend by Modified ZCTA"
output: html_document
runtime: shiny
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE)
library(shiny)
library(tidyverse) # tidy data wrangling
library(vroom) # fast reading/importing data
library(sf) # spatial data
library(tigris) # geojoin
library(leaflet) # interactive maps
library(htmlwidgets) # interactive map labels
```


```{r include=FALSE}
all_modzcta <- readRDS("all_modzcta.RDS")
```



```{r include=FALSE}
ui <- fluidPage(
    # Sidebar with data input
   sidebarLayout(
      sidebarPanel(
         tags$a(href="https://github.com/nychealth/coronavirus-data", 
                "Data Repository", target = "_blank"),
         h5("All data metrics are aggregated by week (categorized by week ending in data).
             Percent positive indicates the percentage of people that tested for COVID-19 with a molecular test who tested positive.
         All data is sourced from the NYC Department of Health."),
         selectInput("date",
                     "Select a date (week ending in):",
                     choices = unique(all_modzcta$week_ending)
         )
         
      ),
      # Show a plot of the generated distribution
   mainPanel(
      tabsetPanel(
         tabPanel("Case Rate", leafletOutput("cases")),
         tabPanel("Test Rate", leafletOutput("tests")),
         tabPanel("Percent Positive", leafletOutput("pctpos"))
           )
         )
   )   
)
```

```{r include=FALSE}
server <- function(input, output) {
   week_zcta <- reactive({
   w <- all_modzcta %>% filter(week_ending == input$date)
   return(w)
})
   
   output$cases <- renderLeaflet({
   pal <- colorBin(palette = "YlGn", 9, domain = all_modzcta$caserate)
   
   labels = sprintf(
      "<strong>%s</strong><br/>%g cases per 100,000 people",
      week_zcta()$MODZCTA, week_zcta()$caserate) %>%
      lapply(htmltools::HTML)
   
   week_zcta() %>%
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(-73.9, 40.7, zoom = 10) %>%
      addPolygons(label = labels,
                  stroke = FALSE,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~pal(week_zcta()$caserate),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "black",
                                                      opacity = 1,
                                                      bringToFront = TRUE))%>%
         addLegend("bottomright",
                   pal = pal,
                   values = ~caserate,
                   title = "Case per 100,000",
                   opacity = 0.7)
      
   
})
   
   output$tests <- renderLeaflet({
   pal <- colorBin(palette = "PuBu", 9, domain = all_modzcta$testrate)
   
   labels = sprintf(
      "<strong>%s</strong><br/>%g cases per 100,000 people",
      week_zcta()$MODZCTA, week_zcta()$testrate) %>%
      lapply(htmltools::HTML)
   
   week_zcta() %>%
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(-73.9, 40.7, zoom = 10) %>%
      addPolygons(label = labels,
                  stroke = FALSE,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~pal(week_zcta()$testrate),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "black",
                                                      opacity = 1,
                                                      bringToFront = TRUE))%>%
         addLegend("bottomright",
                   pal = pal,
                   values = ~testrate,
                   title = "Case per 100,000",
                   opacity = 0.7)
      
   
})
   
   output$pctpos <- renderLeaflet({
   pal <- colorBin(palette = "OrRd", 9, domain = all_modzcta$pctpos)
   
   labels = sprintf(
      "<strong>%s</strong><br/>%g cases per 100,000 people",
      week_zcta()$MODZCTA, week_zcta()$pctpos) %>%
      lapply(htmltools::HTML)
   
   week_zcta() %>%
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(-73.9, 40.7, zoom = 10) %>%
      addPolygons(label = labels,
                  stroke = FALSE,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~pal(week_zcta()$pctpos),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "black",
                                                      opacity = 1,
                                                      bringToFront = TRUE))%>%
         addLegend("bottomright",
                   pal = pal,
                   values = ~testrate,
                   title = "Case per 100,000",
                   opacity = 0.7)
      
   
})
}
```

```{r echo=FALSE}
shinyApp(ui = ui, server = server, options = list(height=1300))
```

```{r eval=FALSE, include=FALSE}
inputPanel(
   # Sidebar with data input
      sidebarPanel(
         tags$a(href="https://github.com/nychealth/coronavirus-data", 
                "Data Repository", target = "_blank"),
         h5("All data metrics are aggregated by week (categorized by week ending in data).
             Percent positive indicates the percentage of people that tested for COVID-19 with a molecular test who tested positive.
         All data is sourced from the NYC Department of Health."),
         selectInput("date",
                     "Select a date (week ending in):",
                     choices = unique(all_modzcta$week_ending,)
         )
      ),
    selectInput("ind", "Index",
                choices = list("Case Rate" = "caserate", "Test Rate" = "testrate", "Percentage Positive" = "pctpos"),
                selected = "Case Rate")
   )   

```

```{r eval=FALSE, include=FALSE}
inputPanel(
   # Sidebar with data input
   sidebarLayout(
      sidebarPanel(
         tags$a(href="https://github.com/nychealth/coronavirus-data", 
                "Data Repository", target = "_blank"),
         h5("All data metrics are aggregated by week (categorized by week ending in data).
             Percent positive indicates the percentage of people that tested for COVID-19 with a molecular test who tested positive.
         All data is sourced from the NYC Department of Health."),
         selectInput("date",
                     "Select a date (week ending in):",
                     choices = unique(all_modzcta$week_ending)
         )
      ),
      # Show a plot of the generated distribution
   mainPanel(
      tabsetPanel(
         tabPanel("Case Rate", leafletOutput("cases")),
         tabPanel("Test Rate", leafletOutput("tests")),
         tabPanel("Percent Positive", leafletOutput("pctpos"))
           )
         )
   )   
)
```


```{r eval=FALSE, include=FALSE}
week_zcta <- reactive({
   w <- all_modzcta %>% filter(week_ending == input$date)
   return(w)
})
```

```{r eval=FALSE, include=FALSE}
renderLeaflet({
   pal <- colorBin(palette = "YlGn", 9, domain = all_modzcta$caserate)
   
   labels = sprintf(
      "<strong>%s</strong><br/>%g cases per 100,000 people",
      week_zcta()$MODZCTA, week_zcta() %>% select(input$ind)) %>%
      lapply(htmltools::HTML)
   
   week_zcta() %>%
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(-73.9, 40.7, zoom = 10) %>%
      addPolygons(label = labels,
                  stroke = FALSE,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~pal(week_zcta() %>% select(input$ind)),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "black",
                                                      opacity = 1,
                                                      bringToFront = TRUE))%>%
         addLegend("bottomright",
                   pal = pal,
                   values = ~input$ind,
                   title = "Case per 100,000",
                   opacity = 0.7)
      
   
})
```


```{r eval=FALSE, include=FALSE}
renderLeaflet({
   pal <- colorBin(palette = "PuBu", 9, domain = all_modzcta$testrate)
   
   labels = sprintf(
      "<strong>%s</strong><br/>%g cases per 100,000 people",
      week_zcta()$MODZCTA, week_zcta()$testrate) %>%
      lapply(htmltools::HTML)
   
   week_zcta() %>%
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(-73.9, 40.7, zoom = 10) %>%
      addPolygons(label = labels,
                  stroke = FALSE,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~pal(week_zcta()$testrate),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "black",
                                                      opacity = 1,
                                                      bringToFront = TRUE))%>%
         addLegend("bottomright",
                   pal = pal,
                   values = ~testrate,
                   title = "Case per 100,000",
                   opacity = 0.7)
      
   
})
```

```{r eval=FALSE, include=FALSE}
renderLeaflet({
   pal <- colorBin(palette = "OrRd", 9, domain = all_modzcta$pctpos)
   
   labels = sprintf(
      "<strong>%s</strong><br/>%g cases per 100,000 people",
      week_zcta()$MODZCTA, week_zcta()$pctpos) %>%
      lapply(htmltools::HTML)
   
   week_zcta() %>%
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(-73.9, 40.7, zoom = 10) %>%
      addPolygons(label = labels,
                  stroke = FALSE,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~pal(week_zcta()$pctpos),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "black",
                                                      opacity = 1,
                                                      bringToFront = TRUE))%>%
         addLegend("bottomright",
                   pal = pal,
                   values = ~testrate,
                   title = "Case per 100,000",
                   opacity = 0.7)
      
   
})
```

