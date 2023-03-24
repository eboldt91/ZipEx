library(tidyverse)
library(readxl)
library(shiny)
library(sf)
library(leaflet)

coordinates_list <- read_xlsx("./Data/zip_codes_states.xlsx")
amenity_scores <- read_csv("./Data/amenity_data_scored.csv")
housing_scores <- read_xlsx("./Data/housing_data.xlsx")
housing_scores$housing_score <- abs(housing_scores$housing_score - 6)
economy_scores <- read_xlsx("./Data/local_economy.xlsx")
traffic_scores <- read_xlsx("./Data/traffic.xlsx")
traffic_scores$traffic_score <- abs(traffic_scores$traffic_score - 6)

score_columns <- c('pop_score', 'housing_score', 'eco_score', 'amenity_score', 'traffic_score')

blue_icons <- iconList(
  blue1 = makeIcon("./Data/imgs/blue1.png", iconWidth = 30, iconHeight = 44, iconAnchorX = 15, iconAnchorY = 44, popupAnchorX = 1, popupAnchorY = -40),
  blue2 = makeIcon("./Data/imgs/blue2.png", iconWidth = 30, iconHeight = 44, iconAnchorX = 15, iconAnchorY = 44, popupAnchorX = 1, popupAnchorY = -40),
  blue3 = makeIcon("./Data/imgs/blue3.png", iconWidth = 30, iconHeight = 44, iconAnchorX = 15, iconAnchorY = 44, popupAnchorX = 1, popupAnchorY = -40),
  blue4 = makeIcon("./Data/imgs/blue4.png", iconWidth = 30, iconHeight = 44, iconAnchorX = 15, iconAnchorY = 44, popupAnchorX = 1, popupAnchorY = -40),
  blue5 = makeIcon("./Data/imgs/blue5.png", iconWidth = 30, iconHeight = 44, iconAnchorX = 15, iconAnchorY = 44, popupAnchorX = 1, popupAnchorY = -40)
)

red_icons <- iconList(
  red1 = makeIcon("./Data/imgs/red1.png", iconWidth = 30, iconHeight = 44, iconAnchorX = 15, iconAnchorY = 44, popupAnchorX = 1, popupAnchorY = -40),
  red2 = makeIcon("./Data/imgs/red2.png", iconWidth = 30, iconHeight = 44, iconAnchorX = 15, iconAnchorY = 44, popupAnchorX = 1, popupAnchorY = -40),
  red3 = makeIcon("./Data/imgs/red3.png", iconWidth = 30, iconHeight = 44, iconAnchorX = 15, iconAnchorY = 44, popupAnchorX = 1, popupAnchorY = -40),
  red4 = makeIcon("./Data/imgs/red4.png", iconWidth = 30, iconHeight = 44, iconAnchorX = 15, iconAnchorY = 44, popupAnchorX = 1, popupAnchorY = -40),
  red5 = makeIcon("./Data/imgs/red5.png", iconWidth = 30, iconHeight = 44, iconAnchorX = 15, iconAnchorY = 44, popupAnchorX = 1, popupAnchorY = -40)
)


ui <- fluidPage(
  navbarPage(
    div(img(src="https://brand.gatech.edu/sites/default/files/inline-images/extended-RGB.png", height="50px", style='margin-right:100px'), "ZipEx: Zip Code Explorer")
  ),
  
  tabsetPanel(  
    tabPanel("Map", leafletOutput(outputId="map")),
    tabPanel("Table", dataTableOutput(outputId="table"))
  ),
  
  hr(),
  
  fluidRow(
    column(2),
    column(8,
           h3(HTML("<b>Top 5 Recommended Neighborhoods</b>"), style = "text-align: center"),
           
           h4(HTML("<b>Instructions:</b>")),
           h4("Set your preferences for each attribute on a scale of 1 - 5, with 1 being low and 5 being high, and check the box next to your most important attribute."),
           h4(HTML("<b>Output:</b>")),
           h4("The map will provide the top 5 neighborhoods matching your search criteria. Clicking each marker will provide insights into each location and provide alternative recommendations within the area.")
           ),
    column(2),
    ),
    
  fluidRow(
    column(2),
    
    column(4,
           sliderInput("population_rating",
                       div(checkboxInput("pop_check", label = h5(HTML("<b>Population Density</b>")), value = FALSE)),
                       min=1, max=5, value=3, step=0.5),
           sliderInput("housing_rating",
                       div(checkboxInput("house_check", label = h5(HTML("<b>Real Estate Affordability</b>")), value = FALSE)),
                       min=1, max=5, value=3, step=0.5),
           sliderInput("economy_rating",
                       div(checkboxInput("eco_check", label = h5(HTML("<b>Job Market</b>")), value = FALSE)),
                       min=1, max=5, value=3, step=0.5)
           ),
           
    column(4,
           sliderInput("amenities_rating",
                       div(checkboxInput("amenity_check", label = h5(HTML("<b>Local Amenities</b>")), value = FALSE)),
                       min=1, max=5, value=3, step=0.5),
           sliderInput("traffic_rating",
                       div(checkboxInput("traffic_check", label = h5(HTML("<b>Traffic Sparsity</b>")), value = FALSE)), 
                       min=1, max=5, value=3, step=0.5)
           ),
    
    column(2)
  )
)

server <- function(input, output) {
  
  observeEvent(input$pop_check, {
    if(input$pop_check == TRUE){
      updateCheckboxInput(inputId='house_check', value=FALSE)
      updateCheckboxInput(inputId='eco_check', value=FALSE)
      updateCheckboxInput(inputId='amenity_check', value=FALSE)
      updateCheckboxInput(inputId='traffic_check', value=FALSE)
    }
  })
  
  observeEvent(input$house_check, {
    if(input$house_check == TRUE){
      updateCheckboxInput(inputId='pop_check', value=FALSE)
      updateCheckboxInput(inputId='eco_check', value=FALSE)
      updateCheckboxInput(inputId='amenity_check', value=FALSE)
      updateCheckboxInput(inputId='traffic_check', value=FALSE)
    }
  })
  
  observeEvent(input$eco_check, {
    if(input$eco_check == TRUE){
      updateCheckboxInput(inputId='pop_check', value=FALSE)
      updateCheckboxInput(inputId='house_check', value=FALSE)
      updateCheckboxInput(inputId='amenity_check', value=FALSE)
      updateCheckboxInput(inputId='traffic_check', value=FALSE)
    }
  })
  
  observeEvent(input$amenity_check, {
    if(input$amenity_check == TRUE){
      updateCheckboxInput(inputId='pop_check', value=FALSE)
      updateCheckboxInput(inputId='house_check', value=FALSE)
      updateCheckboxInput(inputId='eco_check', value=FALSE)
      updateCheckboxInput(inputId='traffic_check', value=FALSE)
    }
  })  
  
  observeEvent(input$traffic_check, {
    if(input$traffic_check == TRUE){
      updateCheckboxInput(inputId='pop_check', value=FALSE)
      updateCheckboxInput(inputId='house_check', value=FALSE)
      updateCheckboxInput(inputId='eco_check', value=FALSE)
      updateCheckboxInput(inputId='amenity_check', value=FALSE)
    }
  })  
  
  data = amenity_scores[,c('zipcode', 'population', 'pop_score', 'amenities_per_sqmile', 'amenity_score')] %>%
          merge(housing_scores[,c('zipcode', 'city', 'state', 'avg_home_value', 'avg_rent', 'housing_score')], by='zipcode') %>%
          merge(economy_scores[,c('zipcode', 'avg_salary', 'eco_score')], by='zipcode') %>%
          merge(traffic_scores[,c('zipcode', 'traffic_score')], by='zipcode') %>%
          merge(coordinates_list[, c('zipcode', 'latitude', 'longitude')], by = 'zipcode') %>%
          filter(!is.na(longitude) & !is.na(latitude))
  
  check_boxes = reactive({
    c(input$pop_check, input$house_check, input$eco_check, input$amenity_check, input$traffic_check)
  })
  
  knn_data = data[,c('zipcode', score_columns)]
  
  weights = reactive({
    c(input$population_rating[1], input$housing_rating[1], input$economy_rating[1], input$amenities_rating[1], input$traffic_rating[1]) / sum(c(input$population_rating[1], input$housing_rating[1], input$economy_rating[1], input$amenities_rating[1], input$traffic_rating[1]))
  })
  
  fav_weights = reactive({
    weights() + check_boxes()
  })
  
  new_point = reactive({
    exp(c(input$population_rating[1], input$housing_rating[1], input$economy_rating[1], input$amenities_rating[1], input$traffic_rating[1]) %*% diag(fav_weights()))
  })
    
  dis = reactive({
    sqrt(rowSums(sweep(exp(as.matrix(knn_data[,score_columns]) %*% diag(fav_weights())), 2, new_point())**2))
  })
  
  dis2 = reactive({
    sort(dis(), index.return=TRUE)})
  
  nearest = reactive({
    data[dis2()$ix[1:5],]
    })
  
  output$map = renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = -98, lat = 40, zoom = 4) %>%
      addMarkers(data = nearest(),
                 label= ~lapply(paste("<b>Rank:", which(nearest()$zipcode == zipcode), '</b>',
                               "<br><b>Zip code: ", zipcode, "</b>",
                               "<br><b>", city, ", ", state, "</b>"), HTML),
                 popup = ~paste("<b>Rank: ", which(nearest()$zipcode == zipcode), "</b>",
                                "<br><b>Zip code: ", zipcode, "</b>",
                                "<br><b>", city, ", ", state, "</b>",
                                "<br>Population: ", paste("<a href='//www.unitedstateszipcodes.org/", zipcode, "/#stats", "', target='_blank'>", round(pop_score, 2), "</a>", sep=''),
                                "<br>Housing: ", paste("<a href='//www.zillow.com/", city, "-", state, "-", zipcode, "', target='_blank'>", round(housing_score, 2), "</a>", sep=''),
                                "<br>Job Market: ", paste("<a href='//www.glassdoor.com/Search/results.htm?keyword=&locName=", city, "%2C%20", state, '%20(US)',  "', target='_blank'>", round(eco_score, 2), "</a>", sep=''),
                                "<br>Amenities: ", paste("<a href='//www.google.com/search?q=entertainment+", zipcode, "', target='_blank'>", round(amenity_score, 2), "</a>", sep=''),
                                "<br>Traffic: ", paste("<a href='//www.google.com/search?q=traffic+", zipcode,"', target='_blank'>", round(traffic_score, 2), "</a>", sep=''),
                                "<br>", paste("<a href='//www.google.com/search?q=", zipcode, "+", city, "+", state, "', target='_blank'>Learn More</a>", sep=''),
                                sep=''),
                 lat = ~latitude,
                 lng = ~longitude,
                 layerId = ~zipcode,
                 icon = ~blue_icons)
    
 })
  
  output$table = renderDataTable(
    data.frame(Rank=c(1:20547), data[dis2()$ix,c('zipcode', 'city', 'state', 'population', 'avg_home_value', 'avg_rent', 'avg_salary', 'amenities_per_sqmile')]),
    options = list(
      lengthMenu = list(c(5, 10, 25, 50, 100), c('5', '10', '25', '50', '100')),
      pageLength = 5)
    
    )
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if(is.null(click))
      return()
    
    if(!(click$id %in% nearest()[,'zipcode']))
      return()
    
    current_point <- data[data['zipcode'] == click$id,]
    cp_weights <- current_point[score_columns] / sum(current_point[score_columns])
    
    distances <- sqrt(rowSums(sweep(as.matrix(data[,c("latitude", "longitude")]), 2, as.matrix(current_point[c('latitude', 'longitude')]))**2))
    near <- data[which(distances <= 0.25 & distances != 0),]
    
    nn <- as.matrix(near[, score_columns]) %*% diag(cp_weights)
    nn2 <- exp(nn)
    nn3 <- sweep(nn2, 2, exp(as.matrix(current_point[score_columns]) %*% diag(cp_weights)))
    nn4 <- nn3**2
    nn5 <- rowSums(nn4)
    nn6 <- sqrt(nn5)
    nn7 <- sort(nn6, index.return=TRUE)
    nearest5 <- near[nn7$ix[1:5],]

    
    leafletProxy('map') %>%
      clearGroup('new_points') %>%
      setView(click$lng, click$lat+0.05, zoom=10) %>%
      addMarkers(data = nearest5,
                       label= ~paste("Alt. Rank:", which(nearest5$zipcode == zipcode)),
                       popup = ~paste("<b>Alt. Rank: ", which(nearest5$zipcode == zipcode), "</b>",
                                      "<br><b>Zip code: ", zipcode, "</b>",
                                      "<br><b>", city, ", ", state, "</b>",
                                      "<br>Population: ", paste("<a href='//www.unitedstateszipcodes.org/", zipcode, "/#stats", "', target='_blank'>", round(pop_score, 2), "</a>", sep=''),
                                      "<br>Housing: ", paste("<a href='//www.zillow.com/", city, "-", state, "-", zipcode, "', target='_blank'>", round(housing_score, 2), "</a>", sep=''),
                                      "<br>Job Market: ", paste("<a href='//www.glassdoor.com/Search/results.htm?keyword=&locName=", city, "%2C%20", state, '%20(US)',  "', target='_blank'>", round(eco_score, 2), "</a>", sep=''),
                                      "<br>Amenities: ", paste("<a href='//www.google.com/search?q=entertainment+", zipcode, "', target='_blank'>", round(amenity_score, 2), "</a>", sep=''),
                                      "<br>Traffic: ", paste("<a href='//www.google.com/search?q=traffic+", zipcode,"', target='_blank'>", round(traffic_score, 2), "</a>", sep=''),
                                      "<br>", paste("<a href='//www.google.com/search?q=", zipcode, "+", city, "+", state, "', target='_blank'>Learn More</a>", sep=''),
                                      sep=''),
                       lat = ~latitude,
                       lng = ~longitude,
                       layerId = ~zipcode,
                       icon = ~red_icons,
                       group = 'new_points')
  })
  
  observeEvent(input$map_click, {
    leafletProxy("map") %>%
      clearGroup('new_points') %>%
      setView(lng = -98, lat = 40, zoom = 4)
  })
}

shinyApp(ui, server)
