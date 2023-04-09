pacman::p_load(shiny, sf, tidyverse, tmap, DT, sp, dplyr, ggplot2, SpatialAcc)

# Fix Stuff
hexagon <- readRDS("data/model/hexagon_sf.rds")
mpsz <- readRDS("data/model/mpsz.rds")
mrt <- readRDS("data/model/mrt_sf.rds")
mpsz_popdata2022 <- readRDS("data/model/mpsz_popdata2022.rds")

# Map Variables
tourism <- readRDS("data/model/tourism_sf.rds")
shopping <- readRDS("data/model/shopping_sf.rds")
childcare <- readRDS("data/model/childcare_sf.rds")
eldercare <- readRDS("data/model/eldercare_sf.rds")
hawker <- readRDS("data/model/hawker_sf.rds")
kindergarten <- readRDS("data/model/kindergarten_sf.rds")
park <- readRDS("data/model/park_sf.rds")
supermarket <- readRDS("data/model/supermarket_sf.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Transit-Ability - Group 5 - IS415"),
  
  # Tabs
  tabsetPanel(
    # First tab
    tabPanel("Overview",
             tags$image(src = "data/image/logo.png"),
             tags$p("Project Transit-Ability helps Singaporeans throughout the city by analyzing 
                      the spatial accessibility using different methods from MRT/LRT stations to 
                      relevant locations.")
    ),
    
    # Second tab
    tabPanel("EDA",
             # Sidebar panel
             sidebarPanel(
               selectInput(
                 inputId = "mapvariable",
                 label = "Mapping Variable:",
                 choices = c("Tourist Attraction" = "tourism",
                             "Shopping Malls" = "shopping",
                             "Child Cares" = "childcare",
                             "Elderly Cares" = "eldercare",
                             "Hawker Centres" = "hawker",
                             "Kindergartens" = "kindergarten",
                             "Parks" = "park",
                             "Supermarkets" = "supermarket"),
                 selected = "tourism"
               ),
               tags$p("Change the Mapping Variable above to view the different 
                        locations with the MRT & LRT lines on the Singapore map."),
               submitButton("Apply Changes")
             ),
             # Main panel
             mainPanel(
               tags$p(HTML("<b>MRT x Mapping Variable</b>")),
               tmapOutput("mapPlot_eda1",
                          width = "100%",
                          height = 400),
               br(),
               tags$p(HTML("<b>Please give a bit of time for the map to load</b>")),
               br(),
               tags$p(HTML("<b>MRT x Population Density</b>")),
               tmapOutput("mapPlot_eda2",
                          width = "100%",
                          height = 400),
               br(),
               tags$p(HTML("<b>Please give a bit of time for the map to load</b>")),
             ),
    ),
    
    # Third tab
    tabPanel("Hansen Method",
             # Sidebar panel
             sidebarPanel(
               width = 3,
               selectInput(
                 inputId = "mapvariable_Hansen",
                 label = "Mapping Variable:",
                 choices = c("Tourist Attraction" = "tourism",
                             "Shopping Malls" = "shopping",
                             "Child Cares" = "childcare",
                             "Elderly Cares" = "eldercare",
                             "Hawker Centres" = "hawker",
                             "Kindergartens" = "kindergarten",
                             "Parks" = "park",
                             "Supermarkets" = "supermarket"),
                 selected = "tourism"
               ),
               tags$p("Change the Mapping Variable above to view the different 
           locations with the MRT & LRT lines on the Singapore map."),
               numericInput("userdemand", "Demand:", 100, min = 100),
               numericInput("usercapacity", "Capacity:", 100, min = 100),
               selectInput(
                 inputId = "region",
                 label = "Region:",
                 choices = c("All Region" = "allregion",
                             "Central Region" = "CENTRAL REGION",
                             "West Region" = "WEST REGION",
                             "East Region" = "EAST REGION",
                             "North-East Region" = "NORTH-EAST REGION",
                             "North" = "NORTH REGION")
               ),
               selectInput( inputId = "colour",
                            label = "Mapping Variable Colour:",
                            choices = list("Grey" = "grey",
                                           "White" = "white",
                                           "Yellow" = "yellow",
                                           "Green" = "green",
                                           "Blue" = "blue",
                                           "Pink" = "pink",
                                           "Purple" = "purple",
                                           "Cyan" = "cyan",
                                           "Lime Green" = "limegreen"),
                            selected = "grey"),
               submitButton("Apply Changes")
             ),
             # Main panel
             mainPanel(
               tags$p(HTML("<b>Hexagon x Mapping Variable</b>")),
               tmapOutput("mapPlot_hansen1", width = "100%", height = 400),
               br(),
               tags$p(HTML("<b>Please give a bit of time for the map to load</b>")),
               br(),
               tags$p(HTML("<b>MRT x Mapping Variable</b>")),
               tmapOutput("mapPlot_hansen2", width = "100%", height = 400),
               br(),
               tags$p(HTML("<b>Please give a bit of time for the map to load</b>")),
               br(),
               tags$p(HTML("<b>MRT - Statistical Graphic Visualisation</b>")),
               plotOutput("mapPlot_hansen3", width = "100%", height = 400),
               br(),
               tags$p(HTML("<b>Please give a bit of time for the chart to load</b>"))
             )
    ),
    # Fourth tab
    tabPanel("KD2SFCA Method",
             # Sidebar panel
             sidebarPanel(
               width = 3,
               selectInput(
                 inputId = "mapvariable_KD2SFCA",
                 label = "Mapping Variable:",
                 choices = c("Tourist Attraction" = "tourism",
                             "Shopping Malls" = "shopping",
                             "Child Cares" = "childcare",
                             "Elderly Cares" = "eldercare",
                             "Hawker Centres" = "hawker",
                             "Kindergartens" = "kindergarten",
                             "Parks" = "park",
                             "Supermarkets" = "supermarket"),
                 selected = "tourism"
               ),
               tags$p("Change the Mapping Variable above to view the different 
           locations with the MRT & LRT lines on the Singapore map."),
               numericInput("userdemand_KD2SFCA", "Demand:", 100, min = 100),
               numericInput("usercapacity_KD2SFCA", "Capacity:", 100, min = 100),
               selectInput(
                 inputId = "region_KD2SFCA",
                 label = "Region:",
                 choices = c("All Region" = "allregion",
                             "Central Region" = "CENTRAL REGION",
                             "West Region" = "WEST REGION",
                             "East Region" = "EAST REGION",
                             "North-East Region" = "NORTH-EAST REGION",
                             "North" = "NORTH REGION")
               ),
               selectInput( inputId = "colour_KD2SFCA",
                            label = "Mapping Variable Colour:",
                            choices = list("Grey" = "grey",
                                           "White" = "white",
                                           "Yellow" = "yellow",
                                           "Green" = "green",
                                           "Blue" = "blue",
                                           "Pink" = "pink",
                                           "Purple" = "purple",
                                           "Cyan" = "cyan",
                                           "Lime Green" = "limegreen"),
                            selected = "grey"),
               submitButton("Apply Changes")
             ),
             # Main panel
             mainPanel(
               tags$p(HTML("<b>Hexagon x Mapping Variable</b>")),
               tmapOutput("mapPlot_KD2SFCA1", width = "100%", height = 400),
               br(),
               tags$p(HTML("<b>Please give a bit of time for the map to load</b>")),
               br(),
               tags$p(HTML("<b>MRT x Mapping Variable</b>")),
               tmapOutput("mapPlot_KD2SFCA2", width = "100%", height = 400),
               br(),
               tags$p(HTML("<b>Please give a bit of time for the map to load</b>")),
               br(),
               tags$p(HTML("<b>MRT - Statistical Graphic Visualisation</b>")),
               plotOutput("mapPlot_KD2SFCA3", width = "100%", height = 400),
               br(),
               tags$p(HTML("<b>Please give a bit of time for the chart to load</b>"))
             )
    ),
    # Fifth tab
    tabPanel("SAM Method",
             # Sidebar panel
             sidebarPanel(
               width = 3,
               selectInput(
                 inputId = "mapvariable_SAM",
                 label = "Mapping Variable:",
                 choices = c("Tourist Attraction" = "tourism",
                             "Shopping Malls" = "shopping",
                             "Child Cares" = "childcare",
                             "Elderly Cares" = "eldercare",
                             "Hawker Centres" = "hawker",
                             "Kindergartens" = "kindergarten",
                             "Parks" = "park",
                             "Supermarkets" = "supermarket"),
                 selected = "tourism"
               ),
               tags$p("Change the Mapping Variable above to view the different 
           locations with the MRT & LRT lines on the Singapore map."),
               numericInput("userdemand_SAM", "Demand:", 100, min = 100),
               numericInput("usercapacity_SAM", "Capacity:", 100, min = 100),
               selectInput(
                 inputId = "region_SAM",
                 label = "Region:",
                 choices = c("All Region" = "allregion",
                             "Central Region" = "CENTRAL REGION",
                             "West Region" = "WEST REGION",
                             "East Region" = "EAST REGION",
                             "North-East Region" = "NORTH-EAST REGION",
                             "North" = "NORTH REGION")
               ),
               selectInput( inputId = "colour_SAM",
                            label = "Mapping Variable Colour:",
                            choices = list("Grey" = "grey",
                                           "White" = "white",
                                           "Yellow" = "yellow",
                                           "Green" = "green",
                                           "Blue" = "blue",
                                           "Pink" = "pink",
                                           "Purple" = "purple",
                                           "Cyan" = "cyan",
                                           "Lime Green" = "limegreen"),
                            selected = "grey"),
               submitButton("Apply Changes")
             ),
             # Main panel
             mainPanel(
               tags$p(HTML("<b>Hexagon x Mapping Variable</b>")),
               tmapOutput("mapPlot_SAM1", width = "100%", height = 400),
               br(),
               tags$p(HTML("<b>Please give a bit of time for the map to load</b>")),
               br(),
               tags$p(HTML("<b>MRT x Mapping Variable</b>")),
               tmapOutput("mapPlot_SAM2", width = "100%", height = 400),
               br(),
               tags$p(HTML("<b>Please give a bit of time for the map to load</b>")),
               br(),
               tags$p(HTML("<b>MRT - Statistical Graphic Visualisation</b>")),
               plotOutput("mapPlot_SAM3", width = "100%", height = 400),
               br(),
               tags$p(HTML("<b>Please give a bit of time for the chart to load</b>"))
             )
    ),
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # EDA
  output$mapPlot_eda1 <- renderTmap({
    tmap_options(check.and.fix = TRUE) +
      tm_shape(mrt) +
      tm_dots(alph=0.5, size=0.07) +
      tm_shape(get(input$mapvariable)) +
      tm_dots(size = 0.1, col = 'yellow') +
      tm_view(set.zoom.limits = c(11,14),
              view.legend.position = c("right", "bottom"))
  })
  
  output$mapPlot_eda2 <- renderTmap({
    tm_shape(mpsz_popdata2022) +
      tm_fill("DEPENDENCY",
              breaks = c(0, 0.60, 0.70, 0.80, 0.90, 1.00)) +
      tm_borders(lwd = 0.1,  alpha = 1) +
      tm_shape(mrt) +
      tm_dots(alph=0.5, size=0.07)+
      tm_view(set.zoom.limits = c(11,14))
  })
  
  # Henson
  
  output$mapPlot_hansen1 <- renderTmap({
    hexagon <- hexagon %>%
      dplyr::select(fid) %>%
      mutate(demand = input$userdemand)
    
    mapvariable <- get(input$mapvariable_Hansen)
    mapvariable <- mapvariable %>%
      mutate(capacity = input$usercapacity)
    
    
    hexagon_coord <- st_centroid(hexagon) %>% 
      st_coordinates()
    
    mapvariable_coord <- st_coordinates(mapvariable)
    
    hex_mapvariable <- SpatialAcc::distance(hexagon_coord,
                                            mapvariable_coord,
                                            type = "euclidean")
    
    hex_mapvariable_km <- as.matrix(hex_mapvariable/1000)
    
    acc_Hansen <- data.frame(ac(hexagon$demand,
                                mapvariable$capacity,
                                hex_mapvariable_km,
                                d0 = 50,
                                power = 2,
                                family = "Hansen"))
    
    colnames(acc_Hansen) <- "accHansen"
    acc_Hansen <- as_tibble(acc_Hansen)
    hex_mapvariable_hansen <- bind_cols(hexagon, acc_Hansen)
    
    # Joining of MPSZ
    mrt <- st_join(mrt, mpsz,
                   join = st_intersects)
    mapvariable <- st_join(mapvariable, mpsz,
                           join = st_intersects)
    hexagon <- st_join(hex_mapvariable_hansen, mpsz,
                       join = st_intersects)
    
    # Check if region parameter is selected
    if (input$region == "allregion") {
      # Use full data
      hexagon_region <- hexagon
      mapvariable_region <- mapvariable
      mrt_region <- mrt
    } else {
      # Filter data for selected region
      hexagon_region <- hexagon %>% 
        filter(REGION_N == input$region)
      mrt_region <- mrt %>% 
        filter(REGION_N == input$region)
      mapvariable_region <- mapvariable %>% 
        filter(REGION_N == input$region)
    }
    
    tm_shape(hexagon_region,
             bbox = st_bbox(hex_mapvariable_hansen)) + 
      tm_fill(col = "accHansen",
              n = 10,
              style = "quantile",
              border.col = "black",
              border.lwd = 1) +
      tm_shape(mrt_region) +
      tm_dots(alph=0.7, size=0.07)+
      tm_shape(mapvariable_region) +
      tm_symbols(size = 0.3, col = input$colour) +
      tm_view(set.zoom.limits = c(11,14),
              view.legend.position = c("right", "bottom"))
  })
  
  output$mapPlot_hansen2 <- renderTmap({
    
    mrt <- mrt %>%
      mutate(demand = input$userdemand)
    
    mapvariable <- get(input$mapvariable_Hansen)
    mapvariable <- mapvariable %>%
      mutate(capacity = input$usercapacity)
    
    
    mrt_coord <- st_centroid(mrt) %>% 
      st_coordinates()
    
    mapvariable_coord <- st_coordinates(mapvariable)
    
    mrt_mapvariable <- SpatialAcc::distance(mrt_coord,
                                            mapvariable_coord,
                                            type = "euclidean")
    
    mrt_mapvariable_km <- as.matrix(mrt_mapvariable/1000)
    
    acc_Hansen <- data.frame(ac(mrt$demand,
                                mapvariable$capacity,
                                mrt_mapvariable_km,
                                d0 = 50,
                                power = 2,
                                family = "Hansen"))
    
    colnames(acc_Hansen) <- "accHansen"
    acc_Hansen <- as_tibble(acc_Hansen)
    mrt_mapvariable_hansen <- bind_cols(mrt, acc_Hansen)
    
    # Joining of MPSZ
    mrt <- st_join(mrt_mapvariable_hansen, mpsz,
                   join = st_intersects)
    mapvariable <- st_join(mapvariable, mpsz,
                           join = st_intersects)
    
    # Check if region parameter is selected
    if (input$region == "allregion") {
      # Use full data
      mapvariable_region <- mapvariable
      mrt_region <- mrt
    } else {
      # Filter data for selected region
      mrt_region <- mrt %>% 
        filter(REGION_N == input$region)
      mapvariable_region <- mapvariable %>% 
        filter(REGION_N == input$region)
    }
    
    boxplot <- st_join(mrt_mapvariable_hansen, mpsz,
                       join = st_intersects)
    
    tm_shape(mrt_region,
             bbox = st_bbox(mrt_mapvariable_hansen)) +
      tm_bubbles(col = "accHansen",
                 n = 10,
                 style = "quantile",
                 size = 0.2,
                 border.col = "black",
                 border.lwd = 1) +
      tm_view(set.zoom.limits = c(11,14),
              view.legend.position = c("right", "bottom")) +
      tm_shape(mapvariable_region) +
      tm_symbols(size = 0.3, col = input$colour) +
      tm_shape(mrt_region) +
      tm_dots(alph=0.1, size=0.1)
  })
  
  output$mapPlot_hansen3 <- renderPlot({
    
    mrt <- mrt %>%
      mutate(demand = input$userdemand)
    
    mapvariable <- get(input$mapvariable_Hansen)
    mapvariable <- mapvariable %>%
      mutate(capacity = input$usercapacity)
    
    
    mrt_coord <- st_centroid(mrt) %>% 
      st_coordinates()
    
    mapvariable_coord <- st_coordinates(mapvariable)
    
    mrt_mapvariable <- SpatialAcc::distance(mrt_coord,
                                            mapvariable_coord,
                                            type = "euclidean")
    
    mrt_mapvariable_km <- as.matrix(mrt_mapvariable/1000)
    
    acc_Hansen <- data.frame(ac(mrt$demand,
                                mapvariable$capacity,
                                mrt_mapvariable_km,
                                d0 = 50,
                                power = 2,
                                family = "Hansen"))
    
    colnames(acc_Hansen) <- "accHansen"
    acc_Hansen <- as_tibble(acc_Hansen)
    mrt_mapvariable_hansen <- bind_cols(mrt, acc_Hansen)
    
    # Joining of MPSZ
    mrt <- st_join(mrt_mapvariable_hansen, mpsz,
                   join = st_intersects)
    mapvariable <- st_join(mapvariable, mpsz,
                           join = st_intersects)
    
    # Check if region parameter is selected
    if (input$region == "allregion") {
      # Use full data
      mapvariable_region <- mapvariable
      mrt_region <- mrt
    } else {
      # Filter data for selected region
      mrt_region <- mrt %>% 
        filter(REGION_N == input$region)
      mapvariable_region <- mapvariable %>% 
        filter(REGION_N == input$region)
    }
    
    boxplot <- st_join(mrt_mapvariable_hansen, mpsz,
                       join = st_intersects)
    
    ggplot(data = boxplot, 
           aes(y = log(accHansen), 
               x = REGION_N)) +
      geom_boxplot() +
      stat_summary(fun.y = "mean", 
                   geom = "point", 
                   colour = "red", 
                   size = 2)
  })

  # KD2SFCA
  
  output$mapPlot_KD2SFCA1 <- renderTmap({
    
    hexagon <- hexagon %>%
      dplyr::select(fid) %>%
      mutate(demand = input$userdemand_KD2SFCA)
    
    mapvariable <- get(input$mapvariable_KD2SFCA)
    mapvariable <- mapvariable %>%
      mutate(capacity = input$usercapacity_KD2SFCA)
    
    
    hexagon_coord <- st_centroid(hexagon) %>% 
      st_coordinates()
    
    mapvariable_coord <- st_coordinates(mapvariable)
    
    hex_mapvariable <- SpatialAcc::distance(hexagon_coord,
                                            mapvariable_coord,
                                            type = "euclidean")
    
    hex_mapvariable_km <- as.matrix(hex_mapvariable/1000)
    
    acc_KD2SFCA <- data.frame(ac(hexagon$demand,
                                mapvariable$capacity,
                                hex_mapvariable_km,
                                d0 = 50,
                                power = 2,
                                family = "KD2SFCA"))
    
    colnames(acc_KD2SFCA) <- "accKD2SFCA"
    acc_KD2SFCA <- as_tibble(acc_KD2SFCA)
    hex_mapvariable_KD2SFCA <- bind_cols(hexagon, acc_KD2SFCA)
    
    # Joining of MPSZ
    mrt <- st_join(mrt, mpsz,
                   join = st_intersects)
    mapvariable <- st_join(mapvariable, mpsz,
                           join = st_intersects)
    hexagon <- st_join(hex_mapvariable_KD2SFCA, mpsz,
                       join = st_intersects)
    
    # Check if region parameter is selected
    if (input$region_KD2SFCA == "allregion") {
      # Use full data
      hexagon_region <- hexagon
      mapvariable_region <- mapvariable
      mrt_region <- mrt
    } else {
      # Filter data for selected region
      hexagon_region <- hexagon %>% 
        filter(REGION_N == input$region_KD2SFCA)
      mrt_region <- mrt %>% 
        filter(REGION_N == input$region_KD2SFCA)
      mapvariable_region <- mapvariable %>% 
        filter(REGION_N == input$region_KD2SFCA)
    }
    
    tm_shape(hexagon_region,
             bbox = st_bbox(hex_mapvariable_KD2SFCA)) + 
      tm_fill(col = "accKD2SFCA",
              n = 10,
              style = "quantile",
              border.col = "black",
              border.lwd = 1) +
      tm_shape(mrt_region) +
      tm_dots(alph=0.7, size=0.07)+
      tm_shape(mapvariable_region) +
      tm_symbols(size = 0.3, col = input$colour_KD2SFCA) +
      tm_view(set.zoom.limits = c(11,14),
              view.legend.position = c("right", "bottom"))
  })
  
  output$mapPlot_KD2SFCA2 <- renderTmap({
    
    mrt <- mrt %>%
      mutate(demand = input$userdemand_KD2SFCA)
    
    mapvariable <- get(input$mapvariable_KD2SFCA)
    mapvariable <- mapvariable %>%
      mutate(capacity = input$usercapacity_KD2SFCA)
    
    
    mrt_coord <- st_centroid(mrt) %>% 
      st_coordinates()
    
    mapvariable_coord <- st_coordinates(mapvariable)
    
    mrt_mapvariable <- SpatialAcc::distance(mrt_coord,
                                            mapvariable_coord,
                                            type = "euclidean")
    
    mrt_mapvariable_km <- as.matrix(mrt_mapvariable/1000)
    
    acc_KD2SFCA <- data.frame(ac(mrt$demand,
                                mapvariable$capacity,
                                mrt_mapvariable_km,
                                d0 = 50,
                                power = 2,
                                family = "KD2SFCA"))
    
    colnames(acc_KD2SFCA) <- "accKD2SFCA"
    acc_KD2SFCA <- as_tibble(acc_KD2SFCA)
    mrt_mapvariable_KD2SFCA <- bind_cols(mrt, acc_KD2SFCA)
    
    # Joining of MPSZ
    mrt <- st_join(mrt_mapvariable_KD2SFCA, mpsz,
                   join = st_intersects)
    mapvariable <- st_join(mapvariable, mpsz,
                           join = st_intersects)
    
    # Check if region parameter is selected
    if (input$region_KD2SFCA == "allregion") {
      # Use full data
      mapvariable_region <- mapvariable
      mrt_region <- mrt
    } else {
      # Filter data for selected region
      mrt_region <- mrt %>% 
        filter(REGION_N == input$region_KD2SFCA)
      mapvariable_region <- mapvariable %>% 
        filter(REGION_N == input$region_KD2SFCA)
    }
    
    boxplot <- st_join(mrt_mapvariable_KD2SFCA, mpsz,
                       join = st_intersects)
    
    tm_shape(mrt_region,
             bbox = st_bbox(mrt_mapvariable_KD2SFCA)) +
      tm_bubbles(col = "accKD2SFCA",
                 n = 10,
                 style = "quantile",
                 size = 0.2,
                 border.col = "black",
                 border.lwd = 1) +
      tm_view(set.zoom.limits = c(11,14),
              view.legend.position = c("right", "bottom")) +
      tm_shape(mapvariable_region) +
      tm_symbols(size = 0.3, col = input$colour_KD2SFCA) +
      tm_shape(mrt_region) +
      tm_dots(alph=0.1, size=0.1)
  })
  
  output$mapPlot_KD2SFCA3 <- renderPlot({
    
    mrt <- mrt %>%
      mutate(demand = input$userdemand_KD2SFCA)
    
    mapvariable <- get(input$mapvariable_KD2SFCA)
    mapvariable <- mapvariable %>%
      mutate(capacity = input$usercapacity_KD2SFCA)
    
    
    mrt_coord <- st_centroid(mrt) %>% 
      st_coordinates()
    
    mapvariable_coord <- st_coordinates(mapvariable)
    
    mrt_mapvariable <- SpatialAcc::distance(mrt_coord,
                                            mapvariable_coord,
                                            type = "euclidean")
    
    mrt_mapvariable_km <- as.matrix(mrt_mapvariable/1000)
    
    acc_KD2SFCA <- data.frame(ac(mrt$demand,
                                mapvariable$capacity,
                                mrt_mapvariable_km,
                                d0 = 50,
                                power = 2,
                                family = "KD2SFCA"))
    
    colnames(acc_KD2SFCA) <- "accKD2SFCA"
    acc_KD2SFCA <- as_tibble(acc_KD2SFCA)
    mrt_mapvariable_KD2SFCA <- bind_cols(mrt, acc_KD2SFCA)
    
    # Joining of MPSZ
    mrt <- st_join(mrt_mapvariable_KD2SFCA, mpsz,
                   join = st_intersects)
    mapvariable <- st_join(mapvariable, mpsz,
                           join = st_intersects)
    
    # Check if region parameter is selected
    if (input$region_KD2SFCA == "allregion") {
      # Use full data
      mapvariable_region <- mapvariable
      mrt_region <- mrt
    } else {
      # Filter data for selected region
      mrt_region <- mrt %>% 
        filter(REGION_N == input$region_KD2SFCA)
      mapvariable_region <- mapvariable %>% 
        filter(REGION_N == input$region_KD2SFCA)
    }
    
    boxplot <- st_join(mrt_mapvariable_KD2SFCA, mpsz,
                       join = st_intersects)
    
    ggplot(data = boxplot, 
           aes(y = log(accKD2SFCA), 
               x = REGION_N)) +
      geom_boxplot() +
      stat_summary(fun.y = "mean", 
                   geom = "point", 
                   colour = "red", 
                   size = 2)
  })
  
  # SAM
  
  output$mapPlot_SAM1 <- renderTmap({
    
    hexagon <- hexagon %>%
      dplyr::select(fid) %>%
      mutate(demand = input$userdemand_SAM)
    
    mapvariable <- get(input$mapvariable_SAM)
    mapvariable <- mapvariable %>%
      mutate(capacity = input$usercapacity_SAM)
    
    
    hexagon_coord <- st_centroid(hexagon) %>% 
      st_coordinates()
    
    mapvariable_coord <- st_coordinates(mapvariable)
    
    hex_mapvariable <- SpatialAcc::distance(hexagon_coord,
                                            mapvariable_coord,
                                            type = "euclidean")
    
    hex_mapvariable_km <- as.matrix(hex_mapvariable/1000)
    
    acc_SAM <- data.frame(ac(hexagon$demand,
                                 mapvariable$capacity,
                                 hex_mapvariable_km,
                                 d0 = 50,
                                 power = 2,
                                 family = "SAM"))
    
    colnames(acc_SAM) <- "accSAM"
    acc_SAM <- as_tibble(acc_SAM)
    hex_mapvariable_SAM <- bind_cols(hexagon, acc_SAM)
    
    # Joining of MPSZ
    mrt <- st_join(mrt, mpsz,
                   join = st_intersects)
    mapvariable <- st_join(mapvariable, mpsz,
                           join = st_intersects)
    hexagon <- st_join(hex_mapvariable_SAM, mpsz,
                       join = st_intersects)
    
    # Check if region parameter is selected
    if (input$region_SAM == "allregion") {
      # Use full data
      hexagon_region <- hexagon
      mapvariable_region <- mapvariable
      mrt_region <- mrt
    } else {
      # Filter data for selected region
      hexagon_region <- hexagon %>% 
        filter(REGION_N == input$region_SAM)
      mrt_region <- mrt %>% 
        filter(REGION_N == input$region_SAM)
      mapvariable_region <- mapvariable %>% 
        filter(REGION_N == input$region_SAM)
    }
    
    tm_shape(hexagon_region,
             bbox = st_bbox(hex_mapvariable_SAM)) + 
      tm_fill(col = "accSAM",
              n = 10,
              style = "quantile",
              border.col = "black",
              border.lwd = 1) +
      tm_shape(mrt_region) +
      tm_dots(alph=0.7, size=0.07)+
      tm_shape(mapvariable_region) +
      tm_symbols(size = 0.3, col = input$colour_SAM) +
      tm_view(set.zoom.limits = c(11,14),
              view.legend.position = c("right", "bottom"))
  })
  
  output$mapPlot_SAM2 <- renderTmap({
    
    mrt <- mrt %>%
      mutate(demand = input$userdemand_SAM)
    
    mapvariable <- get(input$mapvariable_SAM)
    mapvariable <- mapvariable %>%
      mutate(capacity = input$usercapacity_SAM)
    
    
    mrt_coord <- st_centroid(mrt) %>% 
      st_coordinates()
    
    mapvariable_coord <- st_coordinates(mapvariable)
    
    mrt_mapvariable <- SpatialAcc::distance(mrt_coord,
                                            mapvariable_coord,
                                            type = "euclidean")
    
    mrt_mapvariable_km <- as.matrix(mrt_mapvariable/1000)
    
    acc_SAM <- data.frame(ac(mrt$demand,
                                 mapvariable$capacity,
                                 mrt_mapvariable_km,
                                 d0 = 50,
                                 power = 2,
                                 family = "SAM"))
    
    colnames(acc_SAM) <- "accSAM"
    acc_SAM <- as_tibble(acc_SAM)
    mrt_mapvariable_SAM <- bind_cols(mrt, acc_SAM)
    
    # Joining of MPSZ
    mrt <- st_join(mrt_mapvariable_SAM, mpsz,
                   join = st_intersects)
    mapvariable <- st_join(mapvariable, mpsz,
                           join = st_intersects)
    
    # Check if region parameter is selected
    if (input$region_SAM == "allregion") {
      # Use full data
      mapvariable_region <- mapvariable
      mrt_region <- mrt
    } else {
      # Filter data for selected region
      mrt_region <- mrt %>% 
        filter(REGION_N == input$region_SAM)
      mapvariable_region <- mapvariable %>% 
        filter(REGION_N == input$region_SAM)
    }
    
    boxplot <- st_join(mrt_mapvariable_SAM, mpsz,
                       join = st_intersects)
    
    tm_shape(mrt_region,
             bbox = st_bbox(mrt_mapvariable_SAM)) +
      tm_bubbles(col = "accSAM",
                 n = 10,
                 style = "quantile",
                 size = 0.2,
                 border.col = "black",
                 border.lwd = 1) +
      tm_view(set.zoom.limits = c(11,14),
              view.legend.position = c("right", "bottom")) +
      tm_shape(mapvariable_region) +
      tm_symbols(size = 0.3, col = input$colour_SAM) +
      tm_shape(mrt_region) +
      tm_dots(alph=0.1, size=0.1)
  })
  
  output$mapPlot_SAM3 <- renderPlot({
    
    mrt <- mrt %>%
      mutate(demand = input$userdemand_SAM)
    
    mapvariable <- get(input$mapvariable_SAM)
    mapvariable <- mapvariable %>%
      mutate(capacity = input$usercapacity_SAM)
    
    
    mrt_coord <- st_centroid(mrt) %>% 
      st_coordinates()
    
    mapvariable_coord <- st_coordinates(mapvariable)
    
    mrt_mapvariable <- SpatialAcc::distance(mrt_coord,
                                            mapvariable_coord,
                                            type = "euclidean")
    
    mrt_mapvariable_km <- as.matrix(mrt_mapvariable/1000)
    
    acc_SAM <- data.frame(ac(mrt$demand,
                                 mapvariable$capacity,
                                 mrt_mapvariable_km,
                                 d0 = 50,
                                 power = 2,
                                 family = "SAM"))
    
    colnames(acc_SAM) <- "accSAM"
    acc_SAM <- as_tibble(acc_SAM)
    mrt_mapvariable_SAM <- bind_cols(mrt, acc_SAM)
    
    # Joining of MPSZ
    mrt <- st_join(mrt_mapvariable_SAM, mpsz,
                   join = st_intersects)
    mapvariable <- st_join(mapvariable, mpsz,
                           join = st_intersects)
    
    # Check if region parameter is selected
    if (input$region_SAM == "allregion") {
      # Use full data
      mapvariable_region <- mapvariable
      mrt_region <- mrt
    } else {
      # Filter data for selected region
      mrt_region <- mrt %>% 
        filter(REGION_N == input$region_SAM)
      mapvariable_region <- mapvariable %>% 
        filter(REGION_N == input$region_SAM)
    }
    
    boxplot <- st_join(mrt_mapvariable_SAM, mpsz,
                       join = st_intersects)
    
    ggplot(data = boxplot, 
           aes(y = log(accSAM), 
               x = REGION_N)) +
      geom_boxplot() +
      stat_summary(fun.y = "mean", 
                   geom = "point", 
                   colour = "red", 
                   size = 2)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
