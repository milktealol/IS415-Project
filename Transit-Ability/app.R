pacman::p_load(shiny, sf, tidyverse, tmap, DT, sp, dplyr, ggplot2, SpatialAcc, bslib, shinycustomloader)


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
primaryschool <- readRDS("data/model/primaryschool_sf.rds")

loaderGif <- "subway-map-singapore-loading.gif"

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = bs_theme(version = 5, bootswatch = "morph"),
  
  # Application title
  titlePanel("Transit-Ability"),
  
  # Tabs
  tabsetPanel(
    # First tab
    tabPanel("Overview",
             tags$image(style="display: block; margin-left: auto; margin-right: auto;", src = "logo.png", width=300,height=300),
             tags$h1(style="text-align: center;", "IS415 - Geospatial Analytics and Application - Group 5"),
             tags$h4(style="text-align: center;", "Project Transit-Ability helps Singaporeans throughout the city by analyzing 
                      the spatial accessibility using different methods from MRT/LRT stations to 
                      relevant locations.", 
                     tags$a(style="text-align: center;", href="https://is415-group-5.netlify.app/proposal/proposal.html", "Click here to view our project proposal!")),
             tags$h2(style="text-align: center;", "Group Members"),
             HTML("<div style='display: flex; justify-content: center;'>
                  <ul>
                  <li>Daniel Chng - Daniel.Chng.2020@scis.smu.edu.sg</li>
                  <li>Leon Tan - Leon.Tan.2020@scis.smu.edu.sg</li>
                  <li>Zoe Chia - Alanischia.2018x@scis.smu.edu.sg</li>
                  </ul>
                  </div>"),
             
             tags$h2(style="text-align: center;", "Dataset Links"),
             HTML("<div style='display: flex; justify-content: center;'>
                  <ul>
                  <li><a href='https://data.gov.sg/dataset/sample-household-survey-hdb-resident-population-by-geographical-distribution'>Population Density</a></li>
                  <li><a href='https://data.gov.sg/dataset/master-plan-2019-planning-area-boundary-no-sea'>Singapore Zoning</a></li>
                  <li><a href='https://data.gov.sg/dataset/school-directory-and-information'>Primary Schools</a></li>
                  <li><a href='https://datamall.lta.gov.sg/content/datamall/en/static-data.html'>MRT & LRT Stations</a></li>
                  <li><a href='https://www.onemap.gov.sg/main/v2/themes'>Hawker Centres</a></li>
                  <li><a href='https://www.onemap.gov.sg/main/v2/themes'>Childcare Centres </a></li>
                  <li><a href='https://www.onemap.gov.sg/main/v2/themes'>Kindergartens</a></li>
                  <li><a href='https://www.onemap.gov.sg/main/v2/themes'>Parks</a></li>
                  <li><a href='https://www.onemap.gov.sg/main/v2/themes'>Eldercare</a></li>
                  <li><a href='https://www.onemap.gov.sg/main/v2/themes'>Supermarket</a></li>
                  <li><a href='https://www.onemap.gov.sg/main/v2/themes'>Tourist Attractions</a></li>
                  <li><a href='https://en.wikipedia.org/wiki/List_of_shopping_malls_in_Singapore'>Shopping Malls </a></li>
                  </ul>
                  </div>"),
             
             tags$h2(style="text-align: center;", "Useful Links"),
             HTML("<div style='display: flex; justify-content: center; margin-bottom: 80px;'>
                  <ul>
                  <li><a href='https://is415-group-5.netlify.app/'>Project Website</a></li>
                  <li><a href='https://github.com/milktealol/IS415-Project'>Project GitHub</a></li>
                  </ul>
                  </div>")
    ),
    
    # Second tab
    tabPanel("EDA",
             # Sidebar panel
             sidebarLayout(position = "left",
               sidebarPanel(
                 tags$p(HTML("<b>Feel free to change the variables for a better outlook!</b>")),
                 selectInput(
                   inputId = "mapvariable",
                   label = "Places of Interest:",
                   choices = c("Tourist Attraction" = "tourism",
                               "Shopping Malls" = "shopping",
                               "Child Cares" = "childcare",
                               "Elderly Cares" = "eldercare",
                               "Hawker Centres" = "hawker",
                               "Kindergartens" = "kindergarten",
                               "Parks" = "park",
                               "Supermarkets" = "supermarket",
                               "Primary Schools" = "primaryschool"),
                   selected = "tourism"
                 ),
                 submitButton("Apply Changes")
               ),
               # Main panel
               mainPanel(
                 br(),
                 tags$p(HTML("<b>EDA Explained</b>")),
                 tags$p(HTML("Exploratory data analysis (EDA) is used to investigate data sets and summarize their main characteristics, often employing data visualization methods. 
                             It helps determine how best to manipulate data sources to get the answers we need, making it easier 
                             for us to discover patterns, spot anomalies, test a hypothesis, or check assumptions.")),
                 br(),
                 tags$p(HTML("<b><i>Note: Charts might take around 5 - 10 seconds to load. If charts are loading, changing tabs might cause it to crash :(</i></b>")),
                 br(),
                 tags$p(HTML("<b>MRT with Places of Interest</b>")),
                 withLoader(tmapOutput("mapPlot_eda1",
                            width = "100%",
                            height = 400), type="image", 
                            loader="subway-map-singapore-loading.gif"),
                 br(),
                 tags$p(HTML("<b>Analysis</b>")),
                 tags$p("The black dots represents the MRT/LRT stations, and the yellow dots represents the selected places of interest."),
                 br(),
                 tags$p(HTML("<b>MRT x Population Density</b>")),
                 withLoader(tmapOutput("mapPlot_eda2",
                            width = "100%",
                            height = 400), type="image", 
                            loader="subway-map-singapore-loading.gif"),
                 br(),
                 tags$p(HTML("<b>Analysis</b>")),
                 tags$p(style="margin-bottom: 80px;","The population is based off from the 2022 dataset. We use this to have a visual guage on how the population density would look like with the MRT/LRT Stations overlay."),
               ),
             ),
    ),
    
    # Third tab
    tabPanel("Hansen Method",
             # Sidebar panel
             sidebarLayout(position = "left",
               sidebarPanel(
                 width = 4,
                 tags$p(HTML("<b>Feel free to change the variables for a better outlook!</b>")),
                 selectInput(
                   inputId = "mapvariable_Hansen",
                   label = "Places of Interest:",
                   choices = c("Tourist Attraction" = "tourism",
                               "Shopping Malls" = "shopping",
                               "Child Cares" = "childcare",
                               "Elderly Cares" = "eldercare",
                               "Hawker Centres" = "hawker",
                               "Kindergartens" = "kindergarten",
                               "Parks" = "park",
                               "Supermarkets" = "supermarket",
                               "Primary Schools" = "primaryschool"),
                   selected = "tourism"
                 ),
                 numericInput("userdemand", "Demand:", 100, min = 100),
                 HTML("<i>Demand: Represents the population in each area</i>"),
                 numericInput("usercapacity", "Capacity:", 100, min = 100),
                 HTML("<i>Capacity: Represents the number of amenities or services available at each location</i>"),
                 selectInput(
                   inputId = "region",
                   label = "Focus Region:",
                   choices = c("All Region" = "allregion",
                               "Central Region" = "CENTRAL REGION",
                               "West Region" = "WEST REGION",
                               "East Region" = "EAST REGION",
                               "North-East Region" = "NORTH-EAST REGION",
                               "North" = "NORTH REGION")
                 ),
                 selectInput( inputId = "colour",
                              label = "Mapping Colour:",
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
                 selectInput( inputId = "shadeColor",
                              label = "Hexagon Variable Colour:",
                              choices = list("Yellow Orange Brown" = "YlOrBr",
                                             "Yellow Orange Red" = "YlOrRd",
                                             "Purple Blue Green" = "PuBuGn",
                                             "Red Blue" = "RdBu",
                                             "Blue Green" = "BuGn",
                                             "Blue" = "Blues"),
                              selected = "YlOrBr"),
                 submitButton("Apply Changes")
               ),
               # Main panel
               mainPanel(
                 br(),
                 tags$p(HTML("<b>Hanson Explained</b>")),
                 tags$p(HTML("The Hansen method involves using GIS to analyze the spatial relationship between transportation infrastructure and population. 
                             It calculates the travel time between each point in the study area and the nearest transportation facility to create maps that 
                             show the level of accessibility to different types of transportation for each location. This method is useful in identifying 
                             areas that are underserved by transportation infrastructure and evaluating the potential impacts of new transportation investments, 
                             helping planners and policymakers make more informed decisions about transportation investments and ensuring that transportation 
                             services are distributed fairly.")),
                 br(),
                 tags$p(HTML("<b><i>Note: Charts might take around 5 - 10 seconds to load. If charts are loading, changing tabs might cause it to crash :(</i></b>")),
                 br(),
                 tags$p(HTML("<b>Hexagon with Places of Interest</b>")),
                 withLoader(tmapOutput("mapPlot_hansen1", width = "100%", height = 400), type="image", 
                            loader="subway-map-singapore-loading.gif"),
                 br(),
                 tags$p(HTML("<b>Analysis</b>")),
                 tags$p("MRT/LRT which are within the darker hexagon shades, are more accessible to the place of interest. 
                        If a location has high demand but low capacity, it will have a lower accessibility score. 
                        Similarly, a location that has low capacity but nearer to the demand area would have a higher accessibility score."),
                 br(),
                 tags$p(HTML("<b>MRT with Places of Interest</b>")),
                 withLoader(tmapOutput("mapPlot_hansen2", width = "100%", height = 400), type="image", 
                            loader="subway-map-singapore-loading.gif"),
                 br(),
                 tags$p(HTML("<b>Analysis</b>")),
                 tags$p("The darker each MRT/LRT stations is, the more accessible the MRT/LRT is to the the place of interest.
                      If a location has high demand but low capacity, it will have a lower accessibility score. 
                        Similarly, a location that has low capacity but nearer to the demand area would have a higher accessibility score."),
                 br(),
                 tags$p(HTML("<b>MRT - Statistical Graphic Visualisation</b>")),
                 withLoader(plotOutput("mapPlot_hansen3", width = "100%", height = 400), type="image", 
                            loader="subway-map-singapore-loading.gif"),
                 br(),
                 tags$p(HTML("<b>Analysis</b>")),
                 tags$p(style="margin-bottom: 80px;", "We can see that the red dot represents the place of interest and the IQR represents the MRT/LRT Stations.
                    For ever place of interest, there is a MRT/LRT station nearby (Red Dot within the IQR), making it accessible."),
               )
             ),
    ),
    # Fourth tab
    tabPanel("KD2SFCA Method",
             # Sidebar panel
             sidebarLayout(position = "left",
               sidebarPanel(
                 width = 4,
                 tags$p(HTML("<b>Feel free to change the variables for a better outlook!</b>")),
                 selectInput(
                   inputId = "mapvariable_KD2SFCA",
                   label = "Places of Interest:",
                   choices = c("Tourist Attraction" = "tourism",
                               "Shopping Malls" = "shopping",
                               "Child Cares" = "childcare",
                               "Elderly Cares" = "eldercare",
                               "Hawker Centres" = "hawker",
                               "Kindergartens" = "kindergarten",
                               "Parks" = "park",
                               "Supermarkets" = "supermarket",
                               "Primary Schools" = "primaryschool"),
                   selected = "tourism"
                 ),
                 numericInput("userdemand_KD2SFCA", "Demand:", 100, min = 100),
                 HTML("<i>Demand: Represents the population in each area</i>"),
                 numericInput("usercapacity_KD2SFCA", "Capacity:", 100, min = 100),
                 HTML("<i>Capacity: Represents the number of amenities or services available at each location</i>"),
                 selectInput(
                   inputId = "region_KD2SFCA",
                   label = "Focus Region:",
                   choices = c("All Region" = "allregion",
                               "Central Region" = "CENTRAL REGION",
                               "West Region" = "WEST REGION",
                               "East Region" = "EAST REGION",
                               "North-East Region" = "NORTH-EAST REGION",
                               "North" = "NORTH REGION")
                 ),
                 selectInput( inputId = "colour_KD2SFCA",
                              label = "Mapping Colour:",
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
                 selectInput( inputId = "shadeColor",
                              label = "Hexagon Variable Colour:",
                              choices = list("Yellow Orange Brown" = "YlOrBr",
                                             "Yellow Orange Red" = "YlOrRd",
                                             "Purple Blue Green" = "PuBuGn",
                                             "Red Blue" = "RdBu",
                                             "Blue Green" = "BuGn",
                                             "Blue" = "Blues"),
                              selected = "YlOrBr"),
                 submitButton("Apply Changes")
               ),
               # Main panel
               mainPanel(
                 br(),
                 tags$p(HTML("<b>KD2SFCA3 Explained</b>")),
                 tags$p(HTML("The KD2SFCA method involves using GIS to analyze the spatial relationship between train stations and population. 
                             The method calculates the number of train stations within a certain travel time of each location, 
                             taking into account both supply and demand factors. This results in a series of maps that show the 
                             level of mrt stations’ accessibility for each location in the study area. This method is useful in 
                             identifying areas with poor accessibility to stations.")),
                 br(),
                 tags$p(HTML("<b><i>Note: Charts might take around 5 - 10 seconds to load. If charts are loading, changing tabs might cause it to crash :(</i></b>")),
                 br(),
                 tags$p(HTML("<b>Hexagon with Places of Interest</b>")),
                 withLoader(tmapOutput("mapPlot_KD2SFCA1", width = "100%", height = 400), type="image", 
                            loader="subway-map-singapore-loading.gif"),
                 br(),
                 tags$p(HTML("<b>Analysis</b>")),
                 tags$p("MRT/LRT which are within the darker hexagon shades, are more accessible to the place of interest.
                        If a location has high demand but low capacity, it will have a lower accessibility score. 
                        Similarly, a location that has low capacity but nearer to the demand area would have a higher accessibility score."),
                 br(),
                 tags$p(HTML("<b>MRT with Places of Interest</b>")),
                 withLoader(tmapOutput("mapPlot_KD2SFCA2", width = "100%", height = 400), type="image", 
                            loader="subway-map-singapore-loading.gif"),
                 tags$p(HTML("<b>Analysis</b>")),
                 tags$p("The darker each MRT/LRT stations is, the more accessible the MRT/LRT is to the the place of interest.
                      If a location has high demand but low capacity, it will have a lower accessibility score. 
                        Similarly, a location that has low capacity but nearer to the demand area would have a higher accessibility score."),
                 br(),
                 tags$p(HTML("<b>MRT - Statistical Graphic Visualisation</b>")),
                 withLoader(plotOutput("mapPlot_KD2SFCA3", width = "100%", height = 400), type="image", 
                            loader="subway-map-singapore-loading.gif"),
                 br(),
                 tags$p(HTML("<b>Analysis</b>")),
                 tags$p(style="margin-bottom: 80px;", "We can see that the red dot represents the place of interest and the IQR represents the MRT/LRT Stations.
                    For ever place of interest, there is a MRT/LRT station nearby (Red Dot within the IQR), making it accessible."),
               )
             ),
    ),
    # Fifth tab
    tabPanel("SAM Method",
             # Sidebar panel
             sidebarLayout(position = "left",
               sidebarPanel(
                 width = 4,
                 tags$p(HTML("<b>Feel free to change the variables for a better outlook!</b>")),
                 selectInput(
                   inputId = "mapvariable_SAM",
                   label = "Places of Interest:",
                   choices = c("Tourist Attraction" = "tourism",
                               "Shopping Malls" = "shopping",
                               "Child Cares" = "childcare",
                               "Elderly Cares" = "eldercare",
                               "Hawker Centres" = "hawker",
                               "Kindergartens" = "kindergarten",
                               "Parks" = "park",
                               "Supermarkets" = "supermarket",
                               "Primary Schools" = "primaryschool"),
                   selected = "tourism"
                 ),
                 numericInput("userdemand_SAM", "Demand:", 100, min = 100),
                 HTML("<i>Demand: Represents the population in each area</i>"),
                 numericInput("usercapacity_SAM", "Capacity:", 100, min = 100),
                 HTML("<i>Capacity: Represents the number of amenities or services available at each location</i>"),
                 selectInput(
                   inputId = "region_SAM",
                   label = "Focus Region:",
                   choices = c("All Region" = "allregion",
                               "Central Region" = "CENTRAL REGION",
                               "West Region" = "WEST REGION",
                               "East Region" = "EAST REGION",
                               "North-East Region" = "NORTH-EAST REGION",
                               "North" = "NORTH REGION")
                 ),
                 selectInput( inputId = "colour_SAM",
                              label = "Mapping Colour:",
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
                 selectInput( inputId = "shadeColor",
                              label = "Hexagon Variable Colour:",
                              choices = list("Yellow Orange Brown" = "YlOrBr",
                                             "Yellow Orange Red" = "YlOrRd",
                                             "Purple Blue Green" = "PuBuGn",
                                             "Red Blue" = "RdBu",
                                             "Blue Green" = "BuGn",
                                             "Blue" = "Blues"),
                              selected = "YlOrBr"),
                 submitButton("Apply Changes")
               ),
               # Main panel
               mainPanel(
                 br(),
                 tags$p(HTML("<b>SAM Explained</b>")),
                 tags$p(HTML("The Spatial Accessibility Measure (SAM) involves using GIS to analyze the spatial relationship 
                             between a service facility and a population. The method calculates the level of accessibility to 
                             the service facility for each location in the study area, taking into account factors such as 
                             travel time and distance, mode of transportation, and the size of the population served. The 
                             result is a series of maps that show the level of accessibility for each location, which can 
                             be used to identify areas that are underserved by the service facility. This method is useful 
                             in informing policy and planning decisions related to service provision, and can be applied 
                             to a range of service types, including healthcare, education, and retail.")),
                 br(),
                 tags$p(HTML("<b><i>Note: Charts might take around 5 - 10 seconds to load. If charts are loading, changing tabs might cause it to crash :(</i></b>")),
                 br(),
                 tags$p(HTML("<b>Hexagon with Places of Interest</b>")),
                 withLoader(tmapOutput("mapPlot_SAM1", width = "100%", height = 400), type="image", 
                 loader="subway-map-singapore-loading.gif"),
                 br(),
                 tags$p(HTML("<b>Analysis</b>")),
                 tags$p("MRT/LRT which are within the darker hexagon shades, are more accessible to the place of interest.
                        If a location has high demand but low capacity, it will have a lower accessibility score. 
                        Similarly, a location that has low capacity but nearer to the demand area would have a higher accessibility score."),
                 br(),
                 tags$p(HTML("<b>MRT with Places of Interest</b>")),
                 withLoader(tmapOutput("mapPlot_SAM2", width = "100%", height = 400), type="image", 
               loader="subway-map-singapore-loading.gif"),
                 br(),
               tags$p(HTML("<b>Analysis</b>")),
               tags$p("The darker each MRT/LRT stations is, the more accessible the MRT/LRT is to the the place of interest.
                      If a location has high demand but low capacity, it will have a lower accessibility score. 
                        Similarly, a location that has low capacity but nearer to the demand area would have a higher accessibility score."),
                 br(),
                 tags$p(HTML("<b>MRT - Statistical Graphic Visualisation</b>")),
                 withLoader(plotOutput("mapPlot_SAM3", width = "100%", height = 400), type="image", 
             loader="subway-map-singapore-loading.gif"),
                 br(),
             tags$p(HTML("<b>Analysis</b>")),
             tags$p(style="margin-bottom: 80px;", "We can see that the red dot represents the place of interest and the IQR represents the MRT/LRT Stations.
                    For ever place of interest, there is a MRT/LRT station nearby (Red Dot within the IQR), making it accessible."),
               )
             ),
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
              palette = input$shadeColor,
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
                 palette = input$shadeColor,
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
              palette = input$shadeColor,
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
                 palette = input$shadeColor,
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
              palette = input$shadeColor,
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
                 palette = input$shadeColor,
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
