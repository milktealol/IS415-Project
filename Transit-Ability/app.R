pacman::p_load(shiny, sf, tidyverse, tmap, DT, sp, dplyr, ggplot2)

mrt <- readRDS("data/model/mrt_sf.rds")
mrt_rail <- readRDS("data/model/rail_sf.rds")
tourism <- readRDS("data/model/tourism_sf.rds")
shopping <- readRDS("data/model/shopping_sf.rds")
mpsz <- readRDS("data/model/mpsz.rds")
mpsz_popdata2022 <- readRDS("data/model/mpsz_popdata2022.rds")

#Hansen
hex_tour_Hansen <- readRDS("data/hansen/hex_tour_Hansen.rds")
mrt_tour_Hansen <- readRDS("data/hansen/mrt_tour_Hansen.rds")
mrt_tour_boxplot <- readRDS("data/hansen/mrt_tour_boxplot.rds")

hex_shop_Hansen <- readRDS("data/hansen/hex_shop_Hansen.rds")
mrt_shop_Hansen <- readRDS("data/hansen/mrt_shop_Hansen.rds")
mrt_shop_boxplot <- readRDS("data/hansen/mrt_shop_boxplot.rds")

#KD2SFCA
hex_tour_KD2SFCA <- readRDS("data/kd2sfca/hex_tour_KD2SFCA.rds")
mrt_tour_KD2SFCA <- readRDS("data/kd2sfca/mrt_tour_KD2SFCA.rds")
mrt_tour_boxplot_kd2sfca <- readRDS("data/kd2sfca/mrt_tour_boxplot_kd2sfca.rds")

hex_shop_KD2SFCA <- readRDS("data/kd2sfca/hex_shop_KD2SFCA.rds")
mrt_shop_KD2SFCA <- readRDS("data/kd2sfca/mrt_shop_KD2SFCA.rds")
mrt_shop_boxplot_kd2sfca <- readRDS("data/kd2sfca/mrt_shop_boxplot_kd2sfca.rds")

#SAM
hex_tour_SAM <- readRDS("data/sam/hex_tour_SAM.rds")
mrt_tour_SAM <- readRDS("data/sam/mrt_tour_SAM.rds")
mrt_tour_boxplot_sam <- readRDS("data/sam/mrt_tour_boxplot_sam.rds")

hex_shop_SAM <- readRDS("data/sam/hex_shop_SAM.rds")
mrt_shop_SAM <- readRDS("data/sam/mrt_shop_SAM.rds")
mrt_shop_boxplot_sam <- readRDS("data/sam/mrt_shop_boxplot_sam.rds")

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
                   inputId = "variable1",
                   label = "Mapping Variable:",
                   choices = c("Tourist Attraction" = "tourism",
                               "Shopping Malls" = "shopping"),
                   selected = "tourism"
                 ),
                 tags$p("Change the Mapping Variable above to view the different 
                        locations with the MRT & LRT lines on the Singapore map."),
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
                 plotOutput("mapPlot_eda2",
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
                   inputId = "variable2",
                   label = "Mapping Variable:",
                   choices = c("Tourist Attraction" = "hex_tour_Hansen",
                               "Shopping Malls" = "hex_shop_Hansen"),
                   selected = "tourism"
                 ),
                 tags$p("Change the Mapping Variable above to view the different 
           locations with the MRT & LRT lines on the Singapore map.")
               ),
               # Main panel
               mainPanel(
                 tags$p(HTML("<b>Hexagon x Mapping Variable</b>")),
                 plotOutput("mapPlot_hansen1", width = "100%", height = 400),
                 br(),
                 tags$p(HTML("<b>Please give a bit of time for the map to load</b>")),
                 
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
                   inputId = "variable3",
                   label = "Mapping Variable:",
                   choices = c("Tourist Attraction" = "hex_tour_KD2SFCA",
                               "Shopping Malls" = "hex_shop_KD2SFCA"),
                   selected = "tourism"
                 ),
                 tags$p("Change the Mapping Variable above to view the different 
           locations with the MRT & LRT lines on the Singapore map.")
               ),
               # Main panel
               mainPanel(
                 tags$p(HTML("<b>Hexagon x Mapping Variable</b>")),
                 plotOutput("mapPlot_kd2sfca1", width = "100%", height = 400),
                 br(),
                 tags$p(HTML("<b>Please give a bit of time for the map to load</b>")),
                 
                 tags$p(HTML("<b>MRT x Mapping Variable</b>")),
                 tmapOutput("mapPlot_kd2sfca2", width = "100%", height = 400),
                 br(),
                 tags$p(HTML("<b>Please give a bit of time for the map to load</b>")),
                 
                 br(),
                 tags$p(HTML("<b>MRT - Statistical Graphic Visualisation</b>")),
                 plotOutput("mapPlot_kd2sfcan3", width = "100%", height = 400),
                 br(),
                 tags$p(HTML("<b>Please give a bit of time for the chart to load</b>"))
               )
      ),
    
      # Fifth tab
      tabPanel("Spatial Accessibility Measure (SAM) Method",
               # Sidebar panel
               sidebarPanel(
                 width = 3,
                 selectInput(
                   inputId = "variable4",
                   label = "Mapping Variable:",
                   choices = c("Tourist Attraction" = "hex_tour_SAM",
                               "Shopping Malls" = "hex_shop_SAM"),
                   selected = "tourism"
                 ),
                 tags$p("Change the Mapping Variable above to view the different 
           locations with the MRT & LRT lines on the Singapore map.")
               ),
               # Main panel
               mainPanel(
                 tags$p(HTML("<b>Hexagon x Mapping Variable</b>")),
                 plotOutput("mapPlot_sam1", width = "100%", height = 400),
                 br(),
                 tags$p(HTML("<b>Please give a bit of time for the map to load</b>")),
                 
                 tags$p(HTML("<b>MRT x Mapping Variable</b>")),
                 tmapOutput("mapPlot_sam2", width = "100%", height = 400),
                 br(),
                 tags$p(HTML("<b>Please give a bit of time for the map to load</b>")),
                 
                 br(),
                 tags$p(HTML("<b>MRT - Statistical Graphic Visualisation</b>")),
                 plotOutput("mapPlot_sam3", width = "100%", height = 400),
                 br(),
                 tags$p(HTML("<b>Please give a bit of time for the chart to load</b>"))
               )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # EDA
  output$mapPlot_eda1 <- renderTmap({
    tmap_options(check.and.fix = TRUE) +
      tm_shape(mrt_rail) +
      tm_lines(lty = "solid", scale = 1) +
      tm_shape(mrt) +
      tm_dots(alph=0.5, size=0.07) +
      tm_shape(get(input$variable1)) +
      tm_dots(size = 0.1, col = 'yellow') +
      tm_view(set.zoom.limits = c(11,14),
              view.legend.position = c("right", "bottom"))
  })
  
  output$mapPlot_eda2 <- renderPlot({
    tm_shape(mpsz_popdata2022) +
      tm_fill("DEPENDENCY",
              breaks = c(0, 0.60, 0.70, 0.80, 0.90, 1.00)) +
      tm_borders(lwd = 0.1,  alpha = 1) +
      tm_shape(mrt_rail) +
      tm_lines(lty = "solid",
               scale = 1) +
      tm_shape(mrt) +
      tm_dots(alph=0.5, size=0.07)+
      tm_view(set.zoom.limits = c(11,14))
  })
  
  #Hansen
  hansen1 <- reactive({
    switch(input$variable2,
           "hex_tour_Hansen" = tourism,
           "hex_shop_Hansen" = shopping)
  })
  
  hansen2 <- reactive({
    switch(input$variable2,
           "hex_tour_Hansen" = mrt_tour_Hansen,
           "hex_shop_Hansen" = mrt_shop_Hansen)
  })
  
  hansen3 <- reactive({
    switch(input$variable2,
           "hex_tour_Hansen" = mrt_tour_boxplot,
           "hex_shop_Hansen" = mrt_shop_boxplot)
  })
  
  output$mapPlot_hansen1 <- renderPlot({
    tm_shape(get(input$variable2)) + 
      tm_fill(col = "accHansen",
              n = 10,
              style = "quantile",
              border.col = "black",
              border.lwd = 1) +
      tm_shape(mrt_rail) +
      tm_lines(lty = "solid",
               scale = 1) +
      tm_shape(mrt) +
      tm_dots(alph=0.5, size=0.07)+
      tm_shape(hansen1()) +
        tm_symbols(size = 0.1) +
      tm_view(set.zoom.limits = c(11,14),
              view.legend.position = c("right", "bottom"))
  })
  
  output$mapPlot_hansen2 <- renderTmap({
    tm_shape(hansen1()) +
      tm_symbols(size = 0.5) +
      tm_shape(mrt_rail) +
      tm_lines(lty = "solid",
               scale = 1) +
      tm_shape(mrt) +
      tm_dots(alph=0.1, size=0.1)+
      tm_shape(hansen2()) +
      tm_bubbles(col = "accHansen",
                 n = 5,
                 style = "quantile",
                 size = 0.2,
                 border.col = "black",
                 border.lwd = 1) +
      tm_view(set.zoom.limits = c(11,14),
              view.legend.position = c("right", "bottom"))
  })
  
  tmap_mode("plot")
  
  output$mapPlot_hansen3 <- renderPlot({
    ggplot(data = hansen3(), 
           aes(y = log(accHansen), 
               x = REGION_N)) +
      geom_boxplot() +
      stat_summary(fun.y = "mean", 
                   geom = "point", 
                   colour = "red", 
                   size = 2)
  })
  
  
  
  #KD2SFCA
  kd2sfca1 <- reactive({
    switch(input$variable3,
           "hex_tour_KD2SFCA" = tourism,
           "hex_shop_KD2SFCA" = shopping)
  })
  
  kd2sfca2 <- reactive({
    switch(input$variable3,
           "hex_tour_KD2SFCA" = mrt_tour_KD2SFCA,
           "hex_shop_KD2SFCA" = mrt_shop_KD2SFCA)
  })
  
  kd2sfca3 <- reactive({
    switch(input$variable3,
           "hex_tour_KD2SFCA" = mrt_tour_boxplot_kd2sfca,
           "hex_shop_KD2SFCA" = mrt_shop_boxplot_kd2sfca)
  })
  
  output$mapPlot_kd2sfca1 <- renderPlot({
    tm_shape(get(input$variable3)) + 
      tm_fill(col = "accKD2SFCA",
              n = 10,
              style = "quantile",
              border.col = "black",
              border.lwd = 1) +
      tm_shape(mrt_rail) +
      tm_lines(lty = "solid",
               scale = 1) +
      tm_shape(mrt) +
      tm_dots(alph=0.5, size=0.07)+
      tm_shape(kd2sfca1()) +
      tm_symbols(size = 0.1) +
      tm_view(set.zoom.limits = c(11,14),
              view.legend.position = c("right", "bottom"))
  })
  
  output$mapPlot_kd2sfca2 <- renderTmap({
    tm_shape(kd2sfca1()) +
      tm_symbols(size = 0.5) +
      tm_shape(mrt_rail) +
      tm_lines(lty = "solid",
               scale = 1) +
      tm_shape(mrt) +
      tm_dots(alph=0.1, size=0.1)+
      tm_shape(kd2sfca2()) +
      tm_bubbles(col = "accKD2SFCA",
                 n = 5,
                 style = "quantile",
                 size = 0.2,
                 border.col = "black",
                 border.lwd = 1) +
      tm_view(set.zoom.limits = c(11,14),
              view.legend.position = c("right", "bottom"))
  })
  
  tmap_mode("plot")
  
  output$mapPlot_kd2sfca3 <- renderPlot({
    ggplot(data = kd2sfca3(), 
           aes(y = log(accKD2SFCA), 
               x = REGION_N)) +
      geom_boxplot() +
      stat_summary(fun.y = "mean", 
                   geom = "point", 
                   colour = "red", 
                   size = 2) +
      theme(plot.width = unit(8, "cm"))
  })
  
  
  #SAM
  sam1 <- reactive({
    switch(input$variable4,
           "hex_tour_SAM" = tourism,
           "hex_shop_SAM" = shopping)
  })
  
  sam2 <- reactive({
    switch(input$variable4,
           "hex_tour_SAM" = mrt_tour_SAM,
           "hex_shop_SAM" = mrt_shop_SAM)
  })
  
  sam3 <- reactive({
    switch(input$variable4,
           "hex_tour_SAM" = mrt_tour_boxplot_sam,
           "hex_shop_SAM" = mrt_shop_boxplot_sam)
  })
  
  output$mapPlot_sam1 <- renderPlot({
    tm_shape(get(input$variable4)) + 
      tm_fill(col = "accSAM",
              n = 10,
              style = "quantile",
              border.col = "black",
              border.lwd = 1) +
      tm_shape(mrt_rail) +
      tm_lines(lty = "solid",
               scale = 1) +
      tm_shape(mrt) +
      tm_dots(alph=0.5, size=0.07)+
      tm_shape(sam1()) +
      tm_symbols(size = 0.1) +
      tm_view(set.zoom.limits = c(11,14),
              view.legend.position = c("right", "bottom"))
  })
  
  output$mapPlot_sam2 <- renderTmap({
    tm_shape(sam1()) +
      tm_symbols(size = 0.5) +
      tm_shape(mrt_rail) +
      tm_lines(lty = "solid",
               scale = 1) +
      tm_shape(mrt) +
      tm_dots(alph=0.1, size=0.1)+
      tm_shape(sam2()) +
      tm_bubbles(col = "accSAM",
                 n = 5,
                 style = "quantile",
                 size = 0.2,
                 border.col = "black",
                 border.lwd = 1) +
      tm_view(set.zoom.limits = c(11,14),
              view.legend.position = c("right", "bottom"))
  })
  
  tmap_mode("plot")
  
  output$mapPlot_sam3 <- renderPlot({
    ggplot(data = sam3(), 
           aes(y = log(accSAM), 
               x = REGION_N)) +
      geom_boxplot() +
      stat_summary(fun.y = "mean", 
                   geom = "point", 
                   colour = "red", 
                   size = 2) +
      theme(plot.width = unit(8, "cm"))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
