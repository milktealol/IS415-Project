pacman::p_load(shiny, sf, tidyverse, tmap, DT)

mrt <- readRDS("data/model/mrt_sf.rds")
mrt_rail <- readRDS("data/model/rail_sf.rds")
tourism <- readRDS("data/model/tourism_sf.rds")
shopping <- readRDS("data/model/shopping_sf.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("EDA - Visualisation"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(
              inputId = "variable",
              label = "Mapping Variable:",
              choices = c("Tourist Attraction" = "tourism",
                          "Shopping Malls" = "shopping"),
              selected = "tourism"
            ),
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tmapOutput("mapPlot",
                     width = "100%",
                     height = 400),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$mapPlot <- renderTmap({
    tmap_options(check.and.fix = TRUE) +
      tm_shape(mrt_rail) +
      tm_lines(lty = "solid", scale = 1) +
      tm_shape(mrt) +
      tm_dots(alph=0.5, size=0.07) +
      tm_shape(get(input$variable)) +
      tm_dots(size = 0.1, col = 'yellow') +
      tm_view(set.zoom.limits = c(11,14))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
