library(shiny)
library(tidyverse)
library(maps)
library(RColorBrewer)

data = read.csv("data/NCES.csv") %>%
  mutate(State = gsub('.{1}$', '', State))
dataStates = data %>% filter(State != "United States" &
                               State != "District of Columbia")
dataHS = dataStates %>% filter(Degree == "High School")
dataBach = dataStates %>% filter(Degree == "Bachelors")
dataHSMap = dataHS %>% mutate(State = tolower(State))
dataBachMap = dataBach %>% mutate(State = tolower(State))

usMap = map_data("state")
usMapHS = usMap %>% left_join(dataHSMap, by = c("region" = "State"))
usMapBach = usMap %>% left_join(dataBachMap, by = c("region" = "State"))
usMapTotal = rbind(usMapHS, usMapBach)



# Define UI
ui <- fluidPage(
  titlePanel(title = "NISS Data Visualization (working title)", windowTitle = "NISS Data"),
  mainPanel(tabsetPanel(
    type = "tabs",
    tabPanel(title = "Plot",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "race",
                   "Select a racial category to view:",
                   choices = c("White", "Black", "Hispanic", "Asian", "Multiracial"),
                   selected = "Black"
                 ),
                 selectInput(
                   "degreetype",
                   "Select a degree type to view:",
                   choices = c("High School", "Bachelors"),
                   selected = "High School"
                 )
               ),
               mainPanel(
                 width = 8,
                 plotOutput("raceplot")
)
             )
             
    ),
    tabPanel(title = "Data"))
  )
)
# Define server logic
server <- function(input, output) {
  plotdata = reactive({
    usMapTotal %>% filter(Degree == input$degreetype)
  })
  
  output$raceplot = renderPlot(
    ggplot() +
      geom_polygon(
        data = plotdata(),
        aes_string(
          x = "long",
          y = "lat",
          group = "group",
          fill = input$race
        ),
        color = "black"
      ) +
      scale_fill_distiller(
        palette = "RdYlGn",
        direction = 1,
        na.value = "lightgrey",
        name = paste0("Percentage of\n",input$race," Students \nWho Earned\n",input$degreetype," Degree")
      ) +
      theme_void() +
      theme(aspect.ratio = 2/3)
  )
}

# Run the application
shinyApp(ui = ui, server = server)
