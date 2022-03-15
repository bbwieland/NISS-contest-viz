library(shiny)
library(tidyverse)
library(maps)
library(RColorBrewer)
library(gt)

div100 = function(x){
  y = x/100
  return(y)
}

data = read.csv(
  "https://raw.githubusercontent.com/bbwieland/NISS-contest-viz/main/data/NCES.csv"
) %>%
  mutate(State = gsub('.{1}$', '', State)) %>%
  mutate_if(is.numeric,div100)
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
                 ),
                 sliderInput(
                   "maprange",
                   "Select a percent range for graph fill:",
                   min = 0, max = 1,
                   value = c(0.5,1))
                 
               ),
               mainPanel(width = 8,
                         fluidRow(column(
                           width = 12, plotOutput("raceplot")
                         )),
                         fluidRow(
                           column(width = 6, gt_output("racetabletop")),
                           column(width = 6, gt_output("racetablebottom"))
                         ))
             ))
    
  ),
  tabPanel(title = "Data"))
)

# Define server logic
server <- function(input, output) {
  plotdata = reactive({
    usMapTotal %>% filter(Degree == input$degreetype)
  })
  
  tabledata = reactive({
    dataStates %>% filter(Degree == input$degreetype &
                            is.na(get(input$race)) == F) %>%
      arrange(-get(input$race))
  })
  
  tabletop = reactive({
    head(tabledata(), 5) %>% select(State, Overall, !!(input$race)) %>%
      gt() %>%
      tab_header(title = md("**Five Highest Rates**")) %>%
      fmt_percent(columns = c(2,3),decimals = 1) %>%
      cols_align(columns = everything(),align = "c") %>%
      opt_row_striping()
    
  })
  
  tablebottom = reactive({
    tail(tabledata(), 5) %>% select(State, Overall, !!(input$race)) %>%
      arrange(get(input$race)) %>%
      gt() %>%
      tab_header(title = md("**Five Lowest Rates**")) %>%
      fmt_percent(columns = c(2,3),decimals = 1) %>%
      cols_align(columns = everything(),align = "c") %>%
      opt_row_striping()
    
    
  })
  

  raceplot = reactive({ggplot() +
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
      name = paste0(
        "Percentage of\n",
        input$race,
        " Students \nWho Earned\n",
        input$degreetype,
        " Degree\n"
      ),
      limits = input$maprange,
      labels = scales::percent
    ) +
    theme_void() +
    theme(aspect.ratio = 2 / 3,
          plot.title = element_text(face = "bold",size = 20),
          plot.subtitle = element_text(face = "italic",size = 16)) +
    labs(
      title = paste(input$degreetype,"Degrees for",input$race,"Students"),
      subtitle = "Sufficient data not available for states in gray"
    )})
  
  output$raceplot = renderPlot(
    raceplot()
  )
  
  output$racetabletop = render_gt(expr = tabletop())
  output$racetablebottom = render_gt(expr = tablebottom())
  

}

# Run the application
shinyApp(ui = ui, server = server)
