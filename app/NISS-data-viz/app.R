library(shiny)
library(tidyverse)
library(maps)
library(RColorBrewer)
library(gt)
library(gridExtra)

## Creating non-reactive data, presets, etc. for the Shiny app ----

# define a function to divide by 100 — need to call this later to create percentages

div100 = function(x) {
  y = x / 100
  return(y)
}

# importing the raw NCES data from Git repository

data = read.csv(
  "https://raw.githubusercontent.com/bbwieland/NISS-contest-viz/main/data/NCES.csv"
) %>%
  mutate(State = gsub('.{1}$', '', State)) %>%
  mutate_if(is.numeric, div100)

# cleaning the data

dataStates = data %>% filter(State != "United States" &
                               State != "District of Columbia")
dataHS = dataStates %>% filter(Degree == "High School")
dataBach = dataStates %>% filter(Degree == "Bachelors")

# getting the data in a format compatible with maps package rendering

dataHSMap = dataHS %>% mutate(State = tolower(State))
dataBachMap = dataBach %>% mutate(State = tolower(State))

# combining the U.S. map data from maps with the education data

usMap = map_data("state")
usMapHS = usMap %>% left_join(dataHSMap, by = c("region" = "State"))
usMapBach = usMap %>% left_join(dataBachMap, by = c("region" = "State"))
usMapTotal = rbind(usMapHS, usMapBach)

# creating a large dataframe; basically, turning race into a categorical variable instead of set of columns
# this will be used later to create large facet-wrapped ggplot objects

usMapWhite = usMapTotal %>% select(long, lat, group, order, region, White, Degree) %>% mutate(Race = "White") %>% rename(Rate = White)
usMapBlack = usMapTotal %>% select(long, lat, group, order, region, Black, Degree) %>% mutate(Race = "Black") %>% rename(Rate = Black)
usMapHispanic = usMapTotal %>% select(long, lat, group, order, region, Hispanic, Degree) %>% mutate(Race = "Hispanic") %>% rename(Rate = Hispanic)
usMapAsian = usMapTotal %>% select(long, lat, group, order, region, Asian, Degree) %>% mutate(Race = "Asian") %>% rename(Rate = Asian)
usMapMultiracial = usMapTotal %>% select(long, lat, group, order, region, Multiracial, Degree) %>% mutate(Race = "Multiracial") %>% rename(Rate = Multiracial)

usMapByRace = rbind(usMapWhite,
                    usMapBlack,
                    usMapHispanic,
                    usMapAsian,
                    usMapMultiracial) %>% filter(is.na(Degree) == F) %>%
  mutate(Degree = factor(Degree, levels = c("High School", "Bachelors")))

# creating a large dataframe; basically, turning race into a categorical variable instead of set of columns
# this will be used later to create large facet-wrapped ggplot objects
# note: this code is very similar to the previous code, but selects Standard Error instead of raw percentages

usMapWhiteSE = usMapTotal %>% select(long, lat, group, order, region, WhiteSE, Degree) %>% mutate(Race = "WhiteSE") %>% rename(Rate = WhiteSE)
usMapBlackSE = usMapTotal %>% select(long, lat, group, order, region, BlackSE, Degree) %>% mutate(Race = "BlackSE") %>% rename(Rate = BlackSE)
usMapHispanicSE = usMapTotal %>% select(long, lat, group, order, region, HispanicSE, Degree) %>% mutate(Race = "HispanicSE") %>% rename(Rate = HispanicSE)
usMapAsianSE = usMapTotal %>% select(long, lat, group, order, region, AsianSE, Degree) %>% mutate(Race = "AsianSE") %>% rename(Rate = AsianSE)
usMapMultiracialSE = usMapTotal %>% select(long, lat, group, order, region, MultiracialSE, Degree) %>% mutate(Race = "MultiracialSE") %>% rename(Rate = MultiracialSE)

usMapByRaceSE = rbind(usMapWhiteSE,
                      usMapBlackSE,
                      usMapHispanicSE,
                      usMapAsianSE,
                      usMapMultiracialSE) %>% filter(is.na(Degree) == F) %>%
  mutate(Degree = factor(Degree, levels = c("High School", "Bachelors")))

# creating the facet-wrapped plot of the percentages

longitudinalPlot = ggplot(usMapByRace) +
  geom_polygon(aes_string(
    x = "long",
    y = "lat",
    group = "group",
    fill = "Rate"
  ),
  color = "black") +
  theme_bw() +
  theme(
    aspect.ratio = 2 / 3,
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 24),
    plot.subtitle = element_text(face = "italic", size = 18),
    legend.title = element_text(vjust = 0.75),
    legend.key.width = unit(2, "cm")
    
  ) +
  scale_fill_distiller(
    palette = "RdYlGn",
    direction = 1,
    na.value = "lightgrey",
    name = "Percent of Students Who Earned Degree",
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  labs(title = "Data for All Degrees and Racial Categories",
       subtitle = "View all categories at once to draw broader conclusions") +
  facet_grid(Degree ~ Race)

# creating the facet-wrapped plot of the standard errors

longitudinalPlotSE = ggplot(usMapByRaceSE) +
  geom_polygon(aes_string(
    x = "long",
    y = "lat",
    group = "group",
    fill = "Rate"
  ),
  color = "black") +
  theme_bw() +
  theme(
    aspect.ratio = 2 / 3,
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 24),
    plot.subtitle = element_text(face = "italic", size = 18),
    legend.title = element_text(vjust = 0.75),
    legend.key.width = unit(2, "cm")
    
  ) +
  scale_fill_distiller(
    palette = "Purples",
    direction = 1,
    na.value = "lightgrey",
    name = "Standard Error of State Observations",
    labels = scales::percent,
    limits = c(0, 0.12)
  ) +
  labs(title = "Standard Errors for All Degrees and Racial Categories",
       subtitle = "View all standard errors at once to draw broader conclusions") +
  facet_grid(Degree ~ Race)

# Programming the actual Shiny application! ----

# Define application UI

ui <- fluidPage(
  titlePanel(title = "NISS Data Visualization (working title)", windowTitle = "NISS Data"),
  mainPanel(
    # use tabsetPanel to create the basic application structure
    tabsetPanel(
      type = "tabs",
      # note: type = "pill" is another potential option to consider when putting finishing design touches on
      
      tabPanel(title = "Interactive Plot",
               sidebarLayout(
                 # this will be the interactive ggplot object for viewing raw percentages
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
                     min = 0,
                     max = 1,
                     value = c(0.5, 1),
                     step = 0.1,
                     animate = F
                   )
                   # note: a lot of thought went into this third sliderInput!
                   # I strongly considered just allowing the scale of the fill to vary. 
                   # However, while that made for better intraracial visualization of how
                   # degree numbers were affected by state, it made for worse interracial
                   # comparisons across multiple groups. However, given the facet wrapped
                   # plots, I'm considering returning to the old scales which were allowed
                   # to vary and just removing this slider altogther. 
                   
                 ),
                 mainPanel(width = 8,
                           fluidRow(column(
                             width = 12, plotOutput("raceplot")
                           )),
                           fluidRow(
                             column(width = 6, gt_output("racetabletop")),
                             column(width = 6, gt_output("racetablebottom"))
                             # maybe find a way to extend the mainPanel to fit the full screen?
                           ))
               )),
      tabPanel(title = "Interactive Uncertainty Plot",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(
                     "raceSE",
                     "Select a racial category to view standard error for:",
                     choices = c("WhiteSE", "BlackSE", "HispanicSE", "AsianSE", "MultiracialSE"),
                     selected = "BlackSE"
                   ),
                   selectInput(
                     "degreetypeSE",
                     "Select a degree type to view standard error for:",
                     choices = c("High School", "Bachelors"),
                     selected = "High School"
                   ),
                   sliderInput(
                     "maprangeSE",
                     "Select a percent range for graph fill:",
                     min = 0,
                     max = 0.15,
                     value = c(0, 0.09),
                     step = 0.03,
                     animate = F
                   )
                   
                 ),
                 mainPanel(plotOutput("raceplotSE"))
               )),
      tabPanel(title = "Overall Plot",
               plotOutput("longitudinalPlot")),
      tabPanel(title = "Overall Uncertainty Plot",
               plotOutput("longitudinalPlotSE"))
    )
  )
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
    head(tabledata(), 5) %>% select(State, Overall,!!(input$race)) %>%
      gt() %>%
      tab_header(title = md("**Five Highest Rates**")) %>%
      fmt_percent(columns = c(2, 3), decimals = 1) %>%
      cols_align(columns = everything(), align = "c") %>%
      opt_row_striping()
    
  })
  
  tablebottom = reactive({
    tail(tabledata(), 5) %>% select(State, Overall,!!(input$race)) %>%
      arrange(get(input$race)) %>%
      gt() %>%
      tab_header(title = md("**Five Lowest Rates**")) %>%
      fmt_percent(columns = c(2, 3), decimals = 1) %>%
      cols_align(columns = everything(), align = "c") %>%
      opt_row_striping()
    
    
  })
  
  # standard error inputs
  
  plotdataSE = reactive({
    usMapTotal %>% filter(Degree == input$degreetypeSE)
  })
  
  tabledataSE = reactive({
    dataStates %>% filter(Degree == input$degreetypeSE &
                            is.na(get(input$raceSE)) == F) %>%
      arrange(-get(input$raceSE))
  })
  
  tabletopSE = reactive({
    head(tabledata(), 5) %>% select(State, Overall,!!(input$raceSE)) %>%
      gt() %>%
      tab_header(title = md("**Five Highest Std. Error**")) %>%
      fmt_percent(columns = c(2, 3), decimals = 1) %>%
      cols_align(columns = everything(), align = "c") %>%
      opt_row_striping()
    
  })
  
  tablebottomSE = reactive({
    tail(tabledata(), 5) %>% select(State, Overall,!!(input$raceSE)) %>%
      arrange(get(input$race)) %>%
      gt() %>%
      tab_header(title = md("**Five Lowest Std. Error**")) %>%
      fmt_percent(columns = c(2, 3), decimals = 1) %>%
      cols_align(columns = everything(), align = "c") %>%
      opt_row_striping()
    
    
  })
  
  # plots
  
  raceSEclean = reactive({
    substr(input$raceSE, 1, nchar(input$raceSE) - 2)
  })
  
  raceplot = reactive({
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
      theme(
        aspect.ratio = 2 / 3,
        plot.title = element_text(face = "bold", size = 24),
        plot.subtitle = element_text(face = "italic", size = 18)
      ) +
      labs(
        title = paste(input$degreetype, "Degrees for", input$race, "Students"),
        subtitle = "Sufficient data not available for states in gray"
      )
  })
  
  raceplotSE = reactive({
    ggplot() +
      geom_polygon(
        data = plotdataSE(),
        aes_string(
          x = "long",
          y = "lat",
          group = "group",
          fill = input$raceSE
        ),
        color = "black"
      ) +
      scale_fill_distiller(
        palette = "Purples",
        direction = 1,
        na.value = "lightgrey",
        name = paste0(
          "Std. Error of\n",
          raceSEclean(),
          " Students \nWho Earned\n",
          input$degreetypeSE,
          " Degree\n"
        ),
        labels = scales::percent,
        limits = input$maprangeSE
      ) +
      theme_void() +
      theme(
        aspect.ratio = 2 / 3,
        plot.title = element_text(face = "bold", size = 24),
        plot.subtitle = element_text(face = "italic", size = 18)
      ) +
      labs(
        title = paste(input$degreetype, "Degrees for", raceSEclean(), "Students"),
        subtitle = "Sufficient data not available for states in gray"
      )
  })
  
  output$raceplot = renderPlot(raceplot())
  
  output$raceplotSE = renderPlot(raceplotSE())
  
  output$racetabletop = render_gt(expr = tabletop())
  output$racetablebottom = render_gt(expr = tablebottom())
  
  output$longitudinalPlot = renderPlot(longitudinalPlot)
  output$longitudinalPlotSE = renderPlot(longitudinalPlotSE)
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
