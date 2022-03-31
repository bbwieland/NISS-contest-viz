library(shiny)
library(tidyverse)
library(geojsonio)
library(leaflet)
library(RColorBrewer)


## Creating non-reactive data, presets, etc. for the Shiny app ----

# define a function to divide by 100 — need to call this later to create percentages

# importing the raw NCES data from Git repository

div100 = function(x) {
  y = x / 100
  return(y)
}
data = read.csv(
  "https://raw.githubusercontent.com/bbwieland/NISS-contest-viz/main/data/NCES.csv"
) %>%
  mutate(State = gsub('.{1}$', '', State))

# cleaning the data


dataStates = data %>% filter(State != "United States" &
                               State != "District of Columbia") %>%
  mutate(Degree = gsub(" ", "", Degree))

dataStatesBach = dataStates %>% filter(Degree == "Bachelors")
colnames(dataStatesBach) = paste0(colnames(dataStatesBach), "Bachelors")
dataStatesBach = dataStatesBach %>% rename(State = StateBachelors) %>% select(-DegreeBachelors)
dataStatesHS = dataStates %>% filter(Degree == "HighSchool")
colnames(dataStatesHS) = paste0(colnames(dataStatesHS), "HighSchool")
dataStatesHS = dataStatesHS %>% rename(State = StateHighSchool) %>% select(-DegreeHighSchool)

dataStatesPlot = dataStatesBach %>% left_join(dataStatesHS)



# getting leaflet ----

states <-
  geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson",
                          what = "sp")
states@data = states@data %>% left_join(dataStatesPlot, by = c("name" = "State"))
labels <- states$name


# making bar plot ----

getRaceData = function(race) {
  columns = c("State", paste0(race), paste0(race, "SE"), "Degree")
  outputData = dataStates[, columns] %>%
    rename(State = 1,
           Percent = 2,
           StdError = 3) %>%
    mutate(Race = race)
  
}

dataOverall = getRaceData("Overall")
dataWhite = getRaceData("White")
dataBlack = getRaceData("Black")
dataAsian = getRaceData("Asian")
dataHispanic = getRaceData("Hispanic")
dataMultiracial = getRaceData("Multiracial")

dataForBarplot = rbind(dataOverall,
                       dataWhite,
                       dataBlack,
                       dataAsian,
                       dataHispanic,
                       dataMultiracial) %>%
  mutate(Race = factor(
    Race,
    levels = c("Overall", "White", "Black", "Hispanic", "Asian", "Multiracial")
  )) %>%
  mutate(Degree = factor(Degree, levels = c("HighSchool", "Bachelors"))) %>%
  group_by(Race, Degree) %>%
  mutate_if(is.numeric, div100)



dataSummaryForBarplot = dataForBarplot %>%
  group_by(Race, Degree) %>%
  summarise(MeanPct = mean(Percent, na.rm = T),
            MeanSE = mean(StdError, na.rm = T)) %>%
  mutate(Degree = gsub("High", "High ", Degree)) %>%
  mutate(Degree = factor(Degree, levels = c("High School", "Bachelors")))

overallMeanHS = pull((
  dataSummaryForBarplot %>% filter(Race == "Overall" &
                                     Degree == "High School")
)[3])
overallMeanSEHS = pull((
  dataSummaryForBarplot %>% filter(Race == "Overall" &
                                     Degree == "High School")
)[4])
overallMeanBach = pull((
  dataSummaryForBarplot %>% filter(Race == "Overall" &
                                     Degree == "Bachelors")
)[3])
overallMeanSEBach = pull((
  dataSummaryForBarplot %>% filter(Race == "Overall" &
                                     Degree == "Bachelors")
)[4])


ggplot(data = dataSummaryForBarplot, aes(x = Race, y = MeanPct, group = Degree)) +
  geom_col(position = "dodge", aes(fill = Degree), color = "black") +
  theme_minimal()



# Define UI for application that draws a histogram

ui <- fluidPage(
  # textOutput("test"),
  tags$head(tags$style(
    HTML(".leaflet-container { background: white; }")
  )),
  titlePanel(title = "", windowTitle = "NISS Visualization"),
  fluidRow(column(
    width = 12, htmlOutput("textplot", height = "200px")
  )),
  fluidRow(column(
    width = 6,
    plotOutput("barplot", click = "bar_click", height = "450px")
  ),
  column(
    width = 6,
    leafletOutput("leafletmap", height = "450px")
  )),
  fluidRow(column(width = 12, plotOutput("ecdfplot")))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  tag.map.title <- tags$style(
    HTML(
      #       "
      #   .leaflet-control.map-title {
      #     transform: translate(-15%,0%);
      #     text-align: center;
      #     padding-left: 10px;
      #     padding-right: 10px;
      #     padding-top: -30px;
      #     background: rgba(255,255,255,0.8);
      #     font-weight: bold;
      #     font-size: 24px;
      #   }
      # "
      ".leaflet-control.map-title {
      transform:translate(0,-40%);
      text-align: center;
      background: rgba(255,255,255,0.8);
      font-weight: bold;
      font-size: 24px;
      }"
    )
  )
  
  tag.map.subtitle <- tags$style(
    HTML(
      #       "
      #   .leaflet-control.map-title {
      #     transform: translate(-15%,0%);
      #     text-align: center;
      #     padding-left: 10px;
      #     padding-right: 10px;
      #     padding-top: -30px;
      #     background: rgba(255,255,255,0.8);
      #     font-weight: bold;
      #     font-size: 24px;
      #   }
      # "
      ".leaflet-control.map-subtitle {
            transform:translate(0,-120%);

      text-align: center;
      background: rgba(255,255,255,0.8);
      font-style: italic;
      font-size: 16px;
      }"
    )
  )
  
  
  degree = reactive({
    ifelse(((input$bar_click$x * 100) %% 100 > 50), "HighSchool", "Bachelors")
  })
  
  race = reactive({
    case_when(
      0.5 <= (input$bar_click$x) & (input$bar_click$x) <= 1.5 ~ "Overall",
      1.5 <= (input$bar_click$x) &
        (input$bar_click$x) <= 2.5 ~ "White",
      2.5 <= (input$bar_click$x) &
        (input$bar_click$x) <= 3.5 ~ "Black",
      3.5 <= (input$bar_click$x) &
        (input$bar_click$x) <= 4.5 ~ "Hispanic",
      4.5 <= (input$bar_click$x) &
        (input$bar_click$x) <= 5.5 ~ "Asian",
      5.5 <= (input$bar_click$x) &
        (input$bar_click$x) <= 7 ~ "Multiracial",
      TRUE ~ as.character((input$bar_click$x))
    )
  })
  
  eventReactive(input$bar_click, {
    plotRace = race()
  }, ignoreNULL = T)
  eventReactive(input$bar_click, {
    plotDegree = degree()
  }, ignoreNULL = T)
  
  
  
  
  
  title <-
    reactive({
      tags$div(tag.map.title, HTML(paste(# ifelse(degree() == "HighSchool", "High School", "Bachelors"),
        # "Statistics for",
        # race(),
        # "Students"
        if (is_empty(degree()) == T) {
          "Overall Graduation Rates for High Schoolers by State"
        }
        
        else {
          ifelse(
            degree() == "HighSchool",
            paste("High School Degree Statistics for", race(), "Students"),
            paste("Bachelor's Degree Statistics for", race(), "Students")
          )
        })))
    })
  
  subtitle <-
    tags$div(
      tag.map.subtitle,
      HTML(
        "Click on a state to view its individual statistics. Do any regions have unusual degree rates?"
      )
    )
  
  output$textplot = renderText({
    paste(
      "<style> h4 {text-align: left; padding-left: 4%; font-size: 36px; font-weight: bold;} </style>",
      "<style> h3 {text-align: left; padding-left: 4%; padding-bottom: 2%; font-size: 24px; font-weight: normal;} </style>",
      "<h4> Graduation Rates by State, Race, and Degree Type </h4>",
      "<h3>In 2021, the National Center for Education Statistics published data on nationwide high school and college graduation rates. <br>This information can be useful in examining educational disparities across regional and demographic boundaries. </h3>"
    )
  })
  # output$textplot = renderText({
  #   paste(
  #     "<style> h4 {text-align: center; font-size: 30px; font-weight: bold;}></style>",
  #     "<h4> Graduation Rates by State, Race, and Degree Type </h4>",
  #     if (is.numeric(input$bar_click$x) == T) {
  #       paste(
  #         "<style> h3 {text-align: center; font-weight: normal;} p {color:",
  #         ifelse(degree() == "HighSchool", "tomato", "blue"),
  #         "; display:inline; font-weight: bold;}</style>",
  #         "<h3>Now visualizing<p>",
  #         ifelse(
  #           degree() == "HighSchool",
  #           "High School Degrees",
  #           "Bachelor's Degrees"
  #         ),
  #         "</p> for <p>",
  #         race(),
  #         "</p> students on the map.</h3>",
  #         "<h3>Remember, you can click on any state to visualize its percentile plots for each individual category!</h3>"
  #
  #       )
  #     }
  #
  #     else{
  #       paste(
  #         "<style> h3 {text-align: center; font-weight: normal;} p {display:inline; font-weight: bold;}</style>",
  #         "<h3>Click on one of the bar chart's columns to visualize state-by-state geographic data for that category.</h3>",
  #         "<h3>Click a state on the map to visualize its percentile plots for each individual category.</h3>"
  #       )
  #     }
  #   )
  # })
  
  
  
  dataForBarplotFiltered = reactive({
    dataForBarplot %>% filter(Race %in% race() & Degree %in% degree())
  })
  
  # binsHS
  # binsBach
  binsHS = c(100, 95, 90, 85, 80, 75, 70, 65, 60, 55)
  binsBach = c(80, 70, 60, 50, 40, 30, 20, 10)
  
  output$barplot = renderPlot({
    ggplot(data = dataSummaryForBarplot, aes(x = Race, y = MeanPct, group = Degree)) +
      geom_col(
        position = "dodge",
        aes(fill = Degree),
        color = "black",
        alpha = 0.8
      ) +
      scale_fill_manual(values = c("cadetblue2", "lightgreen")) +
      geom_hline(
        yintercept = overallMeanHS,
        color = "cyan3",
        linetype = "dashed",
        size = 1.5
      ) +
      geom_hline(
        yintercept = overallMeanBach,
        color = "forestgreen",
        linetype = "dashed",
        size = 1.5
      ) +
      annotate(
        "label",
        x = 1.05,
        y = overallMeanHS,
        label = "Overall Mean\n(High School)",
        color = "cyan4",
        vjust = 1.2
      ) +
      annotate(
        "label",
        x = 1.05,
        y = overallMeanBach,
        label = "Overall Mean\n(Bachelor's)",
        color = "forestgreen",
        vjust = -0.2
      ) +
      
      theme_minimal() +
      labs(
        title = paste("Overall Graduation Rates for All Demographics"),
        y = "Percentage Graduating",
        x = "",
        subtitle = "Click on a bar to show state-level data for selected demographic and degree type.\nYou can also compare each bar's height to the category's overall mean line."
      ) +
      theme(
        legend.position = "right",
        plot.title = element_text(
          face = "bold",
          size = 24,
          hjust = 0,
        ),
        plot.subtitle = element_text(
          face = "italic",
          size = 16,
          hjust = 0
        ),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14, face = "italic")
        
      )
  })
  
  output$leafletmap = renderLeaflet({
    leaflet(states, options = leafletOptions(zoomControl = F)) %>%
      setView(-96, 40.8, 4)
  })
  
  observe({
    labels = lapply(seq(nrow(states@data)), function(i) {
      if (is_empty(degree())) {
        paste0(
          '<strong>',
          states@data[i, "name"],
          '</strong><br>',
          "Grad. Rate: ",
          round(states@data[i, "OverallHighSchool"], digits = 1),
          '<br><i>',
          "Std. Error: ±",
          round(as.numeric(states@data[i, "OverallSEHighSchool"]), digits = 2),
          '</i>'
        )
      }
      else {
        paste0(
          '<strong>',
          states@data[i, "name"],
          '</strong><br>',
          "Grad. Rate: ",
          round(states@data[i, paste0(race(), degree())], digits = 1),
          '<br><i>',
          "Std. Error: ±",
          round(as.numeric(states@data[i, paste0(race(), "SE", degree())]), digits = 2),
          '</i>'
        )
      }
      
    })
    
    leafletProxy("leafletmap") %>%
      clearControls() %>%
      addPolygons(
        data = states,
        layerId = states@data$name,
        fillColor = ~ if (is_empty(degree())) {
          colorBin(
            palette = "BrBG",
            domain = c(50, 100),
            bins = binsHS,
            na.color = "black"
          )(states@data[, "OverallHighSchool"])
        }
        else {
          colorBin(
            palette = if (is_empty(degree()) == T) {
              "BrBG"
            } else {
              if (nchar(degree()) == 10) {
                "BrBG"
              }
              else{
                "PiYG"
              }
            },
            domain = if (is_empty(degree()) == T) {
              c(50, 100)
            } else {
              if (nchar(degree()) == 10) {
                c(0, 75)
              }
              else{
                c(50, 100)
              }
            },
            bins = if (is_empty(degree()) == T) {
              binsHS
            } else {
              if (nchar(degree()) == 10) {
                binsHS
              }
              else{
                binsBach
              }
            },
            
            na.color = "black"
          )(states@data[, paste0(race(), degree())])
        }
        
        ,
        weight = 2,
        opacity = 1,
        color = "black",
        dashArray = "",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "black",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = lapply(labels, htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        colorBin(
          palette = if (is_empty(degree()) == T) {
            "BrBG"
          } else {
            if (nchar(degree()) == 10) {
              "BrBG"
            }
            else{
              "PiYG"
            }
          },
          domain = if (is_empty(degree()) == T) {
            c(50, 100)
          } else {
            if (nchar(degree()) == 10) {
              c(0, 75)
            }
            else{
              c(50, 100)
            }
          },
          bins = if (is_empty(degree()) == T) {
            binsHS
          } else {
            if (nchar(degree()) == 10) {
              binsHS
            }
            else{
              binsBach
            }
          },
          na.color = "black"
        ),
        values = states@data[, paste0(race(), degree())],
        #states@data[, paste0(race(), degree())],
        opacity = 0.7,
        title = "Percent Graduating",
        position = "bottomright",
        na.label = "No Data"
      )  %>%
      addControl(title(), position = "topleft", className = "map-title") %>%
      addControl(subtitle, position = "topleft", className = "map-subtitle")
    
  })
  
  
  output$table <- renderTable({
    expr = dataForBarplotFiltered()
  })
  
  # output$test = renderPrint({
  #   forprint = input$leafletmap_shape_click$id
  #   print(forprint)
  # })
  #
  selectedState = reactive(input$leafletmap_shape_click$id)
  
  observeEvent(input$leafletmap_shape_click, {
    selectedState = input$leafletmap_shape_click$id
  })
  
  degree.labs = c("High School", "Bachelors")
  names(degree.labs) = c("HighSchool", "Bachelors")
  
  stateFilteredData = reactive({
    if (!is.null(selectedState())) {
      dataForBarplot %>% filter(State == selectedState()) %>%
        group_by(Race, Degree) %>%
        summarise(pct = Percent)
    }
    else {
      dataForBarplot %>% filter(State == "New York") %>%
        group_by(Race, Degree) %>%
        summarise(pct = Percent, se = StdError)
    }
  })
  
  output$ecdfplot <- renderPlot({
    ggplot(dataForBarplot, aes(x = Percent)) +
      stat_ecdf(
        geom = "area",
        color = "black",
        alpha = 0.8,
        aes(fill = Degree)
      ) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1L)) +
      # geom_vline(data = stateFilteredData(), aes(xintercept = pct), size = 1) +
      geom_segment(data = stateFilteredData(),
                   aes(
                     x = pct,
                     xend = pct,
                     y = 0,
                     yend = 1
                   ),
                   size = 1.5) +
      facet_wrap(
        ~ Degree + Race,
        scales = "free",
        nrow = 2,
        labeller = labeller(Degree = degree.labs)
      ) +
      theme_minimal() +
      scale_fill_manual(values = c("cadetblue2", "lightgreen")) +
      
      labs(
        title = paste0(
          "How Does Your Selected State, ",
          ifelse(is.null(selectedState()) == T, "New York", selectedState()),
          ", Stack Up?"
        ),
        x = "Percent of Students Graduating",
        y = "State Percentile",
        subtitle = "These plots present a state's raw graduation rate for each statistic on the x-axis and its percentile rank on the y-axis. Use the interactive map to select a new state.\nThey allow for comparison within a state. For example, why might New York rank below the 25th percentile for high school degree rates but above the 75th percentile for college rates?",
        caption = "The data used for this visualization were originally collected in the U.S. Census Bureau's American Community Survey in 2019.\nThey were compiled for publication by the National Center for Education Statistics in 2021."
      ) +
      theme(
        legend.position = "none",
        plot.title = element_text(
          face = "bold",
          size = 24,
          hjust = 0,
          vjust = 0
        ),
        plot.subtitle = element_text(
          face = "italic",
          size = 16,
          vjust = 0
        ),
        axis.title = element_text(size = 18),
        
        axis.text = element_text(size = 14, face = "italic"),
        strip.text.x = element_text(size = 14, face = "italic")
        
      )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
