library(shiny)
library(tidyverse)
library(geojsonio)
library(leaflet)
library(RColorBrewer)


## Creating non-reactive data, presets, etc. for the Shiny app ----

# define a function to divide by 100 — need to call this later to create percentages

# importing the raw NCES data from Git repository

data = read.csv(
    "https://raw.githubusercontent.com/bbwieland/NISS-contest-viz/main/data/NCES.csv"
) %>%
    mutate(State = gsub('.{1}$', '', State))

# cleaning the data

dataStates = data %>% filter(State != "United States" &
                                 State != "District of Columbia") %>%
    mutate(Degree = gsub(" ","",Degree))

dataStatesBach = dataStates %>% filter(Degree == "Bachelors")
colnames(dataStatesBach) = paste0(colnames(dataStatesBach),"Bachelors")
dataStatesBach = dataStatesBach %>% rename(State = StateBachelors) %>% select(-DegreeBachelors)
dataStatesHS = dataStates %>% filter(Degree == "HighSchool")
colnames(dataStatesHS) = paste0(colnames(dataStatesHS),"HighSchool")
dataStatesHS = dataStatesHS %>% rename(State = StateHighSchool) %>% select(-DegreeHighSchool)

dataStatesPlot = dataStatesBach %>% left_join(dataStatesHS)



# getting leaflet ----

states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
states@data = states@data %>% left_join(dataStatesPlot, by = c("name" = "State"))
labels <- states$name


# making bar plot ----

getRaceData = function(race){
    columns = c("State",paste0(race),paste0(race,"SE"),"Degree")
    outputData = dataStates[,columns] %>%
        rename(State = 1,Percent = 2,StdError = 3) %>%
        mutate(Race = race)
    
}

dataOverall = getRaceData("Overall")
dataWhite = getRaceData("White")
dataBlack = getRaceData("Black")
dataAsian = getRaceData("Asian")
dataHispanic = getRaceData("Hispanic")
dataMultiracial = getRaceData("Multiracial")

dataForBarplot = rbind(dataOverall,dataWhite,dataBlack,dataAsian,dataHispanic,dataMultiracial) %>%
    mutate(Race = factor(Race,levels = c("Overall","White","Black","Hispanic","Asian","Multiracial"))) %>%
    mutate(Degree = factor(Degree, levels = c("HighSchool","Bachelors")))

dataSummaryForBarplot = dataForBarplot %>%
    group_by(Race,Degree) %>%
    summarise(MeanPct = mean(Percent,na.rm = T),
              MeanSE = mean(StdError,na.rm = T)) %>%
    mutate(Degree = gsub("High","High ",Degree)) %>%
    mutate(Degree = factor(Degree, levels = c("High School","Bachelors")))

overallMeanHS = pull((dataSummaryForBarplot %>% filter(Race == "Overall" & Degree == "High School"))[3])
overallMeanSEHS = pull((dataSummaryForBarplot %>% filter(Race == "Overall" & Degree == "High School"))[4])
overallMeanBach = pull((dataSummaryForBarplot %>% filter(Race == "Overall" & Degree == "Bachelors"))[3])
overallMeanSEBach = pull((dataSummaryForBarplot %>% filter(Race == "Overall" & Degree == "Bachelors"))[4])


ggplot(data = dataSummaryForBarplot,aes(x = Race, y = MeanPct, group = Degree)) +
    geom_col(position = "dodge",aes(fill = Degree),color = "black") +
    theme_minimal()



# Define UI for application that draws a histogram

ui <- fluidPage(

    tags$head(
        tags$style(HTML(".leaflet-container { background: white; }"))
    ),
    fluidRow(
      column(width = 12, htmlOutput("textplot",height = "200px"))
    ),
    fluidRow(
        column(width = 6,
               plotOutput("barplot",click = "bar_click",height = "450px")),
        column(width = 6,
               leafletOutput("leafletmap",height = "450px"))
    )
    

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    left: 30%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px;
    padding-top: -30px;
    background: rgba(255,255,255,0.8);
    font-weight: bold;
    font-size: 24px;
  }
"))

    
    degree = reactive({
        ifelse(((input$bar_click$x*100) %% 100 > 50),"HighSchool","Bachelors")})
    
    race = reactive({
        case_when(
            0.5 <= (input$bar_click$x) & (input$bar_click$x) <= 1.5 ~ "Overall",
            1.5 <= (input$bar_click$x) & (input$bar_click$x) <= 2.5 ~ "White",
            2.5 <= (input$bar_click$x) & (input$bar_click$x) <= 3.5 ~ "Black",
            3.5 <= (input$bar_click$x) & (input$bar_click$x) <= 4.5 ~ "Hispanic",
            4.5 <= (input$bar_click$x) & (input$bar_click$x) <= 5.5 ~ "Asian",
            5.5 <= (input$bar_click$x) & (input$bar_click$x) <= 7 ~ "Multiracial",
            TRUE ~ as.character((input$bar_click$x))
        )})
    
   
    
    
    title <- tags$div(
        tag.map.title, HTML(paste("Graduation Statistics for Selected Rates"))
    )  
    
    output$textplot = renderText({
      if (is.numeric(input$bar_click$x) == T) {
      paste("<style> h3 {text-align: center; font-weight: normal;} p {color:",ifelse(degree() == "HighSchool","tomato","blue"),"; display:inline; font-weight: bold;}</style>",
            "<h3>Now visualizing<p>",ifelse(degree() == "HighSchool","High School Degrees","Bachelor's Degrees"),"</p> for <p>",race(),"</p> students on the map.</h3>")
      }
      
      else{paste("<style> h3 {text-align: center; font-weight: normal;} p {color:",ifelse(degree() == "HighSchool","tomato","blue"),"; display:inline; font-weight: bold;}</style>",
                 "<h3>Click on one of the bar chart's columns to visualize state-by-state geographic data for that category.</h3>")}
    })
    

    
    output$vals = renderText(c(degree(),race()))
    
    dataForBarplotFiltered = reactive({dataForBarplot %>% filter(Race %in% race() & Degree %in% degree())})
    

    output$barplot = renderPlot({ggplot(data = dataSummaryForBarplot,aes(x = Race, y = MeanPct, group = Degree)) +
            geom_col(position = "dodge",aes(fill = Degree),color = "black",alpha = 0.6) +
            scale_fill_manual(values = c("red","blue")) +
            geom_hline(yintercept = overallMeanHS,color = "red",linetype = "dashed") +
            geom_hline(yintercept = overallMeanBach,color = "blue",linetype = "dashed") +
            theme_minimal() +
            labs(title = "Graduation Rates for All Demographics",
                 y = "Percentage Graduating",
                 x = "") +
            theme(
                legend.position = "right",
                plot.title = element_text(face = "bold",size = 24,hjust = 0.5,vjust = -3),
                axis.title = element_text(size = 18),
                legend.text = element_text(size = 14),
                legend.title = element_text(size = 14,face = "bold"),
                axis.text = element_text(size = 14,face = "italic")
                
            )})
    
    output$leafletmap = renderLeaflet({leaflet(states) %>%
            setView(-96, 40.8, 4)})
    
    observe({
        
        pal <- (colorBin("PiYG",domain = states@data[,paste0(race(),degree())],bins = 7,na.color = "black"))
        
        labels = lapply(seq(nrow(states@data)), function(i) {
            paste0( '<strong>', states@data[i,"name"], '</strong><br>', 
                    "Grad. Rate: ",round(states@data[i, paste0(race(),degree())],digits = 1), '<br><i>', 
                    "Std. Error: ±",round(as.numeric(states@data[i, paste0(race(),"SE",degree())]),digits = 2),'</i>')})
        
        leafletProxy("leafletmap") %>%
            clearControls() %>%
            addPolygons(data = states,
                        fillColor = ~pal(states@data[,paste0(race(),degree())]),
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
                            bringToFront = TRUE),
                        label = lapply(labels, htmltools::HTML),
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto")) %>%
            addLegend(pal = pal, values = states@data[,paste0(race(),degree())], opacity = 0.7, title = "Percent Graduating",
                      position = "bottomright",na.label = "No Data")  %>%
            addControl(title, position = "topright", className="map-title")
            
    })
    

    output$table <- renderTable({
        expr = dataForBarplotFiltered()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
