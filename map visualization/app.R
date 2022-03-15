#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(maps)
library(ggplot2)
library(tidyverse)

# read in data
data <- read.csv('/Users/feiyasuo/Documents/GitHub/STOR765_Spring2022_FeiyaSuo/CountyLevel_data.csv') %>%
    mutate(volunteer_county = tolower(volunteer_county))
nc_map <- map_data("county") %>% filter(region=="north carolina") %>% rename(volunteer_county=subregion)
data_map = nc_map %>% left_join(data, by = ("volunteer_county"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Volunteer Nurses in NC during COVID-19"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(4,
            selectInput("var",
                        label = "Choose a variable to display",
                        choices = c("Numer of Available Volunteer", 
                                    "Percentage of Available Volunteer",
                                    "Number of Volunteered Nurses",
                                    "Percentage of Volunteered Nurses",
                                    "County with at Least 1 Volunteered Nurses",
                                    "Number of Vaccined Population",
                                    "Percentage of Vaccined Population",
                                    "Percentage  of NP",
                                    "Percentage  of RN",
                                    "Percentage  of LPN",
                                    "Percentage  of NAII",
                                    "Percentage  of Other License",
                                    "Percentage of retired Nurse",
                                    "Ratio of Active Nurse"),
                        selected = "Numer of Available Volunteer")
            ) ,
        column(12,
               plotOutput("map", 
                          hover = hoverOpts("value", delayType = "debounce"))
               )
        ),
    fluidRow(
        column(12,
               verbatimTextOutput("hover_info")
        )
    )
    )
    

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$map <- renderPlot({
        var <- switch(input$var, 
                       "Numer of Available Volunteer" = data_map$available_volunteer,
                       "Percentage of Available Volunteer" = data_map$percent_available,
                      "Number of Volunteered Nurses" = data_map$volunteered,
                      "Percentage of Volunteered Nurses" = data_map$percent_volunteered,
                      "County with at Least 1 Volunteered Nurses" = data_map$binary_volunteered,
                      "Number of Vaccined Population" = data_map$Vaccinated_count,
                      "Percentage of Vaccined Population" = data_map$Vaccinated.percent,
                      "Percentage  of NP" = data_map$percent_NP,
                      "Percentage  of RN" = data_map$percent_RN,
                      "Percentage  of LPN" = data_map$percent_LPN,
                      "Percentage  of NAII" = data_map$percent_NAII,
                      "Percentage  of Other License" = data_map$percent_Other,
                      "Percentage of retired Nurse" = data_map$percent_retired,
                      "Ratio of Active Nurse" = data_map$ratio_active)
        label = switch(input$var, 
                       "Numer of Available Volunteer" = "Count",
                       "Percentage of Available Volunteer" = "Percentage",
                       "Number of Volunteered Nurses" = "Count",
                       "Percentage of Volunteered Nurses" = "Percentage",
                       "County with at Least 1 Volunteered Nurses" = "Count",
                       "Number of Vaccined Population" = "Count",
                       "Percentage of Vaccined Population",
                       "Percentage  of NP" = "Percentage",
                       "Percentage  of RN" = "Percentage",
                       "Percentage  of LPN" = "Percentage",
                       "Percentage  of NAII" = "Percentage",
                       "Percentage  of Other License" = "Percentage",
                       "Percentage of retired Nurse" = "Percentage",
                       "Ratio of Active Nurse" = "Percentage")
        
        ggplot(data_map, aes(x = long, y = lat, group = group)) +
            geom_polygon(aes(fill= var), colour = "white") +
            scale_x_continuous(breaks = seq(-90, -70, 1), labels = function(x){paste0(x, "°")}) +
            scale_y_continuous(breaks = seq(33, 37, 1), labels = function(x){paste0(x, "°")}) +
            scale_fill_gradient(name = label,low = "lightblue", high="dodgerblue4") +
            labs(title=input$var,
                 y="Latitude", x="Longitude"
            ) +
            theme_light() 
    })
    
    output$hover_info <- renderPrint({
        var <- switch(input$var, 
                      "Numer of Available Volunteer" = data_map$available_volunteer,
                      "Percentage of Available Volunteer" = data_map$percent_available,
                      "Number of Volunteered Nurses" = data_map$volunteered,
                      "Percentage of Volunteered Nurses" = data_map$percent_volunteered,
                      "County with at Least 1 Volunteered Nurses" = data_map$binary_volunteered,
                      "Number of Vaccined Population" = data_map$Vaccinated_count,
                      "Percentage of Vaccined Population" = data_map$Vaccinated.percent,
                      "Percentage  of NP" = data_map$percent_NP,
                      "Percentage  of RN" = data_map$percent_RN,
                      "Percentage  of LPN" = data_map$percent_LPN,
                      "Percentage  of NAII" = data_map$percent_NAII,
                      "Percentage  of Other License" = data_map$percent_Other,
                      "Percentage of retired Nurse" = data_map$percent_retired,
                      "Ratio of Active Nurse" = data_map$ratio_active)
        label = switch(input$var, 
                       "Numer of Available Volunteer" = "count",
                       "Percentage of Available Volunteer" = "percentage",
                       "Number of Volunteered Nurses" = "count",
                       "Percentage of Volunteered Nurses" = "percentage",
                       "County with at Least 1 Volunteered Nurses" = "count",
                       "Number of Vaccined Population" = "count",
                       "Percentage of Vaccined Population",
                       "Percentage  of NP" = "percentage",
                       "Percentage  of RN" = "percentage",
                       "Percentage  of LPN" = "percentage",
                       "Percentage  of NAII" = "percentage",
                       "Percentage  of Other License" = "percentage",
                       "Percentage of retired Nurse" = "percentage",
                       "Ratio of Active Nurse" = "percentage")
        if(!is.null(input$value)){
            hover=input$value
            dist=sqrt((hover$x-data_map$long)^2+(hover$y-data_map$lat)^2)
            ind = which.min(dist)
            cat(data_map$volunteer_county[ind],label,"\n")
            var[ind]
        }
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
