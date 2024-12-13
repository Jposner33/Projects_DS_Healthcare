library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(tigris)
library(ggpubr)
library(tibble)
library(stringr)
library(scales)
library(plotly)
library(viridis)

# Load data
sahie_2010 <- read_csv("Data/sahie_2010.csv", skip = 79) %>% filter(state_name == "Minnesota")
sahie_2011 <- read_csv("Data/sahie_2011.csv", skip = 79) %>% filter(state_name == "Minnesota")
sahie_2012 <- read_csv("Data/sahie_2012.csv", skip = 79) %>% filter(state_name == "Minnesota")
sahie_2013 <- read_csv("Data/sahie_2013.csv", skip = 79) %>% filter(state_name == "Minnesota")
sahie_2014 <- read_csv("Data/sahie_2014.csv", skip = 79) %>% filter(state_name == "Minnesota")
sahie_2015 <- read_csv("Data/sahie_2015.csv", skip = 79) %>% filter(state_name == "Minnesota")
sahie_2016 <- read_csv("Data/sahie_2016.csv", skip = 79) %>% filter(state_name == "Minnesota")
sahie_2017 <- read_csv("Data/sahie_2017.csv", skip = 79) %>% filter(state_name == "Minnesota")
sahie_2018 <- read_csv("Data/sahie_2018.csv", skip = 79) %>% filter(state_name == "Minnesota")
sahie_2019 <- read_csv("Data/sahie_2019.csv", skip = 79) %>% filter(state_name == "Minnesota")

# Make data into a list
options(scipen = 999)
sahie_list <- list(
  "2010" = "sahie_2010",
  "2011" = "sahie_2011",
  "2012" = "sahie_2012",
  "2013" = "sahie_2013",
  "2014" = "sahie_2014",
  "2015" = "sahie_2015",
  "2016" = "sahie_2016",
  "2017" = "sahie_2017",
  "2018" = "sahie_2018",
  "2019" = "sahie_2019"
)
sahie_list2 <- enframe(sahie_list) %>% 
  select(2:1) %>% 
  deframe

# Load MN counties data
mn_counties <- counties(state = "MN", cb = TRUE, class = "sf")

# Define UI for application that draws maps
ui <- fluidPage(
  # Application title
  titlePanel("Exploring Insurance Statistics in Minnesota Before and After the Affordable Care Act"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "year_before_ACA",
                  label = "Choose a year before the ACA:",
                  choices = sahie_list),
      helpText("Select a year before the Affordable Care act. <2014"),
      selectInput(inputId = "year_after_ACA",
                  label = "Choose a year after the ACA:",
                  choices = sahie_list),
      helpText("Select a year after the Affordable Care act. >2014"),
      selectInput(inputId = "ageCat",
                  label = "Choose an age category:",
                  choices = list(
                    "Under 65 years" = "0",
                    "18 to 64 years" = "1",
                    "40 to 64 years" = "2",
                    "50 to 64 years" = "3",
                    "Under 19 years" = "4"
                  )),
      helpText("Choose an age category that you want to analyze."),
      selectInput(inputId = "sexCat",
                  label = "Choose a sex category:",
                  choices = list("Both sexes" = "0", "Male" = "1", "Female" = "2")),
      helpText("Select: Male, Female, or All"),
      selectInput(inputId = "incomeCat",
                  label = "Choose an income category:",
                  choices = list(
                    "All income levels" = "0",
                    "At or below 200% of poverty" = "1",
                    "At or below 250% of poverty" = "2",
                    "At or below 138% of poverty" = "3",
                    "At or below 400% of poverty" = "4",
                    "Between 138% - 400% of poverty (Not collected until 2012)" = "5"
                  )),
      helpText("Choose an income category that you want to analyze"),
      selectInput(inputId = "variables",
                  label = "Choose a variable:",
                  choices = list(
                    "Percent uninsured in demographic group for <income category>" = "PCTUI",
                    "Percent insured in demographic group for <income category>" = "PCTIC",
                    "Percent uninsured in demographic group for all income levels" = "PCTELIG",
                    "Percent insured in demographic group for all income levels" = "PCTLIIC"
                  )),
      helpText("Select the variable you want to analyze.")
    ),
    
    mainPanel(
      fluidRow(
        column(6, plotlyOutput("map_before_aca")),
        column(6, plotlyOutput("map_after_aca"))
      ),
      br(),
      div(
        id = "placeholder_text",
        style = "border: 2px solid black; padding: 10px; border-radius: 5px;",
        p("Select from the options to the side, to view insurance rates throughout Minnesota. Make sure that your years are chronologically
            ordered, and also double check to make sure that you do not pick a variable that is not collected in said year. Once you have decided 
            what you would like to look at, see if you can find anything interesting!")
      )
      
    )
  )
)

# Define server logic required to draw the maps
server <- function(input, output){
  
  output$map_before_aca <- renderPlotly({
    before_aca <- get(input$year_before_ACA) %>%
      filter(state_name == "Minnesota") %>%
      mutate(PCTELIG = as.numeric(PCTELIG)) %>%
      mutate(NIPR = as.numeric(NIPR)) %>% 
      mutate(PCTUI = as.numeric(PCTUI)) %>% 
      mutate(NUI = as.numeric(NUI)) %>% 
      mutate(NIC = as.numeric(NIC)) %>% 
      mutate(PCTIC = as.numeric(PCTIC)) %>% 
      mutate(PCTLIIC = as.numeric(PCTLIIC)) %>% 
      filter(iprcat == input$incomeCat) %>%
      filter(sexcat == input$sexCat) %>%
      filter(geocat == 50) %>% 
      filter(agecat == input$ageCat)
    
    after_aca <- get(input$year_after_ACA) %>%
      filter(state_name == "Minnesota") %>%
      mutate(PCTELIG = as.numeric(PCTELIG)) %>%
      mutate(NIPR = as.numeric(NIPR)) %>% 
      mutate(PCTUI = as.numeric(PCTUI)) %>% 
      mutate(NUI = as.numeric(NUI)) %>% 
      mutate(NIC = as.numeric(NIC)) %>% 
      mutate(PCTIC = as.numeric(PCTIC)) %>% 
      mutate(PCTLIIC = as.numeric(PCTLIIC)) %>% 
      filter(iprcat == input$incomeCat) %>%
      filter(sexcat == input$sexCat) %>%
      filter(geocat == 50) %>% 
      filter(agecat == input$ageCat)
    
    shared_range <- range(c(before_aca[[input$variables]], after_aca[[input$variables]]), na.rm = TRUE)
    
    before_aca <- mn_counties %>%
      left_join(before_aca, by = c("NAMELSAD" = "county_name"))
    
    p_before <- ggplot(data = before_aca) +
      geom_sf(aes_string(fill = input$variables, County = "NAMELSAD")) +
      scale_fill_viridis(limits = shared_range, oob = scales::squish) +
      labs(title = paste(sahie_list2[[input$year_before_ACA]])) +
      labs(fill = "%") +
      theme(legend.position = "right")
    
    ggplotly(p_before)
  })
  
  output$map_after_aca <- renderPlotly({
    after_aca <- get(input$year_after_ACA) %>%
      filter(state_name == "Minnesota") %>%
      mutate(PCTELIG = as.numeric(PCTELIG)) %>%
      mutate(NIPR = as.numeric(NIPR)) %>% 
      mutate(PCTUI = as.numeric(PCTUI)) %>% 
      mutate(NUI = as.numeric(NUI)) %>% 
      mutate(NIC = as.numeric(NIC)) %>% 
      mutate(PCTIC = as.numeric(PCTIC)) %>% 
      mutate(PCTLIIC = as.numeric(PCTLIIC)) %>% 
      filter(iprcat == input$incomeCat) %>%
      filter(sexcat == input$sexCat) %>%
      filter(geocat == 50) %>% 
      filter(agecat == input$ageCat)
    
    before_aca <- get(input$year_before_ACA) %>%
      filter(state_name == "Minnesota") %>%
      mutate(PCTELIG = as.numeric(PCTELIG)) %>%
      mutate(NIPR = as.numeric(NIPR)) %>% 
      mutate(PCTUI = as.numeric(PCTUI)) %>% 
      mutate(NUI = as.numeric(NUI)) %>% 
      mutate(NIC = as.numeric(NIC)) %>% 
      mutate(PCTIC = as.numeric(PCTIC)) %>% 
      mutate(PCTLIIC = as.numeric(PCTLIIC)) %>% 
      filter(iprcat == input$incomeCat) %>%
      filter(sexcat == input$sexCat) %>%
      filter(geocat == 50) %>% 
      filter(agecat == input$ageCat)
    
    shared_range <- range(c(before_aca[[input$variables]], after_aca[[input$variables]]), na.rm = TRUE)
    
    after_aca <- mn_counties %>%
      left_join(after_aca, by = c("NAMELSAD" = "county_name"))
    
    p_after <- ggplot(data = after_aca) +
      geom_sf(aes_string(fill = input$variables, County = "NAMELSAD")) +
      scale_fill_viridis(limits = shared_range, oob = scales::squish) +
      labs(title = paste(sahie_list2[[input$year_after_ACA]])) +
      labs(fill = "%") +
      theme(legend.position = "right")
    
    ggplotly(p_after)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
