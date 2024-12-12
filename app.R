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
sahie_2010 <- read_csv("Data/sahie_2010.csv", skip = 79)%>% filter(state_name == "Minnesota")
sahie_2011 <- read_csv("Data/sahie_2011.csv", skip = 79)%>% filter(state_name == "Minnesota")
sahie_2012 <- read_csv("Data/sahie_2012.csv", skip = 79)%>% filter(state_name == "Minnesota")
sahie_2013 <- read_csv("Data/sahie_2013.csv", skip = 79)%>% filter(state_name == "Minnesota")
sahie_2014 <- read_csv("Data/sahie_2014.csv", skip = 79)%>% filter(state_name == "Minnesota")
sahie_2015 <- read_csv("Data/sahie_2015.csv", skip = 79)%>% filter(state_name == "Minnesota")
sahie_2016 <- read_csv("Data/sahie_2016.csv", skip = 79)%>% filter(state_name == "Minnesota")
sahie_2017 <- read_csv("Data/sahie_2017.csv", skip = 79)%>% filter(state_name == "Minnesota")
sahie_2018 <- read_csv("Data/sahie_2018.csv", skip = 79)%>% filter(state_name == "Minnesota")
sahie_2019 <- read_csv("Data/sahie_2019.csv", skip = 79)%>% filter(state_name == "Minnesota")
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

# Make variables into lists for selection
ageCat <- list(
  "Under 65 years" = "0",
  "18 to 64 years" = "1",
  "40 to 64 years" = "2",
  "50 to 64 years" = "3",
  "Under 19 years" = "4"
)

sexCat <- list(
  "Both sexes" = "0",
  "Male" = "1",
  "Female" = "2"
)

# raceCat <- list(
#   "All races" = "0",
#   "White alone, not Hispanic or Latino" = "1",
#   "Black or African American alone, not Hispanic or Latino" = "2",
#   "Hispanic or Latino (any race)" = "3",
#   "American Indian and Alaska Native alone, not Hispanic or Latino" = "4",
#   "Asian alone, not Hispanic or Latino" = "5",
#   "Native Hawaiian and Other Pacific Islander alone, not Hispanic or Latino" = "6",
#   "Two or More Races, not Hispanic or Latino" = "7"
# )

incomeCat <- list(
  "All income levels" = "0",
  "At or below 200% of poverty" = "1",
  "At or below 250% of poverty" = "2",
  "At or below 138% of poverty" = "3",
  "At or below 400% of poverty" = "4",
  "Between 138% - 400% of poverty (Not collected until 2012)" = "5"
)
incomeCat2 <- enframe(incomeCat) %>% 
  select(2:1) %>% 
  deframe

variables <- list(
  "Percent uninsured in demographic group for <income category>" = "PCTUI",
  "Percent insured in demographic group for <income category>" = "PCTIC",
  "Percent uninsured in demographic group for all income levels" = "PCTELIG",
  "Percent insured in demographic group for all income levels" = "PCTLIIC"
)
variablesMinusWords <- list(
                          "Percent uninsured in demographic group for" = "PCTUI",
                          "Percent insured in demographic group for" = "PCTIC",
                          "Percent uninsured in demographic group for all income levels" = "PCTELIG",
                          "Percent insured in demographic group for all income levels" = "PCTLIIC"
)
# Need to make this reason properly for other levels^^^^^^^^
variables2 <- enframe(variablesMinusWords) %>% 
  select(2:1) %>% 
  deframe


# Define UI for application that draws a point plot, bar chart and map
ui <- fluidPage(
  
  
  # Application title
  titlePanel("Exploring Insurance Statistics in Minnesota Before and After the Affordable Care Act"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "year_before_ACA",
                  label = "Choose a year before the ACA:",
                  choices = sahie_list
      ),
      helpText("Select a year before the Affordable Care act. <2014"),
      selectInput(inputId = "year_after_ACA",
                  label = "Choose a year after the ACA:",
                  choices = sahie_list
      ),
      helpText("Select a year after the Affordable Care act. >2014"),
      selectInput(inputId = "ageCat",
                  label = "Choose an age category:",
                  choices = ageCat
      ),
      helpText("Choose an age category that you want to analyze."),
      selectInput(inputId = "sexCat",
                  label = "Choose a sex category:",
                  choices = sexCat
      ),
      helpText("Choose race category that you want to analyze"),
      selectInput(inputId = "incomeCat",
                  label = "Choose an income category:",
                  choices = incomeCat
      ),
      helpText("Choose an income category that you want to analyze"),
      selectInput(inputId = "variables",
                  label = "Choose a variable:",
                  choices = variables),
      helpText("Select the variable you want to analyze.")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        column(6, plotlyOutput("map_before_aca")),
        column(6, plotlyOutput("map_after_aca"))
      )
    )
    
  )
)

# Define server logic required
server <- function(input, output){
  
  
  
  output$map_before_aca <- renderPlotly({
    # Dynamically fetch the dataframe for the selected year
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
      #filter(racecat == input$raceCat) %>%
      filter(agecat == input$ageCat)
    
    # Dynamically fetch the after_aca dataframe to calculate shared range
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
      #filter(racecat == input$raceCat) %>%
      filter(agecat == input$ageCat)
    
    # Compute shared range for consistent legend
    shared_range <- range(before_aca[[input$variables]], na.rm = TRUE)
    
    # Join with MN counties
    before_aca <- mn_counties %>%
      left_join(before_aca, by = c("NAMELSAD" = "county_name"))
    
    # Plot Pre-ACA map

    
    
    
    # if(stringr::str_detect(input$variables, "for", negate = T) && stringr::str_detect(input$variables, "Percent")){
    #   ggplot(data = before_aca) +
    #   geom_sf(aes_string(fill = input$variables), crs = 26915) +
    #     scale_fill_continuous(limits = shared_range, oob = scales::squish) +
    #     labs(title = paste(variables2[[input$variables]], incomeCat2[[input$incomeCat]], "in Minnesota Counties in", sahie_list2[[input$year_after_ACA]], "(Pre-ACA)"), fill ="1%") 
    #     #labs(fill = "%1")
    # }
    # else if (stringr::str_detect(input$variables, "Percent")){
    #   ggplot(data = before_aca) +
    #   geom_sf(aes_string(fill = input$variables), crs = 26915) +
    #     scale_fill_continuous(limits = shared_range, oob = scales::squish) +
    #     labs(title = paste(variables2[[input$variables]], "in Minnesota Counties in", sahie_list2[[input$year_after_ACA]], "(Pre-ACA)")) +
    #     labs(fill = "%2")
    # }
    # else if (stringr::str_detect(input$variables, "for", negate = T)) {
    #   ggplot(data = before_aca) +
    #   geom_sf(aes_string(fill = input$variables), crs = 26915) +
    #     scale_fill_continuous(limits = shared_range, oob = scales::squish) +
    #     labs(title = paste(variables2[[input$variables]], incomeCat2[[input$incomeCat]], "in Minnesota Counties in", sahie_list2[[input$year_after_ACA]], "(Pre-ACA)")) +
    #     labs(fill = "#3")
    # }
    #   else {
    #     ggplot(data = before_aca) +
    #     geom_sf(aes_string(fill = input$variables), crs = 26915) +
    #     scale_fill_continuous(limits = shared_range, oob = scales::squish) +
    #     labs(title = paste(variables2[[input$variables]], "in Minnesota Counties in", sahie_list2[[input$year_after_ACA]], "(Pre-ACA)")) +
    #     labs(fill = "#4")
    #     
    # }
    if(stringr::str_detect(input$variables, "for", negate = T)){
      p <- ggplot(data = before_aca) +
        geom_sf(aes_string(fill = input$variables, County = "NAMELSAD")) +
        scale_fill_continuous(limits = shared_range, oob = scales::squish) +
        labs(title = paste(variables2[[input$variables]], incomeCat2[[input$incomeCat]], "in Minnesota Counties in", sahie_list2[[input$year_after_ACA]], "(Pre-ACA)")) +
        labs(fill = "%") +
        theme(legend.position = "none")
      ggplotly(p)
    }
    else{
      p <- ggplot(data = before_aca) +
        geom_sf(aes_string(fill = input$variables, County = "NAMELSAD")) +
        scale_fill_continuous(limits = shared_range, oob = scales::squish) +
        labs(title = paste(variables2[[input$variables]], "in Minnesota Counties in", sahie_list2[[input$year_after_ACA]], "(Pre-ACA)")) +
        labs(fill = "%") +
        theme(legend.position = "none")
      ggplotly(p)
    }
  })
  
  output$map_after_aca <- renderPlotly({
    # Dynamically fetch the dataframe for the selected year
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
      #filter(racecat == input$raceCat) %>%
      filter(agecat == input$ageCat)
    
    # Dynamically fetch the before_aca dataframe to calculate shared range
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
      #filter(racecat == input$raceCat) %>%
      filter(agecat == input$ageCat)
    
    # Compute shared range for consistent legend
    shared_range <- range(before_aca[[input$variables]], na.rm = TRUE)
    
    # Join with MN counties
    after_aca <- mn_counties %>%
      left_join(after_aca, by = c("NAMELSAD" = "county_name"))
    
    # Plot Post-ACA map
    if(str_detect(input$incomeCat, "for", negate = T)) {
      p <- ggplot(data = after_aca) +
        geom_sf(aes_string(fill = input$variables, County = "NAMELSAD")) +
        scale_fill_continuous(limits = shared_range, oob = scales::squish) +
        labs(title = paste(variables2[[input$variables]], incomeCat2[[input$incomeCat]], "in Minnesota Counties in", sahie_list2[[input$year_after_ACA]], "(Post-ACA)")) +
        labs(fill = "%")
      ggplotly(p)
    }
    else {
      p <- ggplot(data = after_aca) +
        geom_sf(aes_string(fill = input$variables, County = "NAMELSAD")) +
        scale_fill_continuous(limits = shared_range, oob = scales::squish) +
        labs(title = paste(variables2[[input$variables]],"in Minnesota Counties in", sahie_list2[[input$year_after_ACA]], "(Post-ACA)")) +
        labs(fill = "%")
      ggplotly(p)
    }

  })
}



# Run the application
shinyApp(ui = ui, server = server)