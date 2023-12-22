library(shiny)
library(tidyverse)
library(DT)
library(tidyverse)
library(janitor)
library(tidycensus)
library(shinyWidgets)

library(janitor)
library(lubridate)
library(tidycensus)
library(tidyverse)
library(zoo)

# Defining Links
killing_data_link <- "Mapping Police Violence.csv"
agency_locations_link <- "data_agency-reference-list.csv"

killing_data <- read_csv(here::here(killing_data_link))
agency_locations <- read_csv(here::here(agency_locations_link))

df <- killing_data %>%
  
  # Cleaning names
  clean_names() %>%
  
  # Filtering for Louisiana killings
  filter(state == "LA") %>%
  
  # Separating Parish from Parish names
  separate_wider_delim(county, delim = " Parish", names = c("parish", "extra"), too_few = "align_start") %>%
  
  # Removing "extra" column
  select(-extra) %>%
  
  # Fixing demographic variables
  mutate(race = ifelse(is.na(race), "Unknown Race", race),
         race = ifelse(race == "Unknown race", "Unknown Race", race),
         gender = ifelse(is.na(gender), "Unknown Gender", gender),
         age = ifelse(is.na(age), "Unknown Age", age),
         
         # Fixing and defining year and month variables
         date = mdy(date),
         year = year(date),
         year_month = month(date),
         
         # Creating an age category
         age_category = case_when(
           age < 18 ~ "<18",
           age >= 18 & age < 35 ~ "18 - 34",
           age >= 35 & age < 55 ~ "35 - 54",
           age >= 55 ~ "55+",
           TRUE ~ NA),
         parish = ifelse(parish == "Dallas", "Rapides", parish),
         parish = str_replace(parish, "Saint", "St."),
         parish = ifelse(parish == "Acadiana", "Acadia", parish),
         parish = ifelse(city == "Monroe", "St. Tammany", parish)) %>%
  filter(!(name == "Omarr Jackson" & race == "White"))

agencies <- df %>% 
  
  # Splitting agencies by comma and removing all unnecessary text from agency names
  mutate(
    agency_name = str_split(as.character(str_replace_all(agency_responsible, ", ", ",")), ","),
    agency_name = map(agency_name, ~ str_remove_all(.x, '^"|"$|[()]'))
  ) %>%
  unnest(agency_name) %>%
  tabyl(agency_name, race) %>%
  adorn_totals(where = "col")

library(shiny)
library(shinyWidgets)
library(DT)
library(shiny)
library(dplyr)
library(DT)
library(shinyWidgets)
library(shinyjs)  # Don't forget to include shinyjs

# Assuming df is your data frame
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(dplyr)

ui <- fluidPage(
  useShinyjs(),
  extendShinyjs(text = "shinyjs.page.fill();", functions = NULL),
  
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Source+Serif+Pro:wght@400;600&display=swap"),
    tags$style(HTML("
      body {
        font-family: 'Source Serif Pro', sans-serif;
      }
      label {
        color: #0055AA;
        font-size: 25px;
        font-weight: bold;
      }
      select, .selectize-input {
        background-color: #0055AA !important;
        color: #FFFFFF !important;
        border: 1px solid #0055AA !important;
        border-radius: 4px !important;
      }
      .selectize-control.single .selectize-input {
        height: 34px !important;
      }
    "))
  ),
  
  titlePanel(""),
  mainPanel(
    fluidRow(
      column(3, 
             pickerInput("selection",
                         "Agency",
                         choices = unique(agencies$agency_name),
                         selected = unique(agencies$agency_name),
                         options = list(`actions-box` = TRUE), 
                         multiple = TRUE,
                         width = "100%")),  # Set width to 100%
      column(3, 
             selectInput("parish",
                         "Parish",
                         choices = c("All", unique(df$parish)),
                         width = "100%")),  # Set width to 100%
  #    column(3, 
 #            selectInput("cause",
  #                       "Cause",
  #                       choices = c("All", unique(df$cause_of_death)),
  #                       width = "100%")),  # Set width to 100%
  #    column(3, 
  #           selectInput("disposition",
   #                      "Disposition",
   #                      choices = c("All", unique(df$disposition_official)),
   #                      width = "100%")),  # Set width to 100%
    #  column(3,
     #        selectInput("armed",
     #                    "Armed",
      #                   choices = c("All", unique(df$allegedly_armed)),
      #                   width = "100%")),  # Set width to 100%
      #column(3,
     #        selectInput("flee",
     #                    "Flee",
      #                   choices = c("All", unique(df$wapo_flee)),
      #                   width = "100%")),  # Set width to 100%
 column(3,
        selectInput("armed",
                    "Armed",
                    choices = c("All", unique(df$allegedly_armed)),
                    width = "100%")),  # Set width to 100%
 column(3,
        selectInput("flee",
                    "Fleeing",
                    choices = c("All", unique(df$wapo_flee)),
                    width = "100%")),
 column(3,
             selectInput("gender",
                         "Gender",
                         choices = c("All", unique(df$gender)),
                         width = "100%")), 
      column(3,
             selectInput("race",
                         "Race",
                         choices = c("All", unique(df$race)),
                         width = "100%")),
      column(3,
            selectInput("age",
                        "Age",
                        choices = c("All", unique(df$age_category)),
                        width = "100%")),
        # Set width to 100%
 
   #   column(3,
   #          sliderInput("age",
   #                      "Age",
   ##                      min = 0,
   #                      max = 100,
   #                      value = c(0, 100),  # Default age range
    #                     width = "100%")),
  #    column(3,
   #          dateRangeInput("dateRange",
   #                         "Date Range",
   #                         start = min(df$date),
   #                         end = max(df$date),
   #                         width = "100%")),
      column(12, 
             verbatimTextOutput("percentageText")),
      column(12,
             DTOutput("dataTable")),  # Set width to 100%
      column(12,
             downloadButton("downloadData", "Download Data"))
    )
  )
)

strtoi("43")

server <- function(input, output, session) {
  filtered_df <- reactive({
    df %>%
      filter(str_detect(agency_responsible, paste(input$selection, collapse = "|"))) %>%
      filter(if (input$parish != "All") parish %in% input$parish else TRUE) %>%
   #  filter(if (input$cause != "All") cause_of_death %in% input$cause else TRUE) %>%
    #  filter(if (input$disposition != "All") disposition_official %in% input$disposition else TRUE) %>%
      filter(if (input$armed != "All") allegedly_armed %in% input$armed else TRUE) %>%
      filter(if (input$flee != "All") wapo_flee %in% input$flee else TRUE) %>%
      filter(if (input$gender != "All") gender %in% input$gender else TRUE) %>%
      filter(if (input$race != "All") race %in% input$race else TRUE) %>%
      filter(if (input$age != "All") age_category %in% input$age else TRUE) %>%
      select(-c(circumstances, news_urls, victim_image, state, ori, wapo_id, geography, mpv_id, fe_id, officer_known_past_shootings)) %>%
      select(c(name:call_for_service)) %>%
      rename_with(~str_to_title(str_replace_all(str_replace_all(., "_", " "), "wapo", "")), c(name:call_for_service))
    
  })
  
  output$dataTable <- renderDT({
    datatable(
      filtered_df(),
      options = list(pageLength = 10, scrollX = TRUE)  # Add scrollX option
    )
  })
  
  output$percentageText <- renderText({
    total_rows <- nrow(df)
    filtered_rows <- nrow(filtered_df())
    percentage <- ifelse(total_rows > 0, filtered_rows / total_rows * 100, 0)
    paste("Percentage of all police kilings: ", round(percentage, 2), "%")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_df(), file)
    }
  )
}

shinyApp(ui = ui, server = server)
