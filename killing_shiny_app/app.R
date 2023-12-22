# Load required libraries
library(shiny)
library(tidyverse)
library(DT)
library(janitor)
library(tidycensus)
library(shinyWidgets)
library(lubridate)
library(zoo)

# Define data file links
killing_data_link <- "Mapping Police Violence.csv"
agency_locations_link <- "data_agency-reference-list.csv"

# Read data from CSV files
killing_data <- read_csv(here::here(killing_data_link))
agency_locations <- read_csv(here::here(agency_locations_link))

# Data preprocessing
df <- killing_data %>%
  clean_names() %>%
  filter(state == "LA") %>%
  separate_wider_delim(county, delim = " Parish", names = c("parish", "extra"), too_few = "align_start") %>%
  select(-extra) %>%
  mutate(
    race = ifelse(is.na(race), "Unknown Race", race),
    gender = ifelse(is.na(gender), "Unknown Gender", gender),
    age = ifelse(is.na(age), "Unknown Age", age),
    date = mdy(date),
    year = year(date),
    year_month = month(date),
    age_category = case_when(
      age < 18 ~ "<18",
      age >= 18 & age < 35 ~ "18 - 34",
      age >= 35 & age < 55 ~ "35 - 54",
      age >= 55 ~ "55+",
      TRUE ~ NA
    ),
    parish = ifelse(parish == "Dallas", "Rapides", parish),
    parish = str_replace(parish, "Saint", "St."),
    parish = ifelse(parish == "Acadiana", "Acadia", parish),
    parish = ifelse(city == "Monroe", "St. Tammany", parish)
  ) %>%
  filter(!(name == "Omarr Jackson" & race == "White"))

# Extract agency information
agencies <- df %>%
  mutate(
    agency_name = str_split(as.character(str_replace_all(agency_responsible, ", ", ",")), ","),
    agency_name = map(agency_name, ~ str_remove_all(.x, '^"|"$|[()]'))
  ) %>%
  unnest(agency_name) %>%
  tabyl(agency_name, race) %>%
  adorn_totals(where = "col")

# Define Shiny UI
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
  
  titlePanel("Police Killings Analysis"),
  mainPanel(
    fluidRow(
      column(3, 
             pickerInput("selection",
                         "Agency",
                         choices = unique(agencies$agency_name),
                         selected = unique(agencies$agency_name),
                         options = list(`actions-box` = TRUE), 
                         multiple = TRUE,
                         width = "100%")),  
      column(3, 
             selectInput("parish",
                         "Parish",
                         choices = c("All", unique(df$parish)),
                         width = "100%")),
      column(3,
             selectInput("armed",
                         "Armed",
                         choices = c("All", unique(df$allegedly_armed)),
                         width = "100%")),
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
      column(12, 
             verbatimTextOutput("percentageText")),
      column(12,
             DTOutput("dataTable")),
      column(12,
             downloadButton("downloadData", "Download Data"))
    )
  )
)

# Define Shiny server logic
server <- function(input, output, session) {
  filtered_df <- reactive({
    df %>%
      filter(str_detect(agency_responsible, paste(input$selection, collapse = "|"))) %>%
      filter(if (input$parish != "All") parish %in% input$parish else TRUE) %>%
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
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$percentageText <- renderText({
    total_rows <- nrow(df)
    filtered_rows <- nrow(filtered_df())
    percentage <- ifelse(total_rows > 0, filtered_rows / total_rows * 100, 0)
    paste("Percentage of all police killings: ", round(percentage, 2), "%")
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

# Run the Shiny app
shinyApp(ui = ui, server = server)
