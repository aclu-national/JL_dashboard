library(shiny)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(janitor)
library(tidycensus)
library(shinyjs)
library(lubridate)
library(zoo)

# Defining data for the first app
allegation_link <- "data_allegation.csv"
personnel_link <- "data_personnel.csv"
agency_locations_link <- "data_agency-reference-list.csv"
history_ids_link <- "data_post-officer-history.csv"

# Load data for the first app
allegations <- read_csv(allegation_link)
personnel <- read_csv(personnel_link)
agency_locations <- read_csv(agency_locations_link)
history_ids <- read_csv(history_ids_link)

# Renaming "agency_slug" as "agency"
agency_locations <- agency_locations %>%
  rename(agency = agency_slug)

# Creating the misconduct training dataframe
df <- personnel %>%
  unite(col = 'full_name', c('first_name', "middle_name", 'last_name'), sep = " ", na.rm = TRUE, remove = FALSE) %>%
  left_join(history_ids %>% select("uid", "history_id"), by = "uid") %>%
  right_join(allegations, by = c("uid")) %>%
  select(-ends_with(".y")) %>%
  rename("sex" = sex.x,
         "race" = race.x,
         "agency" = agency.x) %>%
  left_join(agency_locations, by = "agency") %>%
  mutate(
    agency_type = case_when(
      str_detect(tolower(agency_name), "university|college|campus") ~ "University or Campus Police",
      str_detect(tolower(agency_name), "marshal") ~ "Marshal's Office",
      str_detect(tolower(agency_name), "constable") ~ "Constable's Office",
      str_detect(tolower(agency_name), "sheriff") ~ "Sheriff's Office",
      str_detect(tolower(agency_name), "department|police department") ~ "Police Department",
      TRUE ~ "Other Law Enforcement Agency"
    ),
    agency_name = case_when(
      agency_name == "New Orleans Parish Sheriff's Office" ~ "Orleans Parish Sheriff's Office",
      agency_name == "Orleans Constable" ~ "Orleans Parish Constable",
      agency_name == "Jefferson Constable" ~ "Jefferson Parish Constable",
      TRUE ~ agency_name
    ),
    uid = ifelse(!is.na(history_id), history_id, uid),
    index = row_number()) %>%
  filter(!is.na(agency_name)) %>%
  mutate(
    allegation = ifelse(is.na(allegation), "no allegation reported", allegation),
    disposition = ifelse(is.na(disposition), "no disposition reported", disposition),
    action = ifelse(is.na(action), "no repercussion reported", action),
    race = ifelse(is.na(race), "Unknown Race", race),
    sex = ifelse(is.na(sex), "Unknown Gender", sex),
    full_name = trimws(gsub("[[:punct:]]", "", full_name))
  )

counting_allegations <- df %>%
  tabyl(uid)

# Loading killing data for the second app
killing_data_link <- "Mapping Police Violence.csv"
agency_locations_link <- "data_agency-reference-list.csv"

killing_data <- read_csv(here::here(killing_data_link))
agency_locations <- read_csv(here::here(agency_locations_link))

df_killing <- killing_data %>%
  
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
  filter(!(name == "Omarr Jackson" & race == "White")) %>%
  mutate(
    wapo_flee = ifelse(is.na(wapo_flee), "Unknown Flee Status", wapo_flee)
  )

agencies <- df_killing %>%
  mutate(
    agency_name = str_split(as.character(str_replace_all(agency_responsible, ", ", ",")), ","),
    agency_name = map(agency_name, ~ str_remove_all(.x, '^"|"$|[()]'))
  ) %>%
  unnest(agency_name) %>%
  tabyl(agency_name, race) %>%
  adorn_totals(where = "col")

# Shiny app with tabs
ui <- fluidPage(
  # titlePanel("Shiny App with Tabs"),
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
  tabsetPanel(
    tabPanel("Police Misconduct",
             fluidRow(
               column(3, 
                      pickerInput("selection",
                                  "Agency",
                                  choices = sort(unique(df$agency_name)),
                                  selected = sort(unique(df$agency_name)),
                                  options = list(`actions-box` = TRUE), 
                                  multiple = TRUE,
                                  width = "100%")),
               column(3, 
                      selectInput("allegation",
                                  "Allegation",
                                  choices = c("All", sort(unique(df$allegation))),
                                  width = "100%")),
               column(3, 
                      selectInput("disposition",
                                  "Disposition",
                                  choices = c("All", sort(unique(df$disposition))),
                                  width = "100%")),
               column(3, 
                      selectInput("action",
                                  "Repercussion",
                                  choices = c("All", sort(unique(df$action))),
                                  width = "100%")),
               column(3,
                      selectInput("officer_name",
                                  "Name",
                                  choices = c("All", sort(unique(df$full_name))),
                                  width = "100%")),
               column(3,
                      selectInput("officer_race",
                                  "Race",
                                  choices = c("All", sort(unique(df$race))),
                                  width = "100%")),
               column(3,
                      selectInput("officer_gender",
                                  "Gender",
                                  choices = c("All", sort(unique(df$sex))),
                                  width = "100%")),
               column(12, 
                      verbatimTextOutput("percentageText_1")),  # Display percentage here
               DTOutput("dataTable_1"),
               downloadButton("downloadData_1", "Download Data")
             )),
    tabPanel("Police Killings",
             fluidRow(
               column(3, 
                      pickerInput("selection2",
                                  "Agency",
                                  choices = sort(unique(agencies$agency_name)),
                                  selected = sort(unique(agencies$agency_name)),
                                  options = list(`actions-box` = TRUE), 
                                  multiple = TRUE,
                                  width = "100%")),
               column(3, 
                      selectInput("parish",
                                  "Parish",
                                  choices = c("All", sort(unique(df_killing$parish))),
                                  width = "100%")),
               column(3,
                      selectInput("armed",
                                  "Armed",
                                  choices = c("All", sort(unique(df_killing$allegedly_armed))),
                                  width = "100%")),
               column(3,
                      selectInput("flee",
                                  "Fleeing",
                                  choices = c("All", sort(unique(df_killing$wapo_flee))),
                                  width = "100%")),
               column(3,
                      selectInput("gender",
                                  "Gender",
                                  choices = c("All", sort(unique(df_killing$gender))),
                                  width = "100%")), 
               column(3,
                      selectInput("race",
                                  "Race",
                                  choices = c("All", sort(unique(df_killing$race))),
                                  width = "100%")),
               column(3,
                      selectInput("age",
                                  "Age",
                                  choices = c("All", sort(unique(df_killing$age_category))),
                                  width = "100%")),
               column(12, 
                      verbatimTextOutput("percentageText_2")),
               DTOutput("dataTable_2"),
               downloadButton("downloadData_2", "Download Data")
             ))
  )
)

server <- function(input, output, session) {
  # Reactive expression for the first app
  filtered_df_1 <- reactive({
    df %>% 
      filter(agency_name %in% input$selection) %>%
      filter(if (input$allegation != "All") allegation %in% input$allegation else TRUE) %>%
      filter(if (input$disposition != "All") disposition %in% input$disposition else TRUE) %>%
      filter(if (input$action != "All") action %in% input$action else TRUE) %>%
      filter(if (input$officer_name != "All") full_name %in% input$officer_name else TRUE) %>%
      filter(if (input$officer_race != "All") race %in% input$officer_race else TRUE) %>%
      filter(if (input$officer_gender != "All") sex %in% input$officer_gender else TRUE) %>%
      select("Full Name" = full_name, 
             "Officer Race" = race,
             "Officer Gender" = sex,
             "Agency" = agency_name, 
             "Allegation" = allegation, 
             "Disposition" = disposition, 
             "Action" = action)
  })
  
  # Reactive expression for the second app
  filtered_df_2 <- reactive({
    df_killing %>%
      filter(str_detect(agency_responsible, paste(input$selection2, collapse = "|"))) %>%
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
  
  # Output functions for the first app
  output$dataTable_1 <- renderDT({
    datatable(
      filtered_df_1(),
      options = list(pageLength = 10, scrollX = TRUE),
    )
  })
  
  output$percentageText_1 <- renderText({
    total_rows <- nrow(df)
    filtered_rows <- nrow(filtered_df_1())
    percentage <- ifelse(total_rows > 0, filtered_rows / total_rows * 100, 0)
    paste("Percentage of all allegations: ", round(percentage, 2), "%")
  })
  
  output$downloadData_1 <- downloadHandler(
    filename = function() {
      paste("filtered_data_allegations_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_df_1(), file)
    }
  )
  
  # Output functions for the second app
  output$dataTable_2 <- renderDT({
    datatable(
      filtered_df_2(),
      options = list(pageLength = 10, scrollX = TRUE),
    )
  })
  
  output$percentageText_2 <- renderText({
    total_rows <- nrow(df_killing)
    filtered_rows <- nrow(filtered_df_2())
    percentage <- ifelse(filtered_rows > 0, filtered_rows / total_rows * 100, 0)
    paste("Percentage of all police killings: ", round(percentage, 2), "%")
  })
  
  
  output$downloadData_2 <- downloadHandler(
    filename = function() {
      paste("filtered_data_police_killings_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_df_2(), file)
    }
  )
}

nrow(df_killing)
shinyApp(ui = ui, server = server)
