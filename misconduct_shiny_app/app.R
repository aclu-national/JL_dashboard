library(shiny)
library(tidyverse)
library(DT)
library(tidyverse)
library(janitor)
library(tidycensus)
library(shinyWidgets)



# Defining data
allegation_link = "data_allegation.csv"
personnel_link = "data_personnel.csv"
agency_locations_link = "data_agency-reference-list.csv"
history_ids_link = "data_post-officer-history.csv"

# Loading data
allegations <- read_csv(here::here(allegation_link))
personnel <- read_csv(here::here(personnel_link))
agency_locations <- read_csv(here::here(agency_locations_link))
history_ids <- read_csv(here::here(history_ids_link))

# Renaming "agency_slug" as "agency"
agency_locations <- agency_locations %>%
  rename(agency = agency_slug)

# Creating the misconduct training dataframe
df <- personnel %>%
  
  # Uniting names into a fullname
  unite(col = 'full_name', 
        c('first_name', "middle_name", 'last_name'), 
        sep = " ", 
        na.rm = TRUE, 
        remove = FALSE) %>%
  
  # Joining personnel with historical IDs
  left_join(history_ids %>% 
              select("uid", "history_id"), 
            by = "uid") %>%
  
  # Joining personnel with their allegations
  right_join(allegations, 
             by = c("uid")) %>%
  select(-ends_with(".y")) %>%
  rename("sex" = sex.x,
         "race" = race.x,
         "agency" = agency.x) %>%
  
  # Joining personnel and allegations with the agency locations
  left_join(agency_locations, 
            by = "agency") %>%
  
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
  filter(!is.na(agency_name))


counting_allegations <- df %>%
  tabyl(uid)


library(shiny)
library(shinyWidgets)
library(DT)
library(shiny)
library(dplyr)
library(DT)
library(shinyWidgets)
library(shinyjs)

# Assuming df is your data frame

library(shiny)
library(shinyWidgets)
library(DT)
library(shiny)
library(dplyr)
library(DT)
library(shinyWidgets)
library(shinyjs)  # Don't forget to include shinyjs

# Assuming df is your data frame

ui <- fluidPage(
  useShinyjs(),  # Add this line to use shinyjs
  extendShinyjs(text = "shinyjs.page.fill();", functions = NULL),  # Add this line to fill the entire page
  
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Source+Serif+Pro:wght@400;600&display=swap"),
    tags$style(HTML("
      body {
        font-family: 'Source Serif Pro', sans-serif;
      }
      label {
        color: #0055AA; /* Set label color to blue */
        font-size: 25px;
        font-weight: bold;
      }
      select, .selectize-input {
        background-color: #0055AA !important; /* Set selector background color to blue */
        color: #FFFFFF !important; /* Set selector text color to white */
        border: 1px solid #0055AA !important; /* Set selector border color to blue */
        border-radius: 4px !important; /* Optional: Add border radius for rounded corners */
      }
      .selectize-control.single .selectize-input {
        height: 34px !important; /* Set the height of the single select input */
      }
    "))
  ),
  
  titlePanel(""),
  mainPanel(
    fluidRow(
      column(3, 
             pickerInput("selection",
                         "Agency",
                         choices = unique(df$agency_name),
                         selected = unique(df$agency_name),
                         options = list(`actions-box` = TRUE), multiple = TRUE)),
      column(3, 
             selectInput("allegation",
                         "Allegation",
                         choices = c("All", unique(df$allegation)))),
      column(3, 
             selectInput("disposition",
                         "Disposition",
                         choices = c("All", unique(df$disposition)))),
      column(3, 
             selectInput("action",
                         "Repercussion",
                         choices = c("All", unique(df$action)))),
      column(3,
             selectInput("officer_name",
                         "Name",
                         choices = c("All", unique(df$full_name)))),
      column(3,
             selectInput("officer_race",
                         "Race",
                         choices = c("All", unique(df$race)))),
      column(3,
             selectInput("officer_gender",
                         "Gender",
                         choices = c("All", unique(df$sex)))),
      column(12, 
             verbatimTextOutput("percentageText")),  # Display percentage here
      DTOutput("dataTable"),
      downloadButton("downloadData", "Download Data")
    )
  )
)

server <- function(input, output, session) {
  # Define a reactive expression for filtering data based on selected officer and agency
  filtered_df <- reactive({
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
  
  output$dataTable <- renderDT({
    datatable(
      filtered_df(),
      options = list(pageLength = 10),
      #options = list(rowCallback = JS(rowCallback))
    ) 
  })
  
  # Display the percentage of all allegations
  output$percentageText <- renderText({
    total_rows <- nrow(df)
    filtered_rows <- nrow(filtered_df())
    percentage <- ifelse(total_rows > 0, filtered_rows / total_rows * 100, 0)
    paste("Percentage of all allegations: ", round(percentage, 2), "%")
  })
  
  # Download handler for the CSV file
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
