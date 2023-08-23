# ------------------------------------- Loading Packages and Data ------------------------------------------

# Loading Libraries
library(tidyverse)
library(lubridate)
library(janitor)
library(zoo)
library(scales)
library(openxlsx)

# Defining Links
allegation_link = "original_data/misconduct_original_data/data_allegation.csv"
personnel_link = "original_data/misconduct_original_data/data_personnel.csv"
agency_locations_link = "original_data/misconduct_original_data/data_agency-reference-list.csv"
history_ids_link = "original_data/misconduct_original_data/data_post-officer-history.csv"

# Reading in data
allegations <- read_csv(here::here(allegation_link))
personnel <- read_csv(here::here(personnel_link))
agency_locations <- read_csv(here::here(agency_locations_link))
history_ids <- read_csv(here::here(history_ids_link))

# ------------------------------------- Text Categories and Key Words --------------------------------------


# Law Enforcement Agencies key words
constable <- c("constable")
marshal <- c("marshal")
police_department <- c("department", "police department")
sheriff <- c("sheriff")
university <- c("university", "college", "campus", "lsu", "uno", "usl", "ula", "lsuhc")

# Allegations key words
alcohol_substance <- c("narcotis", "med abuse", "substance", 
                       "drug", "alcohol", "dwi", "intoxicat")
domestic_violence <- c("domestic")
falsifying_reports <- c("lying", "lied", "obstruction", "truthfulness",
                        "falsification", "dishonesty", "falsifying", "perjury", 
                        "falsified", "fictitious", "inaccurate report", 
                        "falssifying", "false", "falsifaction", "falisfy")
harassment_intimidation <- c("intimidat", "threat", "harassment", "harrassment", 
                             "retaliation", "display of")
improper_equipment <- c("reckless", "misuse", "firearms violation", "speed", 
                        "crash", "damage", "activate", "storage of weapons", 
                        "careless operation", "vehicle", "body cam", "operation", 
                        "equipment", "body worn", "taser", "department property", 
                        "departmental property", "recording", "computer", 
                        "background checks", "e-mail", "email", "firearm", 
                        "gun", "ballistic vest", "cellular", "cell phone", 
                        "police car", "photo", "driving", "accident", "radio", 
                        "siren", "emergency light", "laptop", "fuel", "internet",
                        "armor")
mishandling_evidence <- c("damaging public record", "mishandling", "confiscat", 
                          "evidence", "inmate records", "loss of", "secur")
profiling <- c("racial", "stereotyp", "civil right", "bias")
sexual_assault <- c("sexual battery", "rape")
sexual_harassment <- c("sexual harassment", "sexual conduct", 
                       "sexual misconduct", "sexual harrassment")
wrongful_interference <- c("improper", "interfer", "alarming", "police action", 
                           "intervention", "false arrest", "false search", 
                           "false stop", "unlawful", "illegal stop",
                           "illegal search", "illegal arrest", 
                           "false accusation", "failure to honor", 
                           "unauthorize", "wrongful")
use_of_force <- c("shoot", "battery", "assault", "excessive forcee", "ois",
                  "force", "unnecessary discharge", "hanging", "murder", 
                  "unnecessary", "ods")
professionalism <- c("failure to", "reporting for duty", "unsatisfactory", 
                     "courtesy", "conduct", "professional", "performance",
                     "neglect", "relations", "punctuality", "of forms", 
                     "uniform", "failure to submit", "temper", 
                     "carry out order", "carrying out order", "respect", 
                     "shirking", "sick leave", "absent", "awol", "cowardize",
                     "insubordination", "tobacco", "superior", "absence", 
                     "ethic", "attend", "chain of command", "unbecoming", 
                     "supervisor", "completion", "abandon", "tardiness", 
                     "tardy", "abuse of", "violation of rules", 
                     "association", "attire", "behav", "discourt", 
                     "ceasing", "rude", "cooperat", "confidential", 
                     "control of", "courage", "dereliction", 
                     "devoting entire time", "did not", "disobey", 
                     "disrespect", "disrupt", "inadequate", "immoral", 
                     "disregard", "sleep", "reasonable", "fit", "fraterniz",
                     "gossip", "handling", "deficiency", "inattention", 
                     "indecent", "instructions", "internal affairs",
                     "knowledge", "lack of", "late", "leaving", "loyalty",
                     "shortage", "missed", "neatness", "no show", "tabacco", 
                     "social network", "personal", "procedure", 
                     "policy violation", "prohibited disc", "pursuit",
                     "relations with", "relationship with", "submission of", 
                     "treatment", "unauthorize")

# Dispositions key words
not_sustained <- c("not sustained", "exonerate", "un-substantiated", 
                   "cleared", "unsustain", "unsubstantiat", "dismiss", 
                   "justif", "non-sustain","insufficient", "no criminal",
                   "invalid", "merited", "no fault","exonerated", "rui", 
                   "terminated all charge", "non sustain")
pending <- c("pend")
sustained <- c("sustain", "founded", "at fault")
unfounded <- c("unfound")
withdrawn <- c("withdraw", "withdrew", "abandon")
type_action <- c( "suspen", "convic", "resign", "terminat",
                  "warn", "reprimand", "criminal")


# Actions key words
arrest <- c("arrest")
counseling <- c("counsel")
decertification <- c("decert", "desert")
demotion <- c("demot")
loss_of_unit_privileges <- c("loss of", "unit privilege", 
                             "vehicle use", "day vehicle")
none <- c("charges dismiss", "expung", "life lesson", "case termination", 
          "no action", "justif", "no discipline", "none", "no disposition", 
          "clear")
pay_reduction <- c("reduction")
pending <- c("pend")
resignation <- c("resig", "retire", "quit")
suspension <- c("suspen")
terminated <- c("termin", "fired", "discharg")
training <- c("school", "train", "attend class")
transfer <- c("transfer")
written_or_verbal_warning <- c("written", "verbal", "warn", "letter", 
                               "reprimand", "admonish", "talk")
type_disposition <- c("exonerat", "unfound", "withdraw", "unfound")


# ------------------------------------- Cleaning Data Process ----------------------------------------------

# Defining an allegation check function to increase workflow
allegation_check <- function(data, category){
  ifelse(str_detect(paste(data$allegation, data$allegation_desc, data$allegation_sub_desc), 
                    paste(category, collapse = "|")),
         "Yes", 
         NA)
}

# Defining an action check function to increase workflow
action_check <- function(data, category){
  ifelse(str_detect(data$action, 
                    paste(category, collapse = "|")),
         "Yes", 
         NA)
}

# Renaming "agency_slug" as "agency" in the location data so that we can merge below
agency_locations <- agency_locations %>%
  rename(agency = agency_slug)

# Defining "misconduct" as the primary dataframe to build visualizations
misconduct <- personnel %>%
  
# Creating names of officers
  unite(col = 'full_name', 
        c('first_name', "middle_name", 'last_name'), 
        sep = " ", 
        na.rm = TRUE, 
        remove = FALSE) %>%
  
# Joining with historical ids to remove duplicate ids for the same person
  left_join(history_ids %>% 
              select("uid", "history_id"), by = "uid") %>%
  
# Joining with allegations, removing repeat variables, and renaming variables
  right_join(allegations, 
             by = c("uid")) %>%
  select(-ends_with(".y")) %>%
  rename("sex" = sex.x,
         "race" = race.x,
         "agency" = agency.x) %>%
  left_join(agency_locations, 
            by = "agency") %>%
  mutate(

# Creating an index
    index = row_number(),
    
# Replacing uid with history id where possible
    uid = ifelse(!is.na(history_id), history_id, uid),
    
# Replacing all empty actions with action in the disposition category 
    action = ifelse(is.na(action) & 
                    str_detect(disposition, paste(type_action, collapse = "|")), 
                    disposition,
                    action),
    
# Replacing all empty dispositions with dispositions in the action category 
    disposition = ifelse(is.na(disposition) & 
                         str_detect(action, paste(type_disposition, collapse = "|")), 
                         action,
                         disposition),

# Creating a no allegation reported column
    allegation_type_no_reported = ifelse(is.na(allegation), "Yes", NA),
    
# Creating allegation variables that check for key words in each allegation and assign allegations variables to them
    allegation_type_alcohol_substance = allegation_check(., alcohol_substance),
    allegation_type_domestic_violence = allegation_check(., domestic_violence),
    allegation_type_falsifying_reports = allegation_check(., falsifying_reports),
    allegation_type_harassment_intimidation = allegation_check(., harassment_intimidation),
    allegation_type_improper_equipment = allegation_check(., improper_equipment),
    allegation_type_mishandling_evidence = allegation_check(., mishandling_evidence),
    allegation_type_profiling = allegation_check(., profiling),
    allegation_type_sexual_assault = allegation_check(., sexual_assault),
    allegation_type_sexual_harassment = allegation_check(., sexual_harassment),
    allegation_type_wrongful_interference = allegation_check(., wrongful_interference),
    allegation_type_use_of_force = allegation_check(., use_of_force),
    allegation_type_professionalism = allegation_check(., professionalism),

# Create other column for allegations
    allegation_type_other = ifelse(
      !is.na(allegation) &
      rowSums(across(allegation_type_alcohol_substance:allegation_type_professionalism, is.na)) == ncol(across(allegation_type_alcohol_substance:allegation_type_professionalism)),
      "Yes", 
      NA
    ),

# Creating a no action reported column
    action_type_no_reported = ifelse(is.na(action), "Yes", NA),
    
# Creating action variables that check for key words in each action and assign action variables to them
    action_type_arrest = action_check(., arrest),
    action_type_counseling = action_check(., counseling),
    action_type_decertification = action_check(., decertification),
    action_type_demotion = action_check(., demotion),
    action_type_loss_of_unit_privileges = action_check(., loss_of_unit_privileges),
    action_type_none = action_check(., none),
    action_type_pay_reduction = action_check(., pay_reduction),
    action_type_pending = action_check(., pending),
    action_type_resignation = action_check(., resignation),
    action_type_suspension = action_check(., suspension),
    action_type_terminated = action_check(., terminated),
    action_type_training = action_check(., training),
    action_type_transfer = action_check(., transfer),
    action_type_written_or_verbal_warning = action_check(., written_or_verbal_warning),

# Create other column for actions
    action_type_other = ifelse(
      !is.na(action) &
        rowSums(across(action_type_arrest:action_type_written_or_verbal_warning, is.na)) == ncol(across(action_type_arrest:action_type_written_or_verbal_warning)),
      "Yes", 
      NA
    ),
    
# Assigning dispositions to categories using key words (disposition categories are mutually exclusive)
    disposition_category = ifelse(str_detect(disposition, paste(not_sustained, collapse = "|")), "Not Sustained",
                                  ifelse(str_detect(disposition, paste(sustained, collapse = "|")), "Sustained",
                                         ifelse(str_detect(disposition, paste(unfounded, collapse = "|")), "Unfounded",
                                                ifelse(str_detect(disposition, paste(withdrawn, collapse = "|")), "Withdrawn",
                                                       ifelse(str_detect(disposition, paste(pending, collapse = "|")), "Pending",
                                                              "Other Disposition"
                                                       ))))),

# Renaming NA dispositions
    disposition_category = ifelse(is.na(disposition_category), "No Disposition Reported", disposition_category),

# Assigning agencies to categories using key words (agency categories are mutually exclusive)
    agency_type = ifelse(str_detect(tolower(agency_name), paste(university, collapse = "|")), "University or Campus Police",
                          ifelse(str_detect(tolower(agency_name), paste(marshal, collapse = "|")), "Marshal's Office",
                                  ifelse(str_detect(tolower(agency_name), paste(constable, collapse = "|")), "Constable's Office",
                                         ifelse(str_detect(tolower(agency_name), paste(sheriff, collapse = "|")), "Sheriff's Office",
                                                ifelse(str_detect(tolower(agency_name), paste(police_department, collapse = "|")), "Police Department",
                                                       "Other Law Enforcement Agency"
                                   )))))
)

# Creating a long dataframe for allegations as a single case 
misconduct_allegation_long <- misconduct %>%
  mutate(across(starts_with("allegation_type"), ~ ifelse(. == "Yes", cur_column(), NA))) %>%
  pivot_longer(
    cols = starts_with("allegation_type"),
    names_to = "allegation_category",
    values_to = "value",
    values_drop_na = TRUE
  ) %>%
  mutate(allegation_category = case_when(
    allegation_category == "allegation_type_alcohol_substance" ~ "Alcohol and Substance Abuse",
    allegation_category == "allegation_type_domestic_violence" ~ "Domestic Violence",
    allegation_category == "allegation_type_falsifying_reports" ~ "Falsifying Reports",
    allegation_category == "allegation_type_harassment_intimidation" ~ "Harassment and Intimidation",
    allegation_category == "allegation_type_improper_equipment" ~ "Improper Equipment Use",
    allegation_category == "allegation_type_mishandling_evidence" ~ "Mishandling Evidence",
    allegation_category == "allegation_type_profiling" ~ "Profiling",
    allegation_category == "allegation_type_sexual_assault" ~ "Sexual Assault",
    allegation_category == "allegation_type_sexual_harassment" ~ "Sexual Harassment",
    allegation_category == "allegation_type_wrongful_interference" ~ "Wrongful Interference",
    allegation_category == "allegation_type_use_of_force" ~ "Use of Force",
    allegation_category == "allegation_type_professionalism" ~ "Professional Misconduct",
    allegation_category == "allegation_type_other" ~ "Other Allegation",
    allegation_category == "allegation_type_no_reported" ~ "No Allegation Reported",
    TRUE ~ allegation_category
  ),
  allegation_category_simplified = ifelse(allegation_category %in% c("Domestic Violence",
                                                                     "Sexual Assault",
                                                                     "Use of Force"),
                                          "Physical Violence",
                                          ifelse(allegation_category %in% c("Harassment and Intimidation",
                                                                            "Sexual Harassment"),
                                                 "Harassment", 
                                                 ifelse(allegation_category == "No Allegation Reported", "No Allegation Reported",
                                                        "Other Allegation")))
  ) %>%
  select(-c("value"))
  

misconduct_action_long <- misconduct %>%
  mutate(across(starts_with("action_type"), ~ ifelse(. == "Yes", cur_column(), NA))) %>%
  pivot_longer(
    cols = starts_with("action_type"),
    names_to = "action_category",
    values_to = "value",
    values_drop_na = TRUE
  ) %>%
  mutate(action_category = case_when(
    action_category == "action_type_arrest" ~ "Arrested",
    action_category == "action_type_counseling" ~ "Received Counseling",
    action_category == "action_type_decertification" ~ "Decertified",
    action_category == "action_type_demotion" ~ "Demoted",
    action_category == "action_type_loss_of_unit_privileges" ~ "Lost Unit Privileges",
    action_category == "action_type_none" ~ "No Repercussion Given",
    action_category == "action_type_pay_reduction" ~ "Pay Reduced",
    action_category == "action_type_pending" ~ "Pending Repercussion",
    action_category == "action_type_resignation" ~ "Resigned",
    action_category == "action_type_suspension" ~ "Suspended",
    action_category == "action_type_terminated" ~ "Terminated",
    action_category == "action_type_training" ~ "Received Training",
    action_category == "action_type_transfer" ~ "Transfered to a New Agency",
    action_category == "action_type_written_or_verbal_warning" ~ "Received a Warning",
    action_category == "action_type_no_reported" ~ "No Repercussion Reported",
    action_category == "action_type_other" ~ "Other Repercussion",
    TRUE ~ action_category
  ),
  action_category_simplified = ifelse(action_category %in% c("Terminated","Suspended"),
                                          "Terminated or Suspended",
                                          ifelse(action_category == "Other Repercussion",
                                                 "Other Repercussion", 
                                                 ifelse(action_category == "No Action Reported", "No Action Reported",
                                                        ifelse(action_category %in% c("Arrested", "Decertified"),
                                                               "External Repercussion",
                                                               "Corrective Measures")
                                                 )
                                          )
  ))%>%
  select(-c("value"))


# ------------------------------------- Data Analysis Process ----------------------------------------------

# Number of allegations in the data
n_misconduct <- nrow(misconduct)

# Number of police officers in the misconduct data
n_police <- length(unique(misconduct$uid))

# Number of police departments
n_departments <- length(unique(misconduct$agency_name))

# Representation of police departments in the data
department_count <- misconduct %>% 
  tabyl(agency_name) %>%
  arrange(desc(n))

# Defining a function to get the percent reported 
get_pct_var_reported <- function(group, variable) {
  misconduct %>%
    tabyl({{ group }}, {{ variable }}) %>%
    clean_names() %>%
    mutate(non_na_sum = rowSums(across(!c({{ group }}, na))),
           var_reported = if_else(non_na_sum > 0, TRUE, FALSE)) %>%
    tabyl(var_reported)
}

# % of agencies that reported race
get_pct_var_reported(agency_name, race)

# % of agencies that reported gender
get_pct_var_reported(agency_name, sex)

# % of officers whose names are included
get_pct_var_reported(index, full_name)

# % of allegations that have reported repercussions
get_pct_var_reported(index, action)

# Incompleteness scores for each department. The scoring works by
# counting the number of incomplete values for the officers full name,
# birth year, race, gender, allegation, and disposition. We then sum this value
# for each row and average across for each department to give each department
# a score. 
incompleteness_score <- misconduct %>%
  mutate(empty_count = rowSums(across(all_of(c("full_name",
  "birth_year", "race", "sex", "allegation", "disposition")), is.na))) %>%
  group_by(agency_name) %>%
  summarize(score = mean(empty_count)) %>%
  mutate(score = rescale(-score)*100) %>%
  arrange(desc(score))

# Number of officers by race 
officer_race_count <- misconduct %>%
  distinct(uid, .keep_all = T) %>%
  tabyl(race)

# Number of officers by gender 
officer_sex_count <- misconduct %>%
  distinct(uid, .keep_all = T) %>%
  tabyl(sex)

# People with most allegations of misconduct per department
people_most_allegations <- misconduct %>%
  group_by(uid) %>%
  summarize(name = unique(full_name),
            total = n()) %>%
  distinct(uid, .keep_all = TRUE)

# Percent of officers who are male
pct_officers_male <- officer_sex_count %>%
  filter(sex == "Male") %>%
  adorn_pct_formatting() %>%
  pull(valid_percent)

# Percent of officers who are black
pct_officers_black <- officer_race_count %>%
  filter(race == "Black") %>%
  adorn_pct_formatting() %>%
  pull(valid_percent)

# Percent of officers who are white
pct_officers_white <- officer_race_count %>%
  filter(race == "White") %>%
  adorn_pct_formatting() %>%
  pull(valid_percent)

# Average number of allegations per officer
average_allegations <- mean(tabyl(misconduct$uid)$n)

# Number of allegations in each category
allegation_distribution <- misconduct_allegation_long %>%
  filter(allegation_category != "No Allegation Reported") %>%
  tabyl(allegation_category)

# Number of disposition in each category
disposition_distribution <- misconduct %>%
  filter(disposition_category != "No Disposition Reported") %>%
  tabyl(disposition_category)

# Number of internal repercussions in each category
internal_repercussion_distribution <- misconduct_action_long %>%
  filter(!(action_category %in% c("Arrested", 
                                  "Decertified", 
                                  "No Repercussion Reported"))) %>%
  tabyl(action_category)

# Number of outside repercussions in each category
outside_repercussion_distribution <- misconduct_action_long %>%
  filter(action_category %in% c("Arrested", "Decertified")) %>%
  tabyl(action_category)

# Allegations by department type
allegation_department_type <- misconduct_allegation_long %>%
  filter(allegation_category != "No Allegation Reported") %>%
  tabyl(allegation_category_simplified, agency_type)

# Dispositions by department type
disposition_department_type <- misconduct %>%
  filter(disposition_category != "No Disposition Reported") %>%
  tabyl(disposition_category, agency_type)

# Internal Repercussions by department type
internal_repercussion_department_type <- misconduct_action_long %>%
  filter(!(action_category %in% c("No Repercussion Reported"))) %>%
  tabyl(action_category_simplified, agency_type)

# External Repercussions by department type
external_repercussion_department_type <- misconduct_action_long %>%
  filter(action_category %in% c("Arrested","Decertified")) %>%
  tabyl(action_category, agency_type)



# Number of use of force allegations
n_uof <- allegation_distribution %>%
  filter(allegation_category == "Use of Force") %>%
  pull(n)

# Percent of allegations that were sustained
pct_sustained <- disposition_distribution %>%
  filter(disposition_category == "Sustained") %>%
  adorn_pct_formatting() %>%
  pull(percent)

# Number of terminations
n_terminated <- internal_repercussion_distribution %>%
  filter(action_category == "Terminated") %>%
  pull(n)


# Percent of allegations terminated
pct_terminated <- internal_repercussion_distribution %>%
  filter(action_category == "Terminated") %>%
  adorn_pct_formatting() %>%
  pull(percent)

# Number of arrests
n_arrests <- outside_repercussion_distribution %>%
  filter(action_category == "Arrested") %>%
  adorn_pct_formatting() %>%
  pull(n)

# Percent arrested
n_arrests <- outside_repercussion_distribution %>%
  filter(action_category == "Arrested") %>%
  adorn_pct_formatting() %>%
  pull(percent)

# Percent of each disposition by allegation type
sustain_status_by_allegation_percent <- misconduct_allegation_long %>%
  tabyl(disposition_category,allegation_category) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 2, affix_sign = FALSE)

# Number of sustained per allegation type
sustained_by_allegation <- misconduct_allegation_long %>%
  tabyl(allegation_category, disposition_category) %>%
  adorn_totals("col") %>%
  select(c("allegation_category", "Sustained", "Total"))

# Distribution of internal repercussions by misconduct type
repercussion_by_allegation <- merge(misconduct_action_long, misconduct_allegation_long, by = "index") %>% 
  filter(!(action_category %in% c("Arrested", 
                                  "Decertified"))) %>%
  tabyl(allegation_category, action_category)


the <- merge(misconduct_action_long, misconduct_allegation_long, by = "index") %>% 
  tabyl(allegation_category, action_category_simplified)

# Long distribution of repercussions by misconduct type
long_repercussion_by_allegation <- repercussion_by_allegation %>%
  pivot_longer(.,!allegation_category, names_to = "action_category", values_to = "count") %>%
  filter(allegation_category != "No Allegation Reported") %>%
  filter(action_category != "No Repercussion Reported") %>%
  filter(count != 0)

# Distribution of outside repercussions by misconduct type
outside_repercussion_by_allegation <- merge(misconduct_action_long, misconduct_allegation_long, by = "index") %>% 
  filter(action_category %in% c("Arrested", "Decertified")) %>%
  tabyl(allegation_category, action_category)

# Allegation types and count by police department
allegation_by_pd <- misconduct_allegation_long %>%
  tabyl(agency_name, allegation_category_simplified) %>%
  mutate(across(everything(), ~ifelse(. == "0", "", .)))

write.csv(outside_repercussion_by_pd, "the.csv")

# Disposition types and count by police department
disposition_by_pd <- misconduct %>%
  tabyl(agency_name, disposition_category) %>%
  mutate(across(everything(), ~ifelse(. == "0", "", .)))

# Internal repercussion types and count by police department
internal_repercussion_by_pd <- misconduct_action_long %>%
  tabyl(agency_name, action_category_simplified) %>%
  mutate(across(everything(), ~ifelse(. == "0", "", .)))

write.csv(outside_repercussion_by_pd, "the.csv")

# Outside repercussion types and count by police department
outside_repercussion_by_pd <- misconduct_action_long %>%
  mutate(action_category = ifelse(!(action_category %in% c("Arrested", "Decertified")),
                                  "No External Repercussion Reported",
                                  action_category),
         action_category = ifelse(action_category == "No Repercussion Reported", 
                                  "No External Repercussion Reported",
                                  action_category)) %>%
  tabyl(agency_name, action_category) %>%
  mutate(across(everything(), ~ifelse(. == "0", "", .)))

# Defining the unique allegation types
unique_allegations <- allegation_distribution %>%
  pull(allegation_category)

# Defining lists for the loop below
officer_allegation_list = list()
agency_allegation_list = list()

# Loop
for (allegation_value in unique_allegations){
  
  # Officers with the most of the given allegation
  people_per_allegation <- misconduct_allegation_long %>%
    filter(allegation_category == allegation_value) %>%
    distinct(uid, .keep_all = TRUE) %>%
    group_by(agency_name, full_name) %>%
    summarize(n = n()) %>%
    arrange(desc(n), .by_group = T) %>%
    head(250)
  
  officer_allegation_list[[allegation_value]] <- people_per_allegation
  
  # Agencies with the highest percent of the given allegation
  percent_allegation_by_agency <- misconduct_allegation_long %>% 
    group_by(agency_name) %>%
    summarize(percent_allegation = sum(allegation_category == allegation_value)/n()*100) %>%
    arrange(desc(percent_allegation))
  
  agency_allegation_list[[allegation_value]] <- percent_allegation_by_agency
}

agency_allegation_list$`Alcohol and Substance Abuse`


unique_agencies <- sort(unique(misconduct$agency_name))

officers_misconduct <- list()

for (chosen_agency in unique_agencies) {
  short_name <- str_sub(chosen_agency, 1, 31)
  df <- misconduct %>%
    filter(agency_name == chosen_agency) %>%
    group_by(full_name) %>%
    count() %>%
    arrange(desc(n)) %>% 
    head(250) %>%
    rename(!!chosen_agency := n)  # Use the !! operator for dynamic renaming
  
  officers_misconduct[[short_name]] <- df
}

write.xlsx(officers_misconduct, "the.xlsx")


write.csv(department_count, "the.csv")
misconduct %>%
  filter(agency_name == "New Orleans Police Department") %>%
  group_by(full_name) %>%
  count() %>%
  arrange(desc(n)) %>% 
  head(250) %>%
  rename(n = agency_name)


number_misconduct = list()

for (chosen_agency in unique_agencies) {
  short_name <- str_sub(chosen_agency, 1, 31)
  n = nrow(filter(misconduct, agency_name == chosen_agency))
  
  
  officers_misconduct[[short_name]] <- df
}


nrow(filter(misconduct, agency_name == "New Orleans Police Department"))

