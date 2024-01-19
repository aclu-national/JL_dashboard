# ------------------------------------- Loading Libraries and Data ------------------------------------------
library(tidyverse)
library(janitor)
library(scales)

# Defining data
allegation_link = "data/misconduct_data/data_allegation.csv"
personnel_link = "data/misconduct_data/data_personnel.csv"
agency_locations_link = "data/misconduct_data/data_agency-reference-list.csv"
history_ids_link = "data/misconduct_data/data_post-officer-history.csv"

# Loading data
allegations <- read_csv(here::here(allegation_link))
personnel <- read_csv(here::here(personnel_link))
agency_locations <- read_csv(here::here(agency_locations_link))
history_ids <- read_csv(here::here(history_ids_link))

# ------------------------------------- Defining Functions ------------------------------------------

# Classifying repercussions
repercussion_classifier <- function(df){
  df %>% 
    mutate(
      "Terminated" = str_detect(action, "discharge|terminat|separat|seperat") & !str_detect(action, "resign"),
      "Arrested or Convicted" = str_detect(action, "arrested|convict"),
      "Resigned" = str_detect(action, "quit|resign|resgined|retired"),
      "Decertified" = str_detect(action, "decertified|decertification"),
      "Suspended" = str_detect(action, "suspension|suspen"),
      "Warned" = str_detect(action, "verbal conference|write-up|letter|written|warn|caution|talked to about"),
      "Reprimanded" = str_detect(action, "admonished|repremand|reprimad|reprimand"),
      "Transferred" = str_detect(action, "transfer"),
      "Trained" = str_detect(action, "train|school|course|class|intervention|diversity"),
      "Counseled" = str_detect(action, "counsel"),
      "Demoted" = str_detect(action, "demot"),
      "Lost Privileges" = str_detect(action, "loss unit|loss of|unit privileges|vehicle|loss take home unit|exduty loss"),
      "Pending Repercussion" = str_detect(action, "pend"),
      "No Consequence" = str_detect(action, "none|no action|life lesson learned from experience|no discipline"),
      "Pay Reduced" = str_detect(action, "pay|without pay|reduction|deducted"),
      "Investigated" = str_detect(action, "investigation"),
      "No Repercussion Reported" = is.na(action)
    ) %>%
    mutate("Miscellaneous Repercussion" = ifelse(
      `No Repercussion Reported` == FALSE & rowSums(select(., "Terminated":"No Repercussion Reported"), na.rm = TRUE) == 0,
      TRUE,
      FALSE
    )) %>%
    mutate(across("Terminated":"Miscellaneous Repercussion", ~ifelse(. == FALSE | is.na(.), 0, 1)))
}

# Classifying Dispositions
disposition_classifier <- function(df){
  df %>%
    mutate(
      "Sustained" = disposition %in% c("at fault", "not sustained; sustained", "sustained; dui", "sustained; deceased", "sustained; retired under investigation", "sustained", "sustained; resigned while under investigation", "sustained; resigned", "sustained; dismissed", "founded", "founded/preventable class 2", "founded reduced to letter"),
      "Unfounded" = str_detect(disposition, "unfounded|unsubstantiated"),
      "Not Sustained" = disposition %in% c("federal civil rights suit dismissed by court drug charges against complainant dismissed by district attorney", "cleared of any wrongdoing involing the incident", "no criminal or administrative violation", "no fault", "unsubstantiat", "non sustained", "cleared", "cleared of any wrongdoing", "un-substantiated", "no violations observed", "not sustained; rui", "terminated all charges", "not sustained", "non-sustained", "unsustained", "insufficient evidence to sustain"),
      "Withdrawn" = str_detect(disposition, "withdraw|withdrew"),
      "Settlement Negotiated" = str_detect(disposition, "negotiate|settlement"),
      "No Investigation Merited" = disposition %in% c("no further investigation merited", "no investigation merited"),
      "Pending Investigation" = str_detect(disposition, "pending"),
      "Repercussion" = str_detect(disposition, "resign|suspend|suspension|reprimand|transfer|convicted") | disposition %in% c("terminated; write up attached", "termination", "terminated; arrested; pled guilty to simple battery on 6/1/2021 in criminal court"),
      "Exonerated" = disposition == "exonerated",
      "Admin Review" = disposition == "admin review",
      "Cancelled" = disposition %in% c("cancelled", "abandoned"),
      "No Disposition Reported" = is.na(disposition)
    ) %>%
    mutate("Miscellaneous Disposition" = ifelse(
      `No Disposition Reported` == FALSE & rowSums(select(., "Sustained":"Cancelled"), na.rm = TRUE) == 0,
      TRUE,
      FALSE
    )) %>%
    mutate(across("Sustained":"Miscellaneous Disposition", ~ifelse(. == FALSE | is.na(.), 0, 1)))
}

# Classifying allegations
allegation_classifier <- function(df){
  df %>% 
    mutate(
      full_allegation = paste0(tolower(allegation), tolower(allegation_desc)),
      "Neglect of Duty" = str_detect(full_allegation,"clocking out|leaving assigned area|sleep|sick leave|time off|report for|attend|observance of work schedule|dereliction|late|punctuality|tardiness|failure to perform|puntuality|running errand|attentiv|failure to act|perform required duties|absence|shirk|attention to duty|reporting for duty|report to duty|neglect|absent without leave|awol|unexcused absence|missed court|missed city court|no show|unauthorized leaving"),
      "In Service Deficiency" = str_detect(full_allegation,"in service deficiency"),
      "Weapon Violation" = str_detect(full_allegation, "spray|weapon|tazer|ois|gun|firearm|shooting|tase"),
      "Truthfulness" = str_detect(full_allegation,"falsify|false|lied|perjury|lying|fictitious|truth|dishonesty"),
      "Adherence to Law" = str_detect(full_allegation,"unlawful|lawful order|conformance to law|conformance of law|adherence to law|adherence of law|article 28"),
      "Insubordination" = str_detect(full_allegation,"insubordination|disobey|disobedience|obey|chain of command|instructions|failure to comply|follow orders|respect of rank|carrying out orders|carry out orders"),
      "Discourtesy" = str_detect(full_allegation,"indecent behavior|courtesy|discourtesy|discourteous|respect of fellow members|langauge|rumor|profanities|temper|gossip|inappropriate statements|attitude|rude|prohibited discussion"),
      "Policy and Procedure Violation" = str_detect(full_allegation,"violation of department policy|violation of department policies|violation of rule|safety rule|disregard of agency rules|violating saftey|policies and procedures") | (str_detect(full_allegation, "policy") & str_detect(full_allegation, "procedure")),
      "Use of Force" = str_detect(full_allegation,"shooting|choking|uof|murder|kill|force|shooting|ois|murder|brutality|battery") & !str_detect(full_allegation, "sex"),
      "Substance Violation" = str_detect(full_allegation,"driving while impaired|drinking on duty|on duty drinking|alcohol|drug|substance|impaired|intoxica|contraband|cds|narcotic|dwi|tobacco"),
      "Arrest Violation" = str_detect(full_allegation,"arrest") & str_detect(full_allegation, "probable cause|no reason|improper|unnecessary|illegal|violation") | str_detect(full_allegation, "false arrest"),
      "Search Violation" = str_detect(full_allegation,"search|entry"),
      "Seizure Violation" = str_detect(full_allegation,"seizure"),
      "Detainment Violation" = str_detect(full_allegation,"detention|detain") & str_detect(full_allegation, "excessive|improper|no reason|unnecessary|illegal|violation"),
      "Investigation Violation" = str_detect(full_allegation,"investigat") & str_detect(full_allegation, "improper|interfere|illegal|handle|thorough|inappropriate"),
      "Stop Violation" = str_detect(full_allegation,"stop"),
      "Arrest or Conviction" = str_detect(full_allegation, "arrest for|arrested|conviction|convicted") & !str_detect(full_allegation, "improper|unjust|illegal"),
      "Prison-related Violation" = str_detect(full_allegation,"escape|prison|jail|inmate|custody"),
      "Discrimination" = str_detect(full_allegation,"discrimination|profiling|racial|violation of ods|ods violation|violation of civil rights|bias-free"),
      "Intimidation and Retaliation" = str_detect(full_allegation,"intimidation|threaten|threat|retaliat"),
      "Harassment" = str_detect(full_allegation,"harass") & !str_detect(full_allegation, "sexual"),
      "Sexual Harassment" = str_detect(full_allegation,"sexual") & str_detect(full_allegation, "harassment|misconduct") | str_detect(full_allegation, "penis|vagina"),
      "Sexual Assault" = str_detect(full_allegation,"sexual battery|sexual assault|rape|prea"),
      "Supervision Violation" = str_detect(full_allegation,"supervisor responsibil|supervise|supervisory|improper or lack of supervision|properly supervisor"),
      #"Failure to Report Damage" = str_detect(full_allegation,"report an accident|report damage|reporting damage|reporting incident|accident report"),
      "Handling Evidence Violation" = str_detect(full_allegation,"evidence|secure property|securing property|secure evidence|securing evidence"),
      "Equipment Misuse and Damage" = str_detect(full_allegation,"equipment|destruction|destroy|misuing|misuse|crash|vehicle accident|department property|photographing|report damage|electric control weapons"),
      "Reporting Violation" =str_detect(full_allegation, "(turn in|completion of|submit|hand in|failed|complete|submission of).*?(paperwork|report|document|form)") | str_detect(full_allegation, "report damage|make required report"),
      "Appearance and Cleanliness Violation" = str_detect(full_allegation,"cleanliness|attire|uniform|appearance|dress code|neatness"),
      "Confidentiality Violation" = str_detect(full_allegation,"discussing agency legal matters|confidential|unauthorized release"),
      "Cooperation Violation" = str_detect(full_allegation,"cooperat"),
      "Theft" = str_detect(full_allegation,"seized property|theft|missing money|misappropriation|shoplift|payroll fraud|taking money|pocket property"),
      "Traffic Violation" = str_detect(full_allegation,"operation of agency vehicle|driver|driving|dwi|dui|traffic violation|dl required|dl suspen|dmvr violation|driver's license|dl requipred|dl suspension|speed violation|excessive speed|pursuit"),
      "Abuse of Authority" = str_detect(full_allegation,"abuse of power|abuse of office|malfeasance|misuse of authority|abuse of authority|abuse of position"),
      "Associations Violation" = str_detect(full_allegation,"intimate|romantic|employee relations|fraternization|associations|associate with|fraterniz"),
      "Domestic Violence" = str_detect(full_allegation,"domestic|child abuse|abusing child") | (str_detect(full_allegation,"battery") & str_detect(full_allegation, "partner|wife|husband|child")),
      "Professionalism" = str_detect(full_allegation,"professional|unprofessional"),
      "Technology Violation" = str_detect(full_allegation, "cellular|technology|body camera|bwc|computer|red-flex|redflex|recording|phone|social media|social network|internet|video"),
      "Performance of Duty" = str_detect(full_allegation,"unsatisfactory performance|perform duties|perform duty|performance of duty|performance of duties|standard level of perfor"),
      "Conduct Violation" = str_detect(full_allegation, " conduct|conduct "),
      "No Allegation Reported" = is.na(full_allegation)
    ) %>%
    mutate("Miscellaneous Allegation" = ifelse(
      rowSums(select(., `Neglect of Duty`:`Conduct Violation`), na.rm = TRUE) == 0,
      TRUE,
      FALSE
    )) %>%
    mutate(across("Neglect of Duty":"Miscellaneous Allegation", ~ifelse(. == FALSE | is.na(.), 0, 1)))
}

# Function to remove duplicates when joining dataframes
remove_repeats <- function(df){
  df %>%
    select(-ends_with(".y")) %>%
    rename_all(~gsub("\\.x$", "", .))
}


# ------------------------------------- Cleaning Data ------------------------------------------

# Renaming "agency_slug" as "agency"
agency_locations <- agency_locations %>%
  rename(agency = agency_slug)

# Creating the misconduct training dataframe
misconduct <- personnel %>%
  
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
  select(-c("sex", "race", "agency")) %>%
  
  # Joining personnel with their allegations
  right_join(allegations, 
             by = c("uid")) %>%
  # Joining personnel and allegations with the agency locations
  left_join(agency_locations, 
            by = "agency") %>%
  mutate(
    action = tolower(action),
    disposition = tolower(disposition),
    allegation = tolower(allegation),
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
    uid = ifelse(is.na(history_id), uid, history_id),
    index = row_number())

# ------------------------------------- Creating Data ------------------------------------------

# Creating an entire dataframe
total_misconduct <- misconduct %>%
  allegation_classifier() %>%
  disposition_classifier() %>%
  repercussion_classifier()

# Creating misconduct data
misconduct_allegation <- misconduct %>%
  filter(!(is.na(allegation) & is.na(allegation_desc))) %>%
  allegation_classifier() %>%
  mutate(combined_allegations = apply(select(., 'Neglect of Duty':'Miscellaneous Allegation'), 1, function(row) {
    names_with_ones <- names(row[row == 1])
    if (length(names_with_ones) > 0) {
      paste(names_with_ones, collapse = ', ')
    } else {
      NA
    }
  }))

# Creating disposition data
misconduct_disposition <- misconduct %>%
  filter(!is.na(disposition)) %>%
  disposition_classifier() %>%
  mutate(combined_dispositions = apply(select(., 'Sustained':'Miscellaneous Disposition'), 1, function(row) {
    names_with_ones <- names(row[row == 1])
    if (length(names_with_ones) > 0) {
      paste(names_with_ones, collapse = ', ')
    } else {
      NA
    }
  }))

# Creating repercussion data
misconduct_repercussion <- misconduct %>%
  filter(!is.na(action)) %>%
  repercussion_classifier() %>%
  mutate(combined_repercussions = apply(select(., 'Terminated':'Miscellaneous Repercussion'), 1, function(row) {
    names_with_ones <- names(row[row == 1])
    if (length(names_with_ones) > 0) {
      paste(names_with_ones, collapse = ', ')
    } else {
      NA
    }
  }))

# Creating a dataframe of unique repercussion classifications
unnested_misconduct_repercussion <- misconduct_repercussion %>%
  mutate(repercussion_split = str_split(combined_repercussions, ", ")) %>%
  unnest(repercussion_split) %>%
  select(index, agency_name, repercussion_split)

# Creating a dataframe of unique disposition classifications
unnested_misconduct_disposition <- misconduct_disposition %>%
  mutate(disposition_split = str_split(combined_dispositions, ", ")) %>%
  unnest(disposition_split) %>%
  select(index, agency_name, disposition_split)

# Creating a dataframe of unique allegation classifications
unnested_misconduct_allegation <- misconduct_allegation %>%
  mutate(allegation_split = str_split(combined_allegations, ", ")) %>%
  unnest(allegation_split) %>%
  select(index, agency_name, allegation_split)


# ------------------------------------- Creating Grouped Data ------------------------------------------

# Grouping allegations and repercussions
allegation_repercussion <- inner_join(unnested_misconduct_repercussion, unnested_misconduct_allegation, by = "index", relationship = "many-to-many") %>%
  remove_repeats()

# Grouping allegations and dispositions
allegation_disposition <- inner_join(unnested_misconduct_disposition, unnested_misconduct_allegation, by = "index", relationship = "many-to-many") %>%
  remove_repeats()

# Grouping dispositions and repercussions
disposition_repercussion <- inner_join(unnested_misconduct_disposition, unnested_misconduct_repercussion, by = "index", relationship = "many-to-many") %>%
  remove_repeats()

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

# Representation of total police departments in the data
department_count_total <- agency_locations %>%
  mutate(
    agency_name = case_when(
    agency_name == "New Orleans Parish Sheriff's Office" ~ "Orleans Parish Sheriff's Office",
    agency_name == "Orleans Constable" ~ "Orleans Parish Constable",
    agency_name == "Jefferson Constable" ~ "Jefferson Parish Constable",
    TRUE ~ agency_name
    )
  ) %>% 
  left_join(department_count, by = "agency_name") %>%
  mutate(represented = ifelse(is.na(n), "Did Not Submit Misconduct Data", "Submit Misconduct Data"),
         n = ifelse(is.na(n), "", n)) %>%
  select(agency_name, n, represented, location)

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

incompleteness_score_head <- incompleteness_score %>%
  arrange(desc(score)) %>% 
  mutate(score = round(score,2)) %>%
  head(10)

incompleteness_score_tail <- incompleteness_score %>%
  arrange(score) %>%
  mutate(score = round(score,2))

misconduct %>%
  tabyl(agency_name) %>%
  arrange(desc(n)) %>%
  head(8) %>%
  pull(n) %>%
  sum()/n_misconduct

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
  summarize(Name = unique(full_name),
            Agency = unique(agency_name),
            Total = n(),
            `Allegation(s)` = paste(allegation,collapse=', ')
  ) %>%
  distinct(uid, .keep_all = TRUE) %>%
  select(-c(uid))

# Percent of officers who are male
pct_officers_male <- officer_sex_count %>%
  filter(sex == "male") %>%
  adorn_pct_formatting() %>%
  pull(valid_percent)

# Percent of officers who are black
pct_officers_black <- officer_race_count %>%
  filter(race == "black") %>%
  adorn_pct_formatting() %>%
  pull(valid_percent)

# Percent of officers who are white
pct_officers_white <- officer_race_count %>%
  filter(race == "white") %>%
  adorn_pct_formatting() %>%
  pull(valid_percent)

# Average number of allegations per officer
average_allegations <- mean(tabyl(misconduct$uid)$n)

# Number of allegations in each category
allegation_distribution <- unnested_misconduct_allegation %>%
  filter(allegation_split != "No Allegation Reported") %>%
  tabyl(allegation_split)

# Number of disposition in each category
disposition_distribution <- unnested_misconduct_disposition %>%
  filter(disposition_split != "No Disposition Reported") %>%
  tabyl(disposition_split)

# Number of internal repercussions in each category
internal_repercussion_distribution <- unnested_misconduct_repercussion %>%
  filter(!(repercussion_split %in% c("Arrested or Convicted", 
                                  "Decertified", 
                                  "No Repercussion Reported"))) %>%
  tabyl(repercussion_split)

# Number of external repercussions in each category
external_repercussion_distribution <- unnested_misconduct_repercussion %>%
  filter(repercussion_split %in% c("Arrested or Convicted",
                                "Decertified")) %>%
  tabyl(repercussion_split)


# Number of use of force allegations
n_uof <- allegation_distribution %>%
  filter(allegation_split == "Use of Force") %>%
  pull(n)

# Number of allegations that include sustained
n_sustained <- disposition_distribution %>%
  filter(disposition_split == "Sustained") %>%
  pull(n)

# Number of terminations
n_terminated <- internal_repercussion_distribution %>%
  filter(repercussion_split == "Terminated") %>%
  pull(n)

# Percent of each disposition by allegation type
sustain_status_by_allegation_percent <- allegation_disposition %>%
  tabyl(allegation_split,disposition_split) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 2, affix_sign = FALSE)

sustain_status_by_allegation_percent

# Number of sustained per allegation type
sustained_by_allegation <- allegation_disposition %>%
  tabyl(disposition_split, allegation_split)


# Distribution of internal repercussions by misconduct type
repercussion_by_allegation <- allegation_repercussion %>% 
  filter(!(repercussion_split %in% c("Arrested or Convicted", 
                                  "Decertified"
                                  ))) %>%
  tabyl(allegation_split, repercussion_split)

# External repercussions by allegation
external_repercussion_by_allegation <- allegation_repercussion %>% 
  filter((repercussion_split %in% c("Arrested or Convicted", 
                                 "Decertified"
                                 ))) %>%
  tabyl(allegation_split, repercussion_split)

# Long distribution of repercussions by misconduct type
long_repercussion_by_allegation <- repercussion_by_allegation %>%
  pivot_longer(.,!allegation_split, names_to = "repercussion_split", values_to = "count") %>%
  filter(allegation_split != "No Allegation Reported") %>%
  filter(repercussion_split != "No Repercussion Reported") %>%
  filter(count != 0) %>%
  group_by(allegation_split) %>%
  summarize(allegation = allegation_split,
            repercussion = repercussion_split,
            count = count,
            percent = count/sum(count)) %>%
  ungroup() %>%
  select(allegation, repercussion, count, percent) %>%
  mutate(percent = paste0(allegation, "/", repercussion, ": ", round(percent,2), "% (",count,")"))

df_list = list()
for (allegations in unique(long_repercussion_by_allegation$allegation)) {
  df <- filter(long_repercussion_by_allegation, allegation == allegations)
  print(df)
  df_list <- append(df_list, list(df))
}

library(openxlsx)

write.xlsx(df_list, "df.xlsx")

write.csv(long_repercussion_by_allegation, "it.csv")

 # Allegation types and count by police department
allegation_by_pd <- unnested_misconduct_allegation %>%
  tabyl(allegation_split, agency_name) %>%
  adorn_totals("col") %>%
  select(allegation_split, Total, c("13th District Attorney's Office": "Zachary Police Department")) %>%
  mutate(across(everything(), ~ifelse(. == "0", "", .))) %>%
  arrange(desc(Total))


# Disposition types and count by police department
disposition_by_pd <- unnested_misconduct_disposition %>%
  tabyl(disposition_split, agency_name) %>%
  adorn_totals("col") %>%
  select(disposition_split, Total, c("Abbeville Police Department": "Winnsboro Police Department")) %>%
  mutate(across(everything(), ~ifelse(. == "0", "", .))) %>%
  arrange(desc(Total))

# Internal repercussion types and count by police department
internal_repercussion_by_pd <- unnested_misconduct_repercussion %>%
  tabyl(repercussion_split, agency_name) %>%
  adorn_totals("col") %>%
  select(repercussion_split, Total, c("13th District Attorney's Office": "Winnsboro Police Department")) %>%
  mutate(across(everything(), ~ifelse(. == "0", "", .))) %>%
  arrange(desc(Total))


# Outside repercussion types and count by police department
outside_repercussion_by_pd <- unnested_misconduct_repercussion %>%
  mutate(repercussion_split = ifelse(!(repercussion_split %in% c("Arrested or Convicted", "Decertified")),
                                  "No External Repercussion Reported",
                                  repercussion_split)) %>%
  tabyl(agency_name, repercussion_split) %>%
  mutate(across(everything(), ~ifelse(. == "0", "", .)))

# Defining the unique allegation types
unique_allegations <- allegation_distribution %>%
  pull(allegation_split)


# Plotting confusion matrices
allegation_confusion <- total_misconduct %>%
  select("Neglect of Duty":"Miscellaneous Allegation") %>%
  as.matrix() %>%
  crossprod() %>%
  as.data.frame()

disposition_confusion <- total_misconduct %>%
  select("Sustained":"Miscellaneous Disposition") %>%
  as.matrix() %>%
  crossprod() %>%
  as.data.frame()

repercussion_confusion <- total_misconduct %>%
  select("Terminated":"Miscellaneous Repercussion") %>%
  as.matrix() %>%
  crossprod() %>%
  as.data.frame()
