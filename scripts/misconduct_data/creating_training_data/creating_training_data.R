# Loading Libraries
library(tidyverse)
library(janitor)

# Defining the 10 most prevalent law enforcement agencies in the misconduct data
# as of November 3rd, 2023. Two agencies which are not included in this list 
# include Lafourche Parish Sheriff's Office and Shreveport Police Department
# as many of their allegations are reported as numbers or similarly 
# unclassifiable strings.

it <- misconduct_allegation %>%
  select(allegation_better, allegation_classified)


top_10 <- c(
  "New Orleans Police Department",
  "Baton Rouge Police Department",
  "Orleans Parish Sheriff's Office",
  "Lafayette Police Department",
  "Bossier City Police Department",
  "St. Tammany Parish Sheriff's Office",
  "Lafayette Parish Sheriff's Office",
  "East Baton Rouge Parish Sheriff's Office",
  "Lafourche Parish Sheriff's Office"
)


# Defining data
allegation_link = "data/misconduct_data/training_data/data_allegation.csv"
agency_locations_link = "data/misconduct_data/training_data/data_agency-reference-list.csv"
lafourche_so_codebook_link = "data/misconduct_data/codebooks/lafourche_so_codebook.csv"
shreveport_pd_codebook_link = "data/misconduct_data/codebooks/shreveport_pd_codebook.csv"

# Loading data
allegations <- read_csv(here::here(allegation_link))
agency_locations <- read_csv(here::here(agency_locations_link))
lafourche_so_codebook <- read_csv(here::here(lafourche_so_codebook_link))
shreveport_pd_codebook <- read_csv(here::here(shreveport_pd_codebook_link))


# Renaming "agency_slug" as "agency"
agency_locations <- agency_locations %>%
  rename(agency = agency_slug)

# Creating the misconduct training dataframe
misconduct <- allegations %>%
  left_join(agency_locations, 
            by = "agency") %>%
  left_join(lafourche_so_codebook %>%
              mutate(allegation = str_extract(allegation, "\\d+"),
                     description = str_sub(tolower(description), 7)), 
            by = "allegation") %>%
  mutate(allegation = ifelse(is.na(description), allegation, description)) %>%
  mutate(
    agency_name = case_when(
      agency_name == "New Orleans Parish Sheriff's Office" ~ "Orleans Parish Sheriff's Office",
      agency_name == "Orleans Constable" ~ "Orleans Parish Constable",
      agency_name == "Jefferson Constable" ~ "Jefferson Parish Constable",
      TRUE ~ agency_name
    ),
    action = tolower(action),
    disposition = tolower(disposition),
    allegation = tolower(allegation),
    agency_type = case_when(
      str_detect(tolower(agency_name), "university|college|campus|lsu|uno|usl|ula|lsuhc") ~ "University or Campus Police",
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
    index = row_number(),
    allegation_multiple = case_when(
      agency_name == "St. Tammany Parish Sheriff's Office" ~ ifelse(str_count(allegation, "dr.03|dr. 03|dr.  03|sop.4")>1, "Yes", "No"),
      agency_name == "Orleans Parish Sheriff's Office" ~ ifelse(str_detect(allegation, "\\sand\\s|,|/"), "Yes", "No"),
      agency_name == "Lake Charles Police Department" ~ ifelse(str_detect(allegation, "/"), "Yes", "No"),
      agency_name %in% c("Washington Parish Sheriff's Office", 
                         "Richland Parish Sheriff's Office",
                         "Maurice Police Department") ~ ifelse(str_detect(allegation, ";"), "Yes", "No"),
      agency_name == "Orleans Levee Police Department" ~ ifelse(str_detect(allegation, ","), "Yes", "No"),
      agency_name == "Lafourche Parish Sheriff's Office" ~ ifelse(str_detect(allegation, ",| and "), "Yes", "No"),
      TRUE ~ "No"
    ),
    
    # Fixing allegations to remove unnessary words
    allegation_better = case_when(
      agency_name == "St. Tammany Parish Sheriff's Office" ~ ifelse(str_detect(sub('.*\\-', '', allegation), 
                                                                               "terminated|warning|hours|warmong|suspension|discharged"),
                                                                    trimws(sub("-[^-]*$", "", allegation)),
                                                                    allegation),
      agency_name == "Rayne Police Department" ~ sub(".*(?:officer-|officer;)", "", allegation),
      agency_name == "New Orleans Police Department" ~ trimws(sub(".*[-;:]", "", allegation)),
      agency_name == "Hammond Police Department" ~ sub(".*(?:officer-)", "", allegation),
      agency_name == "Scott Police Department" ~ ifelse(str_detect(allegation, "\\(.*\\)"), 
                                                        str_extract(allegation, "\\((.*?)\\)"),
                                                        allegation),
      agency_name == "Mandeville Police Department" ~ sub(".*(?:conduct unbecoming/|unbecoming behavior/)", "", allegation),
      agency_name == "Terrebonne Parish Sheriff's Office" ~ sub(".*(?::)", "", allegation),
      TRUE ~ allegation
    ),
    
    # Defining which action incidents have multiple allegations 
    action_multiple = case_when(
      agency_name %in% c("New Orleans Police Department", "Baton Rouge Police Department","East Baton Rouge Parish Sheriff's Office") ~ ifelse(str_detect(action, "/|;"), "Yes", "No"),
      agency_name %in% c("Washington Parish Sheriff's Office", "St. James Parish Sheriff's Office", "Denham Springs Police Department", "Eunice Police Department", "Houma Police Department", "Lafayette Police Department", "Richland Parish Sheriff's Office") ~ ifelse(str_detect(action, ";"), "Yes", "No"),
      agency_name %in% c("Ponchatoula Police Department", "Hammond Police Department") ~ ifelse(str_detect(action, "\\|"), "Yes", "No"),
      agency_name == "St. Landry Parish Sheriff's Office" ~ ifelse(str_detect(action, " and "), "Yes", "No"),
      agency_name == "Tangipahoa Parish Sheriff's Office" ~ ifelse(str_detect(action, "/"), "Yes", "No"),
      TRUE ~ "No"
    )
  )

misconduct_allegation <- misconduct %>%
  mutate(
    allegation_better = strsplit(as.character(allegation_better), "\\sand\\s|,|/|;|dr.03|dr. 03|dr.  03|sop.4")
    ) %>%
    unnest(allegation_better) %>%
    mutate(
      allegation_classified = case_when(
      
      is.na(allegation_better) ~ "No Allegation Reported",
      
      allegation_better == "conduct unbecoming" ~ "General Unbecoming Conduct",
      allegation_better == "domestic violence or harassment" ~ "Domestic Violence",
      allegation_better == "failure to complete proper investigation" ~ "Neglecting Duty",
      allegation_better == "failure to perform duties" ~ "Neglecting Duty",
      allegation_better == "failure to perform duty" ~ "General Performance of Duty",
      allegation_better == "failure to properly investigate incident" ~ "Neglecting Duty",
      allegation_better == "failure to report an accident" ~ "Misuse of Equipment and Property",
      allegation_better == "failure to supervise" ~ "Supervision",
      allegation_better == "falsified information on an accident report" ~ "Truthfulness",
      allegation_better == "falsifying police reports" ~ "Truthfulness",
      allegation_better == "firearm discharge" ~ "Misuse of Equipment and Property",
      allegation_better == "improper search" ~ "Improper Search, Seizure, Arrest, or Detainment",
      allegation_better == "in service deficiency" ~ "Effectiveness",
      allegation_better == "late reports" ~ "Completing Reports and Forms",
      allegation_better == "respect of fellow members" ~ "Courtesy",
      allegation_better == "sexual misconduct" ~ "Sexual Misconduct",
      allegation_better == "sick leave violation" ~ "Neglecting Duty",
      allegation_better == "standards of conduct" ~ "General Unbecoming Conduct",
      allegation_better == "unauthorized release of information" ~ "Dissemination of Information",
      allegation_better == "unnecessary discharge of issued firearm" ~ "Misuse of Equipment and Property",
      allegation_better == "harrassment" ~ "Harassment and Intimidation",
      allegation_better == "arrest search and seizure" ~ "Improper Search, Seizure, Arrest, or Detainment",
      allegation_better == "improper care and use of department property and funds" ~ "Misuse of Equipment and Property",
      str_detect(allegation_better, "alcohol|drug") ~ "Use of Substances",
      allegation == "obligation and performance of duty" ~ "General Performance of Duty",
      allegation_better == "absent without leave" ~ "Neglecting Duty",
      allegation_better == "abuse of authority" ~ "Abuse of Position",
      allegation_better == "abuse of position" ~ "Abuse of Position",
      allegation_better == "arrest" ~ "Miscellanious Allegation",
      allegation_better == "behavior" ~ "Courtesy",
      allegation_better == "carrying" ~ "Miscellanious Allegation",
      allegation_better == "discrimination" ~ "Discrimination",
      allegation_better == "evidence" ~ "Mishandling Assets",
      allegation_better == "failure to act" ~ "Neglecting Duty",
      allegation_better == "failure to notify supervisor" ~ "Dissemination of Information",
      allegation_better == "improper traffic stop" ~ "Improper Search, Seizure, Arrest, or Detainment",
      allegation_better == "neglect of duty" ~ "Neglecting Duty",
      allegation_better == "procedures" ~ "Policies and Procedures",
      allegation_better == "reporting" ~ "Neglecting Duty",
      allegation_better %in% c("search"," search","search ") ~ "Improper Search, Seizure, Arrest, or Detainment",
      allegation_better == "seizure" ~ "Improper Search, Seizure, Arrest, or Detainment",
      allegation_better == "sick leave" ~ "Neglecting Duty",
      allegation_better == "social media" ~ "Misuse of Equipment and Property",
      allegation_better == "submission of required forms" ~ "Completing Reports and Forms",
      allegation_better == "unprofessional" ~ "General Professionalism",
      allegation_better == "unsatisfactory performance" ~ "General Performance of Duty",
      allegation_better == "| |  " ~ "No Allegation Reported",
      allegation_better == "search" ~ "Improper Search, Seizure, Arrest, or Detainment",
      allegation_better == "chain of command" ~ "Following Instructions",
      allegation_better == "command of temper" ~ "Courtesy",
      allegation_better == "conduct" ~ "General Unbecoming Conduct",
      allegation_better == "conduct unbecoming an officer" ~ "General Unbecoming Conduct",
      allegation_better == "excessive force" ~ "Use of Force",
      allegation_better == "insubordination" ~ "Following Instructions",
      allegation_better == "payment of debts" ~ "Theft",
      allegation_better == "personal conduct" ~ "General Unbecoming Conduct",
      allegation_better == "residence" ~ "Miscellanious Allegation",
      allegation_better == "sexual harrassment" ~ "Sexual Misconduct",
      allegation_better == "submission of reports" ~ "Completing Reports and Forms",
      allegation_better == "traffic violation" ~ "Misuse of Equipment and Property",
      allegation_better == "truthfulness" ~ "Truthfulness",
      allegation_better == "use of department equipment" ~ "Misuse of Equipment and Property",
      allegation_better == "use of force" ~ "Use of Force",
      str_detect(allegation_better, "cooperat") ~ "Cooperation",
      allegation_better == "discourteous" ~ "Courtesy",
      allegation_better == "failure to act" ~ "Neglecting Duty",
      allegation_better == "procedures" ~ "Policies and Procedures",
      allegation_better == "unprofessional conduct" ~ "General Unbecoming Conduct",
      allegation_better == "procedures" ~ "Policies and Procedures",
      
      (agency_name == "New Orleans Police Department") & str_detect(allegation_better, "abuse of office|malfeasance|abuse of position") ~ "Abuse of Position",
      (agency_name == "New Orleans Police Department") & str_detect(allegation_better, "force|assault|battery") ~ "Use of Force",
      (agency_name == "New Orleans Police Department") & str_detect(allegation_better, "alcohol|drugs|tabacco|tobacco") ~ "Use of Substances",
      (agency_name == "New Orleans Police Department") & str_detect(allegation_better, "reporting for duty|neglect|ceasing to perform|devoting entire time|hours of duty|attention to duty|leaving") ~ "Neglecting Duty",
      (agency_name == "New Orleans Police Department") & str_detect(allegation_better, "adherence to law|criminal proceeding") ~ "Adherence to Law",
      (agency_name == "New Orleans Police Department") & str_detect(allegation_better, "authorized operator|camera|social network|department equipment|department property") ~ "Misuse of Equipment and Property",
      (agency_name == "New Orleans Police Department") & str_detect(allegation_better, "courage|effective|in service deficiency") ~ "Effectiveness",
      (agency_name == "New Orleans Police Department") & str_detect(allegation_better, "rules of procedures|chain of command|instructions from an authoritative source|instructions from authoritative source") ~ "Following Instructions",
      (agency_name == "New Orleans Police Department") & str_detect(allegation_better, "confidentiality") ~ "Confidentiality",
      (agency_name == "New Orleans Police Department") & str_detect(allegation_better, "cleanliness|neat|attire") ~ "Cleanliness",                                                                                                                                                                                         
      (agency_name == "New Orleans Police Department") & str_detect(allegation_better, "professionalism") ~ "General Professionalism",
      (agency_name == "New Orleans Police Department") & str_detect(allegation_better, "withholding|failure to report|false or inaccurate|fictitious|honesty|truthfulness|dishonest") ~ "Truthfulness",
      (agency_name == "New Orleans Police Department") & str_detect(allegation_better, "security of records") ~ "Mishandling Assets",
      (agency_name == "New Orleans Police Department") & str_detect(allegation_better, "courtesy") ~ "Courtesy",
      (agency_name == "New Orleans Police Department") & str_detect(allegation_better, "interfering with investigations") ~ "Interfering with Investigations",
      (agency_name == "New Orleans Police Department") & str_detect(allegation_better, "discrimination") ~ "Discrimination",
      (agency_name == "New Orleans Police Department") & str_detect(allegation_better, "complete report") ~ "Completing Reports and Forms",
      (agency_name == "New Orleans Police Department") & str_detect(allegation_better, "associations") ~ "Improper Relationships",
      (agency_name == "New Orleans Police Department") & str_detect(allegation_better, "sexual|rape") ~ "Sexual Misconduct",
      (agency_name == "New Orleans Police Department") & (str_detect(allegation_better, "intimidat|retaliat")&!str_detect(allegation_better, "sexual|rape|domestic")) ~ "Harassment and Intimidation",
      (agency_name == "New Orleans Police Department") & str_detect(allegation_better, "acting impartially") ~ "Acting Impartially",
      
      (agency_name == "Baton Rouge Police Department") & str_detect(allegation_better, "abuse of office|malfeasance|abuse of position") ~ "Abuse of Position",
      (agency_name == "Baton Rouge Police Department") & str_detect(allegation_better, "punctuality|shirking|abuse of sick leave||awol|attention to duty|carrying out order") ~ "Neglecting Duty",
      (agency_name == "Baton Rouge Police Department") & str_detect(allegation_better, "drug|alcohol|cds|narcotic|dwi|tobacco") ~ "Use of Substances",
      (agency_name == "Baton Rouge Police Department") & str_detect(allegation_better, "force") ~ "Use of Force",
      (agency_name == "Baton Rouge Police Department") & str_detect(allegation_better, "violation of law|violation of known law") ~ "Adherence to Law",
      (agency_name == "Baton Rouge Police Department") & str_detect(allegation_better, "conduct|code of conduct") ~ "General Unbecoming Conduct",
      (agency_name == "Baton Rouge Police Department") & str_detect(allegation_better, "respect|command of temper") ~ "Courtesy",
      (agency_name == "Baton Rouge Police Department") & str_detect(allegation_better, "dl required|dl suspen|dmvr violation|traffic violation|driver's license|weapon|camera|telephone|departmental equipment|department equipment|confiscated property|dl requipred|dl suspension|damaged equipment") ~ "Misuse of Equipment and Property",
      (agency_name == "Baton Rouge Police Department") & str_detect(allegation_better, "falsification|truthful|failure to report") ~ "Truthfulness",
      (agency_name == "Baton Rouge Police Department") & str_detect(allegation_better, "secure property|securing property|secure evidence|securing evidence") ~ "Mishandling Assets",
      (agency_name == "Baton Rouge Police Department") & str_detect(allegation_better, "submission of forms|submit required forms|required documents|completion of form|req form|required form|submission form|submission of form|submission report|submission of report|submission of required form") ~ "Completing Reports and Forms",
      (agency_name == "Baton Rouge Police Department") & str_detect(allegation_better, "sexual|assault") ~ "Sexual Misconduct",
      (agency_name == "Baton Rouge Police Department") & str_detect(allegation_better, "chain of command|insubordinat") ~ "Following Instructions",
      (agency_name == "Baton Rouge Police Department") & str_detect(allegation_better, "coward|in service deficiency") ~ "Effectiveness",
      (agency_name == "Baton Rouge Police Department") & str_detect(allegation_better, "escape|release of prisoner") ~ "Prisoner Escape",
      (agency_name == "Baton Rouge Police Department") & str_detect(allegation_better, "cooperation") ~ "Cooperation",
      (agency_name == "Baton Rouge Police Department") & str_detect(allegation_better, "conduct|code of conduct") ~ "General Unbecoming Conduct",
      (agency_name == "Baton Rouge Police Department") & str_detect(allegation_better, "uniform|appearance|dress code") ~ "Cleanliness",
      (agency_name == "Baton Rouge Police Department") & str_detect(allegation_better, "confidential") ~ "Confidentiality",
      (agency_name == "Baton Rouge Police Department") & str_detect(allegation_better, "associations with|assocation with|association with criminals") ~ "Improper Relationships",
      (agency_name == "Baton Rouge Police Department") & str_detect(allegation_better, "domestic") ~ "Domestic Violence",
      (agency_name == "Baton Rouge Police Department") & str_detect(allegation_better, "public statement|3:5 unauthorized||information to supervisor|info to superior|info to supervisor|info to a supervisor|info to a superior|information to superior") ~ "Dissemination of Information",
      
      (agency_name == "Orleans Parish Sheriff's Office") & str_detect(allegation_better, "malfeasance|abuse of position") ~ "Abuse of Position",
      (agency_name == "Orleans Parish Sheriff's Office") & str_detect(allegation_better, "sexual") ~ "Sexual Misconduct",
      (agency_name == "Orleans Parish Sheriff's Office") & str_detect(allegation_better, "force") ~ "Use of Force",
      (agency_name == "Orleans Parish Sheriff's Office") & str_detect(allegation_better, "alcohol|drug") ~ "Use of Substances",
      (agency_name == "Orleans Parish Sheriff's Office") & str_detect(allegation_better, "leaving assinged area|reporting|ceasing to perfrom|neglect of|reporing for|ceasing to perform|sick leave workers' compensation|late for duty|entire time to duty|attention to duty|leaving assigned area") ~ "Neglecting Duty",
      (agency_name == "Orleans Parish Sheriff's Office") & str_detect(allegation_better, "adherence to law") ~ "Adherence to Law",
      (agency_name == "Orleans Parish Sheriff's Office") & str_detect(allegation_better, "cellular|camera|departmental property") ~ "Misuse of Equipment and Property",
      (agency_name == "Orleans Parish Sheriff's Office") & str_detect(allegation_better, "supervisor responsibil|courage|in service deficiency") ~ "Effectiveness",
      (agency_name == "Orleans Parish Sheriff's Office") & str_detect(allegation_better, "instrustions|instruction|chain of command") ~ "Following Instructions",
      (agency_name == "Orleans Parish Sheriff's Office") & str_detect(allegation_better, "interfering with investigations") ~ "Interfering with Investigations",
      (agency_name == "Orleans Parish Sheriff's Office") & str_detect(allegation_better, "cooperation") ~ "Cooperation",          
      (agency_name == "Orleans Parish Sheriff's Office") & str_detect(allegation_better, "moral conduct|code of conduct") ~ "General Unbecoming Conduct",    
      (agency_name == "Orleans Parish Sheriff's Office") & str_detect(allegation_better, "knowledge of policies|procedures") ~ "Policies and Procedures",
      (agency_name == "Orleans Parish Sheriff's Office") & str_detect(allegation_better, "neatness|attire") ~ "Cleanliness",
      (agency_name == "Orleans Parish Sheriff's Office") & str_detect(allegation_better, "professionalism|professionaliam") ~ "General Professionalism",
      (agency_name == "Orleans Parish Sheriff's Office") & str_detect(allegation_better, "truthfulness|inaccurate reports") ~ "Truthfulness",
      (agency_name == "Orleans Parish Sheriff's Office") & str_detect(allegation_better, "intimidation|harassment")&!str_detect(allegation_better, "sexual|rape|domestic") ~ "Harassment and Intimidation",
      (agency_name == "Orleans Parish Sheriff's Office") & str_detect(allegation_better, "domestic") ~ "Domestic Violence",
      (agency_name == "Orleans Parish Sheriff's Office") & str_detect(allegation_better, "courtesy|prohibited discussion") ~ "Courtesy",
      (agency_name == "Orleans Parish Sheriff's Office") & str_detect(allegation_better, "associations") ~ "Improper Relationships",
      (agency_name == "Orleans Parish Sheriff's Office") & str_detect(allegation_better, "complete report") ~ "Completing Reports and Forms",
      
      (agency_name == "Bossier City Police Department") & str_detect(allegation_better, "inappropriate|rude") ~"Courtesy",
      (agency_name == "Bossier City Police Department") & str_detect(allegation_better, "force|shooting|battery|ois|unnecessary discharge") ~"Use of Force",
      (agency_name == "Bossier City Police Department") & str_detect(allegation_better, "malfeasance") ~ "Abuse of Position",
      (agency_name == "Bossier City Police Department") & str_detect(allegation_better, "vehicle|towing|driving|pursuit|mve|camera") ~ "Misuse of Equipment and Property",
      (agency_name == "Bossier City Police Department") & str_detect(allegation_better, "failure to investigation|derellction|failure to investigate|neglect|attendance|attention to duty|dereliction") ~ "Neglecting Duty",
      (agency_name == "Bossier City Police Department") & str_detect(allegation_better, "insubordinat|chain of command") ~ "Following Instructions",
      (agency_name == "Bossier City Police Department") & str_detect(allegation_better, "code of conduct") ~ "General Unbecoming Conduct",
      (agency_name == "Bossier City Police Department") & str_detect(allegation_better, "sexual") ~ "Sexual Misconduct",
      (agency_name == "Bossier City Police Department") & str_detect(allegation_better, "standards of conduct") ~ "General Unbecoming Conduct",
      (agency_name == "Bossier City Police Department") & str_detect(allegation_better, "professional conduct") ~ "General Professionalism",
      (agency_name == "Bossier City Police Department") & str_detect(allegation_better, "harassment")&!str_detect(allegation_better, "sexual|rape|domestic") ~ "Harassment and Intimidation",
      (agency_name == "Bossier City Police Department") & str_detect(allegation_better, "improper traffic stop|illegal|improper investigation|search|false arrest") ~ "Improper Search, Seizure, Arrest, or Detainment",
      (agency_name == "Bossier City Police Department") & str_detect(allegation_better, "state law") ~ "Adherence to Law",
      (agency_name == "Bossier City Police Department") & str_detect(allegation_better, "complete report") ~ "Completing Reports and Forms",
      
      (agency_name == "Lafayette Police Department") & str_detect(allegation_better, "standards of conduct accountability, responsibility, and discipline|standards of conduct|conduct unbecoming|cubo|code of conduct") ~"General Unbecoming Conduct",
      (agency_name == "Lafayette Police Department") & str_detect(allegation_better, "indecent behavior|cursing|rude|rumor") ~ "Courtesy",
      (agency_name == "Lafayette Police Department") & str_detect(allegation_better, "force|shooting|battery|ois|unnecessary discharge") ~"Use of Force",
      (agency_name == "Lafayette Police Department") & str_detect(allegation_better, "malfeasance") ~ "Abuse of Position",
      (agency_name == "Lafayette Police Department") & str_detect(allegation_better, "informant policy|release of information") ~ "Dissemination of Information",
      (agency_name == "Lafayette Police Department") & str_detect(allegation_better, "evidence|process scene|recovered property|injuring public record|damaging public record") ~ "Mishandling Assets",
      (agency_name == "Lafayette Police Department") & str_detect(allegation_better, "accident|tech|computer|report damage|media|bwc|driving|pursuit policy|vehicle|unauthorized criminal history and background check|video camera|damage to his patrol|report an accident|departmental equipment|pursuit policy violation|computer equipment|operation of a vehicle|undocumented damage|red-flex|speeding|mishandling of a crash|speed violation|excessive speed|driving while impaired|car camera|vehicle pursuit policy|damage to lcg vehicle|damage to vehicle|camera") ~ "Misuse of Equipment and Property",
      (agency_name == "Lafayette Police Department") & str_detect(allegation_better, "failure to complete proper investigation|failure to properly investigate|failure to report for mandatory meeting|failed to attend|court attendance|sick leave violation|failure to take action|unexcused absence|investigate incident|attention to duty|failure to act|failure to assist|attention to duty|observance of work schedule") ~ "Neglecting Duty",
      (agency_name == "Lafayette Police Department") & str_detect(allegation_better, "turn in paperwork|late reports|complete report|complete her report|take report") ~ "Completing Reports and Forms",
      (agency_name == "Lafayette Police Department") & str_detect(allegation_better, "confidential") ~ "Confidentiality",
      (agency_name == "Lafayette Police Department") & str_detect(allegation_better, "in service deficiency|fit for duty") ~ "Effectiveness",
      (agency_name == "Lafayette Police Department") & str_detect(allegation_better, "supervise|poor supervisory judgment|improper or lack of supervision") ~ "Supervision",
      (agency_name == "Lafayette Police Department") & str_detect(allegation_better, "disobey|insubordination|chain of command") ~ "Following Instructions",
      (agency_name == "Lafayette Police Department") & str_detect(allegation_better, "sexual") ~ "Sexual Misconduct",
      (agency_name == "Lafayette Police Department") & str_detect(allegation_better, "unprofessional|unprofessonal conduct|professional conduct|unprofessional behavior") ~ "General Professionalism",
      (agency_name == "Lafayette Police Department") & str_detect(allegation_better, "perform dut") ~ "General Performance of Duty",
      (agency_name == "Lafayette Police Department") & str_detect(allegation_better, "ods violation|ods general order") ~ "Discrimination",
      (agency_name == "Lafayette Police Department") & str_detect(allegation_better, "falsif|perjury|false information|false statement") ~ "Truthfulness",
      (agency_name == "Lafayette Police Department") & str_detect(allegation_better, "harassment|threatened complainant|threats|intimidation|harrassment")&!str_detect(allegation_better, "sexual|rape|domestic") ~ "Harassment and Intimidation",
      (agency_name == "Lafayette Police Department") & str_detect(allegation_better, "illegal investigation|improper traffic stop|improper vehicle stop|unauthorized investigation|unauthorized search|unlawful arrest|entry into the complainant's residence|illegal search|improper search|false arrest") ~ "Improper Search, Seizure, Arrest, or Detainment",
      (agency_name == "Lafayette Police Department") & str_detect(allegation_better, "seized property|theft|missing money|misappropriation") ~ "Theft",
      (agency_name == "Lafayette Police Department") & str_detect(allegation_better, "substance") ~ "Use of Substances",
      
      agency_name == "St. Tammany Parish Sheriff's Office" & str_detect(allegation_better, "conduct|work cooperatively|code of conduct") ~ "General Unbecoming Conduct",
      agency_name == "St. Tammany Parish Sheriff's Office" & str_detect(allegation_better, "work cooperatively") ~ "Cooperation",
      agency_name == "St. Tammany Parish Sheriff's Office" & str_detect(allegation_better, "language|hostile|rude|disruptive work behavior|discourteous") ~ "Courtesy",
      agency_name == "St. Tammany Parish Sheriff's Office" & str_detect(allegation_better, "force|shooting|battery|ois|unnecessary discharge") ~ "Use of Force",
      agency_name == "St. Tammany Parish Sheriff's Office" & str_detect(allegation_better, "tobacco|drug|impaired") ~ "Use of Substances",
      agency_name == "St. Tammany Parish Sheriff's Office" & str_detect(allegation_better, "malfeasance") ~ "Abuse of Position",
      agency_name == "St. Tammany Parish Sheriff's Office" & str_detect(allegation_better, "evidence|eviudence") ~ "Mishandling Assets",
      agency_name == "St. Tammany Parish Sheriff's Office" & str_detect(allegation_better, "proper use of equiment|social network|cell phone|cash drawer|email|equipment|computer|software|driving|vehicle|accident|wear seatbelt|misuse of agency's property|camera") ~ "Misuse of Equipment and Property",
      agency_name == "St. Tammany Parish Sheriff's Office" & str_detect(allegation_better, "abuse of time|tardy|neglecct|perform all assigned duties|leaving|failure to work|leaving early|absence|leaving assigned work|missed 142.5 hours since august|failure to report|show up|absent|tardiness|sleep|absences|attention to duty|failure to act|neglect") ~ "Neglecting Duty",
      agency_name == "St. Tammany Parish Sheriff's Office" & str_detect(allegation_better, "standard level of perfor|performance") ~ "General Performance of Duty",
      agency_name == "St. Tammany Parish Sheriff's Office" & str_detect(allegation_better, "insurbordination|insubordnation|chain of command|disobeying|insubordination|chain of command|instruction") ~ "Following Instructions",
      agency_name == "St. Tammany Parish Sheriff's Office" & str_detect(allegation_better, "observe policy|policy and procedures") ~ "Policies and Procedures",
      agency_name == "St. Tammany Parish Sheriff's Office" & str_detect(allegation_better, "violation of rule|safety rule|disregard of agency rules|violating saftey") ~ "Violating Rules",
      agency_name == "St. Tammany Parish Sheriff's Office" & str_detect(allegation_better, "supervise subordinates|properly supervisor") ~ "Supervision",
      agency_name == "St. Tammany Parish Sheriff's Office" & str_detect(allegation_better, "unsatisfactory|physical fitness") ~ "Effectiveness",
      agency_name == "St. Tammany Parish Sheriff's Office" & str_detect(allegation_better, "confidential") ~ "Confidentiality",
      agency_name == "St. Tammany Parish Sheriff's Office" & str_detect(allegation_better, "romantic or intimate|employee relations|fraternization") ~ "Improper Relationships",
      agency_name == "St. Tammany Parish Sheriff's Office" & str_detect(allegation_better, "falsyifying|falsifying|dishonest|false|false entry") ~ "Truthfulness",
      agency_name == "St. Tammany Parish Sheriff's Office" & str_detect(allegation_better, "written report|make require reports|failure to submit|k9 record|submit properly written reports|make required report") ~ "Completing Reports and Forms",
      agency_name == "St. Tammany Parish Sheriff's Office" & str_detect(allegation_better, "criminal conduct|criminal conviction") ~ "Adherence to Law",
      agency_name == "St. Tammany Parish Sheriff's Office" & str_detect(allegation_better, "harassment")&!str_detect(allegation_better, "sexual|rape") ~ "Harassment and Intimidation",
      agency_name == "St. Tammany Parish Sheriff's Office" & str_detect(allegation_better, "shoplift|payroll fraud|cash balance procedure") ~ "Theft",
      
      (agency_name == "East Baton Rouge Parish Sheriff's Office") & (str_detect(allegation_better, "code of conduct")|allegation_better == "conduct") ~"General Unbecoming Conduct",
      (agency_name == "East Baton Rouge Parish Sheriff's Office") & (str_detect(allegation_better, "work cooperatively")|allegation_better == "conduct") ~"Cooperation",
      (agency_name == "East Baton Rouge Parish Sheriff's Office") & (str_detect(allegation_better, "courtesty|courtesy|behavior|language|hostile|rude|disruptive work behavior|discourteous")|allegation_better == "conduct") ~"Courtesy",
      (agency_name == "East Baton Rouge Parish Sheriff's Office") & str_detect(allegation_better, "force|shooting|battery|ois|unnecessary discharge") ~"Use of Force",
      (agency_name == "East Baton Rouge Parish Sheriff's Office") & str_detect(allegation_better, "tobacco|drug|impaired") ~"Use of Substances",
      (agency_name == "East Baton Rouge Parish Sheriff's Office") & str_detect(allegation_better, "abuse of authority|malfeasance") ~ "Abuse of Position",
      (agency_name == "East Baton Rouge Parish Sheriff's Office") & str_detect(allegation_better, "evidence|eviudence") ~ "Mishandling Assets",
      (agency_name == "East Baton Rouge Parish Sheriff's Office") & str_detect(allegation_better, "supervise subordinates|properly supervisor") ~ "Supervision",
      (agency_name == "East Baton Rouge Parish Sheriff's Office") & str_detect(allegation_better, "recording conversations|social media|cell phone|cash drawer|email|equipment|computer|software|driving|vehicle|accident|wear seatbelt|misuse of agency's property|camera") ~ "Misuse of Equipment and Property",
      (agency_name == "East Baton Rouge Parish Sheriff's Office") & str_detect(allegation_better, "attendance requirement|reporting for duty|leaving|failure to work|leaving early|absence|leaving assigned work|missed 142.5 hours since august|failure to report|show up|absent|tardiness|sleep|absences|attention to duty|failure to act|neglect|attention to duty") ~ "Neglecting Duty",
      (agency_name == "East Baton Rouge Parish Sheriff's Office") & str_detect(allegation_better, "performance") ~ "General Performance of Duty",
      (agency_name == "East Baton Rouge Parish Sheriff's Office") & str_detect(allegation_better, "violating saftey|violation of rule|safety rule|disregard of agency rules") ~ "Violating Rules",
      (agency_name == "East Baton Rouge Parish Sheriff's Office") & str_detect(allegation_better, "confidential") ~ "Confidentiality",
      (agency_name == "East Baton Rouge Parish Sheriff's Office") & str_detect(allegation_better, "policy and") ~ "Policies and Procedures",
      (agency_name == "East Baton Rouge Parish Sheriff's Office") & str_detect(allegation_better, "dissemination of information") ~ "Dissemination of Information",
      (agency_name == "East Baton Rouge Parish Sheriff's Office") & str_detect(allegation_better, "insurbordination|insubordnation|disobeying|insubordination|chain of command|instruction") ~ "Following Instructions",
      (agency_name == "East Baton Rouge Parish Sheriff's Office") & str_detect(allegation_better, "in service deficiency|standard level of perfor|unsatisfactory|physical fitness") ~ "Effectiveness",
      (agency_name == "East Baton Rouge Parish Sheriff's Office") & str_detect(allegation_better, "associations|relations|romantic or intimate|employee relations|fraternization") ~ "Improper Relationships",
      (agency_name == "East Baton Rouge Parish Sheriff's Office") & str_detect(allegation_better, "false statement|false statement|false entry") ~ "Truthfulness",
      (agency_name == "East Baton Rouge Parish Sheriff's Office") & str_detect(allegation_better, "make require reports|failure to submit|k9 record|submit properly written reports|make required report") ~ "Completing Reports and Forms",
      (agency_name == "East Baton Rouge Parish Sheriff's Office") & str_detect(allegation_better, "conformance to law|lawful order|criminal conduct|criminal conviction") ~ "Adherence to Law",
      (agency_name == "East Baton Rouge Parish Sheriff's Office") & str_detect(allegation_better, "payroll fraud|cash balance procedure") ~ "Theft",
      (agency_name == "East Baton Rouge Parish Sheriff's Office") & str_detect(allegation_better, "retaliation|harrassment|harassment")&!str_detect(allegation_better, "sexual|rape|domestic") ~ "Harassment and Intimidation",
      (agency_name == "East Baton Rouge Parish Sheriff's Office") & str_detect(allegation_better, "arrest, search, and seizure|arrest, search, or seizure") ~ "Improper Search, Seizure, Arrest, or Detainment",
      (agency_name == "East Baton Rouge Parish Sheriff's Office") & str_detect(allegation_better, "pornography|rape|sexual|indecent behavior") ~ "Sexual Misconduct",
      
      (agency_name == "Lafayette Parish Sheriff's Office") & str_detect(allegation_better, "conduct unbecoming|code of conduct") ~"General Unbecoming Conduct",
      (agency_name == "Lafayette Parish Sheriff's Office") & str_detect(allegation_better, "treatment|courtesy") ~"Courtesy",
      (agency_name == "Lafayette Parish Sheriff's Office") & str_detect(allegation_better, "force|discharge") ~"Use of Force",
      (agency_name == "Lafayette Parish Sheriff's Office") & str_detect(allegation_better, "associations|relations") ~"Improper Relationships",
      (agency_name == "Lafayette Parish Sheriff's Office") & str_detect(allegation_better, "abuse of authority|malfeasance") ~ "Abuse of Position",
      (agency_name == "Lafayette Parish Sheriff's Office") & str_detect(allegation_better, "e-mail|camera|vehicle|social networking|camera") ~ "Misuse of Equipment and Property",
      (agency_name == "Lafayette Parish Sheriff's Office") & str_detect(allegation_better, "attention to duty|attendance") ~ "Neglecting Duty",
      (agency_name == "Lafayette Parish Sheriff's Office") & str_detect(allegation_better, "performance of duty") ~ "General Performance of Duty",
      (agency_name == "Lafayette Parish Sheriff's Office") & str_detect(allegation_better, "chain of command") ~ "Following Instructions",
      (agency_name == "Lafayette Parish Sheriff's Office") & str_detect(allegation_better, "interference") ~ "Interfering with Investigations",
      (agency_name == "Lafayette Parish Sheriff's Office") & str_detect(allegation_better, "in service deficiency|unsatisfactory performance") ~ "Effectiveness",
      (agency_name == "Lafayette Parish Sheriff's Office") & str_detect(allegation_better, "departmental reports") ~ "Completing Reports and Forms",
      (agency_name == "Lafayette Parish Sheriff's Office") & str_detect(allegation_better, "conformance of law|convicted") ~ "Adherence to Law",
      (agency_name == "Lafayette Parish Sheriff's Office") & str_detect(allegation_better, "profiling") ~ "Discrimination",
      (agency_name == "Lafayette Parish Sheriff's Office") & str_detect(allegation_better, "unlawful harassment")&!str_detect(allegation_better, "sexual|rape|domestic") ~ "Harassment and Intimidation",
      (agency_name == "Lafayette Parish Sheriff's Office") & str_detect(allegation_better, "prea|sexual") ~ "Sexual Misconduct",
      (agency_name == "Lafayette Parish Sheriff's Office") & str_detect(allegation_better, "intoxicant") ~ "Use of Substances",
      (agency_name == "Lafayette Parish Sheriff's Office") & str_detect(allegation_better, "dissemination of information") ~ "Dissemination of Information",
      (agency_name == "Lafayette Parish Sheriff's Office") & str_detect(allegation_better, "employee searches|illegal search|arrest search and seizure") ~ "Improper Search, Seizure, Arrest, or Detainment",
      
      (agency_name == "Lafourche Parish Sheriff's Office") & str_detect(allegation_better, "unsatisfactory|article 55") ~ "Effectiveness",
      (agency_name == "Lafourche Parish Sheriff's Office") & str_detect(allegation_better, "personal conduct|article 25|immoral conduct") ~ "General Unbecoming Conduct",
      (agency_name == "Lafourche Parish Sheriff's Office") & str_detect(allegation_better, "adherence to law|article 28|criminal conviction") ~ "Adherence to Law",
      (agency_name == "Lafourche Parish Sheriff's Office") & str_detect(allegation_better, "truthfulness") ~ "Truthfulness",
      (agency_name == "Lafourche Parish Sheriff's Office") & str_detect(allegation_better, "intimidation") ~ "Harassment and Intimidation",
      (agency_name == "Lafourche Parish Sheriff's Office") & str_detect(allegation_better, "association|fraternization") ~ "Improper Relationships",
      (agency_name == "Lafourche Parish Sheriff's Office") & str_detect(allegation_better, "false|article 30") ~ "Truthfulness",
      (agency_name == "Lafourche Parish Sheriff's Office") & str_detect(allegation_better, "insubordination|disobedience|duty to advise commanding officer") ~ "Following Instructions",
      (agency_name == "Lafourche Parish Sheriff's Office") & str_detect(allegation_better, "social networking|soc med abuse|firearm|office property|firearm discharge|operating vehicles|use of weapon") ~ "Misuse of Equipment and Property",
      (agency_name == "Lafourche Parish Sheriff's Office") & str_detect(allegation_better, "malfeasance|abuse of position") ~ "Abuse of Position",
      (agency_name == "Lafourche Parish Sheriff's Office") & str_detect(allegation_better, "courtesy") ~ "Courtesy",
      (agency_name == "Lafourche Parish Sheriff's Office") & str_detect(allegation_better, "drug|use of alcohol") ~ "Use of Substances",
      (agency_name == "Lafourche Parish Sheriff's Office") & str_detect(allegation_better, "taking money|payment of debt") ~ "Theft",
      (agency_name == "Lafourche Parish Sheriff's Office") & str_detect(allegation_better, "reporting infraction") ~ "Completing Reports and Forms",
      (agency_name == "Lafourche Parish Sheriff's Office") & str_detect(allegation_better, "loss of contraband|security of records") ~ "Mishandling Assets",
      (agency_name == "Lafourche Parish Sheriff's Office") & str_detect(allegation_better, "violation of rules") ~ "Violating Rules",
      (agency_name == "Lafourche Parish Sheriff's Office") & str_detect(allegation_better, "sexual|rape") ~ "Sexual Misconduct",
      (agency_name == "Lafourche Parish Sheriff's Office") & str_detect(allegation_better, "domestic") ~ "Domestic Violence",
      (agency_name == "Lafourche Parish Sheriff's Office") & str_detect(allegation_better, "obligations in general|period of duty") ~ "Neglecting Duty",
      
      !(agency_name %in% top_10) & (str_detect(allegation_better, "unethical behavior|unbecoming behavior|on duty conduct|immoral conduct|conduct unbecoming|personal conduct|unbecoming conduct|code of conduct|conduct unbecoming")|allegation_better == "conduct") ~"General Unbecoming Conduct",
      !(agency_name %in% top_10) & str_detect(allegation_better, "langauge|rumor|profanities|temper|gossip|inappropriate statements|attitude|rude|courtesy|respect") ~"Courtesy",
      !(agency_name %in% top_10) & str_detect(allegation_better, "ois|unnecessary restraint|battery|force|shooting|murder|brutality") ~"Use of Force",
      !(agency_name %in% top_10) & str_detect(allegation_better, "relation|associations") ~"Improper Relationships",
      !(agency_name %in% top_10) & str_detect(allegation_better, "abuse of power|abuse of office|malfeasance|misuse of authority") ~ "Abuse of Position",
      !(agency_name %in% top_10) & str_detect(allegation_better, "recovered property|control of property|failure to turn in found property|damaging public record|injuring public records|evidence") ~ "Mishandling Assets",
      !(agency_name %in% top_10) & str_detect(allegation_better, "red-flex|recording conversation|computer|software|pointing weapon|crash|unauthorized criminal history and background checks|use of weapon|using sheriff's office data|call handling|reckless|social networking|social media|photos|taser use|siren|property damage|pursuit|speed|department property|traffic|speeding|damage to police car|destroying property|fleet accident|fuel|impact weapon use|emergency light|internet|video|driving|vehicle|firearms|equipment|fleet crash|use of firearms|camera") ~ "Misuse of Equipment and Property",
      !(agency_name %in% top_10) & str_detect(allegation_better, "dereliction|missed court|no show|observance of work|passing an accident|late|punctuality|tardiness|failure to perform|unauthorized leaving|puntuality|running errand|sleep|attentiv|failure to act|missed city court|perform required duties|carrying out orders|absence|shirk|attention to duty|reporting for duty|report to duty|neglect") ~ "Neglecting Duty",
      !(agency_name %in% top_10) & str_detect(allegation_better, "professional|professi") ~ "General Professionalism",
      !(agency_name %in% top_10) & str_detect(allegation_better, "uniform") ~ "Cleanliness",
      !(agency_name %in% top_10) & str_detect(allegation_better, "violation of department policies") ~ "Violating Rules",
      !(agency_name %in% top_10) & str_detect(allegation_better, "supervisory judgment") ~ "Supervision",
      !(agency_name %in% top_10) & str_detect(allegation_better, "perform duties|performance") ~ "General Performance of Duty",
      !(agency_name %in% top_10) & str_detect(allegation_better, "on duty control|fit for duty|fitness|performance|in service deficiency") ~ "Effectiveness",
      !(agency_name %in% top_10) & str_detect(allegation_better, "obey|disobedience|not following proper training|disobey|failure to comply|failure to obey|follow orders|respect of rank|insubordination|chain of command") ~ "Following Instructions",
      !(agency_name %in% top_10) & str_detect(allegation_better, "perjury|lying|false information|false or inaccurate|false presentation|false reporting|false reports|fictitious|filed false report|false statements|truthfulness|dishonesty") ~ "Truthfulness",
      !(agency_name %in% top_10) & str_detect(allegation_better, "complete report|take report|paperwork completion|failed to complete/submit|department reports|late reports|file report|submission of reports|report in a timely manner") ~ "Completing Reports and Forms",
      !(agency_name %in% top_10) & str_detect(allegation_better, "unlawful and unethical activity|arrested|lawful orders|conformance to law|conformance of law|adherence to law|criminal conviction") ~ "Adherence to Law",
      !(agency_name %in% top_10) & str_detect(allegation_better, "profiling|racial|violation of ods|ods violation|violation of civil rights") ~ "Discrimination",
      !(agency_name %in% top_10) & str_detect(allegation_better, "harrassment|harassment|intimidation|intimation|threaten|threat")&!str_detect(allegation_better, "sexual|rape|domestic") ~ "Harassment and Intimidation",
      !(agency_name %in% top_10) & str_detect(allegation_better, "pornography|rape|sexual|indecent behavior") ~ "Sexual Misconduct",
      !(agency_name %in% top_10) & str_detect(allegation_better, "intoxica|contraband|drug|alcohol|substance") ~ "Use of Substances",
      !(agency_name %in% top_10) & str_detect(allegation_better, "pocket property|theft|missing money|misuse of funds") ~ "Theft",
      !(agency_name %in% top_10) & str_detect(allegation_better, "domestic") ~ "Domestic Violence",
      !(agency_name %in% top_10) & str_detect(allegation_better, "notify|dissemination of information") ~ "Dissemination of Information",
      !(agency_name %in% top_10) & str_detect(allegation_better, "unauthorized entry|illegal search|unauthorized investigation|unauthorized search|unlawful search|unlawful arrest|false detention|false accusation|improper charges|improper arrest|false arrest") ~ "Improper Search, Seizure, Arrest, or Detainment",
      
      TRUE ~ "Miscellanious Allegation"
    ),
    allegation_classified2 = case_when(
      
      is.na(allegation_desc) ~ "No Allegation Description Reported",
      (str_detect(allegation_desc, "unethical behavior|unbecoming behavior|on duty conduct|immoral conduct|conduct unbecoming|personal conduct|unbecoming conduct|code of conduct|conduct unbecoming")|allegation_better == "conduct") ~"General Unbecoming Conduct",
      str_detect(allegation_desc, "langauge|rumor|profanities|temper|gossip|inappropriate statements|attitude|rude|courtesy|respect") ~"Courtesy",
      str_detect(allegation_desc, "uof|ois|unnecessary restraint|battery|force|shooting|murder|brutality") ~"Use of Force",
      str_detect(allegation_desc, "fratinize|relation|associations") ~"Improper Relationships",
      str_detect(allegation_desc, "abuse of power|abuse of office|malfeasance|misuse of authority|abuse of position") ~ "Abuse of Position",
      str_detect(allegation_desc, "recovered property|control of property|failure to turn in found property|damaging public record|injuring public records|evidence") ~ "Mishandling Assets",
      str_detect(allegation_desc, "red-flex|recording conversation|computer|software|pointing weapon|crash|handcuffing and restraint devices|unauthorized criminal history and background checks|use of weapon|using sheriff's office data|call handling|reckless|social networking|social media|photos|taser use|siren|property damage|pursuit|speed|department property|traffic|speeding|damage to police car|destroying property|fleet accident|fuel|impact weapon use|emergency light|internet|video|driving|vehicle|firearms|equipment|fleet crash|use of firearms|camera") ~ "Misuse of Equipment and Property",
      str_detect(allegation_desc, "court appearance|failed to attend|refused an assignment|failure to appear|complete the proper observations|dereliction|missed court|no show|observance of work|passing an accident|late|punctuality|tardiness|failure to perform|unauthorized leaving|puntuality|running errand|sleep|attentiv|failure to act|missed city court|perform required duties|carrying out orders|absence|shirk|attention to duty|reporting for duty|report to duty|neglect|failing to take appropriate and necessary police action") ~ "Neglecting Duty",
      str_detect(allegation_desc, "professional|professi") ~ "General Professionalism",
      str_detect(allegation_desc, "uniform") ~ "Cleanliness",
      str_detect(allegation_desc, "policies and procedures|mandatory training|manadatory training") ~ "Policies and Procedures",
      str_detect(allegation_desc, "violation of department policies") ~ "Violating Rules",
      str_detect(allegation_desc, "supervisory judgment") ~ "Supervision",
      str_detect(allegation_desc, "perform duties|performance") ~ "General Performance of Duty",
      str_detect(allegation_desc, "on duty control|fit for duty|fitness|performance|in service deficiency") ~ "Effectiveness",
      str_detect(allegation_desc, "obey|disobedience|not following proper training|disobey|failure to comply|failure to obey|follow orders|respect of rank|insubordination|chain of command|comply with instructions|following instuctions") ~ "Following Instructions",
      str_detect(allegation_desc, "false of inaccurate reports|perjury|lying|false information|false or inaccurate|false presentation|false reporting|false reports|fictitious|filed false report|false statements|truthfulness|dishonesty") ~ "Truthfulness",
      str_detect(allegation_desc, "complete report|take report|paperwork completion|failed to complete/submit|department reports|late reports|file report|submission of reports|report in a timely manner|failing to make a written report|report preparation") ~ "Completing Reports and Forms",
      str_detect(allegation_desc, "unlawful and unethical activity|arrested|lawful orders|conformance to law|conformance of law|adherence to law|criminal conviction") ~ "Adherence to Law",
      str_detect(allegation_desc, "profiling|racial|violation of ods|ods violation|violation of civil rights|bias-free") ~ "Discrimination",
      str_detect(allegation_desc, "retaliation|harrassment|harassment|intimidation|intimation|threaten|threat")&!str_detect(allegation_better, "sexual|rape|domestic") ~ "Harassment and Intimidation",
      str_detect(allegation_desc, "pornography|rape|sexual|indecent behavior") ~ "Sexual Misconduct",
      str_detect(allegation_desc, "intoxica|contraband|drug|alcohol|substance") ~ "Use of Substances",
      str_detect(allegation_desc, "public payroll fraud|pocket property|theft|missing money|misuse of funds") ~ "Theft",
      str_detect(allegation_desc, "domestic violence") ~ "Domestic Violence",
      str_detect(allegation_desc, "notify|dissemination of information") ~ "Dissemination of Information",
      str_detect(allegation_desc, "unauthorized entry|illegal search|unauthorized investigation|unauthorized search|unlawful search|false imprisoment|search and seizure|stops / terry stops|unlawful arrest|false detention|false accusation|improper charges|improper arrest|false arrest") ~ "Improper Search, Seizure, Arrest, or Detainment",
      
      TRUE ~ "Miscellanious Allegation Description"
    ),
    allegation_classified = case_when(
      allegation_classified2 %in% c("General Professionalism","General Unbecoming Conduct", "General Performance of Duty","No Allegation Description Reported", "Miscellanious Allegation Description") ~ allegation_classified,
      TRUE ~ allegation_classified2
    )
  )

misconduct_disposition <- misconduct %>%
  mutate(
    disposition_classified = case_when(
      disposition %in% c("at fault","not sustained; sustained","sustained; dui","sustained; deceased","sustained; retired under investigation","sustained", "sustained; resigned while under investigation", "sustained; resigned", "sustained; dismissed") ~ "Sustained",
      disposition %in% c("no further investigation merited", "no investigation merited") ~ "No Investigation Merited",
      str_detect(disposition, "unfounded|unsubstantiated") ~ "Unfounded",
      disposition %in% c("federal civil rights suit dismissed by court drug charges against complainant dismissed by district attorney","cleared of any wrongdoing involing the incident","no criminal or administrative violation","no fault","unsubstantiat","non sustained","cleared","cleared of any wrongdoing","un-substantiated","no violations observed","not sustained; rui","terminated all charges","not sustained", "non-sustained", "unsustained", "insufficient evidence to sustain") ~ "Not Sustained",
      disposition == "exonerated" ~ "Exonerated",
      str_detect(disposition, "pending") ~ "Pending Investigation",
      disposition == "admin review" ~ "Admin Review",
      disposition == "invalid complaint" ~ "Invalid Complaint",
      disposition %in% c("founded", "founded/preventable class 2", "founded reduced to letter") ~ "Founded",
      str_detect(disposition, "withdraw|withdrew") ~ "Withdrawn",
      str_detect(disposition, "negotiate|settlement") ~ "Negotiated Settlement",
      disposition %in% c("pre-disciplinary hearing","pre-termination hearing", "awaiting hearing") ~ "Hearing",
      disposition == "convicted" ~ "Conviction",
      disposition %in% c("office investigation" , "active") ~ "Active Investigation",
      str_detect(disposition, "resign") ~ "Resigned",
      disposition %in% c("cancelled","abandoned") ~ "Cancelled",
      disposition == "inconclusive" ~ "Inconclusive",
      disposition %in% c("duplicate allegation", "duplicate investigation") ~ "Duplicate Allegation",
      str_detect(disposition, "justif") ~ "Justified",
      disposition == "investigation terminated" ~ "Investigation Terminated",
      disposition == "partially sustained" ~ "Partially Sustained",
      str_detect(disposition, "di-2") ~ "DI-2",
      str_detect(disposition, "suspend|suspension") ~ "Suspended",
      disposition %in% c("terminated; write up attached","termination", "terminated; arrested; pled guilty to simple battery on 6/1/2021 in criminal court") ~ "Terminated",
      disposition == "administrative closed" ~ "Administratively Closed",
      str_detect(disposition, "reprimand") ~ "Reprimand",
      str_detect(disposition, "referred") ~ "Referred",
      str_detect(disposition, "transfer") ~ "Transfered",
      
      is.na(disposition) ~ "No Disposition Reported",
      TRUE ~ "Miscellanious Disposition"
    )
  )


misconduct_action <- misconduct %>%
  mutate(action_better = strsplit(as.character(action), "/|;| and |\\|")) %>%
  unnest(action_better) %>%
  mutate(
    action_classified = case_when(
      (str_detect(action_better, "discharge|terminat|separat|seperat") & !str_detect(action_better, "resign")) ~ "Terminated",
      str_detect(action_better, "arrested") ~ "Arrested",
      str_detect(action_better, "quit|resign|resgined|retired") ~ "Resigned",
      str_detect(action_better, "decertified|decertification") ~ "Decertified",
      str_detect(action_better, "susepnsion|suspen") ~ "Suspension",
      str_detect(action_better, "convict") ~ "Conviction",
      str_detect(action_better, "admonished|verbal conference|write-up|letter|written|warn|caution|talked to about") ~ "Warning",
      str_detect(action_better, "repremand|reprimad|reprimand") ~ "Reprimand",
      str_detect(action_better, "investigation") ~ "Investigation",
      str_detect(action_better, "transfer") ~ "Transferred",
      str_detect(action_better, "train|school|course|class|intervention|diversity") ~ "Training",
      str_detect(action_better, "referred") ~ "Referred",
      str_detect(action_better, "counsel") ~ "Counseling",
      str_detect(action_better, "demot") ~ "Demoted",
      str_detect(action_better, "pend") ~ "Pending",
      str_detect(action_better, "evaluation") ~ "Evaluation",
      str_detect(action_better, "dm-l|dm1|dm-1") ~ "DM-1",
      str_detect(action_better, "pay|without pay|reduction|deducted") ~ "Reduced Pay",
      str_detect(action_better, "none|no action|life lesson learned from experience|no discipline") ~ "No Repercussion",
      str_detect(action_better, "loss unit|loss of|unit privileges|vehicle|loss take home unit|exduty loss") ~ "Loss of Privileges",
      str_detect(action_better, "non-sustained|exonerated|dismissed|not sustained|withdrawn|dismissed|justified|cleared|unfounded|unfounded|no criminal|cooperate") ~ "No Consequence Necessary",

      is.na(action_better) ~ "No Repercussion Reported",
      TRUE ~ "Miscellanious Repercussion"
    )
  )

write.csv(misconduct_allegation, "scripts/misconduct_data/creating_training_data/misconduct_allegation.csv")
write.csv(misconduct_disposition, "scripts/misconduct_data/creating_training_data/misconduct_disposition.csv")
write.csv(misconduct_action, "scripts/misconduct_data/creating_training_data/misconduct_action.csv")