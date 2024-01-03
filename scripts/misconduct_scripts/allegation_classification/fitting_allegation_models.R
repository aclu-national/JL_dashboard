# ----------------------------- Loading Libraries and Data ------------------------------
# Loading Libraries
library(tidyverse)

# Importing the labelled data
labelled_data <- read_csv("labelled_data.csv")

# Importing the ML predictions
rf_predictions = read_csv("rf_predictions.csv")
svm_predictions = read_csv("svm_predictions.csv")
test = read_csv("test_for_predictions.csv")

# ----------------------------- Defining Functions ------------------------------------

# Key-word classification function
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
    mutate(across("Neglect of Duty":"Miscellaneous Allegation", ~ifelse(. == TRUE, 1, 0)))
}

# Multi-label evaluation function
evaluation <- function(training_df, predictions, all_labels) {
  precisions <- c()
  recalls <- c()
  accuracies <- c()
  f1_scores <- c()
  
  results_df <- data.frame(Label = character(), Precision = numeric(), Recall = numeric(), Accuracy = numeric(), F1_Score = numeric(), stringsAsFactors = FALSE)
  
  for (label in all_labels) {
    true_positive <- sum(training_df[[label]] == 1 & predictions[[label]] == 1)
    false_positive <- sum(training_df[[label]] == 0 & predictions[[label]] == 1)
    false_negative <- sum(training_df[[label]] == 1 & predictions[[label]] == 0)
    true_negative <- sum(training_df[[label]] == 0 & predictions[[label]] == 0)
    
    precision <- true_positive / (true_positive + false_positive + 1e-10)
    recall <- true_positive / (true_positive + false_negative + 1e-10)
    accuracy <- (true_positive + true_negative) / (true_positive + false_positive + false_negative + true_negative + 1e-10)
    f1_score <- 2 * precision * recall / (precision + recall + 1e-10)
    
    predicted_probs <- as.numeric(predictions[[label]])
    
    precisions <- c(precisions, precision)
    recalls <- c(recalls, recall)
    accuracies <- c(accuracies, accuracy)
    f1_scores <- c(f1_scores, f1_score)
    
    result_row <- data.frame(Label = label, Precision = precision, Recall = recall, Accuracy = accuracy, F1_Score = f1_score, stringsAsFactors = FALSE)
    results_df <- rbind(results_df, result_row)
    
  }
  
  overall_precision <- mean(precisions)
  overall_recall <- mean(recalls)
  overall_accuracy <- mean(accuracies)
  overall_f1_score <- mean(f1_scores)
  
  overall_row <- data.frame(Label = "Mean", Precision = overall_precision, Recall = overall_recall, Accuracy = overall_accuracy, F1_Score = overall_f1_score,stringsAsFactors = FALSE)
  results_df <- rbind(results_df, overall_row)
  
  # Return the results data frame
  return(results_df)
}

# ----------------------------- Cleaning Data ------------------------------------

# Cleaning the labelled data
labels <- strsplit(labelled_data$classification, ", ")
all_labels <- unique(unlist(labels))
label_matrix <- sapply(labels, function(x) all_labels %in% x)
labelled_data <- cbind(labelled_data, sapply(all_labels, function(label) grepl(label, labelled_data$classification))) %>%
  mutate(across("Abuse of Authority":"Seizure Violation", ~ifelse(. == TRUE, 1, 0)))

# Variables appearing in the test data
some_labels <- all_labels[sapply(all_labels, function(variable) sum(test[[variable]]) != 0)]


# ----------------------------- Comparing Models ------------------------------------

# Text-classification
evaluation(labelled_data, allegation_classifier(training), all_labels)

# RF
evaluation(test, rf_predictions, some_labels)

# SVM
evaluation(test, svm_predictions, some_labels)
