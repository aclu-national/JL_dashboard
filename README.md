# Visualizing Police Violence in Louisiana
Git for the Justice Lab: Visualizing Police Violence in Louisiana dashboard. This Git includes documentation, data, and scripts used to build this project, as well as all other work associated with the project.

![](https://github.com/aclu-national/JL_dashboard_test/blob/ed9009b156db4ccdfc4fb3df33af8573584b11bf/image/gif3.gif)


## Mission
We built this dashboard as an interactive tool to empower interested citizens, nonprofit organizations, journalists, attorneys, academics, and even law enforcement agencies to explore and compare statewide policing statistics, uncover crucial insights and identify areas needing greater transparency.

## Goals
### 1. Enhancing Data Accessibility and Stewardship 
Our mission is to offer seamless access to crucial policing statistics in Louisiana. Beyond presenting these statistics, we hope to empower users by providing access to the raw data, transforming them into custodians of this valuable information.

### 2. Amplifying Voices: 
This initiative serves as a powerful tool to magnify the experiences of individuals who have faced police violence and their families. Through a combination of data and narratives, we hope to bring attention to and amplify the stories of those affected by violence.

### 3. Ensuring Accountability: 
We are committed to holding the police accountable for their actions. This involves representing instances of violence against the community and ensuring that individuals have transparent access to information about violent departments and officers.

## Git Structure

<img src = https://github.com/aclu-national/JL_dashboard_test2/blob/fdcea16566ab7e170e9151b82c4c53019b1bad4d/image/git_structure >

## Sources
Data on known killings by police were obtained by [Mapping Police Violence](http://mappingpoliceviolence.org/) and include crowd-sourced data on killings by police officers that have been reported by the media beginning in 2013. The Mapping Police Violence data are [sourced](https://mappingpoliceviolence.org/methodology) from the Google News and validated using [Fatal Encounters](https://fatalencounters.org/), [Fatal Force](https://www.washingtonpost.com/graphics/investigations/police-shootings-database/), and governmental data sources. More information on these data can be found at [mappingpoliceviolence.org](http://mappingpoliceviolence.org). Data used for this report was downloaded from Mapping Police Violence on January 27th, 2025.

Demographic information was obtained from the [2020 U.S. Census](https://data.census.gov/table?g=040XX00US22,22$0500000&amp;y=2020&amp;d=DEC+Redistricting+Data+(PL+94-171)&amp;tid=DECENNIALPL2020.P1). In the analyses presented here, the Black population includes individuals who identified their race as Black or African American alone or in combination with another race. The white population includes individuals who identified their race as white and their ethnicity as not “Hispanic or Latino”.

Data on police officers were obtained from the [FBI Crime Data Explorer](https://cde.ucr.cjis.gov/LATEST/webapp/#/pages/downloads). [The Law Enforcement Employees](https://cde.ucr.cjis.gov/LATEST/webapp/#) dataset comprises annual data concerning personnel employed by law enforcement agencies, encompassing both officers and civilians. This data was accessed on January 27th, 2025. The Law Enforcement Agency names were cleaned using the [Law Enforcement Agency Identifiers Crosswalk, United States, 2012](https://www.icpsr.umich.edu/web/ICPSR/studies/35158/datadocumentation#) accessed through the University of Michigan on January 11th, 2024.

Data on known misconduct by police officers were obtained by the [Louisiana Law Enforcement and Accountability Database](https://llead.co/) (LLEAD) through public records requests and include public data collected from law enforcement agencies including police departments, sheriff’s offices, and civil service commissions. Note that as the data is a result of law enforcement reporting, we cannot guarantee the accuracy or depth of the reporting. To learn more about how the data is collected, visit [llead.co](http://llead.co) or their [GitHub page](https://github.com/ipno-llead/processing). Data used for this report was downloaded from LLEAD on February 27th, 2024.

Please note that the categorization of misconduct allegations, dispositions, and repercussions was carried out by analyzing key word stems, with the aim of standardizing categories across various police department reports. It is important to acknowledge that in certain instances, these classifications may not fully reflect the actual allegations, dispositions, and repercussions due to inconsistent reporting. For a detailed breakdown of our classification methodology, you can refer to this [PDF document](https://github.com/aclu-national/JL_dashboard/blob/main/scripts/misconduct_scripts/allegation_classification/classification_methodology.pdf).

Internal data on misconduct by police officers were obtained by Justice Lab's [Louisiana Police Misconduct Data Collection](https://action.aclu.org/la-misconduct-data-collection) form. This form serves as a data collection tool aimed at enhancing police accountability and monitoring and reporting incidents of police misconduct specifically within Louisiana. This comprehensive approach helps identify patterns, trends, and potential areas of concern, ultimately contributing to a more informed and data-driven effort toward promoting transparency and accountability in law enforcement activities in the state. Data used for this report was downloaded on January 27th, 2025.


## Data Cleaning
### Police Killings
The process of cleaning police killings includes ([see the code and analysis here](https://github.com/aclu-national/JL_dashboard/blob/4cc81d42c5068be139fd52ce8d21a816b737fda1/scripts/killing_scripts/killing_data_cleaning.R)):
1. Cleaning the Mapping Police Violence data variable names.
2. Filtering for police killings in Louisiana.
3. Creating unknown options for the demographic variables.
4. Extracting different date, year, and month combinations.
5. Creating age categories.
6. Cleaning Parish names and removing duplicate victim names.
7. Retreiving 2020 Census demographic data for Parishes in Louisiana.

### Overview
The process of cleaning the overview data includes ([see the code and analysis here](https://github.com/aclu-national/JL_dashboard/blob/3db697133a5cd5b05c3dd88d98bb46816bcb72e9/scripts/overview_scripts/overview_data_cleaning.R)):
1. Filtering the FBI Crime Data Explorer Employement data for agencies in Louisiana.
2. Joining the FBI Crime Data Explorer Employement data with Law Enforcement Agency Identifiers Crosswalk data to yeild more complete names.
3. Cleaning agency names more thoroughly

### Misconduct
The process of cleaning the misconduct data includes ([see the code and analysis here](https://github.com/aclu-national/JL_dashboard/blob/c57de852d5ce5509d5409c759b3b4a1d252a4bf2/scripts/misconduct_scripts/misconduct_data_cleaning.R)):
1. Creating a full name variable in the LLEAD personnel data.
2. Joining the LLEAD personnel data with the LLEAD History ID data via their unique identifiers.
3. Renaming the repeated officer demographic variables.
4. Joining the above dataframe with the LLEAD agency location data via the agency names.
5. Categorizing the agencies.
6. Cleaning the agency names.
7. Using regex to categorize the dispositions and actions
8. Using a multi-label classification model to categorize allegations ([click here to see how we built this model](https://github.com/aclu-national/JL_dashboard/blob/cce003c05222bf8609552bcd951a0afe33cbbde4/scripts/misconduct_scripts/allegation_classification/classification_methodology.pdf)). 

## Data
- Louisiana Police Misconduct Data Collection: https://action.aclu.org/la-misconduct-data-collection
- Mapping Police Violence data: https://airtable.com/shroOenW19l1m3w0H/tblxearKzw8W7ViN8
- LLEAD data: https://hub.wrgl.co/@ipno/r/data (use `data_years.csv` to see the years the data is coming from)
- FBI Crime Data Explorer Employement data: https://cde.ucr.cjis.gov/LATEST/webapp/#/pages/downloads
- Law Enforcement Agency Identifiers Crosswalk data: https://www.icpsr.umich.edu/web/ICPSR/studies/35158/datadocumentation#
- 2020 U.S. Census demographics data: https://data.census.gov/table?g=040XX00US22,22$0500000&y=2020&d=DEC+Redistricting+Data+(PL+94-171)&tid=DECENNIALPL2020.P1

## Visualization
The vizualizations for this project were primarily creating using [Infogram](https://infogram.com/). The only other visualization used were created through R Shiny and the code is provided in the above. 

## Product
The dashboard can be found here: https://www.aclujusticelab.org/dashboard/ 
