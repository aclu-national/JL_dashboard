# Visualizing Police Violence in Louisiana
Git for the Justice Lab: Visualizing Police Violence in Louisiana dashboard. This Git includes documentation, data, and scripts used to build this project, as well as all other work associated with the project.

<img src = https://github.com/aclu-national/JL_dashboard_new2/blob/a68702f6a4860122f69b8d2c89ab16a29dd8b4fc/image/visual.png >

## Mission
Louisiana is experiencing an epidemic of police violence that disproportionately impacts people of color. Our mission is to enhance public awareness and promote a deeper understanding of police misconduct in Louisiana. Through this interactive platform, we aim to inform the public about police violence incidents, including killings, by providing comprehensive data visualizations, with data sourced from Mapping Police Violence, the Louisiana Law Enforcement Accountability Database, the FBI Crime Data Explorer, and the 2020 U.S. Census.

## Basic Git Structure
<img src = https://github.com/aclu-national/JL_dashboard/blob/dba9941b92bded06f02b65751e41728554196290/image/basic_git_structure.png >

## Sources
Data on known killings by police officers were obtained by [Mapping Police Violence](https://airtable.com/shroOenW19l1m3w0H/tblxearKzw8W7ViN8) and include crowdsourced data on killings by police officers that have been reported by the media beginning in 2013. The Mapping Police Violence data are sourced from the three largest crowdsourced databases on police killings in the U.S. (FatalEncounters.org, the U.S. Police Shootings Database, and KilledbyPolice.net). These data have been supplemented by additional research on the victim’s race. More information on these data can be found on the Mapping Police Violence website. The data that we use was downloaded from Mapping Police Violence on August 21st, 2023.

Demographic information was obtained from the [2020 U.S. Census](https://data.census.gov/table?g=040XX00US22,22$0500000&y=2020&d=DEC+Redistricting+Data+(PL+94-171)&tid=DECENNIALPL2020.P1). In the analyses presented here, the Black population includes individuals who identified their race as Black or African American alone or in combination with another race. The white population includes individuals who identified their race as white and their ethnicity as not Latinx.

Data on the number of officer were obtained from the [FBI Crime Data Explorer](https://cde.ucr.cjis.gov/LATEST/webapp/#/pages/downloads). The Law Enforcement Employees dataset comprises annual data concerning personnel employed by law enforcement agencies, encompassing both officers and civilians. The Law Enforcement Agency names were cleaned using the [Law Enforcement Agency Identifiers Crosswalk, United States, 2012](https://www.icpsr.umich.edu/web/ICPSR/studies/35158/datadocumentation#) accessed through the University of Michigan on August 21st, 2023.

Data on known misconduct by police officers were obtained by [LLEAD](llead.co) through public records requests and include public data collected from a range of law enforcement agencies including police departments, sheriff’s offices, civil service commissions, and more. To learn more about how the data is collected, you can visit their website (llead.co) or their [GitHub page](https://github.com/ipno-llead). The data that we are using was downloaded from [LLEAD](https://hub.wrgl.co/@ipno/r/data) on August 21st, 2023.

## Data Cleaning
### Police Killings
The process of cleaning police killings includes (see the code and analysis here):
1. Cleaning the Mapping Police Violence data variable names.
2. Filtering for police killings in Louisiana.
3. Creating unknown options for the demographic variables.
4. Extracting different date, year, and month combinations.
5. Creating age categories.
6. Cleaning Parish names and removing duplicate victim names.
7. Retreiving 2020 Census demographic data for Parishes in Louisiana.

### Overview
The process of cleaning the overview data includes (see the code and analysis here):
1. Filtering the FBI Crime Data Explorer Employement data for agencies in Louisiana.
2. Joining the FBI Crime Data Explorer Employement data with Law Enforcement Agency Identifiers Crosswalk data to yeild more complete names.
3. Cleaning agency names more thoroughly

### Misconduct
The process of cleaning the misconduct data includes (see the code and analysis here):
1. Creating a full name variable in the LLEAD personnel data.
2. Joining the LLEAD personnel data with the LLEAD History ID data via their unique identifiers.
3. Renaming the repeated officer demographic variables.
4. Joining the above dataframe with the LLEAD agency location data via the agency names.
5. Categorizing the agencies.
6. Cleaning the agency names.
7. Using regex to categorize the dispositions and actions
8. Using a multi-label classification model to categorize allegations (click here to see how we built this model). 

## Data
- Mapping Police Violence data: https://airtable.com/shroOenW19l1m3w0H/tblxearKzw8W7ViN8
- LLEAD data: https://hub.wrgl.co/@ipno/r/data (use `data_years.csv` to see the years the data is coming from)
- FBI Crime Data Explorer Employement data: https://cde.ucr.cjis.gov/LATEST/webapp/#/pages/downloads
- Law Enforcement Agency Identifiers Crosswalk data: https://www.icpsr.umich.edu/web/ICPSR/studies/35158/datadocumentation#
- 2020 U.S. Census demographics data: https://data.census.gov/table?g=040XX00US22,22$0500000&y=2020&d=DEC+Redistricting+Data+(PL+94-171)&tid=DECENNIALPL2020.P1

## Product
The dashboard can be found here (currently password protect, contact eappelson@laaclu.org for access): https://www.aclujusticelab.org/data-2/
