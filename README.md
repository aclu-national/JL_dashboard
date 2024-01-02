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

## Data Cleaning and Analysis
### Police Killings
The process of cleaning police killings includes:
1. Cleaning the Mapping Police Violence data variable names.
2. Filtering for police killings in Louisiana.
3. Creating unknown options for the demographic variables.
4. Extracting different date, year, and month combinations.
5. Creating age categories.
6. Cleaning Parish names and removing duplicate victim names.
7. Retreiving 2020 Census demographic data for Parishes in Louisiana.

### Overview
The process of cleaning the overview data includes:
1. Filting the FBI Crime Data Explorer Employement data for agencies in Louisiana.
2. Joining the FBI Crime Data Explorer Employement data with Law Enforcement Agency Identifiers Crosswalk data to yeild more complete names.
3. Cleaning agency names more thoroughly

### Misconduct
The process of cleaning the misconduct data includes:



Misconduct dispositions and repercussions were categorized through a process using key word stems. However, misconduct allegations posed a challenge for the following reasons:

1. **Reporting Discrepancies:** Various police departments use distinct approaches to report allegations, leading to differences in descriptions.
2. **Data Imbalance:** Some departments contribute significantly to misconduct incidents, causing data imbalance issues. Conversely, certain agencies report minimal allegations, making it challenging for models to provide meaningful insights.
3. **Multiple Categories within a Single Allegation:** Specific allegations and repercussions may encompass multiple categories, making assigning a solitary label challenging.

To address the challenge of classifying allegations, we used the following comprehensive approach:

1. **Category Definition:** We manually scrutinized data to define and select categories, initially identifying patterns of misconduct.
2. **Text Clustering Techniques:** We used techniques like Gibbs Sampling Dirichlet Multinomial Mixture (GSDMM) for refining categories. While successful, some clusters lacked real-world relevance, leading to their merging with assumed categories.
3. **Labeling Data:** We multi-labeled 500 randomly sampled unique allegation and allegation description pairs (they represent over 6000 allegations-allegation description pairs in our total dataframe) with 41 categories. [Click here](fakelink.com) to see the categories and their descriptions.

Following the creation of the training data, we implemented a zero-shot multi-label model (regex on key word stems) and two few-shot multi-label models (Random Forest and Support Vector Machine using grid-fold cross-validation). It's important to note that the zero-shot model was trained on the entire labeled dataset, while the few-shot models underwent training using an 80-20 train-test split to ensure ample training data. The best model was identified based on overall accuracy, as well as macro and micro-F1 scores. Subsequently, using this optimized model, we predicted classifications for all misconduct allegations. Note that this process remains dynamic and will evolve with the continuous influx of additional data.

## Data
- Mapping Police Violence data: https://airtable.com/shroOenW19l1m3w0H/tblxearKzw8W7ViN8
- LLEAD data: https://hub.wrgl.co/@ipno/r/data (use `data_years.csv` to see the years the data is coming from)
- FBI Crime Data Explorer Employement data: https://cde.ucr.cjis.gov/LATEST/webapp/#/pages/downloads
- Law Enforcement Agency Identifiers Crosswalk data: https://www.icpsr.umich.edu/web/ICPSR/studies/35158/datadocumentation#
- 2020 U.S. Census demographics data: https://data.census.gov/table?g=040XX00US22,22$0500000&y=2020&d=DEC+Redistricting+Data+(PL+94-171)&tid=DECENNIALPL2020.P1

## Product
The dashboard can be found here (currently password protect, contact eappelson@laaclu.org for access): https://www.aclujusticelab.org/data-2/
