# Visualizing Police Violence in Louisiana
Git for the Justice Lab: Visualizing Police Violence in Louisiana dashboard. This Git includes documentation, data, and scripts used to build this project, as well as all other work associated with the project.

<img src = https://github.com/aclu-national/JL_dashboard/blob/c9dff0e54754f8c63b683ff4e0241ba184d54a52/snippet/visual.png >

## Mission
Louisiana is experiencing an epidemic of police violence that disproportionately impacts people of color. Our mission is to enhance public awareness and promote a deeper understanding of police misconduct in Louisiana. Through this interactive platform, we aim to inform the public about police violence incidents, including killings, by providing comprehensive data visualizations, with data sourced from Mapping Police Violence, the Louisiana Law Enforcement Accountability Database, the FBI Crime Data Explorer, and the 2020 U.S. Census.

## Sources
Data on known killings by police officers were obtained by [Mapping Police Violence](https://airtable.com/shroOenW19l1m3w0H/tblxearKzw8W7ViN8) and include crowdsourced data on killings by police officers that have been reported by the media beginning in 2013. The Mapping Police Violence data are sourced from the three largest crowdsourced databases on police killings in the U.S. (FatalEncounters.org, the U.S. Police Shootings Database, and KilledbyPolice.net). These data have been supplemented by additional research on the victim’s race. More information on these data can be found on the Mapping Police Violence website. The data that we use was downloaded from Mapping Police Violence on August 21st, 2023.

Demographic information was obtained from the [2020 U.S. Census](https://data.census.gov/table?g=040XX00US22,22$0500000&y=2020&d=DEC+Redistricting+Data+(PL+94-171)&tid=DECENNIALPL2020.P1). In the analyses presented here, the Black population includes individuals who identified their race as Black or African American alone or in combination with another race. The white population includes individuals who identified their race as white and their ethnicity as not Latinx.

Data on the number of officer were obtained from the [FBI Crime Data Explorer](https://cde.ucr.cjis.gov/LATEST/webapp/#/pages/downloads). The Law Enforcement Employees dataset comprises annual data concerning personnel employed by law enforcement agencies, encompassing both officers and civilians. The Law Enforcement Agency names were cleaned using the [Law Enforcement Agency Identifiers Crosswalk, United States, 2012](https://www.icpsr.umich.edu/web/ICPSR/studies/35158/datadocumentation#) accessed through the University of Michigan on August 21st, 2023.

Data on known misconduct by police officers were obtained by [LLEAD](llead.co) through public records requests and include public data collected from a range of law enforcement agencies including police departments, sheriff’s offices, civil service commissions, and more. To learn more about how the data is collected, you can visit their website (llead.co) or their [GitHub page](https://github.com/ipno-llead). The data that we are using was downloaded from [LLEAD](https://hub.wrgl.co/@ipno/r/data) on August 21st, 2023.

## Data Creation
Records of police killings from Mapping Police Violence were retained for analysis and visualizations. Misconduct allegations, dispositions, and repercussions were categorized through a training data creation process using key word stems and machine learning models. Challenges included:

1. **Reporting Discrepancies:** Various police departments use distinct approaches to report allegations, dispositions, and repercussions, leading to differences in descriptions.
2. **Data Imbalance:** Some departments contribute significantly to misconduct incidents, causing data imbalance issues. Conversely, certain agencies report minimal allegations, making it challenging for models to provide meaningful insights.
3. **Multiple Categories within a Single Allegation:** Specific allegations and repercussions may encompass multiple categories, making assigning a solitary label challenging.

Training data on misconduct allegations from LLEAD was built on November 3rd, 2023, employing a context-based approach with 32 primary categories. To address classification challenges, we used the following comprehensive approach:

1. **Category Definition:** We manually scrutinized data to define and select categories, initially identifying patterns of misconduct.
2. **Text Clustering Techniques:** We used techniques like Gibbs Sampling Dirichlet Multinomial Mixture (GSDMM) for refining categories. While successful, some clusters lacked real-world relevance, leading to their merging with assumed categories.
3. **Jaro-Winkler Distance:** We calculated the Jaro-Winkler distance between allegations to cluster those with similar semantics, resulting in 32 primary categories.
4. **Context-Based Models:** We crafted 11 distinct models, one for each of the top 10 most frequently reported law enforcement agencies (constituting 93% of all allegations) and one for the remaining 183 agencies. This approach allowed us to tailor classification to unique agency characteristics.
5. **Keyword Analysis:** Keywords were employed to analyze unique allegations for agencies with a substantial number of training samples, leaving less than 5% of allegations unclassified. This approach was then applied to all other agencies.

After creating the training data, three different models (Random Forest, Support Vector Machine, Naive Bayes) were fit on allegations, dispositions, and repercussions using 50-50 train-test splits, including departments as a variable. The best models and hyperparameters were identified based on accuracy and F1 scores. Using these models, we predicted classifications for future misconduct data from LLEAD. Note that this process will continue to evolve as more data is downloaded.

## Data
- Mapping Police Violence data: https://airtable.com/shroOenW19l1m3w0H/tblxearKzw8W7ViN8
- LLEAD data: https://hub.wrgl.co/@ipno/r/data (use `data_years.csv` to see the years the data is coming from)
- FBI Crime Data Explorer Employement data: https://cde.ucr.cjis.gov/LATEST/webapp/#/pages/downloads
- Law Enforcement Agency Identifiers Crosswalk data: https://www.icpsr.umich.edu/web/ICPSR/studies/35158/datadocumentation#
- 2020 U.S. Census demographics data: https://data.census.gov/table?g=040XX00US22,22$0500000&y=2020&d=DEC+Redistricting+Data+(PL+94-171)&tid=DECENNIALPL2020.P1

## Product
The dashboard can be found here (currently password protect, contact eappelson@laaclu.org for access): https://www.aclujusticelab.org/data-2/
