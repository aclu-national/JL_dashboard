# What data is included? 

## Killing Data 
The killing data folder includes police killing data collected from Mapping Police Violence. 

## Overview Data
The overview data folder contains the Inter-University Consortium for Political and Social Research Law Enforcement Agency Identifiers Crosswalk, United States, 2012 Codebook (`35158-0001-Codebook.pdf`) and Data as an RDA file (`35158-0001-Data.rda`). This data is used to extract correct Law Enforcement Agency names from their identifiers. The overview data folder does not contain the 2022 FBI Crime Data Explorer Employment data because the file is too large to store in GitHub. Please download it [here](https://cde.ucr.cjis.gov/LATEST/webapp/#). This data contains law enforcement employement statistics.

## Misconduct Data 
The misconduct data folder contains data collected from the Louisiana Law Enforcement Accountability Database. This data includes the `data_agency-reference-list.csv`, which provides the location of every POST-classified law enforcement agency in Louisiana. It also includes `data_personnel.csv`, which is a list of every law enforcement officer and their agency in Louisiana. This is used to gather key insights about individual officers. In addition, the misconduct data includes the `data_post-officer-history.csv`, which provides officers with multiple unique identifiers. This is used to ensure that we do not duplicate officers in our analysis process. Finally, the misconduct data includes `data_allegation.csv`. This contains all of the data on allegations against officers including allegations, dispositions, and actions. 