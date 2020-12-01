# Natural Mortality Estimate for Oregon Dungeness Crab 


This repository contains the datasets and code for estimating natural mortality rates of sub-legal male Dungeness crab. 

Folders include:
- Data 
- Code 
- library (within code folder)

## Steps to calculating the mortality estimate:
- Copt the repository to your machine. 

- Run the *Estimating_Mortality.Rmd* to derive both of the mortality estimates.

- To account for variation and a small sample size, run the *Resampling.Rmd* file. 


#### To view distributions and graphs of datasets, in the **Code** folder, run *Distributions.Rmd*. 
---
#### If you would like to source from the raw .csv files, complete the following:
1. Go to the **Data** folder and download "*CrabSizes2020.csv*", "*Crabbing_data_MCR.csv*", and "*Crabbing_data_N.csv*".
2. Go to the **Code** folder and run *Data_Manipulation.Rmd*. 
3. In the same folder run the *Estimating_Mortality.Rmd* followed by *Resampling.Rmd* to get your bootstrapped estimate.


## Contact Information
mcleodmo@oregonstate.edu

