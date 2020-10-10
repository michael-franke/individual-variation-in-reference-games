# Code for analysis for Franke & Degen (2016, "Reasoning in Reference Games" PLoS ONE)

- `data` contains data from several experiments
  - types of data: comprehension, production and salience/prior
  - files `Monster-data*.R` proprocess the data
- to run JAGS-based model fitting go to 'jags_E10/RSA_listener_mixed_E10.R' and 'jags_E10/RSA_speaker_mixed_E10.R'
- code in `BF_estimation` uses the posterior samples from JAGS to estimate Bayes factors
- the file in `MLE_RSA` obtains a maximum likelihoof fit for a standard RSA model
- files in `plot_scripts` plot results