# Mapping the Risk of Spreading Fake News via Wisdom-of-the-Crowd & MrP

## Summary
This Github provides the necessary scripts to replicate the analysis of the paper: "Mapping the Risk of Spreading Fake-News via Wisdom-of-the-Crowd & MrP".
The objective of the scripts are threefold:
 + **Data Gathering** Gather the necessary data for the analysis;
 + **Step1** Model the reviewer ratings;
 + **Step2** Model the likelihood of spreading fake news.

Scripts are Written predominantly in R but also make use of Python to download external data and Stan to run Bayesian models.

## Data Gathering
All necessary data is provided as-is, other than the data present on this page, no data needs to be downloaded from other sources.
The scripts to download are provided but often require a private API key. To effectively run these scripts any user should first acquire his/her key and add it into the scripts. APIs and their interface may chang or be discontinued over time. 
 + The *census_data* folder contains the necessary data to create the stratification frame. The *./Scripts - MRPs/create_stratframe.R* script aggregates all the information into one comprehensible dataset.
 + The *Scripts - get merge external* folder contains the scripts to download the necessary external information.
 + The *tweets_inputdata* folder contains the raw tweets, initially downloaded from the Twitter API.

## Step 1: Aggregating reviewer rating
The paper features two ways of estimating Data: naive and model-based. 
 + Naive methods are computed by taking various averages of the existing reviewer data, they can be easily run on any modern computer.
 + Model-based methods are estimates through the fitted values of a Bayesian model. Using an i5 and 8GB of RAM the model takes ~5 days to run. The computed version of this model can be found under */stan trained models step1/*, the stan code for the model is in *stan models step1*.

All scripts to process and plot the estimates are in the *./Scripts - MRPs/step1/* folder.

## Step 2: Predicting the spread of Fake News

Lastly, the paper includes all the models based on the accuracy metrics estimated in step 1. As for step 1, the scripts for the model can be found in the *stan models step2/* folder, and their computed forms in */stan trained models step2/*. 

Note that for only one stan model multiple estimations are derived. This is because the same model is fitted to the multiple accuracy metrics (i.e. the different estimations made in step 1).

The *./Scripts - MRPs/step2/* contains all scripts to run the analysis. To run the analysis on a given accuracy metric users, should change the input accuracy object in the *./Scripts - MRPs/step2/step2_runmodel.R* script.
