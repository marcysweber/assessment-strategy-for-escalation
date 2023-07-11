This is a repository for the research project:

Steep hierarchies without skew? Modeling how ecology and decision-making shape despotism of relationships 

Authors:
Marcy Ekanayake-Weber, Chris O’Connor-Coates, Andreas Koenig



This repository contains a .nlogo file, which is the agent-based model itself; the ODD for the model; the raw data which was used for our American Naturalist manuscript; the interaction matrices which we further analyzed; and the results for the individual replicates included in the MS.


HOW TO RUN THE MODEL

You must have NetLogo 6.2.2 downloaded and setup on your machine. To run the model, open "dominance_despotism_energy.nlogo" in NetLogo. From there, you will be presented with an interface that allows you to control the scenario to be run, using various drop-down lists. 

"sensitivity_analysis_dominance_despotism_energy.nlogo" is a version of the model where many of the constants were implemented as parameters, so that sensitivity analysis could be performed. The code to rerun the sensitivity analyses is in the file "sensitivity_analysis_constants.R".


WHERE TO FIND THE DATA

All the data used in this study is contained in the folder "output_data". The folder "matrices" contains all the behavioral matrices used to calculate steepness as presented in the main text. These are further divided into subfolders for "wins" (i.e., fight outcomes), "attacks", and "not-attacks". Each matrix is saved as a separate CSV file. Another folder, "orderliness_each_replicate", contains the steepness, DCI, and triangle transitivity values for every single replicate (i.e., each row corresponds to each one of the matrices in the folder "matrices"). Summary data averaged across replicates for each scenario, most of which is presented in tables in the main text, is also saved in CSV files within the "output_data" folder. 


HOW TO RUN THE ANALYSIS

Most code for data analysis is located within the "R_code" folder, various R markdown (.Rmd) files. 

Some additional analysis is saved separately, in the file "sensitivity_analysis_constants.r" (containing the sensitivity analysis) and in the folder "Probabilistic_data_analysis" (containing the data for the alternative scenarios where the winners of fights were selected probabilistically). 

