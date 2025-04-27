Code for my Masterthesis Analysis
Janna Pauline Dirks
1. Supervisor: Dr. Katharina Pittner
Freie Universität Berlin
Master of Cognitive Neuroscience (MCNB)
Titel: Infant sleep trajectoires across the first year of life: Comparing Actigraphy and BISQ 
Berlin, April, 2025

This is the GitHub repository for the Master's thesis conducted as part of the Cognitive Neuroscience program at Freie Universität Berlin. The thesis investigates the Assesment of infant sleep trajectories over the first year of life, with a focus on comparing the trajectories of linear mixed models created based on BISQ and actigraphy data. Furthermore, I conducted a moderation analysis to investigate a possible moderating effect of maternal depression onto the association between the BISQ and actigraphy data. 

This repository contains the R-Scripts used to prepare and analyse the data. 

The study used RStudio Version 4.3.2. For the formulation of the linear mixed models, we used the lme-function of the nlme package within R that allows the fit and comparison of linear and non-linear mixed effects models. All coefficients were subject to a significance analysis assuming an alpha-level of 5%. 

Structure of the Code:

BISQPrep.R contains the Code used to clean and prepare the BISQ data for the modelling process. 

ActiPrep.R contains the Code used to clean and prepare the Actigraphy data for the modelling process. 

CESDPrep.R contains the Code used to clean and prepare the CESD data for the moderation analysis.

Deviations_BISQ_Acti.R contains the Code used to calculate and plot the deviations between the planned and real assessment timepoints in the study. 

LMM+Moderation_Script contains the formulation of the linear mixed models, the correlation of the parameters and the moderation analysis. 

