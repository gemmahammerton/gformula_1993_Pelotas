# gformula_1993_Pelotas
Stata and R code used to impute data and run -gformula- across imputed datasets (examining conduct problems and substance use in the 1993 Pelotas Birth Cohort) 

Code here is to impute data from 1993 Pelotas Birth Cohort and to perform counterfactual mediation across imputed datasets to examine whether potential snares (gang membership, school non-completion and police arrest) explain the relationship between childhood conduct problems and later substance use. There is a Stata do file and an R file. The Stata do file contains code to impute data used in the mediation model, run exposure-mediator and mediator-outcome regression models on imputed data, and run counterfactual mediation using -gformula- across the imputed datasets using a loop (this produces a logfile from which the estimates need to be obtained using the corresponding R code). The R file is to be used after the Stata do file to extract the estimates from the log files, create plots of the estimates, combine estimates across imputed datasets using Rubin's Rules, and create a table of results.

1a. Stata do file to impute data and run regressions and mediation models on imputed data

1b. R file to extract mediation estimates from Stata log file and combine across imputed datasets using Rubin's Rules
