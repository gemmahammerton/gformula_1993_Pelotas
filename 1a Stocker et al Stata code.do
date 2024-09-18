*Stata code for main analyses in Stocker et al "Childhood conduct problems, potential snares in adolescence and problematic substance use in Brazil"

*dataset contains 4,599 participants and 38 variables with missing data on some variables
use "current drugs.dta", clear

*analysis variables in dataset

*exposure (assessed at age 11)
*"conduct_11": conduct problems (binary) 

*outcomes (assessed at age 22)
*"audit_haz_22": hazardous alcohol consumption (binary) 
*"drugs_22": illicit drug use (binary)

*mediators (assessed between age 18 and 22)
*"gang_18": gang membership at age 18 (binary) 
*"gang_22": gang membership at age 22 (binary) 
*"edu_22": school non-completion (binary) 
*"arrest_18": police arrest (binary)

*intermediate confounders (assessed between age 11 and 15)
*"cigf_15": frequency of cigarette use (ordinal) 
*"alcf_15": frequency of alcohol use (ordinal) 
*"peer_drugs_15": peer drug use (binary) 
*"peer_dev_11": peer deviance (binary) 

*baseline confounders (assessed perinatally or age 11)
*"female": female sex (binary) 
*"rs_soc": sociodemographic risk score (numeric) 
*"rs_bio": health risk score (numeric) 
*"matdep_11": maternal depression (numeric) 
*"fear_11": fear of the neighbourhood (binary) 
*"sep_11": parental separation (binary) 
*"par_smoke_11": parental smoking (binary) 
*"par_alc_11": parental alcohol use (binary) 
*"mum_rel_11": relationship with mother (numeric) 
*"dad_rel_11": relationship with father (numeric) 

*comorbidity
*"hyper_11": hyperactivity problems at age 11 (numeric) 
*"emo_15": emotional problems at age 15 (numeric)

*auxiliary variables in dataset
 *"a_malcohol": maternal alcohol consumption in pregnancy 
 *"a_msmoke": maternal smoking in prepnancy 
 *"emo_11": emotional problems at age 11
 *"audit_18": alcohol use at age 18
 *"drugs_18": drug use at age 18
 *"drugs_15": drug use at age 15 
 *"edu_18": repeating a school grade at age 18
 *"edu_15": repeating a school grade at age 15 
 *"aces_15": adverse childhood experiences at age 15 
 *"cp_15": conduct problems at age 15
 *"cig_11": cigarette use at age 11 
 *"alc_11": alcohol use at age 11 
 *"edu_11": repeating a school grade at age 11 
 *"fearc_11": child report of fear of the neighbourhood at age 11 
 *"cpc_11": child report of conduct problems at age 11

*1) Impute data
******************************************************************************************************

*set up the dataset for imputation
mi set flong
*register the complete variables (that don't need to be imputed)
mi register regular female rs_soc sep_11 a_malcohol a_msmoke
*register the variables with missing values (to be imputed)
mi register imputed conduct_11 audit_haz_22 drugs_22 gang_18 gang_22 edu_22 arrest_18 cigf_15 alcf_15 peer_drugs_15 peer_dev_11 rs_bio matdep_11 fear_11 par_smoke_11 par_alc_11 mum_rel_11 dad_rel_11 hyper_11 emo_15 emo_11 audit_18 drugs_18 drugs_15 edu_18 edu_15 aces_15 cp_15 cig_11 alc_11 edu_11 fearc_11 cpc_11
	
*gang membership needs to be imputed passively so reverse code	
*00 0  11  1
*01 1  10  0
*10 1  01  0
*11 1  00  0
recode gang_18 (0=1) (1=0)
recode gang_22 (0=1) (1=0)
	
*add in omit commands for auxiliary variables when they are not associated with variable to be imputed
*create 40 imputed datasets

local gang_18_22 "(gang_18*gang_22)"
local gangs "i.gang_18 i.gang_22"
	
set more off
mi impute chained ///
 (logit, include(`gang_18_22') omit(`gangs' i.a_msmoke i.drugs_18 i.drugs_15 i.fearc_11 audit_18 edu_18 aces_15 i.cig_11 emo_11)) conduct_11 ///
 (logit, include(`gang_18_22') omit(`gangs' i.a_malcohol i.a_msmoke i.drugs_15 i.fearc_11 emo_11 edu_18 edu_15 aces_15 cp_15 i.cig_11 edu_11 cpc_11)) audit_haz_22 ///
 (logit, include(`gang_18_22') omit(`gangs' i.a_malcohol i.a_msmoke i.drugs_15 i.fearc_11 emo_11 edu_18 edu_15 aces_15 cp_15 i.cig_11 cpc_11)) drugs_22 ///
 (logit, omit(i.a_malcohol i.a_msmoke i.drugs_15 i.fearc_11 emo_11 edu_18 edu_15 aces_15 cp_15 i.cig_11 cpc_11)) gang_18 gang_22 /// 
 (logit, include(`gang_18_22') omit(`gangs' i.a_malcohol i.a_msmoke i.drugs_18 i.drugs_15 i.fearc_11 emo_11 aces_15 i.cig_11 i.alc_11 cpc_11)) edu_22 ///
 (logit, include(`gang_18_22') omit(`gangs' i.a_malcohol i.fearc_11 emo_11 aces_15 cp_15 i.alc_11 edu_11 cpc_11 i.drugs_18 audit_18 edu_18)) arrest_18 ///
 (logit, include(`gang_18_22') omit(`gangs' i.a_malcohol i.a_msmoke i.fearc_11 emo_11 edu_18 cp_15 cpc_11 i.drugs_15 edu_15 aces_15)) peer_drugs_15 ///
 (logit, include(`gang_18_22') omit(`gangs' i.a_malcohol i.a_msmoke i.drugs_15 audit_18 edu_18 edu_15 aces_15 i.cig_11 edu_11 cpc_11 emo_11)) peer_dev_11 ///
 (logit, include(`gang_18_22') omit(`gangs' i.a_malcohol i.a_msmoke i.drugs_18 i.drugs_15 audit_18 edu_18 edu_15 aces_15 cp_15 i.cig_11 i.alc_11 cpc_11 emo_11)) fear_11 ///
 (logit, include(`gang_18_22') omit(`gangs' i.drugs_18 i.drugs_15 i.fearc_11 audit_18 edu_18 edu_15 aces_15 cp_15 i.cig_11 cpc_11 emo_11)) par_smoke_11 ///
 (logit, include(`gang_18_22') omit(`gangs' i.a_msmoke i.drugs_18 i.drugs_15 i.fearc_11 edu_18 edu_15 aces_15 edu_11 cpc_11 emo_11)) par_alc_11 ///
 (logit, include(`gang_18_22') omit(`gangs')) drugs_18 fearc_11 ///
 (logit, include(`gang_18_22') omit(`gangs' i.a_malcohol i.a_msmoke i.fearc_11 emo_11 audit_18 edu_18 cp_15 i.alc_11 cpc_11 i.peer_drugs_15 edu_15 aces_15)) drugs_15 /// 
 (ologit, include(`gang_18_22') omit(`gangs' i.fearc_11 emo_11 edu_18 cpc_11 i.drugs_15 edu_15 aces_15)) cigf_15 ///
 (ologit, include(`gang_18_22') omit(`gangs' i.a_msmoke emo_11 cp_15 cpc_11 i.drugs_15 edu_15 aces_15)) alcf_15 ///
 (ologit, include(`gang_18_22') omit(`gangs')) cig_11 alc_11 ///
 (pmm, knn(10) include(`gang_18_22') omit(`gangs' i.drugs_18 i.drugs_15 emo_11 audit_18 aces_15 cp_15 i.cig_11 cpc_11 i.a_malcohol i.a_msmoke)) rs_bio ///
 (pmm, knn(10) include(`gang_18_22') omit(`gangs' i.a_malcohol i.drugs_18 i.drugs_15 audit_18 edu_18 edu_15 i.cig_11 i.alc_11 emo_11)) matdep_11 ///
 (pmm, knn(10) include(`gang_18_22') omit(`gangs' i.a_malcohol i.a_msmoke i.drugs_18 i.drugs_15 emo_11 edu_18 i.fearc_11 i.cig_11 i.alc_11 edu_11 cpc_11)) mum_rel_11 ///
 (pmm, knn(10) include(`gang_18_22') omit(`gangs' i.a_malcohol i.a_msmoke i.drugs_15 audit_18 edu_18 i.fearc_11 i.cig_11 i.alc_11 edu_11 cpc_11)) dad_rel_11 ///
 (pmm, knn(10) include(`gang_18_22') omit(`gangs' i.a_malcohol i.a_msmoke i.drugs_18 i.drugs_15 aces_15 edu_11 emo_11)) hyper_11 ///
 (pmm, knn(10) include(`gang_18_22') omit(`gangs' i.a_malcohol i.a_msmoke i.drugs_18 i.fearc_11 edu_18 edu_15 i.cig_11 edu_11 cp_15)) emo_15 ///
 (pmm, knn(10) include(`gang_18_22') omit(`gangs')) emo_11 audit_18 edu_18 edu_15 aces_15 cp_15 edu_11 cpc_11 ///
	= i.female rs_soc i.sep_11 i.a_malcohol i.a_msmoke, add(40) rseed(1234) noisily		

*create combined variable for gang membership at either 18 or 22 years to use in analysis	
gen gang_18_22 = gang_18*gang_22
recode gang_18_22 (0=1) (1=0)

*the imputations are indexed by three new variables - _mi_id is the participant ID number; _mi_m is the imputation number (1 through 40, where 0 is the original, non-imputed, data); _mi_miss indicates which IDs had complete data for all the variables in the imputation model
sum _mi_id _mi_miss _mi_m

*Monte Carlo error 
*check whether the Monte Carlo error of B is approximately 10 per cent of its standard error
mi estimate, mcerror: logistic drugs_22 conduct_11 gang_18_22 edu_22 arrest_18 female rs_soc rs_bio matdep_11 fear_11 sep_11 par_smoke_11 par_alc_11 mum_rel_11 dad_rel_11 hyper_11

*estimate the mean and summary stats for the original data and compare it with imputed data
foreach var of varlist drugs_22 audit_haz_22 conduct_11 gang_18_22 edu_22 arrest_18 female rs_soc rs_bio matdep_11 fear_11 sep_11 par_smoke_11 par_alc_11 mum_rel_11 dad_rel_11 cigf_15 alcf_15 peer_drugs_15 peer_dev_11 emo_15 hyper_11 {
tab _mi_m, sum(`var')
}

save "current drugs_mi.dta", replace

*2) Regression models using imputed data
******************************************************************************************************

*regress mediators on exposure
*school completion
mi estimate, or: logistic edu_22 conduct_11
mi estimate, or: logistic edu_22 conduct_11 female rs_soc rs_bio matdep_11 sep_11 dad_rel_11 mum_rel_11 par_alc_11 par_smoke_11 fear_11
mi estimate, or: logistic edu_22 conduct_11 female rs_soc rs_bio matdep_11 sep_11 dad_rel_11 mum_rel_11 par_alc_11 par_smoke_11 fear_11 hyper_11

*police arrest
mi estimate, or: logistic arrest_18 conduct_11
mi estimate, or: logistic arrest_18 conduct_11 female rs_soc rs_bio matdep_11 sep_11 dad_rel_11 mum_rel_11 par_alc_11 par_smoke_11 fear_11
mi estimate, or: logistic arrest_18 conduct_11 female rs_soc rs_bio matdep_11 sep_11 dad_rel_11 mum_rel_11 par_alc_11 par_smoke_11 fear_11 hyper_11

*gang membership
mi estimate, or: logistic gang_18_22 conduct_11 
mi estimate, or: logistic gang_18_22 conduct_11 female rs_soc rs_bio matdep_11 sep_11 dad_rel_11 mum_rel_11 par_alc_11 par_smoke_11 fear_11
mi estimate, or: logistic gang_18_22 conduct_11 female rs_soc rs_bio matdep_11 sep_11 dad_rel_11 mum_rel_11 par_alc_11 par_smoke_11 fear_11 hyper_11

*regress outcome on mediators (inc intermediate confounders)
*drugs
*school completion
mi estimate, or: logistic drugs_22 edu_22
mi estimate, or: logistic drugs_22 edu_22 conduct_11 female rs_soc rs_bio matdep_11 sep_11 dad_rel_11 mum_rel_11 par_alc_11 par_smoke_11 fear_11 i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11
mi estimate, or: logistic drugs_22 edu_22 conduct_11 female rs_soc rs_bio matdep_11 sep_11 dad_rel_11 mum_rel_11 par_alc_11 par_smoke_11 fear_11 i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 hyper_11 emo_15
mi estimate, or: logistic drugs_22 edu_22 conduct_11 female rs_soc rs_bio matdep_11 sep_11 dad_rel_11 mum_rel_11 par_alc_11 par_smoke_11 fear_11 i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 hyper_11 emo_15 arrest_18 gang_18_22

*police arrest
mi estimate, or: logistic drugs_22 arrest_18
mi estimate, or: logistic drugs_22 arrest_18 conduct_11 female rs_soc rs_bio matdep_11 sep_11 dad_rel_11 mum_rel_11 par_alc_11 par_smoke_11 fear_11 i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11
mi estimate, or: logistic drugs_22 arrest_18 conduct_11 female rs_soc rs_bio matdep_11 sep_11 dad_rel_11 mum_rel_11 par_alc_11 par_smoke_11 fear_11 i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 hyper_11 emo_15

*gang membership
mi estimate, or: logistic drugs_22 gang_18_22
mi estimate, or: logistic drugs_22 gang_18_22 conduct_11 female rs_soc rs_bio matdep_11 sep_11 dad_rel_11 mum_rel_11 par_alc_11 par_smoke_11 fear_11 i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11
mi estimate, or: logistic drugs_22 gang_18_22 conduct_11 female rs_soc rs_bio matdep_11 sep_11 dad_rel_11 mum_rel_11 par_alc_11 par_smoke_11 fear_11 i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 hyper_11 emo_15

*alcohol
*school completion
mi estimate, or: logistic audit_haz_22 edu_22
mi estimate, or: logistic audit_haz_22 edu_22 conduct_11 female rs_soc rs_bio matdep_11 sep_11 dad_rel_11 mum_rel_11 par_alc_11 par_smoke_11 fear_11 i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11
mi estimate, or: logistic audit_haz_22 edu_22 conduct_11 female rs_soc rs_bio matdep_11 sep_11 dad_rel_11 mum_rel_11 par_alc_11 par_smoke_11 fear_11 i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 hyper_11 emo_15
mi estimate, or: logistic audit_haz_22 edu_22 conduct_11 female rs_soc rs_bio matdep_11 sep_11 dad_rel_11 mum_rel_11 par_alc_11 par_smoke_11 fear_11 i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 hyper_11 emo_15 arrest_18 gang_18_22

*police arrest
mi estimate, or: logistic audit_haz_22 arrest_18
mi estimate, or: logistic audit_haz_22 arrest_18 conduct_11 female rs_soc rs_bio matdep_11 sep_11 dad_rel_11 mum_rel_11 par_alc_11 par_smoke_11 fear_11 i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11
mi estimate, or: logistic audit_haz_22 arrest_18 conduct_11 female rs_soc rs_bio matdep_11 sep_11 dad_rel_11 mum_rel_11 par_alc_11 par_smoke_11 fear_11 i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 hyper_11 emo_15

*gang membership
mi estimate, or: logistic audit_haz_22 gang_18_22
mi estimate, or: logistic audit_haz_22 gang_18_22 conduct_11 female rs_soc rs_bio matdep_11 sep_11 dad_rel_11 mum_rel_11 par_alc_11 par_smoke_11 fear_11 i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11
mi estimate, or: logistic audit_haz_22 gang_18_22 conduct_11 female rs_soc rs_bio matdep_11 sep_11 dad_rel_11 mum_rel_11 par_alc_11 par_smoke_11 fear_11 i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 hyper_11 emo_15

*3) Mediation models using imputed data
******************************************************************************************************

*run gformula across the imputed datasets using a loop - this produces a logfile from which the estimates need to be obtained (this can be done using R - see "Stocker et al R code")

*drugs
*1a: run all mediators together and drugs (unadjusted)
log using "gformula_on_imp1a.log", replace

forvalues impdata = 1/40 { 
	di "Imp number is:  " `impdata'
	use "current drugs_mi.dta", clear
	di "`impdata'"
	keep if _mi_m==`impdata'
	
local exposure "conduct_11"
local mediator "gang_18_22 edu_22 arrest_18"
local outcome "drugs_22"

gformula `outcome' `exposure' `mediator', ///
mediation outcome(`outcome') exposure(`exposure') mediator(`mediator') ///
commands(`outcome':logit, `mediator':logit) ///
equations(`outcome': `exposure' `mediator', ///
gang_18_22: `exposure', ///
edu_22: `exposure', ///
arrest_18: `exposure') control(`mediator':0) obe ///
samples(50) seed(79) moreMC sim(10000) minsim logOR 

return list 
}

log close

*1b: run all mediators together and drugs (baseline and intermediate confounders)
log using "gformula_on_imp1b.log", replace

forvalues impdata = 1/40 { 
	di "Imp number is:  " `impdata'
	use "current drugs_mi.dta", clear
	di "`impdata'"
	keep if _mi_m==`impdata'
	
local exposure "conduct_11"
local baseline "female rs_soc rs_bio matdep_11 fear_11 sep_11 par_smoke_11 par_alc_11 mum_rel_11 dad_rel_11"
local intermediate "cigf_15 alcf_15 peer_drugs_15 peer_dev_11"
local mediator "gang_18_22 edu_22 arrest_18"
local outcome "drugs_22"

gformula `outcome' `exposure' `mediator' `baseline' `intermediate', ///
mediation outcome(`outcome') exposure(`exposure') mediator(`mediator') post_confs(`intermediate') base_confs(`baseline') ///
commands(`outcome':logit, `mediator':logit, peer_dev_11 peer_drugs_15:logit, alcf_15 cigf_15:ologit) ///
equations(`outcome': `exposure' `mediator' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 `baseline', ///
gang_18_22: `exposure' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 `baseline', ///
edu_22: `exposure' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 `baseline', ///
arrest_18: `exposure' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 `baseline', ///
peer_drugs_15: i.alcf_15 i.cigf_15 peer_dev_11 `exposure' `baseline', ///
alcf_15: i.cigf_15 peer_dev_11 `exposure' `baseline', ///
cigf_15: peer_dev_11 `exposure' `baseline', ///
peer_dev_11: `exposure' `baseline') control(`mediator':0) obe ///
samples(50) seed(79) moreMC sim(10000) minsim logOR 

return list 
}

log close

*1c: run all mediators together and drugs (comorbidity)
log using "gformula_on_imp1c.log", replace

forvalues impdata = 1/40 { 
	di "Imp number is:  " `impdata'
	use "current drugs_mi.dta", clear
	di "`impdata'"
	keep if _mi_m==`impdata'
	
local exposure "conduct_11"
local baseline "female rs_soc rs_bio matdep_11 fear_11 sep_11 par_smoke_11 par_alc_11 mum_rel_11 dad_rel_11 hyper_11"
local intermediate "cigf_15 alcf_15 peer_drugs_15 peer_dev_11 emo_15"
local mediator "gang_18_22 edu_22 arrest_18"
local outcome "drugs_22"

gformula `outcome' `exposure' `mediator' `baseline' `intermediate', ///
mediation outcome(`outcome') exposure(`exposure') mediator(`mediator') post_confs(`intermediate') base_confs(`baseline') ///
commands(`outcome':logit, `mediator':logit, peer_dev_11 peer_drugs_15:logit, alcf_15 cigf_15:ologit, emo_15:regress) ///
equations(`outcome': `exposure' `mediator' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 emo_15 `baseline', ///
gang_18_22: `exposure' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 emo_15 `baseline', ///
edu_22: `exposure' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 emo_15 `baseline', ///
arrest_18: `exposure' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 emo_15 `baseline', ///
emo_15: peer_drugs_15 i.alcf_15 i.cigf_15 peer_dev_11 `exposure' `baseline', ///
peer_drugs_15: i.alcf_15 i.cigf_15 peer_dev_11 `exposure' `baseline', ///
alcf_15: i.cigf_15 peer_dev_11 `exposure' `baseline', ///
cigf_15: peer_dev_11 `exposure' `baseline', ///
peer_dev_11: `exposure' `baseline') control(`mediator':0) obe ///
samples(50) seed(79) moreMC sim(10000) minsim logOR 

return list 
}

log close

*alcohol
*2a: run all mediators together and alcohol (unadjusted)
log using "gformula_on_imp2a.log", replace

forvalues impdata = 1/40 { 
	di "Imp number is:  " `impdata'
	use "current drugs_mi.dta", clear
	di "`impdata'"
	keep if _mi_m==`impdata'
	
local exposure "conduct_11"
local mediator "gang_18_22 edu_22 arrest_18"
local outcome "audit_haz_22"

gformula `outcome' `exposure' `mediator', ///
mediation outcome(`outcome') exposure(`exposure') mediator(`mediator') ///
commands(`outcome':logit, `mediator':logit) ///
equations(`outcome': `exposure' `mediator', ///
gang_18_22: `exposure', ///
edu_22: `exposure', ///
arrest_18: `exposure') control(`mediator':0) obe ///
samples(50) seed(79) moreMC sim(10000) minsim logOR 

return list 
}

log close

*2b: run all mediators together and alcohol (baseline and intermediate confounders)
log using "gformula_on_imp2b.log", replace

forvalues impdata = 1/40 { 
	di "Imp number is:  " `impdata'
	use "current drugs_mi.dta", clear
	di "`impdata'"
	keep if _mi_m==`impdata'
	
local exposure "conduct_11"
local baseline "female rs_soc rs_bio matdep_11 fear_11 sep_11 par_smoke_11 par_alc_11 mum_rel_11 dad_rel_11"
local intermediate "cigf_15 alcf_15 peer_drugs_15 peer_dev_11"
local mediator "gang_18_22 edu_22 arrest_18"
local outcome "audit_haz_22"

gformula `outcome' `exposure' `mediator' `baseline' `intermediate', ///
mediation outcome(`outcome') exposure(`exposure') mediator(`mediator') post_confs(`intermediate') base_confs(`baseline') ///
commands(`outcome':logit, `mediator':logit, peer_dev_11 peer_drugs_15:logit, alcf_15 cigf_15:ologit) ///
equations(`outcome': `exposure' `mediator' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 `baseline', ///
gang_18_22: `exposure' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 `baseline', ///
edu_22: `exposure' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 `baseline', ///
arrest_18: `exposure' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 `baseline', ///
peer_drugs_15: i.alcf_15 i.cigf_15 peer_dev_11 `exposure' `baseline', ///
alcf_15: i.cigf_15 peer_dev_11 `exposure' `baseline', ///
cigf_15: peer_dev_11 `exposure' `baseline', ///
peer_dev_11: `exposure' `baseline') control(`mediator':0) obe ///
samples(50) seed(79) moreMC sim(10000) minsim logOR 

return list 
}

log close


*2c: run all mediators together and alcohol (comorbidity)
log using "gformula_on_imp2c.log", replace

forvalues impdata = 1/40 { 
	di "Imp number is:  " `impdata'
	use "current drugs_mi.dta", clear
	di "`impdata'"
	keep if _mi_m==`impdata'
	
local exposure "conduct_11"
local baseline "female rs_soc rs_bio matdep_11 fear_11 sep_11 par_smoke_11 par_alc_11 mum_rel_11 dad_rel_11 hyper_11"
local intermediate "cigf_15 alcf_15 peer_drugs_15 peer_dev_11 emo_15"
local mediator "gang_18_22 edu_22 arrest_18"
local outcome "audit_haz_22"

gformula `outcome' `exposure' `mediator' `baseline' `intermediate', ///
mediation outcome(`outcome') exposure(`exposure') mediator(`mediator') post_confs(`intermediate') base_confs(`baseline') ///
commands(`outcome':logit, `mediator':logit, peer_dev_11 peer_drugs_15:logit, alcf_15 cigf_15:ologit, emo_15:regress) ///
equations(`outcome': `exposure' `mediator' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 emo_15 `baseline', ///
gang_18_22: `exposure' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 emo_15 `baseline', ///
edu_22: `exposure' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 emo_15 `baseline', ///
arrest_18: `exposure' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 emo_15 `baseline', ///
emo_15: peer_drugs_15 i.alcf_15 i.cigf_15 peer_dev_11 `exposure' `baseline', ///
peer_drugs_15: i.alcf_15 i.cigf_15 peer_dev_11 `exposure' `baseline', ///
alcf_15: i.cigf_15 peer_dev_11 `exposure' `baseline', ///
cigf_15: peer_dev_11 `exposure' `baseline', ///
peer_dev_11: `exposure' `baseline') control(`mediator':0) obe ///
samples(50) seed(79) moreMC sim(10000) minsim logOR 

return list 
}

log close


*4) Sensitivity analyses
******************************************************************************************************

*arrest and gangs as intermediate confounders

*drugs
*3a: education and drugs (baseline and intermediate confounders)
log using "gformula_on_imp3a.log", replace

forvalues impdata = 1/40 { 
	di "Imp number is:  " `impdata'
	use "current drugs_mi.dta", clear
	di "`impdata'"
	keep if _mi_m==`impdata'
	
local exposure "conduct_11"
local baseline "female rs_soc rs_bio matdep_11 fear_11 sep_11 par_smoke_11 par_alc_11 mum_rel_11 dad_rel_11"
local intermediate "cigf_15 alcf_15 peer_drugs_15 peer_dev_11 arrest_18 gang_18_22"
local mediator "edu_22 "
local outcome "drugs_22"

gformula `outcome' `exposure' `mediator' `baseline' `intermediate', ///
mediation outcome(`outcome') exposure(`exposure') mediator(`mediator') post_confs(`intermediate') base_confs(`baseline') ///
commands(`outcome':logit, `mediator':logit, peer_dev_11 peer_drugs_15 arrest_18 gang_18_22:logit, alcf_15 cigf_15:ologit) ///
equations(`outcome': `exposure' `mediator' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 arrest_18 gang_18_22 `baseline', ///
edu_22: `exposure' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 arrest_18 gang_18_22 `baseline', ///
gang_18_22: `exposure' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 arrest_18 `baseline', ///
arrest_18: `exposure' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 `baseline', ///
peer_drugs_15: i.alcf_15 i.cigf_15 peer_dev_11 `exposure' `baseline', ///
alcf_15: i.cigf_15 peer_dev_11 `exposure' `baseline', ///
cigf_15: peer_dev_11 `exposure' `baseline', ///
peer_dev_11: `exposure' `baseline') control(`mediator':0) obe ///
samples(50) seed(79) moreMC sim(10000) minsim logOR 

return list
}

log close

*3b: education and drugs (comorbidity)
log using "gformula_on_imp3b.log", replace

forvalues impdata = 1/40 { 
	di "Imp number is:  " `impdata'
	use "current drugs_mi.dta", clear
	di "`impdata'"
	keep if _mi_m==`impdata'
	
local exposure "conduct_11"
local baseline "female rs_soc rs_bio matdep_11 fear_11 sep_11 par_smoke_11 par_alc_11 mum_rel_11 dad_rel_11 hyper_11"
local intermediate "cigf_15 alcf_15 peer_drugs_15 peer_dev_11 emo_15 arrest_18 gang_18_22"
local mediator "edu_22 "
local outcome "drugs_22"

gformula `outcome' `exposure' `mediator' `baseline' `intermediate', ///
mediation outcome(`outcome') exposure(`exposure') mediator(`mediator') post_confs(`intermediate') base_confs(`baseline') ///
commands(`outcome':logit, `mediator':logit, peer_dev_11 peer_drugs_15 arrest_18 gang_18_22:logit, alcf_15 cigf_15:ologit, emo_15:regress) ///
equations(`outcome': `exposure' `mediator' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 arrest_18 gang_18_22 emo_15 `baseline', ///
edu_22: `exposure' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 arrest_18 gang_18_22 emo_15 `baseline', ///
gang_18_22: `exposure' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 arrest_18 emo_15 `baseline', ///
arrest_18: `exposure' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 emo_15 `baseline', ///
emo_15: peer_drugs_15 i.alcf_15 i.cigf_15 peer_dev_11 `exposure' `baseline', ///
peer_drugs_15: i.alcf_15 i.cigf_15 peer_dev_11 `exposure' `baseline', ///
alcf_15: i.cigf_15 peer_dev_11 `exposure' `baseline', ///
cigf_15: peer_dev_11 `exposure' `baseline', ///
peer_dev_11: `exposure' `baseline') control(`mediator':0) obe ///
samples(50) seed(79) moreMC sim(10000) minsim logOR 

return list
}

log close

*alcohol
*4a: education and alcohol (baseline and intermediate confounders)
log using "gformula_on_imp4a.log", replace

forvalues impdata = 1/40 { 
	di "Imp number is:  " `impdata'
	use "current drugs_mi.dta", clear
	di "`impdata'"
	keep if _mi_m==`impdata'
	
local exposure "conduct_11"
local baseline "female rs_soc rs_bio matdep_11 fear_11 sep_11 par_smoke_11 par_alc_11 mum_rel_11 dad_rel_11"
local intermediate "cigf_15 alcf_15 peer_drugs_15 peer_dev_11 arrest_18 gang_18_22"
local mediator "edu_22 "
local outcome "audit_haz_22"

gformula `outcome' `exposure' `mediator' `baseline' `intermediate', ///
mediation outcome(`outcome') exposure(`exposure') mediator(`mediator') post_confs(`intermediate') base_confs(`baseline') ///
commands(`outcome':logit, `mediator':logit, peer_dev_11 peer_drugs_15 arrest_18 gang_18_22:logit, alcf_15 cigf_15:ologit) ///
equations(`outcome': `exposure' `mediator' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 arrest_18 gang_18_22 `baseline', ///
edu_22: `exposure' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 arrest_18 gang_18_22 `baseline', ///
gang_18_22: `exposure' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 arrest_18 `baseline', ///
arrest_18: `exposure' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 `baseline', ///
peer_drugs_15: i.alcf_15 i.cigf_15 peer_dev_11 `exposure' `baseline', ///
alcf_15: i.cigf_15 peer_dev_11 `exposure' `baseline', ///
cigf_15: peer_dev_11 `exposure' `baseline', ///
peer_dev_11: `exposure' `baseline') control(`mediator':0) obe ///
samples(50) seed(79) moreMC sim(10000) minsim logOR 

return list
}

log close

*4b: education and alcohol (comorbidity)
log using "gformula_on_imp4b.log", replace

forvalues impdata = 1/40 { 
	di "Imp number is:  " `impdata'
	use "current drugs_mi.dta", clear
	di "`impdata'"
	keep if _mi_m==`impdata'
	
local exposure "conduct_11"
local baseline "female rs_soc rs_bio matdep_11 fear_11 sep_11 par_smoke_11 par_alc_11 mum_rel_11 dad_rel_11 hyper_11"
local intermediate "cigf_15 alcf_15 peer_drugs_15 peer_dev_11 emo_15 arrest_18 gang_18_22"
local mediator "edu_22 "
local outcome "audit_haz_22"

gformula `outcome' `exposure' `mediator' `baseline' `intermediate', ///
mediation outcome(`outcome') exposure(`exposure') mediator(`mediator') post_confs(`intermediate') base_confs(`baseline') ///
commands(`outcome':logit, `mediator':logit, peer_dev_11 peer_drugs_15 arrest_18 gang_18_22:logit, alcf_15 cigf_15:ologit, emo_15:regress) ///
equations(`outcome': `exposure' `mediator' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 arrest_18 gang_18_22 emo_15 `baseline', ///
edu_22: `exposure' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 arrest_18 gang_18_22 emo_15 `baseline', ///
gang_18_22: `exposure' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 arrest_18 emo_15 `baseline', ///
arrest_18: `exposure' i.cigf_15 i.alcf_15 peer_drugs_15 peer_dev_11 emo_15 `baseline', ///
emo_15: peer_drugs_15 i.alcf_15 i.cigf_15 peer_dev_11 `exposure' `baseline', ///
peer_drugs_15: i.alcf_15 i.cigf_15 peer_dev_11 `exposure' `baseline', ///
alcf_15: i.cigf_15 peer_dev_11 `exposure' `baseline', ///
cigf_15: peer_dev_11 `exposure' `baseline', ///
peer_dev_11: `exposure' `baseline') control(`mediator':0) obe ///
samples(50) seed(79) moreMC sim(10000) minsim logOR 

return list
}

log close

****************************************
* to extract the estimates from the logfiles created above and combine across imputed datasets using Rubin's Rule see "Stocker et al R code.R".
****************************************

