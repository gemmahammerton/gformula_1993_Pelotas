# *Example R code for analyses in Stocker et al "Childhood conduct problems, potential snares in adolescence and problematic substance use in Brazil"
# To be used after running corresponding Stata code "Stocker et al Stata code.do"
#===============================================================================
# Completed: 18/09/2024
#===============================================================================

# Notes:
# Code originally written by Andrea Cortes and adapted by Kimberly Burrows
# input files are Stata log files:
## g-formula logs with "return" called after model has run to give list of scalars with
# model estimates needed.
# If HPC used to run each imputation set individually, log files need appending

# Example code for unadjusted mediation model for drug use using "gformula_on_imp1a.log"

#-------------------------------------------------------------------------------
# Admin ------------------------------------------------------------------------
rm(list = ls())#clear global R environment

# Library
library(data.table) #v.1.14.0
library(readr)
library(stringr)
library(reshape2)
library(tidyverse)
library(gt)
library(ggplot2)

## Define paths for input/output
Tables <-
  "enter file location/tables/"
Figures <-
  "enter file location/figures/"
Data <-
  "enter file location/data/"
Scripts <-
  "enter file location/scripts/"

setwd(Data)
log_outcome <- readLines(paste0(Data,"mediation_logs/gformula_on_imp1a.log"))

#===============================================================================
# Extract estimates from log files:
#===============================================================================

# Outcome Y
DT_log <- data.table(txt = log_outcome[grepl(pattern = 'Imp number is:  |) =', log_outcome)]) # to convert it to 1 column data table + get only those columns with the info I need.
DT_log <- DT_log[!grepl(". di ", DT_log$txt),]
DT_log[, grp := cumsum(grepl('Imp number is', txt))] # putting a number per each imputation, based on when "MC_sims" appears.
str(DT_log)
DT_log$var2 <- parse_number(DT_log$txt) # Extracting numbers (note error is expected if r(cde) and r(se_cde) are not being used in project)
DT_log$var3 <- unlist(str_extract_all(DT_log$txt,  "Imp number|(?<=\\().+?(?=\\))")) # To extract all things within brackets ().Unlist: so output is not a list.
#https://stackoverflow.com/questions/8613237/extract-info-inside-all-parenthesis-in-r
#https://stackoverflow.com/questions/16502219/how-to-use-cast-in-reshape-without-aggregation

DT_log$seq <- with(DT_log, ave(grp, var3, FUN = seq_along))
(gformula_dataset <- dcast(grp + seq ~ var3, data = DT_log, value.var = "var2"))
gformula_dataset$grp <- NULL

# Reordering columns
names(gformula_dataset)
col_order <- c("seq", "Imp number", "tce", "se_tce", "nde", "se_nde", "nie", "se_nie", "pm", "se_pm", "cde", "se_cde", "MC_sims", "N")
(outcome_data <- gformula_dataset[, col_order])

#===============================================================================
# Plots of estimates (hist and density)
#===============================================================================

# TCE
bins <- (2 * IQR(outcome_data$tce)) / length(outcome_data$tce)^(1/3)

pdf(paste0(Figures,"tce_hist_effects.pdf"))
ggplot(outcome_data, aes(x=x) ) +
  geom_histogram( aes(x = tce, y = after_stat(density)), fill="#69b3a2", binwidth = bins ) +
  xlab("TCE") +
  ggtitle("Drug use") +
  theme_classic()
dev.off()

pdf(paste0(Figures,"tce_dens_effects.pdf"))
ggplot(outcome_data, aes(x=x) ) +
  geom_density( aes(x = tce, y = after_stat(density)), fill="#69b3a2") +
  xlab("TCE") +
  ggtitle("Drug use") +
  theme_classic()
dev.off()

pdf(paste0(Figures,"tce_dens_se.pdf"))
ggplot(outcome_data, aes(x=x) ) +
  geom_density( aes(x = se_tce, y = after_stat(density)), fill="#69b3a2" ) +
  xlab("TCE SE") +
  ggtitle("Drug use") +
  theme_classic()
dev.off()

# NIE
bins <- (2 * IQR(outcome_data$nie)) / length(outcome_data$nie)^(1/3)

pdf(paste0(Figures,"nie_hist_effects.pdf"))
ggplot(outcome_data, aes(x=x) ) +
  geom_histogram( aes(x = nie, y = after_stat(density)), fill="#69b3a2", binwidth = bins ) +
  xlab("NIE") +
  ggtitle("Drug use") +
  theme_classic()
dev.off()

pdf(paste0(Figures,"nie_dens_effects.pdf"))
ggplot(outcome_data, aes(x=x) ) +
  geom_density( aes(x = nie, y = after_stat(density)), fill="#69b3a2") +
  xlab("NIE") +
  ggtitle("Drug use") +
  theme_classic()
dev.off()

pdf(paste0(Figures,"nie_dens_se.pdf"))
ggplot(outcome_data, aes(x=x) ) +
  geom_density( aes(x = se_nie, y = after_stat(density)), fill="#69b3a2" ) +
  xlab("NIE SE") +
  ggtitle("Drug use") +
  theme_classic()
dev.off()

# NDE
bins <- (2 * IQR(outcome_data$nde)) / length(outcome_data$nde)^(1/3)

pdf(paste0(Figures,"nde_hist_effects.pdf"))
ggplot(outcome_data, aes(x=x) ) +
  geom_histogram( aes(x = nde, y = after_stat(density)), fill="#69b3a2", binwidth = bins ) +
  xlab("NDE") +
  ggtitle("Drug use") +
  theme_classic()
dev.off()

pdf(paste0(Figures,"nde_dens_effects.pdf"))
ggplot(outcome_data, aes(x=x) ) +
  geom_density( aes(x = nde, y = after_stat(density)), fill="#69b3a2") +
  xlab("NDE") +
  ggtitle("Drug use") +
  theme_classic()
dev.off()

pdf(paste0(Figures,"nde_dens_se.pdf"))
ggplot(outcome_data, aes(x=x) ) +
  geom_density( aes(x = se_nde, y = after_stat(density)), fill="#69b3a2" ) +
  xlab("NDE SE") +
  ggtitle("Drug use") +
  theme_classic()
dev.off()

#===============================================================================
# Set up tibble
#===============================================================================

# Tibble for estimates
med_mod <- tibble(
  Outcome = character(),
  TCE_coef = numeric(),
  TCE_CI_l = numeric(),
  TCE_CI_u = numeric(),
  NIE_coef = numeric(),
  NIE_CI_l = numeric(),
  NIE_CI_u = numeric(),
  NDE_coef = numeric(),
  NDE_CI_l = numeric(),
  NDE_CI_u = numeric(),
  PM = numeric()
)

#===============================================================================
# Rubins Rules
#===============================================================================

# Your Outcome
med_mod[1,1] <- "Drug use"

# TCE [cols 2-4]
(pooledMeanTCE <- mean(as.numeric(outcome_data$tce)))
(med_mod[1,2] <- exp(mean(as.numeric(outcome_data$tce)))) # Pooled TCE
(withinVarTCE <- mean(as.numeric(outcome_data$se_tce^2))) # mean of variances
(betweenVarTCE <- var(as.numeric(outcome_data$tce))) # variance of coefficients
(dfCorrectionTCE <- (nrow(outcome_data)+1)/(nrow(outcome_data))) # dfCorrection
(totVarTCE <- withinVarTCE + betweenVarTCE*dfCorrectionTCE)
#(totVarTCE <- withinVarTCE + betweenVarTCE + betweenVarTCE/80) # same thing
(pooledSETCE <- sqrt(totVarTCE)) # standard error
(med_mod[1,3] <- exp(pooledMeanTCE-(1.96*pooledSETCE))) # lower CI
(med_mod[1,4] <- exp(pooledMeanTCE+(1.96*pooledSETCE))) # upper CI

# NIE [cols 5-7]
(pooledMeanNIE <- mean(as.numeric(outcome_data$nie)))
(med_mod[1,5] <- exp(mean(as.numeric(outcome_data$nie)))) # Pooled NIE
(withinVarNIE <- mean(as.numeric(outcome_data$se_nie^2))) # mean of variances
(betweenVarNIE <- var(as.numeric(outcome_data$nie))) # variance of coefficients
(dfCorrectionNIE <- (nrow(outcome_data)+1)/(nrow(outcome_data))) # dfCorrection
(totVarNIE <- withinVarNIE + betweenVarNIE*dfCorrectionNIE)
#(totVarNIE <- withinVarNIE + betweenVarNIE + betweenVarNIE/80) # same thing
(pooledSENIE <- sqrt(totVarNIE)) # standard error
(med_mod[1,6] <- (exp(pooledMeanNIE)-(1.96*pooledSENIE))) # lower CI
(med_mod[1,7] <- (exp(pooledMeanNIE)+(1.96*pooledSENIE))) # upper CI

# NDE [cols 8-10]
(pooledMeanNDE <- mean(as.numeric(outcome_data$nde)))
(med_mod[1,8] <- exp(mean(as.numeric(outcome_data$nde)))) # Pooled NDE
(withinVarNDE <- mean(as.numeric(outcome_data$se_nde^2))) # mean of variances
(betweenVarNDE <- var(as.numeric(outcome_data$nde))) # variance of coefficients
(dfCorrectionNDE <- (nrow(outcome_data)+1)/(nrow(outcome_data))) # dfCorrection
(totVarNDE <- withinVarNDE + betweenVarNDE*dfCorrectionNDE)
#(totVarNIE <- withinVarNDE + betweenVarNDE + betweenVarNDE/80) # same thing
(pooledSENDE <- sqrt(totVarNDE)) # standard error
(med_mod[1,9] <- exp(pooledMeanNDE-(1.96*pooledSENDE))) # lower CI
(med_mod[1,10] <- exp(pooledMeanNDE+(1.96*pooledSENDE))) # upper CI

# PM [cols 1, 11]
#(med_mod[1,11] <- mean(as.numeric(outcome_data$pm))*100) # mean PM [however, I've recalculated this from the pooled estimates instead]
(med_mod[1,11] <- (pooledMeanNIE/pooledMeanTCE)*100)

# Repeat the above if there are additional outcomes required for your tables
# just make sure to change the references to the rows/cols e.g.
# med_mod[1,1] <- "Your Outcome" becomes med_mod[2,1] <- "Your second Outcome" (for row two of the table)

#===============================================================================
# Make into a pretty table
#===============================================================================

med_drugs <-gt::gt(med_mod, rownames_to_stub = FALSE) %>%
  cols_merge(columns = c("NDE_CI_l", "NDE_CI_u"), pattern = "{1}, {2}") %>%
  cols_merge(columns = c("NIE_CI_l", "NIE_CI_u"), pattern = "{1}, {2}") %>%
  cols_merge(columns = c("TCE_CI_l", "TCE_CI_u"), pattern = "{1}, {2}") %>%
  fmt_number(decimals = 3) %>%
  cols_align(align = "center") %>%
  cols_align(align = "left", columns = "Outcome") %>%
  cols_label(
    NDE_coef = "OR",
    NDE_CI_l = "95% CI",
    NIE_coef = "OR",
    NIE_CI_l = "95% CI",
    TCE_coef = "OR",
    TCE_CI_l = "95% CI"
  ) %>%
  tab_spanner(
    label = "Natural direct effect",
    columns = c("NDE_coef", "NDE_CI_l")
  ) %>%
  tab_spanner(
    label = "Natural indirect effect",
    columns = c("NIE_coef", "NIE_CI_l")
  ) %>%
  tab_spanner(
    label = "Total causal effect",
    columns = c("TCE_coef", "TCE_CI_l")
  )

med_drugs

gtsave(med_drugs, file = paste0(Tables,"table_gformula_imputations.docx"))

# END

