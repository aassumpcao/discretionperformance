#------------------------------------------------------------------------------#
# Estimating the Effect of Discretion on Corruption:
# Evidence from Brazilian Municipalities
#
# Analysis Script
# Prepared by:
# Andre Assumpcao
# aassumpcao@unc.edu
#------------------------------------------------------------------------------#
rm(list=ls())

#------------------------------------------------------------------------------#
# README:
#
# This third script produces the analysis in the paper. If you would like to re-
# produce the analysis you should run this script after all other three have
# been run.
#
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
################################### Packages ###################################
#------------------------------------------------------------------------------#
# Minimum requirements to run script
library(tidyverse)   # Version 1.2.1
library(haven)       # Version 1.1.1
library(lubridate)   # Version 1.7.4
library(readxl)      # Version 1.1.0
library(psych)       # Version 1.8.4
library(magrittr)    # Version 1.5
library(Hmisc)       # Version 4.1.1
library(rdd)         # Version 0.99.1
library(extrafont)   # Version 0.17
library(tikzDevice)  # Version 0.11
library(stargazer)   # Version 5.2.2
library(xtable)      # Version 1.8.2
library(fuzzyjoin)   # Version 0.1.4
library(zeligverse)  # Version 5.1.6

#------------------------------------------------------------------------------#
############################## Summary Statistics ##############################
#------------------------------------------------------------------------------#
# Load datasets
load("so.data.Rda")
load("appendix.data.Rda")
load("mun.data.Rda")
load("mun.election.Rda")

#-------------------------#
# Table: Corruption Codes #
#-------------------------#
# IN MARKDOWN/LATEX ONLY

#--------------------------#
# Table: Procurement Types #
#--------------------------#
# IN MARKDOWN/LATEX ONLY

#---------------------------#
# Table: Audits by Ministry #
#---------------------------#
# Run following command then manually edit in latex.
so.data %>%
  group_by(ibge.id) %>%
  dplyr::summarize(
    so.education = max(so.education),
    so.health    = max(so.health)
  ) %>%
  with(., table(so.education, so.health)) %>%
  xtable()

#---------------------------#
# Table: Summary Statistics #
#---------------------------#
# Produce statistics (and their labels) for summary statistics table
# SO stats first
so.statistics <- c("so.amount", "infraction.count", "corruption.binary",
  "corruption.share", "corruption.amount", "mismanagement.binary",
  "mismanagement.share", "mismanagement.amount")
so.statistics.labels <- c("Amount (in R)", "Infraction Count",
  "Corruption Indicator I (Binary)",
  "Corruption Indicator II (Share of Total Infractions)",
  "Corruption Indicator III (Amount)", "Mismanagement Indicator I (Binary)",
  "Mismanagement Indicator II (Share of Total Infractions)",
  "Mismanagement Indicator III (Amount)")

# Then municipal stats second
mun.statistics <- setdiff(c(names(mun.data), names(mun.election)),
  c("ibge.id", "mun.election"))
mun.statistics.labels <- c("Urban Population (Share)", "Female (Share)",
  "Illiteracy Rate", "GDP", "Gini Index", "Human Development Indicator",
  "Poverty Rate", "Presence of AM Radio", "Education Council Established",
  "Health Council Established", "Seat of Judiciary Branch", "Vote Margin",
  "Mayor Reelection Rate")

# Now I have to join all three datasets (service order data and municipal poli-
# tical and SES characteristics). Before I do that, however, I need to remove
# duplicated election data (due to runoff elections for municipalities
# whose number of voters is larger than 200,000).
mun.election %<>%
  group_by(., mun.election) %>%
  group_by(ibge.id, add = TRUE) %>%
  slice(which.max(mun.election)) %>%
  ungroup()
  mutate(mun.election = as.Date(mun.election))

# We now join them all at once.
analysis.data <- left_join(so.data, mun.data, by = c("ibge.id" = "ibge.id")) %>%
  mutate(
    mun.election = as.Date(
      ifelse(
        audit.start <= ymd("2004-10-03"),
        ymd("2000-10-01"),
        ifelse(
          audit.start <= ymd("2008-10-05"),
          ymd("2004-10-03"),
          ymd("2008-10-05")
        )
      )
    )
  ) %>%
  left_join(
    .,
    mun.election,
    by = c("ibge.id" = "ibge.id", "mun.election" = "mun.election")
  ) %>%
  mutate(
    mun.votemargin = ifelse(
      is.na(mun.votemargin),
      mean(.$mun.votemargin, na.rm = TRUE),
      mun.votemargin
    ),
    mun.reelected  = ifelse(
      is.na(mun.reelected),
      mean(.$mun.reelected,  na.rm = TRUE),
      mun.reelected
    )
  )

# Finally, we break municipalities data apart from the main dataset
summary.stats.panelB <-
  analysis.data %>%
  dplyr::select(ibge.id, mun.statistics) %>%
  group_by(ibge.id) %>%
  dplyr::summarize_all(funs(mean))

# Produce Panel A: Service Order Summary Statistics
stargazer(
  as.data.frame(analysis.data[,so.statistics]),
  title            = "Summary Statistics",
  out              = paste0(getwd(), "/article/tab_summarystats1.tex"),
  out.header       = FALSE,
  covariate.labels = so.statistics.labels,
  align            = TRUE,
  column.sep.width = "2pt",
  digits           = 3,
  # digits.extra     = 4,
  font.size        = "small",
  header           = FALSE,
  label            = "tab:descriptivestatistics",
  table.placement  = "!htbp"
)

# Produce Panel B: Municipal Characteristics Summary Statistics
stargazer(
  as.data.frame(summary.stats.panelB[,c(mun.statistics)]),
  title            = "Summary Statistics",
  out              = paste0(getwd(), "/article/tab_summarystats2.tex"),
  out.header       = FALSE,
  covariate.labels = mun.statistics.labels,
  align            = TRUE,
  column.sep.width = "2pt",
  digits           = 3,
  # digits.extra     = 4,
  font.size        = "small",
  header           = FALSE,
  label            = "tab:descriptivestatistics",
  table.placement  = "!htbp"
)

#------------------------------------------------------------------------------#
################################### Analysis ###################################
#------------------------------------------------------------------------------#
#-----------------#
# Main regression #
#-----------------#
# First we need to create the municipal corruption variable
analysis.data %<>%
  group_by(ibge.id) %>%
  mutate(
    mun.corruption = sum(corruption.count) / sum(infraction.count),
    mun.corruption = mun.corruption - (corruption.count /sum(infraction.count)),
    mun.corruption = ifelse(is.na(mun.corruption), 0, mun.corruption)) %>%
  dplyr::select(c(1:66), mun.corruption, c(67:80))

# Check the variables we should use
names(analysis.data)

# Define vector of outcomes
outcomes <- setdiff(so.statistics, c("so.amount", "infraction.count"))

# Define outcome labels
outcome.labels <- setdiff(
  so.statistics.labels, c("Amount (in R)", "Infraction Count")
)

# Define vector of procurement-specific regressors
# so.covariates <- paste(
#   "(so.amount)", "mun.corruption", "I(mun.corruption^2)",
#   "factor(so.procurement)", sep = " + "
# )
so.covariates <- paste(
  "so.amount", "I(so.amount^2)", "mun.corruption", "I(mun.corruption^2)",
  "factor(so.procurement)", sep = " + "
)
# Define Covariates Labels
so.covariates.labels <- c("Amount (in R)", "Amount (in R, squared)",
  "Municipal Corruption", "Municipal Corruption (Squared)",
  "Procurement Type 1", "Procurement Type 2", "Procurement Type 3")

# Define vector of municipality characteristics
mun.covariates <- analysis.data %>%
  ungroup() %>%
  dplyr::select(c(67:78, 80, 81), so.education, so.health, lottery.id) %>%
  names()

# Define Covariates labels (not necessary)
# mun.covariates.labels <- c()


# Pull factor positions
factors <- grep(
  mun.covariates, pattern = "radio|council|lottery|judiciary|reelected|so\\.")

# Concatenate municipal covariates vector
for (i in factors) {
  mun.covariates[[i]] <- paste0("factor(", mun.covariates[[i]], ")")
}

# Collapse to single vector
mun.covariates <- paste(mun.covariates, collapse = " + ")

# Run service order regressions w/o covariates
for (i in seq(from = 1, to = 6)) {
  # Run each regression
  lm <- lm(
    as.formula(paste(outcomes[[i]], so.covariates, sep = " ~ ")),
    data = analysis.data
  )
  # Store corruption regressions
  if (i <= 3) {
    assign(paste0("so.lm.corruption.", i), lm)
  }
  # And mismanagement regressions
  else {
    assign(paste0("so.lm.mismanagement.", i-3), lm)
  }
}

# Run service order regressions w/ covariates
for (i in seq(from = 1, to = 6)) {
  # Run each regression
  lm <- lm(
    as.formula(
      paste(
        outcomes[[i]], paste(so.covariates, mun.covariates, sep = " + "),
        sep = " ~ "
      )
    ),
    data = analysis.data
  )
  # Store corruption regressions
  if (i <= 3) {
    assign(paste0("so.lm.corruption.covariates.", i), lm)
  }
  # And mismanagement regressions
  else {
    assign(paste0("so.lm.mismanagement.covariates.", i-3), lm)
  }
}

#---------------------------------#
# Table: First Linear Regressions #
#---------------------------------#
stargazer(
  # Regressions that will be printed to table
  list(so.lm.corruption.1, so.lm.corruption.covariates.1, so.lm.corruption.2,
  so.lm.corruption.covariates.2, so.lm.corruption.3,
  so.lm.corruption.covariates.3),

  # Table commands
  title                  = "Corruption Determinants in Brazilian Municipalities",
  out                    = paste0(getwd(), "/article/tab_mainregression.tex"),
  out.header             = FALSE,
  column.labels          = rep(
                            c(outcome.labels[[1]],
                              outcome.labels[[2]],
                              outcome.labels[[3]]
                            ),
                            2
                           ),
  column.separate        = rep(1, 6),
  covariate.labels       = so.covariates.labels,
  dep.var.labels.include = FALSE,
  align                  = TRUE,
  column.sep.width       = "2pt",
  digits                 = 3,
  # digits.extra         = 4,
  font.size              = "small",
  header                 = FALSE,
  keep                   = c("amount|corrupt|procurement"),
  label                  = "tab:mainregression",
  no.space               = TRUE,
  table.placement        = "!htbp",
  omit                   = c("control", "ministry", "lottery"),
  omit.labels            = c("Municipal Characteristics",
                             "Ministry Fixed-Effects", "Lottery Fixed-Effects"),
  omit.stat              = c("rsq", "ser"),
  star.cutoffs           = c(.1, .05, .01)
)

summary(so.lm.corruption.1)