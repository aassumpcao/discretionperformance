################################################################################
# Estimating the Effect of Discretion on Government Performance:
# Evidence from Brazilian Municipalities
#
# Analysis Script
# Prepared by:
# Andre Assumpcao
# aassumpcao@unc.edu
################################################################################
rm(list = ls())

################################################################################
# README:
#
#
#
################################################################################

################################################################################
# Packages
################################################################################
# Minimum requirements to run script
# library(haven)       # Version 1.1.2
library(here)        # Version 0.1
library(tidyverse)   # Version 1.2.1
library(lubridate)   # Version 1.7.4
library(psych)       # Version 1.8.4
library(magrittr)    # Version 1.5
library(tikzDevice)  # Version 0.12
library(stargazer)   # Version 5.2.2
library(xtable)      # Version 1.8.2
library(estimatr)    # Version 0.10.0
library(rdrobust)    # Version 0.99.3
library(rdmulti)     # Version 0.20
library(rdpower)     # Version 0.2
library(extrafont)   # Version 0.17

################################################################################
# Functions
################################################################################
# Function to help subset data to be used in RDD estimation
bandwidthRange <- function(x, cutpoint, limit){
  # Args:
  #   x:        column serving as assignment variable
  #   cutpoint: cutoff assigning observations to different treatments
  #   limit:    symmetric range on either side of cutoff value

  # Returns:
  #   Logical vector used to subset dataset

  # Body:
  #   Invisibly subset data
  invisible(between(x, cutpoint - limit, cutpoint + limit))
}

# Function to break away cluster var in starprep from data. Useful when lm
# objects passed to stargazer use different datasets.
starprepMod <- function(model, data, cluster, alpha){
  # Args:
  #   model:   model passed to starprep for s.e. calculation
  #   data:    data from which to pull cluster variable
  #   cluster: cluster variable passed to starprep
  #   alpha:   significance level passed to starprep

  # Returns:
  #   starprep list

  # Body:
  #   Run starprep with used-provided data (instead of standard dataset)
  temp      <- starprep(model, cluster = data$cluster, alpha = alpha)
  temp[[2]] <- NULL

  return(temp)
}

# Function to quickly produce OLS models and clustered standard errors for first
# table
lmMod <- function(y, se = FALSE){
  # Args:
  #   y:  outcome variable
  #   se: clustered se

  # Retuns:
  #   List of lm objects OR clustered se for stargazer

  # Body:
  #   We first produce the four models to print out table
  #   Purchases
  lm.1 <- lm(as.formula(paste0(y, " ~ ", so.covariates)), data = purchases.data)
  lm.2 <- lm(as.formula(paste0(y, " ~ ", paste(so.covariates, mun.covariates,
             sep = " + "))), data = purchases.data)

  #   Works
  lm.3 <- lm(as.formula(paste0(y, " ~ ", so.covariates)), data = works.data)
  lm.4 <- lm(as.formula(paste0(y, " ~ ", paste(so.covariates, mun.covariates,
             sep = " + "))), data = works.data)

  #   Put all models in list
  models <- list(lm.1, lm.2, lm.3, lm.4)

  #   Then we produce the four vectors of s.e. for stargazer
  if (se == TRUE) {

    #   Purchases
    se.1 <- starprep(lm.1, clusters = purchases.data$ibge.id, alpha = .1)
    se.2 <- starprep(lm.2, clusters = purchases.data$ibge.id, alpha = .1)
    se.3 <- starprep(lm.3, clusters = works.data$ibge.id,     alpha = .1)
    se.4 <- starprep(lm.4, clusters = works.data$ibge.id,     alpha = .1)

    #   Put all se's in list
    se <- c(se.1, se.2, se.3, se.4)

    #   Return se vector
    return(se)

  } else {

    #   Return lm list
    return(models)
  }
}

# Function to join all bandwidths spit out by rdms() and rdmc() into one table
bandwidthTable <- function(){
  # Args:
  #   (empty)

  # Returns:
  #   List of bandwidth ranges for bandwidth table

  # Body:
  #   Add corruption and mismanagement bandwidths to table
  table <- as.tibble()

  for (i in seq(1:6)) {

      # Create empty vector which will take up bandwidth values
      indicator    <- c()
      first.third  <- c()
      second.third <- c()
      last.third   <- c()

      # Loop over cutoffs
      for (x in seq(1:3)) {

        # Define object name
        if (i < 4) obj <- paste0("non.cumulative.corruption.", i, ".cutoff.", x)
        else       obj <- paste0("non.cumulative.mismanagement.",i,".cutoff.",x)

        # Define vector elements
        first.third[x]  <- unlist(get(obj))$H2
        second.third[x] <- unlist(get(obj))$H3
        last.third[x]   <- unlist(get(obj))$H1
      }

      # Fill in empty vector
      indicator <- c(first.third, second.third, last.third)

      # Rename indicator vector
      if (i == 1) {table <- indicator}
      else        {table <- cbind(table, indicator)}

      rm(indicator)
    }

  return(as.tibble(table))
}

# Function to perform multiple t-tests across variables in different datasets
multiple_ttest <- function(x, df) {
  # Args:
  #   x:  variable for which you want the t-test performed
  #   df: dataset from which to pull variable

  # Returns:
  #   List of means and p-values

  # Body:
  #   Perform multiple t-test
  t.test(x ~ df$so.procurement, var.equal = FALSE, conf.level = .9)
}

################################################################################
# Load data
################################################################################
# Load datasets
load("so.data.Rda")
load("appendix.data.Rda")
load("mun.data.Rda")
load("mun.election.Rda")
load("falsification.data.Rda")

################################################################################
# Definition of variables included in tables
################################################################################
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
mun.statistics        <- setdiff(c(names(mun.data), names(mun.election)),
                                 c("ibge.id", "mun.election"))
mun.statistics.labels <- c("Urban Population (Share)", "Female (Share)",
                           "Illiteracy Rate", "GDP", "Gini Index",
                           "Human Development Indicator", "Poverty Rate",
                           "Presence of AM Radio",
                           "Education Council Established",
                           "Health Council Established",
                           "Seat of Judiciary Branch", "Vote Margin",
                           "Mayor Reelection Rate")

################################################################################
# Descriptive Statistics and Simple Tabulations
################################################################################
#-------------------------------------------------------------------------------
# Table: Audits by Ministry
#-------------------------------------------------------------------------------
# Run following command then manually edit in latex.
so.data %>%
  group_by(ibge.id) %>%
  summarize(so.education = max(so.education),
            so.health    = max(so.health)) %$%
  table(so.education, so.health) %>%
  xtable()

#-------------------------------------------------------------------------------
# Table: Corruption Codes
#-------------------------------------------------------------------------------
# In Latex only

#-------------------------------------------------------------------------------
# Table: Procurement Categories
#-------------------------------------------------------------------------------
# In Latex only

#-------------------------------------------------------------------------------
# Table: Descriptive Statistics
#-------------------------------------------------------------------------------
# I have to join all three datasets (service order data, municipal, political,
# and SES characteristics). Before that, however, I need to remove duplicated
# election data (due to runoff elections for municipalities whose number of
# voters is larger than 200,000).
mun.election %<>%
  group_by(., mun.election) %>%
  group_by(ibge.id, add = TRUE) %>%
  slice(which.max(mun.election)) %>%
  ungroup() %>%
  mutate(mun.election = as.Date(mun.election))

# We now join them all at once.
analysis.data <- left_join(so.data, mun.data, by = c("ibge.id" = "ibge.id")) %>%
  mutate(
    mun.election = case_when(
                     audit.start <= ymd("2004-10-03") ~ ymd("2000-10-01"),
                     audit.start <= ymd("2008-10-05") ~ ymd("2004-10-03"),
                     audit.start  > ymd("2008-10-05") ~ ymd("2008-10-05")
                   )
  ) %>%
  left_join(., mun.election,
            by = c("ibge.id" = "ibge.id", "mun.election" = "mun.election")
  ) %>%
  mutate(
    mun.votemargin = ifelse(is.na(mun.votemargin),
                            mean(.$mun.votemargin, na.rm = TRUE),
                            mun.votemargin),
    mun.reelected  = ifelse(is.na(mun.reelected),
                            mean(.$mun.reelected,  na.rm = TRUE),
                            mun.reelected),
    so.cutoff.1    = case_when(so.type == 1 ~ 8000,   so.type == 2 ~ 15000),
    so.cutoff.2    = case_when(so.type == 1 ~ 80000,  so.type == 2 ~ 150000),
    so.cutoff.3    = case_when(so.type == 1 ~ 650000, so.type == 2 ~ 1500000)
  )

# Finally, we break municipalities data apart from the main dataset
summary.stats.panelB <- analysis.data %>%
  select(ibge.id, mun.statistics) %>%
  group_by(ibge.id) %>%
  summarize_all(funs(mean))

# Produce Panel A: Service Order Summary Statistics
stargazer(
  as.data.frame(analysis.data[,so.statistics]),
  title            = "Summary Statistics",
  out              = "./article/tab_summarystats1.tex",
  out.header       = FALSE,
  covariate.labels = so.statistics.labels,
  align            = TRUE,
  column.sep.width = "2pt",
  digits           = 3,
  # digits.extra     = 4,
  font.size        = "scriptsize",
  header           = FALSE,
  label            = "tab:descriptivestatistics",
  table.placement  = "!htbp"
)

# Produce Panel B: Municipal Characteristics Summary Statistics
stargazer(
  as.data.frame(summary.stats.panelB[,c(mun.statistics)]),
  title            = "Summary Statistics",
  out              = "./article/tab_summarystats2.tex",
  out.header       = FALSE,
  covariate.labels = mun.statistics.labels,
  align            = TRUE,
  column.sep.width = "2pt",
  digits           = 3,
  # digits.extra     = 4,
  font.size        = "scriptsize",
  header           = FALSE,
  # label            = "tab:descriptivestatistics",
  table.placement  = "!htbp"
)

################################################################################
# OLS Analysis
################################################################################
#-------------------------------------------------------------------------------
# Table: Corruption Determinants
#-------------------------------------------------------------------------------
# First we need to create the municipal corruption variable
analysis.data %<>%
  group_by(ibge.id) %>%
  mutate(
    mun.corruption = sum(corruption.count) / sum(infraction.count),
    mun.corruption = mun.corruption - (corruption.count/sum(infraction.count)),
    mun.corruption = ifelse(is.na(mun.corruption), 0, mun.corruption)
  ) %>%
  select(c(1:18), contains("so.cutoff"), c(19:66), mun.corruption, c(67:83)) %>%
  ungroup()

# Subset variable for OLS regressions
purchases.data <- filter(analysis.data, so.type == 1)
works.data     <- filter(analysis.data, so.type == 2)

# Check the variables we should use
names(analysis.data)

# Define vector of outcomes
outcomes <- setdiff(so.statistics, c("so.amount", "infraction.count"))

# Define outcome labels
outcome.labels <- setdiff(so.statistics.labels,
                          c("Amount (in R)", "Infraction Count"))

# Define vector of procurement-specific regressors
so.covariates <- paste("so.amount", "I(so.amount^2)", "mun.corruption",
                       "I(mun.corruption^2)", "factor(so.procurement)",
                       sep = " + ")

# Define Covariates Labels
so.covariates.labels <- c("Amount (in R)", "Amount (in R, squared)",
                          "Municipal Corruption",
                          "Municipal Corruption (Squared)",
                          "Proc. Category 1", "Proc. Category 2",
                          "Proc. Category 3")

# Define vector of municipality characteristics
mun.covariates <- analysis.data %>%
  select(c(70:81, 83, 84), so.education, so.health, lottery.id) %>%
  names()

# Define Covariates labels (not necessary)
# mun.covariates.labels <- c()

# Pull factor positions
factors <- grep(pattern = "radio|council|lottery|judiciary|reelected|so\\.",
                mun.covariates)

# Concatenate municipal covariates vector
for (i in factors) {
  mun.covariates[[i]] <- paste0("factor(", mun.covariates[[i]], ")")
}

# Collapse to unitary vector
mun.covariates <- paste(mun.covariates, collapse = " + ")

#-------------------------------------------------------------------------------
# Run OLS regressions
#-------------------------------------------------------------------------------
# Corruption and mismananagement models and clustered standard errors
corruption.binary.models    <- lmMod("corruption.binary")
mismanagement.binary.models <- lmMod("mismanagement.binary")
se.corruption               <- lmMod("corruption.binary",    se = TRUE)
se.mismanagement            <- lmMod("mismanagement.binary", se = TRUE)

#-------------------------------------------------------------------------------
# Table: First Linear Regressions
#-------------------------------------------------------------------------------
# Corruption
stargazer(

  # Regressions that will be printed to table
  list(corruption.binary.models, mismanagement.binary.models),

  # Table commands
  title                 ="Performance Determinants in Brazilian Municipalities",
  out                   = "./article/tab_mainregression.tex",
  out.header            = FALSE,
  column.labels         = c(outcome.labels[[1]], outcome.labels[[4]]),
  column.separate       = c(4, 4),
  covariate.labels      = so.covariates.labels,
  dep.var.labels.include= FALSE,
  align                 = TRUE,

  # Provide self-calculated standard errors
  se                    = c(se.corruption, se.mismanagement),

  column.sep.width      = "2pt",
  digits                = 3,
  digits.extra          = 0,
  # initial.zero          = FALSE,
  # float.env             = "sidewaystable",
  font.size             = "scriptsize",
  header                = FALSE,
  keep                  = c("amount|corrupt|procurement"),
  label                 = "tab:mainregression",
  no.space              = TRUE,
  table.placement       = "!htbp",
  omit                  = c("control", "ministry", "lottery"),
  omit.labels           = c("Municipal Controls", "Ministry Fixed-Effects",
                            "Lottery Fixed-Effects"),
  omit.stat             = c("rsq", "ser"),
  star.cutoffs          = c(.1, .05, .01)
)

# Remove the models from the environment in R
rm(list = objects(pattern = "se\\.|\\.binary|factors|summary.stats.panelB"))

# ##############################################################################
# # Bandwidth choice test
# ##############################################################################
# # We use Cattaneo (2016)'s rules for sub-setting the sample. In cases where
# # assignment to different treatments is cumulative on the running variable,
# # they suggest using observations with running values up to c-1 and c+1 cutoff
# # values for each c cutoff.
# # Purchases
# p.bandwidth.1 <- filter(purchases.data, so.amount <=80000)
# p.bandwidth.2 <- filter(purchases.data, so.amount > 8000 & so.amount<=650000)
# p.bandwidth.3 <- filter(purchases.data, so.amount > 80000)
# w.bandwidth.1 <- filter(works.data,     so.amount <=150000)
# w.bandwidth.2 <- filter(works.data,     so.amount > 15000 &so.amount<=1500000)
# w.bandwidth.3 <- filter(works.data,     so.amount > 150000)

# # Define vector of datasets used for bandwidth tests
# purchases.bandwidth.list <- c(ls(pattern = "p\\.bandwidth\\.[0-9]"))
# works.bandwidth.list     <- c(ls(pattern = "w\\.bandwidth\\.[0-9]"))

# # Define vector of cutoffs for bandwidth tests
# purchases.cutoff.list <- c(8000, 80000, 650000)
# works.cutoff.list     <- c(15000, 150000, 1500000)

# #-------------------------------------
# # Test 1: CCT = Calonico et al. (2015)
# #-------------------------------------
# # Create list of vectors which will take up the bandwidth values
# purchases.cct.vector <- rep(0, 18)
# works.cct.vector     <- rep(0, 18)

# # Loop over cutoffs and outcomes for CCT bandwidth calculation (PURCHASES)
# for (i in seq(from = 1, to = 3)) {

#   # Loop over outcomes
#   for (x in seq(from = 1, to = 6)) {

#     # Assign temporary object to hold bandwidth calculation output
#     cct <- get(purchases.bandwidth.list[i]) %$%
#       rdbwselect(get(outcomes[[x]]),
#                  so.amount,
#                  c = as.double(purchases.cutoff.list[i]),
#                  cluster = ibge.id
#       )

#     # # Assign new object name (uncomment if you'd like to see individual
#     # # bandwidth results)
#     # assign(paste0("rdrobust.purchases.1.outcome.", x), cct)

#     # Extract bandwidth from bandwidth function and assign to bandwidth vector
#     if      (i == 1) purchases.cct.vector[x]    <- cct$bws[[1]]
#     else if (i == 2) purchases.cct.vector[x+6]  <- cct$bws[[1]]
#     else             purchases.cct.vector[x+12] <- cct$bws[[1]]
#   }
# }

# # Loop over cutoffs and outcomes for CCT bandwidth calculation (WORKS)
# for (i in seq(from = 1, to = 3)) {

#   # Loop over outcomes
#   for (x in seq(from = 1, to = 6)) {

#     # Assign temporary object to hold bandwidth calculation output
#     cct <- get(works.bandwidth.list[i]) %$%
#       rdbwselect(get(outcomes[[x]]),
#                  so.amount,
#                  c = as.double(works.cutoff.list[i]),
#                  cluster = ibge.id
#       )

#     # # Assign new object name (uncomment if you'd like to see individual
#     # # bandwidth results)
#     # assign(paste0("rdrobust.works.1.outcome.", x), cct)

#     # Extract bandwidth from bandwidth function and assign to bandwidth vector
#     if      (i == 1) works.cct.vector[x]    <- cct$bws[[1]]
#     else if (i == 2) works.cct.vector[x+6]  <- cct$bws[[1]]
#     else             works.cct.vector[x+12] <- cct$bws[[1]]
#   }
# }

# # Remove unnecessary objects
# rm(list = objects(pattern = "^(cct)$"))

# # CCT bandwidth chosen
# p.bandwidth.1 <- mean(purchases.cct.vector[1:6])
# p.bandwidth.2 <- mean(purchases.cct.vector[7:12])
# p.bandwidth.3 <- mean(purchases.cct.vector[13:18])
# w.bandwidth.1 <- mean(works.cct.vector[1:6])
# w.bandwidth.2 <- mean(works.cct.vector[7:12])
# w.bandwidth.3 <- mean(works.cct.vector[13:18])

# #------------------------
# # Table: Bandwidth Choice
# #------------------------
# # Format bandwidth limits as data frame and produce table. Last step is
# # editing the table in the TeX file.
# tibble(purchases.bandwidth = c(p.bandwidth.1, p.bandwidth.2, p.bandwidth.3),
#        works.bandwidth     = c(w.bandwidth.1, w.bandwidth.2, w.bandwidth.3)
# ) %>%
# xtable()

################################################################################
# RD Multiple, Non-Cumulative Cutoff Analysis
################################################################################
# We use Cattaneo's rdmulti package to deal with multiple cutoff nature of our
# data. First, we run non-cumulative multiple cutoff by pooling together purcha-
# ses and works and centering them around their equivalent cutoff(8k and 15k)
for (x in seq(from = 1, to = 6)) {
  cutoff.1 <- analysis.data %$%
    rdmc(Y          = get(outcomes[x]),
         X          = so.amount,
         C          = so.cutoff.1,
         pooled.opt = "level = 90, cluster = analysis.data$ibge.id, all = TRUE",
         pvec       = c(2, 2)
    )
  cutoff.2 <- analysis.data %$%
    rdmc(Y          = get(outcomes[x]),
         X          = so.amount,
         C          = so.cutoff.2,
         pooled.opt = "level = 90, cluster = analysis.data$ibge.id, all = TRUE",
         pvec       = c(2, 2)
    )
  cutoff.3 <- analysis.data %$%
    rdmc(Y          = get(outcomes[x]),
         X          = so.amount,
         C          = so.cutoff.3,
         pooled.opt = "level = 90, cluster = analysis.data$ibge.id, all = TRUE",
         pvec       = c(2, 2)
    )

  # Assign new object names
  if (x <= 3) {
    assign(paste0("non.cumulative.corruption.", x, ".cutoff.1"), cutoff.1)
    assign(paste0("non.cumulative.corruption.", x, ".cutoff.2"), cutoff.2)
    assign(paste0("non.cumulative.corruption.", x, ".cutoff.3"), cutoff.3)
  } else {
    assign(paste0("non.cumulative.mismanagement.", x, ".cutoff.1"), cutoff.1)
    assign(paste0("non.cumulative.mismanagement.", x, ".cutoff.2"), cutoff.2)
    assign(paste0("non.cumulative.mismanagement.", x, ".cutoff.3"), cutoff.3)
  }
  rm(cutoff.1, cutoff.2, cutoff.3)
}

################################################################################
# Bandwidth Choice
################################################################################
# We use the RD regressions above to construct our bandwidth table for covariate
# balance test. Here we run the bandwidthTable() function defined at the start
# of this script
# bandwidth.table <- bandwidthTable()
bandwidth.table <- readRDS("bandwidth.table.Rds")

# # Save R object which is used again in appendix
# writeRDS(bandwidth.table, file = "./bandwidth.table.Rds")

# Formatting work
bandwidth.table %>%
  rename_at(
    vars(c(1:3)),
    funs(paste0("CIndicator ", c("I", "II", "III")))
  ) %>%
  rename_at(
    vars(c(4:6)),
    funs(paste0("MIndicator ", c("I", "II", "III")))
  ) %>%
  xtable(., caption = "CCT Bandwidths (in R\\$)",
            label   = "tab:cctbandwidth",
            align   = "lrrrrrr",
            digits  = 0
  ) %>%
  print.xtable(., type              = "latex",
                  file              = "./article/tab_bandwidth.tex",
                  table.placement   = "!htbp",
                  caption.placement = "top",
                  hline.after       = c(rep(-1, 2), 0, 3, 6, rep(9, 2))
  )

################################################################################
# Covariate Balance
################################################################################
# Define purchase and works bandwidths as the average across corruption and
# mismanagement bandwidths from call to rmds() in main analysis script
bandwidth.means <- map(data.table::transpose(bandwidth.table[1:6,]), mean)

# Subset sample for corruption cutoff 1
purchases.cutoff.1 <- analysis.data %>%
  mutate(so.amount = case_when(so.type != 2 ~ so.amount - 8000)) %>%
  filter(bandwidthRange(so.amount, 0, bandwidth.means$V1))

# Subset sample for corruption cutoff 2
purchases.cutoff.2 <- analysis.data %>%
  mutate(so.amount = case_when(so.type != 2 ~ so.amount - 80000)) %>%
  filter(bandwidthRange(so.amount, 0, bandwidth.means$V2)) %>%
  filter(so.procurement != 0)

# Subset sample for corruption cutoff 3
#   Strong evidence of manipulation (McCrary(2008) test)

# Subset sample for mismanagement cutoff 1
works.cutoff.1 <- analysis.data %>%
  mutate(so.amount = case_when(so.type == 2 ~ so.amount - 15000)) %>%
  filter(bandwidthRange(so.amount, 0, bandwidth.means$V4))

# Subset sample for mismanagement cutoff 2
works.cutoff.2 <- analysis.data %>%
  mutate(so.amount = case_when(so.type == 2 ~ so.amount - 150000)) %>%
  filter(bandwidthRange(so.amount, 0, bandwidth.means$V5))

# Subset sample for mismanagement cutoff 3
works.cutoff.3 <- analysis.data %>%
  mutate(so.amount = case_when(so.type == 2 ~ so.amount - 1500000))%>%
  filter(bandwidthRange(so.amount, 0, bandwidth.means$V6))

# Perform multiple t-test and store everything back into each vector
# Purchases 1
p.balance.1 <- lapply(purchases.cutoff.1[,c(so.statistics, mun.statistics)],
                      multiple_ttest,
                      df = purchases.cutoff.1) %>%
              {lapply(c(so.statistics, mun.statistics),
                      function(x){x <- c(.[[x]][["p.value"]]
                                         # .[[x]][["estimate"]][1],
                                         # .[[x]][["estimate"]][2]
                                         )})}
# Purchases 2
p.balance.2 <- lapply(purchases.cutoff.2[,c(so.statistics, mun.statistics)],
                      multiple_ttest,
                      df = purchases.cutoff.2) %>%
              {lapply(c(so.statistics, mun.statistics),
                      function(x){x <- c(.[[x]][["p.value"]]
                                         # .[[x]][["estimate"]][1],
                                         # .[[x]][["estimate"]][2]
                                         )})}
# Works 1
w.balance.1 <- lapply(works.cutoff.1[,c(so.statistics, mun.statistics)],
                      multiple_ttest,
                      df = works.cutoff.1) %>%
              {lapply(c(so.statistics, mun.statistics),
                      function(x){x <- c(.[[x]][["p.value"]]
                                         # .[[x]][["estimate"]][1],
                                         # .[[x]][["estimate"]][2]
                                         )})}
# Works 2
w.balance.2 <- lapply(works.cutoff.2[,c(so.statistics, mun.statistics)],
                      multiple_ttest,
                      df = works.cutoff.2) %>%
              {lapply(c(so.statistics, mun.statistics),
                      function(x){x <- c(.[[x]][["p.value"]]
                                         # .[[x]][["estimate"]][1],
                                         # .[[x]][["estimate"]][2]
                                         )})}
# Works 3
w.balance.3 <- lapply(works.cutoff.3[,c(so.statistics, mun.statistics)],
                      multiple_ttest,
                      df = works.cutoff.3) %>%
              {lapply(c(so.statistics, mun.statistics),
                      function(x){x <- c(.[[x]][["p.value"]]
                                         # .[[x]][["estimate"]][1],
                                         # .[[x]][["estimate"]][2]
                                         )})}

# Compute number of observations for last row in table
model1.sample <- as.vector(purchases.cutoff.1 %$% table(so.procurement)) %>%
                 {paste0("(", paste0(., collapse = "; "), ")")}
model2.sample <- as.vector(purchases.cutoff.2 %$% table(so.procurement)) %>%
                 {paste0("(", paste0(., collapse = "; "), ")")}
model3.sample <- as.vector(works.cutoff.1 %$% table(so.procurement)) %>%
                 {paste0("(", paste0(., collapse = "; "), ")")}
model4.sample <- as.vector(works.cutoff.2 %$% table(so.procurement)) %>%
                 {paste0("(", paste0(., collapse = "; "), ")")}
model5.sample <- as.vector(works.cutoff.3 %$% table(so.procurement)) %>%
                 {paste0("(", paste0(., collapse = "; "), ")")}
sample.size   <- c("Sample Size:", model1.sample, model2.sample, model3.sample,
                   model4.sample, model5.sample)
# Create table
covs <- c(p.balance.1, p.balance.2, w.balance.1, w.balance.2, w.balance.3) %>%
        unlist() %>%
        cbind(sort(rep(paste("model", 1:5), 21))) %>%
        as.tibble() %>%
        unstack(`.` ~ V1) %>%
        mutate_all(funs(as.double)) %>%
        mutate_all(funs(sprintf("%0.3f", .))) %>%
        cbind(c(so.statistics.labels, mun.statistics.labels)) %>%
        select(6, c(1:5)) %>%
        rbind(sample.size)

# Produce table
covs %>%
  xtable(caption = "Means Tests Across Cutoffs",
         label   = "tab:covariates",
         align   = "llrrrrr",
         digits  = 0
  ) %>%
  print.xtable(type              = "latex",
               file              = "./article/tab_covariates.tex",
               table.placement   = "!htbp",
               caption.placement = "top",
               size              = "scriptsize",
               hline.after       = c(rep(-1, 2), 0, 8, 21, rep(22, 2)),
               include.rownames  = FALSE
  )

# Remove unnecessary objects
rm(list = objects(pattern = "sample|covs|\\.balance"))

#-------------------------------------------------------------------------------
# Table: Multiple, Non-Cumulative Cutoff
#-------------------------------------------------------------------------------
# Create empty table
rdmc.table <- as.tibble()

# Unlist and get the six statistics for each procurement type: all three
# bias-adjusted parameters and their standard errors
# Loop over outcomes
for (i in seq(1:6)) {

    # Loop over cutoffs
    for (x in seq(1:3)) {

      # Define object name
      if (i < 4) {obj <- paste0("non.cumulative.corruption.", i, ".cutoff.", x)}
      else       {obj <- paste0("non.cumulative.mismanagement.",i,".cutoff.",x)}

      # Unlist object
      obj <- unlist(get(obj))

      # Determine row count for filling in rdmc table
      if      (x == 1) {a <- x}
      else if (x == 2) {a <- x + 2}
      else             {a <- x + 4}

      # Fill in table
      rdmc.table[a,      i] <- obj$B2
      rdmc.table[a + 1,  i] <- sqrt(obj$V5)
      rdmc.table[a + 2,  i] <- obj$Nh2
      rdmc.table[a + 9,  i] <- obj$B3
      rdmc.table[a + 10, i] <- sqrt(obj$V9)
      rdmc.table[a + 11, i] <- obj$Nh3
      rdmc.table[a + 18, i] <- obj$B1
      rdmc.table[a + 19, i] <- sqrt(obj$V1)
      rdmc.table[a + 20, i] <- obj$Nh1
    }

    # Remove uncessary objects
    rm(obj, a, x)
}

# Edit rdmc.table
rdmc.table %>%
  rename_at(vars(c(1:3)), funs(paste0("Corruption ", as.roman(1:3)))) %>%
  rename_at(vars(c(4:6)), funs(paste0("Mismanagement ", as.roman(1:3)))) %>%
  mutate(Variable = rep(c("Proc. Category 1",   " ", " ",
                          "Proc. Category 2",  " ", " ",
                          "Proc. Category 3", " ", " "),
                        3)
  ) %>%
  select(Variable, everything()) %>%
  mutate_at(vars(c(2, 3, 5, 6)), funs(sprintf("%0.3f", .))) %>%
  mutate_at(vars(c(4, 7)), funs(sprintf("%0.0f", .))) %>%
  xtable(caption = "Multiple Non-Cumulative Cutoff Procurement Estimates",
         label   = "tab:rdmc",
         align   = "llcccccc"
  ) %>%
  print.xtable(type              = "latex",
               file              = "./article/tab_rdmc.tex",
               table.placement   = "!htbp",
               caption.placement = "top",
               size              = "scriptsize",
               hline.after       = c(rep(-1, 2), 9, 18, rep(27, 2)),
               include.rownames  = FALSE
  )

#-------------------------------------------------------------------------------
# RD Plot for significant cutoffs (Cutoff 1, Works, for Mismanagement I-III)
#-------------------------------------------------------------------------------
# Significant pooled results with municipal corruption as a covariate
# Subset sample first
works.mm.1 <- analysis.data %>%
  mutate(so.amount = case_when(so.type == 2 ~ so.amount - 15000)) %>%
  filter(bandwidthRange(so.amount, 0,
                        unlist(non.cumulative.mismanagement.4.cutoff.1)$H3))
works.mm.2 <- analysis.data %>%
  mutate(so.amount = case_when(so.type == 2 ~ so.amount - 15000)) %>%
  filter(bandwidthRange(so.amount, 0,
                        unlist(non.cumulative.mismanagement.5.cutoff.1)$H3))
works.mm.3 <- analysis.data %>%
  mutate(so.amount = case_when(so.type == 2 ~ so.amount - 15000)) %>%
  filter(bandwidthRange(so.amount, 0,
                        unlist(non.cumulative.mismanagement.6.cutoff.1)$H3))

# Generate graphics from file
# Outcome I
png("./article/workscutoff1.png", width = 560, height = 560)
works.mm.1 %$% rdplot(y = mismanagement.binary, x = so.amount, p = 2,
  title = "Mismanagement Outcome I", y.label = "", y.lim = c(0,1),
  cex.axis = 2.0, cex.lab = 2.0, cex.main = 2.0, mgp = c(3, 1, 0),
  family = "LM Roman 10", mar = c(5, 4.1, 4.1, 2.1), pin = c(5.83, 5.83),
  x.label = paste0("Bandwidth: R$ ", as.integer(bandwidth.table[4, 4]),
                   " (n = ", nrow(works.mm.1), ")")
)
dev.off()

# Outcome II
png("./article/workscutoff2.png", width = 560, height = 560)
works.mm.2 %$% rdplot(y = mismanagement.share,  x = so.amount, p = 2,
  title = "Mismanagement Outcome II", y.label = "", y.lim = c(0, 1),
  cex.axis = 2.0, cex.lab = 2.0, cex.main = 2.0, mgp = c(3, 1, 0),
  family = "LM Roman 10", mar = c(5, 4.1, 4.1, 2.1), pin = c(5.83, 5.83),
  x.label = paste0("Bandwidth: R$ ", as.integer(bandwidth.table[4, 5]),
                   " (n = ", nrow(works.mm.2), ")")
)
dev.off()

# Outcome III
png("./article/workscutoff3.png", width = 560, height = 560)
works.mm.3 %$% rdplot(y = mismanagement.amount, x = so.amount, p = 2,
  title = "Mismanagement Outcome III", y.label = "",
  cex.axis = 2.0, cex.lab = 2.0, cex.main = 2.0, mgp = c(3, 1, 0),
  family = "LM Roman 10", mar = c(5, 4.1, 4.1, 2.1), pin = c(5.83, 5.83),
  x.label = paste0("Bandwidth: R$ ", as.integer(bandwidth.table[4, 6])+1,
                   " (n = ", nrow(works.mm.3), ")")
)
dev.off()

# Compute results for various bandwidths
mm.1 <- tibble(point = rep(0, 9), ci_lower = NA, ci_upper = NA, n = NA)
mm.2 <- tibble(point = rep(0, 9), ci_lower = NA, ci_upper = NA, n = NA)
mm.3 <- tibble(point = rep(0, 9), ci_lower = NA, ci_upper = NA, n = NA)
bandwidth <- seq(from = 40000, to = 5000, by = -5000)

# Loop over bandwidths and spit out point estimates and confidence intervals
for (i in seq(1:8)) {

  # Use mismanagement outcomes from outcomes vector
  for (x in seq(1:3)) {

    # Run regressions
    obj <- analysis.data %$% rdmc(
      Y       = get(outcomes[x + 3]),
      X       = so.amount,
      C       = so.cutoff.1,
      pvec    = c(2, 2),
      hvec    = c(25000, bandwidth[i]),
      bvec    = c(25000, bandwidth[i]),
      pooled.opt = "level = 90, cluster = analysis.data$ibge.id, all = TRUE"
    )

    # Unlist and pull estimates
    obj <- unlist(obj)

    # Manually calculate 90% CIs from rdmc call (only does 95% CIs)
    mean  <- obj$B3
    error <- qnorm(.1) * sqrt(obj$V9)
    left  <- mean - error
    right <- mean + error

    # Fill in data table
    if      (x == 1) {mm.1[i + 1, ] <- c(obj$B3, left, right, obj$Nh3)}
    else if (x == 2) {mm.2[i + 1, ] <- c(obj$B3, left, right, obj$Nh3)}
    else             {mm.3[i + 1, ] <- c(obj$B3, left, right, obj$Nh3)}
  }

  # Remove unnecessary objects
  rm(obj, x, i, mean, error, left, right)
}

# Fill in point estimates and CI from rdmc table
mm.1[1, ] <- c(rdmc.table[10,4],
               rdmc.table[10,4] - qnorm(.1) * (rdmc.table[11,4]),
               rdmc.table[10,4] + qnorm(.1) * (rdmc.table[11,4]),
               nrow(works.mm.1))
mm.2[1, ] <- c(rdmc.table[10,5],
               rdmc.table[10,5] - qnorm(.1) * (rdmc.table[11,5]),
               rdmc.table[10,5] + qnorm(.1) * (rdmc.table[11,5]),
               nrow(works.mm.2))
mm.3[1, ] <- mm.3[2, ]
mm.3[2, ] <- c(rdmc.table[10,6],
               rdmc.table[10,6] - qnorm(.1) * (rdmc.table[11,6]),
               rdmc.table[10,6] + qnorm(.1) * (rdmc.table[11,6]),
               nrow(works.mm.3))

# Define vectors of x axis tick marks for CI plots
mm1.bandwidth <- c(unlist(non.cumulative.mismanagement.4.cutoff.1)$H3,
                   bandwidth
                 ) %>%
                 lapply(format, trim = TRUE, digits = 5, big.mark = ",") %>%
                 unlist() %>%
                 paste0(., " \n (n = ", unlist(mm.1[, 4]), ")")
mm2.bandwidth <- c(unlist(non.cumulative.mismanagement.5.cutoff.1)$H3,
                   bandwidth
                 ) %>%
                 lapply(format, trim = TRUE, digits = 5, big.mark = ",") %>%
                 unlist() %>%
                 paste0(., " \n (n = ", unlist(mm.2[, 4]), ")")
mm3.bandwidth <- c(bandwidth[1],
                   unlist(non.cumulative.mismanagement.6.cutoff.1)$H3,
                   bandwidth[2:8]
                 ) %>%
                 lapply(format, trim = TRUE, digits = 5, big.mark = ",") %>%
                 unlist() %>%
                 paste0(., " \n (n = ", unlist(mm.3[, 4]), ")")

# Loop over values and build each plot
for (i in seq(1:3)) {

  # Temporary object to get tibbles
  x <- paste0("mm.", i)

  # Temporary object to change font type in ggplot
  if (i != 3) {z <- c("bold", rep("plain", 8))}
  else        {z <- c("plain", "bold", rep("plain", 7))}

  # ggplot call to construct graphs
  ggplot(get(x), aes(y = point, x = c(1:9))) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymax = ci_upper, ymin = ci_lower)) +
    theme(text        = element_text(family = "LM Roman 10"),
          axis.text.x = element_text(face = z)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray33") +
    ylab("Point Estimates") + xlab("") +
    scale_x_continuous(
      breaks = c(1:9),
      labels = get(paste0("mm", i, ".bandwidth")))

  # ggsave to save them to file
  ggsave(paste0("mismanagementplot", i, ".png"),
         device = "png",
         path   = "./article",
         width  = 7,
         height = 3
  )
  dev.off()

  # Remove temporary objects
  rm(i, x, z)
}

# # Single purchases result on mismanagement
# purchases.cutoff.2 %$% rdplot(y = mismanagement.binary, x = so.amount, p = 2)

# Remove unnecessary objects
rm(list = objects(pattern = "(bandwidth(\\.table|\\.means))|mm\\.[1-3]|mun"))
rm(list = objects(pattern = "non\\.cumulative|rdmc|cutoff|mm[1-3]"))

# ##############################################################################
# # RD Multiple, Cumulative Cutoff Analysis
# ##############################################################################
# # Now we run the cumulative analysis separating out purchases and works. We
# # once again use Cataneo's rdmulti package but now the focus is on function
# # rdms. We loop over each database and each outcome and spit out the
# # coefficients of the cumulative regressions
# for (x in seq(1:6)) {
#   purchases <- purchases.data %$%
#     rdms(Y          = get(outcomes[x]),
#          X          = so.amount,
#          C          = c(8000, 80000, 650000),
#          pooled.opt = "level = 90, cluster = analysis.data$ibge.id, all = TRUE",
#          pvec       = c(2, 2, 2)
#     )
#   works <- works.data %$%
#     rdms(Y          = get(outcomes[x]),
#          X          = so.amount,
#          C          = c(15000, 150000, 1500000),
#          pooled.opt = "level = 90, cluster = analysis.data$ibge.id, all = TRUE",
#          pvec       = c(2, 2, 2)
#     )

#   # Assign new object names
#   if (x <= 3) {
#     assign(paste0("cumulative.corruption.", x, ".purchases"), purchases)
#     assign(paste0("cumulative.corruption.", x, ".works"), works)
#   } else {
#     assign(paste0("cumulative.mismanagement.", x, ".purchases"), purchases)
#     assign(paste0("cumulative.mismanagement.", x, ".works"), works)
#   }
#   rm(purchases, works)
# }

# #-----------------------------------------------------------------------------
# All cumulative results are the same, so I omit these regressions
# #-----------------------------------------------------------------------------

################################################################################
# Falsification Tests 1 (Purchases SOs as if they were Works)
################################################################################
# What we do over here is to use the works regressions on the purchases sample.
# The effect at works cutoff for purchases regressions should be zero.

# Compute results for fake cutoff
fake.1 <- tibble(point = rep(0, 9), ci_l = NA, ci_u = NA, n = NA, b = NA)
fake.2 <- tibble(point = rep(0, 9), ci_l = NA, ci_u = NA, n = NA, b = NA)
fake.3 <- tibble(point = rep(0, 9), ci_l = NA, ci_u = NA, n = NA, b = NA)
fake.bandwidth <- seq(from = 40000, to = 5000, by = -5000)

# Create loop for: pulling bandwidth, se, and CI plots. The first loop covers
# all rows which will be used for CI plot
for (x in seq(1:3)) {

  # Run one regression per outcome
  obj <- purchases.data %$%
    rdrobust(y       = get(outcomes[x + 3]),
             x       = so.amount,
             c       = 15000,
             p       = 1,
             q       = 2,
             level   = 90,
             cluster = purchases.data$ibge.id,
             all     = TRUE
    )

  # Unlist obj
  obj <- unlist(obj)

  # Manually calculate 90% CIs from rdmc call (only does 95% CIs)
  mean  <- obj$Estimate2
  error <- qnorm(.1) * obj$se3
  l     <- mean + error
  r     <- mean - error

  # Fill in data table
  if (x == 1) {
    fake.1[4, ] <- c(obj$Estimate2, l, r, obj$Nb1 + obj$Nb2, obj$bws2)
  } else if (x == 2) {
    fake.2[4, ] <- c(obj$Estimate2, l, r, obj$Nb1 + obj$Nb2, obj$bws2)
  } else {
    fake.3[4, ] <- c(obj$Estimate2, l, r, obj$Nb1 + obj$Nb2, obj$bws2)
  }

  # Remove unnecessary objects
  rm(obj, mean, error, l, r)
}

# Loop over other bandwidths and spit out point estimates and CIs
for (i in seq(1:8)) {

  # Use mismanagement outcomes from outcomes vector
  for (x in seq(1:3)) {

    # Run regressions
    obj <- purchases.data %$%
      rdrobust(y       = get(outcomes[x + 3]),
               x       = so.amount,
               c       = 15000,
               p       = 1,
               q       = 2,
               h       = fake.bandwidth[i],
               b       = fake.bandwidth[i],
               level   = 90,
               cluster = purchases.data$ibge.id,
               all     = TRUE
      )

    # Unlist and pull estimates
    obj <- unlist(obj)

    # Manually calculate 90% CIs from rdmc call (only does 95% CIs)
    mean  <- obj$Estimate2
    error <- qnorm(.05) * obj$se3
    l     <- mean + error
    r     <- mean - error

    # Fill in data table
    if (i <= 3) {z <- i}
    else        {z <- i + 1}

    if (x == 1) {
      fake.1[z, ] <- c(obj$Estimate2, l, r, obj$Nb1 + obj$Nb2, obj$bws2)
    } else if (x == 2) {
      fake.2[z, ] <- c(obj$Estimate2, l, r, obj$Nb1 + obj$Nb2, obj$bws2)
    } else {
      fake.3[z, ] <- c(obj$Estimate2, l, r, obj$Nb1 + obj$Nb2, obj$bws2)
    }
  }

  # Remove unnecessary objects
  rm(obj, x, mean, error, l, r)
}

# Loop over values and build each plot
for (i in seq(1:3)) {

  # Temporary object to get tibbles
  x <- paste0("fake.", i)

  # Temporary object to change font type in ggplot
  z <- c(rep("plain", 3), "bold", rep("plain", 4))
  fake.label <- c(unlist(get(x)[, 5])) %>%
              lapply(format, trim = TRUE, digits = 5, big.mark = ",") %>%
              unlist() %>%
              paste0(., " \n (n = ", unlist(get(x)[, 4]), ")")

  # ggplot call to construct graphs
  ggplot(get(x), aes(y = point, x = c(1:9))) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymax = ci_u, ymin = ci_l)) +
    theme(text        = element_text(family = "LM Roman 10"),
          axis.text.x = element_text(face = z)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray33") +
    ylab("Point Estimates") + xlab("") +
    scale_x_continuous(breaks = c(1:9), labels = fake.label)

  # ggsave to save them to file
  ggsave(paste0("01falsificationplot", i, ".png"),
         device = "png",
         path   = "./article",
         width  = 7,
         height = 3
  )
  dev.off()

  # Remove temporary objects
  rm(i, x, z, fake.label)
}

################################################################################
# Falsification Tests 2 (Non-Procurement SOs)
################################################################################
# The second falsification test is measuring the effect of works discretion on
# non-procurement SOs. If they truly are causal, then here again there should
# be no effect of discretion on mismanagement.

# Wrangle falsification data so that we are left with a non-procurement sample
# but for which we do have SO amount so we can calculate the effect on mismana-
# gement.
load("falsification.data.Rda")
falsification.data %<>%
  anti_join(so.data, by = c("so.id" =  "so.id")) %>%
  left_join(appendix.data, by = c("so.id" = "so.id")) %>%
  filter(so.works.bygranttext != 1 | so.works.bycode != 1) %>%
  filter(!is.na(so.amount.x) & so.type == 0) %>%
  rename(so.amount = so.amount.x) %>%
  mutate(
    so.fake.type   = case_when(so.amount <=  15000 ~ 0,
                               so.amount >   15000 & so.amount <=  150000 ~ 1,
                               so.amount >  150000 & so.amount <= 1500000 ~ 2,
                               so.amount > 1500000 ~ 3),
    mun.corruption = sum(corruption.count) / sum(infraction.count),
    mun.corruption = mun.corruption - (corruption.count/sum(infraction.count)),
    mun.corruption = ifelse(is.na(mun.corruption), 0, mun.corruption)
  )

# Compute results for fake cutoff
fake.1 <- tibble(point = rep(0, 9), ci_l = NA, ci_u = NA, n = NA, b = NA)
fake.2 <- tibble(point = rep(0, 9), ci_l = NA, ci_u = NA, n = NA, b = NA)
fake.3 <- tibble(point = rep(0, 9), ci_l = NA, ci_u = NA, n = NA, b = NA)
fake.bandwidth <- seq(from = 40000, to = 5000, by = -5000)

# Create loop for: pulling bandwidth, se, and CI plots. The first loop covers
# all rows which will be used for CI plot
for (x in seq(1:3)) {

  # Run one regression per outcome
  obj <- falsification.data %$%
    rdrobust(y       = get(outcomes[x + 3]),
             x       = so.amount,
             c       = 15000,
             p       = 1,
             q       = 2,
             level   = 90,
             cluster = falsification.data$ibge.id,
             all     = TRUE
    )

  # Unlist obj
  obj <- unlist(obj)

  # Manually calculate 90% CIs from rdmc call (only does 95% CIs)
  mean  <- obj$Estimate2
  error <- qnorm(.1) * obj$se3
  l     <- mean + error
  r     <- mean - error

  # Fill in data table
  if (x == 1) {
    fake.1[1, ] <- c(obj$Estimate2, l, r, obj$Nb1 + obj$Nb2, obj$bws2)
  } else if (x == 2) {
    fake.2[1, ] <- c(obj$Estimate2, l, r, obj$Nb1 + obj$Nb2, obj$bws2)
  } else {
    fake.3[1, ] <- c(obj$Estimate2, l, r, obj$Nb1 + obj$Nb2, obj$bws2)
  }

  # Remove unnecessary objects
  rm(obj, mean, error, l, r)
}

# Loop over other bandwidths and spit out point estimates and CIs
for (i in seq(1:8)) {

  # Use mismanagement outcomes from outcomes vector
  for (x in seq(1:3)) {

    # Run regressions
    obj <- falsification.data %$%
      rdrobust(y       = get(outcomes[x + 3]),
               x       = so.amount,
               c       = 15000,
               p       = 1,
               q       = 2,
               h       = fake.bandwidth[i],
               b       = fake.bandwidth[i],
               level   = 90,
               cluster = falsification.data$ibge.id,
               all     = TRUE
      )

    # Unlist and pull estimates
    obj <- unlist(obj)

    # Manually calculate 90% CIs from rdmc call (only does 95% CIs)
    mean  <- obj$Estimate2
    error <- qnorm(.05) * obj$se3
    l     <- mean + error
    r     <- mean - error

    # Fill in data table
    if (x == 1) {
      fake.1[i + 1, ] <- c(obj$Estimate2, l, r, obj$Nb1 + obj$Nb2, obj$bws2)
    } else if (x == 2) {
      fake.2[i + 1, ] <- c(obj$Estimate2, l, r, obj$Nb1 + obj$Nb2, obj$bws2)
    } else {
      fake.3[i + 1, ] <- c(obj$Estimate2, l, r, obj$Nb1 + obj$Nb2, obj$bws2)
    }
  }

  # Remove unnecessary objects
  rm(obj, x, mean, error, l, r)
}

# Loop over values and build each plot
for (i in seq(1:3)) {

  # Temporary object to get tibbles
  x <- paste0("fake.", i)

  # Temporary object to change font type in ggplot
  z <- c("bold", rep("plain", 8))
  fake.label <- c(unlist(get(x)[, 5])) %>%
              lapply(format, trim = TRUE, digits = 5, big.mark = ",") %>%
              unlist() %>%
              paste0(., " \n (n = ", unlist(get(x)[, 4]), ")")

  # ggplot call to construct graphs
  ggplot(get(x), aes(y = point, x = c(1:9))) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymax = ci_u, ymin = ci_l)) +
    theme(text        = element_text(family = "LM Roman 10"),
          axis.text.x = element_text(face = z)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray33") +
    ylab("Point Estimates") + xlab("") +
    scale_x_continuous(breaks = c(1:9), labels = fake.label)

  # ggsave to save them to file
  ggsave(paste0("02falsificationplot", i, ".png"),
         device = "png",
         path   = "./article",
         width  = 7,
         height = 3
  )
  dev.off()

  # Remove temporary objects
  rm(i, x, z, fake.label)
}

# Remove unnecessary objects
rm(fake.1, fake.2, fake.3)

################################################################################
# Discretion Effect Discussion
################################################################################
# 1. Why isn't there a corruption effect?
# We put together a table of average infractions per SO per year. We communicate
# the results using a facet grid further breaking effects down by so.type and
# procurement category
# First, we create the average number of infractions per so.type-procurement
# category pair
discussion.right <- analysis.data %>%
  group_by(so.type, so.procurement) %>%
  summarize(avg.infractions = mean(infraction.count))

# We then compute yearly average infractions by adding up all infractions for
# each type-category pair and dividing all up by the number of SOs in each year
discussion.left <- analysis.data %>%
  mutate(so.year = year(audit.end)) %>%
  select(-audit.end) %>%
  group_by(so.year, so.type, so.procurement) %>%
  summarize(total.infractions = mean(infraction.count)) %>%
  filter(!is.na(so.year))

# Join the two datasets into one serving for aesthetics in R
discussion.data <- left_join(discussion.left, discussion.right,
  by = c("so.type" = "so.type", "so.procurement" = "so.procurement"))

# ggplot call to construct graphs
discussion.data %>%
  ggplot(aes(y = total.infractions, x = so.year)) +
    geom_col(color = "grey33") +
    facet_grid(
      so.type ~ so.procurement,
      labeller = labeller(so.type = c(`1` = "Purchases", `2`= "Works"),
        so.procurement = c(`0`= "Direct Contracting", `1` = "Invitational",
          `2` = "Price Comparison", `3` = "Competitive")),
      switch = "both"
    ) +
    geom_hline(aes(yintercept = avg.infractions), linetype   = "dashed") +
    ylab("Average Infractions Count") + xlab("") +
    scale_x_continuous(breaks = c(2004:2010)) +
    theme(text = element_text(family = "LM Roman 10"),
          axis.text.x = element_text(angle = 90, hjust = 1))

# ggsave to save them to file
ggsave("01discussionplot.png",
       device = "png",
       path   = "./article",
       width  = 6,
       height = 4
)
dev.off()

# Remove unnecessary objects
rm(list = objects(pattern = "discussion"))

################################################################################
# Welfare Effects
################################################################################
# Here we conduct a back of the envelope calculation of the costs and benefits
# of Law 8,666/93. COSTS: not preventing corruption and mismanagement; BENEFITS:
# preventing management
# First we pull bandwidths for all outcomes in purchases and works at cutoff 1
bandwidth.table <- readRDS("bandwidth.table.Rds")

# Subsetting purchases sample to corruption and mismanagement bandwidths for
# outcome III (amount potentially lost to corruption)
welfare.corruption.p <- purchases.data %>%
  filter(bandwidthRange(so.amount, 8000, unlist(bandwidth.table[1, 3])))
welfare.mismanagement.p <- purchases.data %>%
  filter(bandwidthRange(so.amount, 8000, unlist(bandwidth.table[1, 6])))

# Subsetting works sample to corruption and mismanagement bandwidths for
# outcome III (amount potentially lost to corruption)
welfare.corruption.w <- works.data %>%
  filter(bandwidthRange(so.amount, 15000, unlist(bandwidth.table[4, 3])))
welfare.mismanagement.w <- works.data %>%
  filter(bandwidthRange(so.amount, 15000, unlist(bandwidth.table[4, 6])))

# Pull average amount potentially lost to corruption and mismanagement
avg.1 <- as.vector(summary(welfare.corruption.p$corruption.amount))
avg.2 <- as.vector(summary(welfare.mismanagement.p$mismanagement.amount))
avg.3 <- as.vector(summary(welfare.corruption.w$corruption.amount))
avg.4 <- as.vector(summary(welfare.mismanagement.w$mismanagement.amount))

# Create empty table
welfare.table <- tibble(Cost = rep(NA, 15), Type = rep(NA, 15),
  `Avg. Loss (in R$)` = rep(NA, 15), `# Obs.` = rep(NA, 15),
  `Total (in R$)` = rep(NA, 15))

# Fill in table with standard entries
welfare.table[1:4, 1]   <- rep(c("Corruption", "Mismanagement"), 2)
welfare.table[1:4, 2]   <- rep(c("Purchases", "Works"), each = 2)
welfare.table[7, 1]     <- "Benefits"
welfare.table[8, 1:2]   <- c("Works", "Mismanagement")
welfare.table[5, 1]     <- "Total Cost"
welfare.table[9, 1]     <- "Total Benefit"
welfare.table[11, 1]    <- "Welfare Effect"
welfare.table[12:15, 1] <- c(
  "Cost (in R$) in the absence of discretion benefit",
  "Cost Reduction (in %)",
  "Works Cost (in R$) in the absence of discretion benefit",
  "Works Cost Reduction (in %)")

# Fill in with relevant COST statistics
welfare.table[1, 3:5] <- c(avg.1[4], nrow(welfare.corruption.p),
  avg.1[4]*nrow(welfare.corruption.p))
welfare.table[2, 3:5] <- c(avg.2[4], nrow(welfare.mismanagement.p),
  avg.2[4]*nrow(welfare.mismanagement.p))
welfare.table[3, 3:5] <- c(avg.3[4], nrow(welfare.corruption.w),
  avg.3[4]*nrow(welfare.corruption.w))
welfare.table[4, 3:5] <- c(avg.4[4], nrow(welfare.mismanagement.w),
  avg.4[4]*nrow(welfare.mismanagement.w))
welfare.table[5, 5]   <- sum(unlist(lapply(welfare.table[1:4, 5], as.numeric)))

# Fill in with relevant BENEFIT statistics (Mismanagement III, Cutoff)
welfare.table[8, 3:5] <- c(-4611, nrow(welfare.mismanagement.w),
  -4611*nrow(welfare.mismanagement.w))

# Fill in with relevant welfare effect statistics
welfare.table[12, 5] <- sum(unlist(lapply(welfare.table[1:4, 5], as.numeric)),
                            -as.numeric(welfare.table[8, 5]))
welfare.table[13, 5] <- abs(welfare.table[8, 5]) / welfare.table[12, 5]
welfare.table[14, 5] <- sum(unlist(lapply(welfare.table[3:4, 5], as.numeric)),
                            -as.numeric(welfare.table[8, 5]))
welfare.table[15, 5] <- abs(welfare.table[8, 5]) / welfare.table[14, 5]

# Format numerical entries in table
welfare.table[,3]           <- as.integer(unlist(welfare.table[, 3]))
welfare.table[c(1:12,14),5] <- as.integer(unlist(welfare.table[c(1:12, 14), 5]))
welfare.table[c(13, 15), 5] <- 100*welfare.table[c(13, 15), 5]

# Produce out table
welfare.table %>%
  xtable(caption = "Welfare Effects from Discretion at Cutoff 1",
         label   = "tab:welfare",
         align   = "llrrrr"
  ) %>%
  print.xtable(type              = "latex",
               file              = "./article/tab_welfare.tex",
               table.placement   = "!htbp",
               caption.placement = "top",
               size              = "scriptsize",
               hline.after       = c(rep(-1, 2), 2, 6, 9, 10, 13, rep(15, 2)),
               include.rownames  = FALSE
  )
