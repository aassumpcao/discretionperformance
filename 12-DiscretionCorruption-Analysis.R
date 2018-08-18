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
# Second go at School Aid Project Analysis produce the analysis you should run
# this script after all other three have been run.
#
################################################################################

################################################################################
# Packages
################################################################################
# Minimum requirements to run script
library(here)        # Version 0.1
library(tidyverse)   # Version 1.2.1
# library(haven)       # Version 1.1.2
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
      else        {table <- cbind(table.2, indicator)}

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
                          "Procurement Type 1", "Procurement Type 2",
                          "Procurement Type 3")

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
bandwidth.table <- bandwidthTable()

# # Save R object which is used again in appendix
# writeRDS(bandwidth.table, file = "./bandwidth.table.Rds")

# Formatting work
bandwidth.table %>%
  rename_at(
    vars(matches("c")),
    funs(paste0("CIndicator ", c("I", "II", "III")))
  ) %>%
  rename_at(
    vars(matches("mm")),
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
  filter(bandwidthRange(so.amount, 0, bandwidth.means$V2))

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
rm(list = objects(pattern = "sample|covs|bandwidth|\\.balance"))

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
      else if (x == 2) {a <- x + 1}
      else             {a <- x + 2}

      # Fill in table
      rdmc.table[a,      i] <- obj$B2
      rdmc.table[a + 1,  i] <- obj$V5^(1/2)
      rdmc.table[a + 6,  i] <- obj$B3
      rdmc.table[a + 7,  i] <- obj$V9^(1/2)
      rdmc.table[a + 12, i] <- obj$B1
      rdmc.table[a + 13, i] <- obj$V1^(1/2)
    }

    # Remove uncessary objects
    rm(obj, a, x)
}







# Significant pooled results with municipal corruption as a covariate
analysis.data %$%
  rdmc(Y          = corruption.share,
       X          = so.amount,
       C          = so.cutoff.1,
       pooled.opt = paste("covs = analysis.data$mun.corruption",
                          "level = 90",
                          "cluster = analysis.data$ibge.id",
                          "all = TRUE", sep = ", "),
       pvec       = c(2, 2)
  )
analysis.data %$%
  rdmc(Y          = mismanagement.binary,
       X          = so.amount,
       C          = so.cutoff.1,
       pooled.opt = paste("covs = analysis.data$mun.corruption",
                          "level = 90",
                          "cluster = analysis.data$ibge.id",
                          "all = TRUE", sep = ", "),
       pvec       = c(2, 2)
  )
analysis.data %$%
  rdmc(Y          = mismanagement.share,
       X          = so.amount,
       C          = so.cutoff.1,
       pooled.opt = paste("covs = analysis.data$mun.corruption",
                          "level = 90",
                          "cluster = analysis.data$ibge.id",
                          "all = TRUE", sep = ", "),
       pvec       = c(2, 2)
  )

analysis.data %$%
  rdmc(Y          = mismanagement.amount,
       X          = so.amount,
       C          = so.cutoff.1,
       pooled.opt = paste("covs = analysis.data$mun.corruption",
                          "level = 90",
                          "cluster = analysis.data$ibge.id",
                          "all = TRUE", sep = ", "),
       pvec       = c(2, 2)
  )

# Remove unnecessary objects
rm(list = objects(pattern = "bandwidth\\.table")

################################################################################
# RD Multiple, Cumulative Cutoff Analysis
################################################################################
# Now we run the cumulative analysis separating out purchases and works. We once
# again use Cataneo's rdmulti package but now the focus is on function rdms. We
# loop over each database and each outcome and spit out the coefficients of the
# cumulative regressions
for (x in seq(from = 1, to = 6)) {
  purchases <- purchases.data %$%
    rdms(Y          = get(outcomes[x]),
         X          = so.amount,
         C          = c(8000, 80000, 650000),
         pooled.opt = "level = 90, cluster = analysis.data$ibge.id, all = TRUE",
         pvec       = c(2, 2, 2)
    )
  works <- works.data %$%
    rdms(Y          = get(outcomes[x]),
         X          = so.amount,
         C          = c(15000, 150000, 1500000),
         pooled.opt = "level = 90, cluster = analysis.data$ibge.id, all = TRUE",
         pvec       = c(2, 2, 2)
    )

  # Assign new object names
  if (x <= 3) {
    assign(paste0("cumulative.corruption.", x, ".purchases"), purchases)
    assign(paste0("cumulative.corruption.", x, ".works"), works)
  } else {
    assign(paste0("cumulative.mismanagement.", x, ".purchases"), purchases)
    assign(paste0("cumulative.mismanagement.", x, ".works"), works)
  }
  rm(purchases, works)
}
