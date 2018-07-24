#------------------------------------------------------------------------------#
# Estimating the Effect of Discretion on Corruption:
# Evidence from Brazilian Municipalities
#
# Analysis Script
# Prepared by:
# Andre Assumpcao
# aassumpcao@unc.edu
#------------------------------------------------------------------------------#
rm(list = ls())

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
library(haven)       # Version 1.1.2
library(lubridate)   # Version 1.7.4
library(readxl)      # Version 1.1.0
library(psych)       # Version 1.8.4
library(magrittr)    # Version 1.5
library(rdd)         # Version 0.57
library(tikzDevice)  # Version 0.12
library(stargazer)   # Version 5.2.2
library(xtable)      # Version 1.8.2
library(commarobust) # Version 0.1.0
library(rlist)       # Version 0.4.6.1
library(estimatr)    # Version 0.10.0
library(rdrobust)    # Version 0.99.3
library(RDDtools)    # Version 0.22

#------------------------------------------------------------------------------#
################################## Functions ###################################
#------------------------------------------------------------------------------#
# Function to help subset data to be used in RDD estimation
bandwidthRange <- function(x, cutpoint, limit){
  # Args:
  #   x:        column serving as assignment variable
  #   cutpoint: cutoff assigning observations to different treatments
  #   limit:    symmetric range on either side of cutoff value

  # Returns:
  #   Logical vector used to subset dataset
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
  temp      <- starprep(model, cluster = data$cluster, alpha = alpha)
  temp[[2]] <- NULL

  return(temp)
}

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
# IN LATEX ONLY

#--------------------------#
# Table: Procurement Types #
#--------------------------#
# IN LATEX ONLY

#---------------------------#
# Table: Audits by Ministry #
#---------------------------#
# Run following command then manually edit in latex.
so.data %>%
  group_by(ibge.id) %>%
  summarize(
    so.education = max(so.education),
    so.health    = max(so.health)
  ) %$%
  table(so.education, so.health) %>%
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

# Now I have to join all three datasets (service order data, municipal, poli-
# tical, and SES characteristics). Before that, however, I need to remove dupli-
# cated election data (due to runoff elections for municipalities
# whose number of voters is larger than 200,000).
mun.election %<>%
  group_by(., mun.election) %>%
  group_by(ibge.id, add = TRUE) %>%
  slice(which.max(mun.election)) %>%
  ungroup() %>%
  mutate(mun.election = as.Date(mun.election))

# We now join them all at once.
analysis.data <- left_join(so.data, mun.data, by = c("ibge.id" = "ibge.id")) %>%
  mutate(
    mun.election = as.Date(ifelse(audit.start <= ymd("2004-10-03"),
                                  ymd("2000-10-01"),
                                  ifelse(audit.start <= ymd("2008-10-05"),
                                         ymd("2004-10-03"),
                                         ymd("2008-10-05")
                                  )
                           )
                   )
  ) %>%
  left_join(., mun.election,
    by = c("ibge.id" = "ibge.id", "mun.election" = "mun.election")
  ) %>%
  mutate(
    mun.votemargin = ifelse(is.na(mun.votemargin),
                            mean(.$mun.votemargin, na.rm = TRUE),
                            mun.votemargin
                     ),
    mun.reelected  = ifelse(is.na(mun.reelected),
                            mean(.$mun.reelected,  na.rm = TRUE),
                            mun.reelected
                     )
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
  font.size        = "small",
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
    mun.corruption = mun.corruption - (corruption.count/sum(infraction.count)),
    mun.corruption = ifelse(is.na(mun.corruption), 0, mun.corruption)
  ) %>%
  select(c(1:66), mun.corruption, c(67:80)) %>%
  ungroup()

# Check the variables we should use
names(analysis.data)

# Define vector of outcomes
outcomes <- setdiff(so.statistics, c("so.amount", "infraction.count"))

# Define outcome labels
outcome.labels <- setdiff(so.statistics.labels,
                          c("Amount (in R)", "Infraction Count")
                  )

# Define vector of procurement-specific regressors
so.covariates <- paste("so.amount", "I(so.amount^2)", "mun.corruption",
  "I(mun.corruption^2)", "factor(so.procurement)", sep = " + ")

# Define Covariates Labels
so.covariates.labels <- c("Amount (in R)", "Amount (in R, squared)",
  "Municipal Corruption", "Municipal Corruption (Squared)",
  "Procurement Type 1", "Procurement Type 2", "Procurement Type 3")

# Define vector of municipality characteristics
mun.covariates <- analysis.data %>%
  select(c(67:78, 80, 81), so.education, so.health, lottery.id) %>%
  names()

# Define Covariates labels (not necessary)
# mun.covariates.labels <- c()

# Pull factor positions
factors <- grep(mun.covariates,
                pattern = "radio|council|lottery|judiciary|reelected|so\\."
           )

# Concatenate municipal covariates vector
for (i in factors) {
  mun.covariates[[i]] <- paste0("factor(", mun.covariates[[i]], ")")
}

# Collapse to unitary vector
mun.covariates <- paste(mun.covariates, collapse = " + ")

# Run service order regressions w/o covariates
for (i in seq(from = 1, to = 6)) {

  # Run each regression w/ covariates
  lm <- lm(
    as.formula(
      paste(outcomes[[i]], so.covariates, sep = " ~ ")),
    data = analysis.data
  )

  # Run each regression w/covariates
  lm.x <- lm(
    as.formula(
      paste(
        outcomes[[i]], paste(so.covariates, mun.covariates, sep = " + "),
        sep = " ~ "
      )
    ),
    data = analysis.data
  )
  # Store corruption and mismanagement regressions
  if (i <= 3) {
    assign(paste0("lm.corruption.", i), lm)
    assign(paste0("lm.corruption.x.", i), lm.x)
  } else {
    assign(paste0("lm.mismanagement.", i-3), lm)
    assign(paste0("lm.mismanagement.x.", i-3), lm.x)
  }
 # Remove temporary object
  rm(lm, lm.x)
}

#---------------------------------#
# Table: First Linear Regressions #
#---------------------------------#
stargazer(

  # Regressions that will be printed to table
  list(lm.corruption.1, lm.corruption.x.1, lm.corruption.2,
  lm.corruption.x.2, lm.corruption.3,
  lm.corruption.x.3),

  # Table commands
  title                 = "Corruption Determinants in Brazilian Municipalities",
  out                   = "./article/tab_mainregression.tex",
  out.header            = FALSE,
  column.labels         = rep(
                            c(outcome.labels[[1]],
                              outcome.labels[[2]],
                              outcome.labels[[3]]
                            ),
                            2
                          ),
  column.separate       = rep(1, 6),
  covariate.labels      = so.covariates.labels,
  dep.var.labels.include= FALSE,
  align                 = TRUE,

  # Ask for robust standard errors
  se                    = starprep(
                            lm.corruption.1, lm.corruption.x.1,
                            lm.corruption.2, lm.corruption.x.2,
                            lm.corruption.3, lm.corruption.x.3,
                            clusters = analysis.data$ibge.id,
                            alpha    = .1
                          ),
  # p                     = starprep(
  #                           list(
  #                             lm.corruption.1, lm.corruption.x.1,
  #                             lm.corruption.2, lm.corruption.x.2,
  #                             lm.corruption.3, lm.corruption.x.3
  #                           ),
  #                           stat     = "p.value",
  #                           clusters = analysis.data$ibge.id,
  #                           alpha    = .1
  #                         ),
  column.sep.width      = "2pt",
  digits                = 3,
  digits.extra          = 0,
  # initial.zero          = FALSE,
  font.size             = "small",
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
rm(list = objects(pattern = "lm\\.|summary\\.stats"))

#------------------------------------------------------------------------------#
############################ Bandwidth choice test #############################
#------------------------------------------------------------------------------#
# We use Cattaneo (2016)'s rules for sub-setting the sample. In cases where
# assignment to different treatments is cumulative on the running variable, they
# suggest using observations with running values up to c-1 and c+1 cutoff values
# for each c cutoff.
purchases.bandwidth.1 <- analysis.data %>%
  filter(so.amount <=  80000 & so.type == 1)
purchases.bandwidth.2 <- analysis.data %>%
  filter(so.amount >    8000 & so.amount <= 650000 & so.type == 1)
purchases.bandwidth.3 <- analysis.data %>%
  filter(so.amount >   80000 & so.type == 1)
works.bandwidth.1     <- analysis.data %>%
  filter(so.amount <= 150000 & so.type == 2)
works.bandwidth.2     <- analysis.data %>%
  filter(so.amount >   15000 & so.amount <= 1500000 & so.type == 2)
works.bandwidth.3     <- analysis.data %>%
  filter(so.amount > 150000  & so.type == 2)

# Define vector of datasets used for bandwidth tests
purchases.bandwidth.list <- c(ls(pattern = "purchases\\.bandwidth\\.[0-9]"))
works.bandwidth.list     <- c(ls(pattern = "works\\.bandwidth\\.[0-9]"))

# Define vector of cutoffs for bandwidth tests
purchases.cutoff.list <- c(8000, 80000, 650000)
works.cutoff.list     <- c(15000, 150000, 1500000)

#--------------------------------------#
# Test 1: CCT = Calonico et al. (2015) #
#--------------------------------------#
# Create list of vectors which will take up the bandwidth values
purchases.cct.vector <- rep(0, 18)
works.cct.vector     <- rep(0, 18)

# Loop over cutoffs and outcomes for CCT bandwidth calculation (PURCHASES)
for (i in seq(from = 1, to = 3)) {

  # Loop over outcomes
  for (x in seq(from = 1, to = 6)) {

    # Assign temporary object to hold bandwidth calculation output
    cct <- get(purchases.bandwidth.list[i]) %$%
      rdbwselect(get(outcomes[[x]]),
                 so.amount,
                 c = as.double(purchases.cutoff.list[i]),
                 cluster = ibge.id
      )

    # # Assign new object name (uncomment if you'd like to see individual band-
    # # width results)
    # assign(paste0("rdrobust.purchases.1.outcome.", x), cct)

    # Extract bandwidth from bandwidth function and assign to bandwidth vector
    if      (i == 1) purchases.cct.vector[x]    <- cct$bws[[1]]
    else if (i == 2) purchases.cct.vector[x+6]  <- cct$bws[[1]]
    else             purchases.cct.vector[x+12] <- cct$bws[[1]]
  }
}

# Loop over cutoffs and outcomes for CCT bandwidth calculation (WORKS)
for (i in seq(from = 1, to = 3)) {

  # Loop over outcomes
  for (x in seq(from = 1, to = 6)) {

    # Assign temporary object to hold bandwidth calculation output
    cct <- get(works.bandwidth.list[i]) %$%
      rdbwselect(get(outcomes[[x]]),
                 so.amount,
                 c = as.double(works.cutoff.list[i]),
                 cluster = ibge.id
      )

    # # Assign new object name (uncomment if you'd like to see individual band-
    # # width results)
    # assign(paste0("rdrobust.works.1.outcome.", x), cct)

    # Extract bandwidth from bandwidth function and assign to bandwidth vector
    if      (i == 1) works.cct.vector[x]    <- cct$bws[[1]]
    else if (i == 2) works.cct.vector[x+6]  <- cct$bws[[1]]
    else             works.cct.vector[x+12] <- cct$bws[[1]]
  }
}

# Remove unnecessary objects
rm(list = objects(pattern = "^(cct)$"))

#----------------------------------------------#
# Test 2: IK  = Imbens and Kalyanaraman (2012) #
#----------------------------------------------#
# Create list of vectors which will take up the bandwidth values
purchases.ik.vector <- rep(0, 18)
works.ik.vector     <- rep(0, 18)

# Loop over cutoffs and outcomes for IK bandwidth calculation (PURCHASES)
for (i in seq(from = 1, to = 3)) {

  # Loop over outcomes
  for (x in seq(from = 1, to = 6)) {

    # Assign temporary object to hold bandwidth calculation output
    ik <- RDDdata(y        = get(outcomes[[x]]),
                  x        = so.amount,
                  cutpoint = as.double(purchases.cutoff.list[i]),
                  data     = get(purchases.bandwidth.list[i])
          )

    # Assign new object name
    assign(paste0("ik.", x), ik)

    # Extract bandwidth from bandwidth function and assign to bandwidth vector
    if        (i == 1) {
      purchases.ik.vector[x]    <- RDDbw_IK(get(paste0("ik.", x)))
    } else if (i == 2) {
      purchases.ik.vector[x+6]  <- RDDbw_IK(get(paste0("ik.", x)))
    } else {
      purchases.ik.vector[x+12] <- RDDbw_IK(get(paste0("ik.", x)))
    }
  }
  # Remove unnecessary objects
  rm(list = objects(pattern = "ik\\.[0-9]"))
}

# Loop over cutoffs and outcomes for IK bandwidth calculation (WORKS)
for (i in seq(from = 1, to = 3)) {

  # Loop over outcomes
  for (x in seq(from = 1, to = 6)) {

    # Assign temporary object to hold bandwidth calculation output
    ik <- RDDdata(y        = get(outcomes[[x]]),
                  x        = so.amount,
                  cutpoint = as.double(works.cutoff.list[i]),
                  data     = get(works.bandwidth.list[i])
          )

    # Assign new object name
    assign(paste0("ik.", x), ik)

    # Extract bandwidth from bandwidth function and assign to bandwidth vector
    if        (i == 1) {
      works.ik.vector[x]    <- RDDbw_IK(get(paste0("ik.", x)))
    } else if (i == 2) {
      works.ik.vector[x+6]  <- RDDbw_IK(get(paste0("ik.", x)))
    } else {
      works.ik.vector[x+12] <- RDDbw_IK(get(paste0("ik.", x)))
    }
  }
  # Remove unnecessary objects
  rm(list = objects(pattern = "ik\\.[0-9]"))
}

# IK bandwidth is out of range
mean(purchases.ik.vector[1:6])
mean(purchases.ik.vector[7:12])
mean(purchases.ik.vector[13:18])
mean(works.ik.vector[1:6])
mean(works.ik.vector[7:12])
mean(works.ik.vector[13:18])

# CCT bandwidth chosen
p.bandwidth.1 <- mean(purchases.cct.vector[1:6])
p.bandwidth.2 <- mean(purchases.cct.vector[7:12])
p.bandwidth.3 <- mean(purchases.cct.vector[13:18])
w.bandwidth.1 <- mean(works.cct.vector[1:6])
w.bandwidth.2 <- mean(works.cct.vector[7:12])
w.bandwidth.3 <- mean(works.cct.vector[13:18])

# Subset dataset along CCT bandwidth limits
purchases.bandwidth.1.cct <- analysis.data %>%
  filter(bandwidthRange(so.amount,    8000, p.bandwidth.1) & so.type == 1)
purchases.bandwidth.2.cct <- analysis.data %>%
  filter(bandwidthRange(so.amount,   80000, p.bandwidth.2) & so.type == 1)
purchases.bandwidth.3.cct <- analysis.data %>%
  filter(bandwidthRange(so.amount,  650000, p.bandwidth.3) & so.type == 1)
works.bandwidth.1.cct     <- analysis.data %>%
  filter(bandwidthRange(so.amount,   15000, w.bandwidth.1) & so.type == 2)
works.bandwidth.2.cct     <- analysis.data %>%
  filter(bandwidthRange(so.amount,  150000, w.bandwidth.2) & so.type == 2)
works.bandwidth.3.cct     <- analysis.data %>%
  filter(bandwidthRange(so.amount, 1500000, w.bandwidth.3) & so.type == 2)

#-------------------------#
# Table: Bandwidth Choice #
#-------------------------#
# Format bandwidth limits as data frame and produce table. Last step is editing
# the table in the TeX file.
tibble(purchases.bandwidth = c(p.bandwidth.1, p.bandwidth.2, p.bandwidth.3),
       works.bandwidth     = c(w.bandwidth.1, w.bandwidth.2, w.bandwidth.3)
) %>%
xtable()

#------------------------------------#
# Table: Multiple Cutoff Regressions #
#------------------------------------#
# Merge dataset vectors
multiple.cutoff.data <- c(purchases.bandwidth.list, works.bandwidth.list)
cct.cutoff.data      <- c(ls(pattern = "\\.bandwidth\\.[1-3]\\.cct"))

# Run service order regressions w/o covariates.
for (i in seq(from = 1, to = 6)) {

  # Loop over outcomes
  for (x in seq(from = 1, to = 6)) {

    # Run each regression w/o covariates
    lm <- lm(
      as.formula(
        paste(outcomes[[x]], so.covariates, sep = " ~ ")),
      data = get(multiple.cutoff.data[i])
    )

    # Run each regression w/ covariates
    lm.x <- lm(
      as.formula(
        paste(
          outcomes[[x]], paste(so.covariates, mun.covariates, sep = " + "),
          sep = " ~ "
        )
      ),
      data = get(multiple.cutoff.data[i])
    )

    # If database number (i) is less than or equal to 3, this is a purchases
    # regression. If the outcome number (x) is less than or equal to 3, this is
    # a corruption regression.
    if (i <= 3) {
      if (x <= 3) {
        assign(paste0("lm.corruption.", x, ".purchases.", i), lm)
        assign(paste0("lm.corruption.x.", x, ".purchases.", i), lm.x)
      } else {
        assign(paste0("lm.mismanagement.", x,".purchases.", i), lm)
        assign(paste0("lm.mismanagement.x.", x,".purchases.", i), lm.x)
      }
    } else {
      if (x <= 3) {
        assign(paste0("lm.corruption.", x, ".works.", i-3), lm)
        assign(paste0("lm.corruption.x.", x, ".works.", i-3), lm.x)
      } else {
        assign(paste0("lm.mismanagement.", x, ".works.", i-3), lm)
        assign(paste0("lm.mismanagement.x.", x, ".works.", i-3), lm.x)
      }
    }
    rm(lm, lm.x)
  }
}

# Make it easier to visualize corruption coefficients
# Filter models
effect.1 <- lapply(
  as.list(objects(pattern = "lm\\.corruption\\.[1-3]\\.(.)+\\.(1)$")), get)
effect.2 <- lapply(
  as.list(objects(pattern = "lm\\.corruption\\.[1-3]\\.(.)+\\.(2)$")), get)
effect.3 <- lapply(
  as.list(objects(pattern = "lm\\.corruption\\.[1-3]\\.(.)+\\.(3)$")), get)

# Create column with model names
names(effect.1) <- objects(pattern= "lm\\.corruption\\.[1-3]\\.(.)+\\.(1)$")
names(effect.2) <- objects(pattern= "lm\\.corruption\\.[1-3]\\.(.)+\\.(2)$")
names(effect.3) <- objects(pattern= "lm\\.corruption\\.[1-3]\\.(.)+\\.(3)$")

# Put them into table format
effect.1 %<>%
  plyr::ldply(tidy, .id = "model") %>%
  filter(term == "factor(so.procurement)1") %>%
  mutate(p.value = format(p.value, digits = 3))
effect.2 %<>%
  plyr::ldply(tidy, .id = "model") %>%
  filter(term == "factor(so.procurement)2") %>%
  mutate(p.value = format(p.value, digits = 3))
effect.3 %<>%
  plyr::ldply(tidy, .id = "model") %>%
  filter(term == "factor(so.procurement)3") %>%
  mutate(p.value = format(p.value, digits = 3))

# Bind into one dataset
lm.corruption.models <- rbind(effect.1, effect.2, effect.3)

# Make it easier to visualize corruption coefficients
# Filter models
effect.1 <- lapply(
  as.list(objects(pattern = "lm\\.mismanagement\\.[4-6]\\.(.)+\\.(1)$")), get)
effect.2 <- lapply(
  as.list(objects(pattern = "lm\\.mismanagement\\.[4-6]\\.(.)+\\.(2)$")), get)
effect.3 <- lapply(
  as.list(objects(pattern = "lm\\.mismanagement\\.[4-6]\\.(.)+\\.(3)$")), get)

# Create column with model names
names(effect.1) <- objects(pattern= "lm\\.mismanagement\\.[4-6]\\.(.)+\\.(1)$")
names(effect.2) <- objects(pattern= "lm\\.mismanagement\\.[4-6]\\.(.)+\\.(2)$")
names(effect.3) <- objects(pattern= "lm\\.mismanagement\\.[4-6]\\.(.)+\\.(3)$")

# Put them into table format
effect.1 %<>%
  plyr::ldply(tidy, .id = "model") %>%
  filter(term == "factor(so.procurement)1") %>%
  mutate(p.value = format(p.value, digits = 3))
effect.2 %<>%
  plyr::ldply(tidy, .id = "model") %>%
  filter(term == "factor(so.procurement)2") %>%
  mutate(p.value = format(p.value, digits = 3))
effect.3 %<>%
  plyr::ldply(tidy, .id = "model") %>%
  filter(term == "factor(so.procurement)3") %>%
  mutate(p.value = format(p.value, digits = 3))

# Bind into one dataset
lm.mismanagement.models <- rbind(effect.1, effect.2, effect.3)

# # Empty standard error matrix
# se.matrix  <- NULL

# # Loop for adding s.e. from each model to s.e. matrix
# for (i in seq(from = 1, to = 9)) {
#   se.matrix[i] <- starprepMod(reg.models[[i]], reg.data[i], ibge.id, .1)
# }


# # Produce purchases table without covariates
# stargazer(

#   # Regressions that will be printed to table
#   list(
#     lm.corruption.1.purchases.1,
#     lm.corruption.2.purchases.1,
#     lm.corruption.3.purchases.1,
#     lm.corruption.1.purchases.2,
#     lm.corruption.2.purchases.2,
#     lm.corruption.3.purchases.2,
#     lm.corruption.1.purchases.3,
#     lm.corruption.2.purchases.3,
#     lm.corruption.3.purchases.3
#   ),

#   # Table commands
#   title                 = "Effect of Procurement Type on Corruption Outcomes",
#   # type                  = "text",
#   out                   = "./article/tab_multiplecutoff_purchases.tex",
#   out.header            = FALSE,
#   column.labels         = rep(
#                             c(outcome.labels[[1]],
#                               outcome.labels[[2]],
#                               outcome.labels[[3]]
#                             ),
#                             3
#                           ),
#   column.separate       = rep(1, 9),
#   covariate.labels      = so.covariates.labels,
#   dep.var.labels.include= FALSE,
#   align                 = TRUE,

#   # Ask for robust standard errors
#   se                    = se.matrix,
#   # p                     = starprep(
#   #                           list(
#   #                             lm.corruption.1, lm.corruption.x.1,
#   #                             lm.corruption.2, lm.corruption.x.2,
#   #                             lm.corruption.3, lm.corruption.x.3
#   #                           ),
#   #                           stat     = "p.value",
#   #                           clusters = analysis.data$ibge.id,
#   #                           alpha    = .1
#   #                         ),
#   column.sep.width      = "2pt",
#   digits                = 3,
#   digits.extra          = 0,
#   # initial.zero          = FALSE,
#   font.size             = "small",
#   header                = FALSE,
#   keep                  = c("amount|corrupt|procurement"),
#   label                 = "tab:multiplecutoffpurchases",
#   no.space              = TRUE,
#   table.placement       = "!htbp",
#   omit                  = c("control", "ministry", "lottery"),
#   omit.labels           = c("Municipal Controls", "Ministry Fixed-Effects",
#                             "Lottery Fixed-Effects"),
#   omit.stat             = c("rsq", "ser"),
#   star.cutoffs          = c(.1, .05, .01)
# )

# # Produce works table without covariates
# stargazer(

#   # Regressions that will be printed to table
#   list(
#     ll.mismanagement.4.works.1,
#     ll.mismanagement.5.works.1,
#     ll.mismanagement.6.works.1,
#     ll.mismanagement.4.works.2,
#     ll.mismanagement.5.works.2,
#     ll.mismanagement.6.works.2,
#     ll.mismanagement.4.works.3,
#     ll.mismanagement.5.works.3,
#     ll.mismanagement.6.works.3
#   ),

#   # Table commands
#   title                 = "Effect of Procurement Type on Corruption Outcomes",
#   out                   = "./article/tab_multiplecutoff_works.tex",
#   out.header            = FALSE,
#   column.labels         = rep(
#                             c(outcome.labels[[1]],
#                               outcome.labels[[2]],
#                               outcome.labels[[3]]
#                             ),
#                             3
#                           ),
#   column.separate       = rep(1, 9),
#   covariate.labels      = so.covariates.labels,
#   dep.var.labels.include= FALSE,
#   align                 = TRUE,

#   # # Ask for robust standard errors
#   # se                    = starprep(
#   #                           lm.corruption.1.purchases.1,
#   #                           lm.corruption.2.purchases.1,
#   #                           lm.corruption.3.purchases.1,
#   #                           lm.corruption.1.purchases.2,
#   #                           lm.corruption.2.purchases.2,
#   #                           lm.corruption.3.purchases.2,
#   #                           lm.corruption.1.purchases.3,
#   #                           lm.corruption.2.purchases.3,
#   #                           lm.corruption.3.purchases.3,
#   #                           clusters = analysis.data$ibge.id,
#   #                           alpha    = .1
#   #                         ),
#   # p                     = starprep(
#   #                           list(
#   #                             lm.corruption.1, lm.corruption.x.1,
#   #                             lm.corruption.2, lm.corruption.x.2,
#   #                             lm.corruption.3, lm.corruption.x.3
#   #                           ),
#   #                           stat     = "p.value",
#   #                           clusters = analysis.data$ibge.id,
#   #                           alpha    = .1
#   #                         ),
#   column.sep.width      = "2pt",
#   digits                = 3,
#   digits.extra          = 0,
#   # initial.zero          = FALSE,
#   font.size             = "small",
#   header                = FALSE,
#   keep                  = c("amount|corrupt|procurement"),
#   label                 = "tab:multiplecutoffworks",
#   no.space              = TRUE,
#   table.placement       = "!htbp",
#   omit                  = c("control", "ministry", "lottery"),
#   omit.labels           = c("Municipal Controls", "Ministry Fixed-Effects",
#                             "Lottery Fixed-Effects"),
#   omit.stat             = c("rsq", "ser"),
#   star.cutoffs          = c(.1, .05, .01)
# )

#--------------------------------------------------------#
# Table: Local Quadratic Regressions using CCT bandwidth #
#--------------------------------------------------------#
# Create vector of datasets
local.reg.data <- c(ls(pattern = "\\.(cct)$"))

# Create vector of bandwidths
local.reg.bandwidth <- c(ls(pattern = "(p|w)\\.bandwidth\\.[1-3]?"))

# Create vector of cutpoints
local.reg.cutpoints <- c(purchases.cutoff.list, works.cutoff.list)

# 1st loop: 6 datasets (3x cutoffs for purchases and works)
for (i in seq(from = 1, to = 6)) {

  # 2nd loop: 6 outcomes (3x corruption, 3x mismanagement)
  for (x in seq(from = 1, to = 6)) {

    # No municipal covariates regression
    ll <- lm(
      as.formula(
        paste(outcomes[[x]], so.covariates, sep = " ~ ")),
      data = get(local.reg.data[i])
    )

    # Municipal covariates regression
    ll.x <- lm(
      as.formula(
        paste(
          outcomes[[x]], paste(so.covariates, mun.covariates, sep = " + "),
          sep = " ~ "
        )
      ),
      data = get(local.reg.data[i])
    )

    # If database number (i) is less than or equal to 3, this is a purchases
    # regression. If the outcome number (x) is less than or equal to 3, this is
    # a corruption regression.
    if (i <= 3) {
      if (x <= 3) {
        assign(paste0("ll.corruption.", x, ".purchases.", i), ll)
        assign(paste0("ll.corruption.x.", x, ".purchases.", i), ll.x)
      } else {
        assign(paste0("ll.mismanagement.", x,".purchases.", i), ll)
        assign(paste0("ll.mismanagement.x.", x,".purchases.", i), ll.x)
      }
    } else {
      if (x <= 3) {
        assign(paste0("ll.corruption.", x, ".works.", i-3), ll)
        assign(paste0("ll.corruption.x.", x, ".works.", i-3), ll.x)
      } else {
        assign(paste0("ll.mismanagement.", x, ".works.", i-3), ll)
        assign(paste0("ll.mismanagement.x.", x, ".works.", i-3), ll.x)
      }
    }
    rm(ll, ll.x)
  }
}


# Make it easier to visualize corruption coefficients
# Filter models
effect.1 <- lapply(
  as.list(objects(pattern = "ll\\.corruption\\.[1-3]\\.(.)+\\.(1)$")), get)
effect.2 <- lapply(
  as.list(objects(pattern = "ll\\.corruption\\.[1-3]\\.(.)+\\.(2)$")), get)
effect.3 <- lapply(
  as.list(objects(pattern = "ll\\.corruption\\.[1-3]\\.(.)+\\.(3)$")), get)

# Create column with model names
names(effect.1) <- objects(pattern= "ll\\.corruption\\.[1-3]\\.(.)+\\.(1)$")
names(effect.2) <- objects(pattern= "ll\\.corruption\\.[1-3]\\.(.)+\\.(2)$")
names(effect.3) <- objects(pattern= "ll\\.corruption\\.[1-3]\\.(.)+\\.(3)$")

# Put them into table format
effect.1 %<>%
  plyr::ldply(tidy, .id = "model") %>%
  filter(term == "factor(so.procurement)1") %>%
  mutate(p.value = format(p.value, digits = 3))
effect.2 %<>%
  plyr::ldply(tidy, .id = "model") %>%
  filter(term == "factor(so.procurement)2") %>%
  mutate(p.value = format(p.value, digits = 3))
effect.3 %<>%
  plyr::ldply(tidy, .id = "model") %>%
  filter(term == "factor(so.procurement)3") %>%
  mutate(p.value = format(p.value, digits = 3))

# Bind into one dataset
ll.corruption.models <- rbind(effect.1, effect.2, effect.3)

# Make it easier to visualize corruption coefficients
# Filter models
effect.1 <- lapply(
  as.list(objects(pattern = "ll\\.mismanagement\\.[4-6]\\.(.)+\\.(1)$")), get)
effect.2 <- lapply(
  as.list(objects(pattern = "ll\\.mismanagement\\.[4-6]\\.(.)+\\.(2)$")), get)
effect.3 <- lapply(
  as.list(objects(pattern = "ll\\.mismanagement\\.[4-6]\\.(.)+\\.(3)$")), get)

# Create column with model names
names(effect.1) <- objects(pattern= "ll\\.mismanagement\\.[4-6]\\.(.)+\\.(1)$")
names(effect.2) <- objects(pattern= "ll\\.mismanagement\\.[4-6]\\.(.)+\\.(2)$")
names(effect.3) <- objects(pattern= "ll\\.mismanagement\\.[4-6]\\.(.)+\\.(3)$")

# Put them into table format
effect.1 %<>%
  plyr::ldply(tidy, .id = "model") %>%
  filter(term == "factor(so.procurement)1") %>%
  mutate(p.value = format(p.value, digits = 3))
effect.2 %<>%
  plyr::ldply(tidy, .id = "model") %>%
  filter(term == "factor(so.procurement)2") %>%
  mutate(p.value = format(p.value, digits = 3))
effect.3 %<>%
  plyr::ldply(tidy, .id = "model") %>%
  filter(term == "factor(so.procurement)3") %>%
  mutate(p.value = format(p.value, digits = 3))

# Bind into one dataset
ll.mismanagement.models <- rbind(effect.1, effect.2, effect.3)


#---------------------------------------#
# Table: CCT Non-parametric Regressions #
#---------------------------------------#
# Run service order regressions w/o covariates.
# 1st loop: 6 datasets (3x cutoffs for purchases and works)
for (i in seq(from = 1, to = 6)) {

  # 2nd loop: 6 outcomes (3x corruption, 3x mismanagement)
  for (x in seq(from = 1, to = 6)) {

    # Local linear regression for each unique combination dataset-cutoff-outcome
    nonp <- get(local.reg.data[i]) %$%
      rdrobust(y       = get(outcomes[x]),
               x       = so.amount,
               c       = local.reg.cutpoints[i],
               h       = get(local.reg.bandwidth[i]),
               cluster = ibge.id,
               level   = 90,
               all     = TRUE
      )

    # If database number (i) is less than or equal to 3, this is a purchases
    # regression. If the outcome number (x) is less than or equal to 3, this is
    # a corruption regression.
    if (i <= 3) {
      if (x <= 3) assign(paste0("nonp.corruption.", x, ".purchases.", i), nonp)
      else        assign(paste0("nonp.mismanagement.", x,".purchases.", i),nonp)
    } else {
      if (x <= 3) assign(paste0("nonp.corruption.", x, ".works.", i-3), nonp)
      else        assign(paste0("nonp.mismanagement.", x, ".works.", i-3), nonp)
    }
    rm(nonp)
  }
}

# # Run service order regressions w/ covariates.
# # 1st loop: 6 datasets (3x cutoffs for purchases and works)
# for (i in seq(from = 1, to = 6)) {

#   # 2nd loop: 6 outcomes (3x corruption, 3x mismanagement)
#   for (x in seq(from = 1, to = 6)) {

#     # Local linear regression for each unique combination dataset-cutoff-outcome
#     ll <- get(local.linear.data[i]) %$%
#       rdrobust(y       = get(outcomes[x]),
#                x       = so.amount,
#                c       = local.linear.cutpoints[i],
#                h       = get(local.linear.bandwidth[i]),
#                cluster = ibge.id,
#                level   = 90,
#                all     = TRUE
#       )

#     # If database number (i) is less than or equal to 3, this is a purchases
#     # regression. If the outcome number (x) is less than or equal to 3, this is
#     # a corruption regression.
#     if (i <= 3) {
#       if (x <= 3) assign(paste0("ll.corruption.x.", x, ".purchases.", i), ll)
#       else        assign(paste0("ll.mismanagement.x.", x,".purchases.", i), ll)
#     } else {
#       if (x <= 3) assign(paste0("ll.corruption.x.", x, ".works.", i-3), ll)
#       else        assign(paste0("ll.mismanagement.x.", x, ".works.", i-3), ll)
#     }
#     rm(ll)
#   }
# }

#------------------------------------------------------------------------------#
#################################### Plots #####################################
#------------------------------------------------------------------------------#