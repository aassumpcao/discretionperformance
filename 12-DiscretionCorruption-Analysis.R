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
library(haven)       # Version 1.1.1
library(lubridate)   # Version 1.7.4
library(readxl)      # Version 1.1.0
library(psych)       # Version 1.8.4
library(magrittr)    # Version 1.5
library(rdd)         # Version 0.99.1
library(tikzDevice)  # Version 0.11
library(stargazer)   # Version 5.2.2
library(xtable)      # Version 1.8.2
library(commarobust) # Version 0.1.0
library(rlist)       # Version 0.4.6.1
library(rdrobust)

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
  # Run each regression
  lm <- lm(
    as.formula(
      paste(outcomes[[i]], so.covariates, sep = " ~ ")),
    data = analysis.data
  )
  # Store corruption regressions
  if (i <= 3) {
    assign(paste0("lm.corruption.", i), lm)
  } else {
    # And mismanagement regressions
    assign(paste0("lm.mismanagement.", i-3), lm)
  }
  rm(lm)
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
    assign(paste0("lm.corruption.covariates.", i), lm)
  } else {
    # And mismanagement regressions
    assign(paste0("lm.mismanagement.covariates.", i-3), lm)
  }
  rm(lm)
}

#---------------------------------#
# Table: First Linear Regressions #
#---------------------------------#
stargazer(

  # Regressions that will be printed to table
  list(lm.corruption.1, lm.corruption.covariates.1, lm.corruption.2,
  lm.corruption.covariates.2, lm.corruption.3,
  lm.corruption.covariates.3),

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
                            lm.corruption.1, lm.corruption.covariates.1,
                            lm.corruption.2, lm.corruption.covariates.2,
                            lm.corruption.3, lm.corruption.covariates.3,
                            clusters = analysis.data$ibge.id,
                            alpha    = .1
                          ),
  # p                     = starprep(
  #                           list(
  #                             lm.corruption.1, lm.corruption.covariates.1,
  #                             lm.corruption.2, lm.corruption.covariates.2,
  #                             lm.corruption.3, lm.corruption.covariates.3
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

#-----------------------#
# Bandwidth choice test #
#-----------------------#
# We use Cattaneo (2016)'s rules for sub-setting the sample. In cases where
# assignment to different treatments is cumulative on the running variable, they
# suggest using observations with running values up to c-1 and c+1 cutoff values
# for each c cutoff.
purchases.bandwidth.1 <- analysis.data %>%
  select(outcomes, so.amount, so.type, ibge.id) %>%
  filter(so.amount <=  80000 & so.type == 1)
purchases.bandwidth.2 <- analysis.data %>%
  select(outcomes, so.amount, so.type, ibge.id) %>%
  filter(so.amount >    8000 & so.amount <= 650000 & so.type == 1)
purchases.bandwidth.3 <- analysis.data %>%
  select(outcomes, so.amount, so.type, ibge.id) %>%
  filter(so.amount >   80000 & so.type == 1)
works.bandwidth.1     <- analysis.data %>%
  select(outcomes, so.amount, so.type, ibge.id) %>%
  filter(so.amount <= 150000 & so.type == 2)
works.bandwidth.2     <- analysis.data %>%
  select(outcomes, so.amount, so.type, ibge.id) %>%
  filter(so.amount >   15000 & so.amount <= 1500000 & so.type == 2)
works.bandwidth.3     <- analysis.data %>%
  select(outcomes, so.amount, so.type, ibge.id) %>%
  filter(so.amount > 150000  & so.type == 2)

# Test 1: CCT = Calonico et al. (2015)
# Create empty vectors for later calculation of mean bandwidth
p.cutoff1.bw <- double(length = 6)
p.cutoff2.bw <- double(length = 6)
p.cutoff3.bw <- double(length = 6)
w.cutoff1.bw <- double(length = 6)
w.cutoff2.bw <- double(length = 6)
w.cutoff3.bw <- double(length = 6)



rdrobust.purchases.1.outcome.1 <- with(purchases.bandwidth.1, rdbwselect(get(outcomes[[1]]), so.amount, c = 8000,    cluster = ibge.id))
rdrobust.purchases.1.outcome.2 <- with(purchases.bandwidth.1, rdbwselect(get(outcomes[[2]]), so.amount, c = 8000,    cluster = ibge.id))
rdrobust.purchases.1.outcome.3 <- with(purchases.bandwidth.1, rdbwselect(get(outcomes[[3]]), so.amount, c = 8000,    cluster = ibge.id))
rdrobust.purchases.1.outcome.4 <- with(purchases.bandwidth.1, rdbwselect(get(outcomes[[4]]), so.amount, c = 8000,    cluster = ibge.id))
rdrobust.purchases.1.outcome.5 <- with(purchases.bandwidth.1, rdbwselect(get(outcomes[[5]]), so.amount, c = 8000,    cluster = ibge.id))
rdrobust.purchases.1.outcome.6 <- with(purchases.bandwidth.1, rdbwselect(get(outcomes[[6]]), so.amount, c = 8000,    cluster = ibge.id))

rdrobust.purchases.2.outcome.1 <- with(purchases.bandwidth.2, rdbwselect(get(outcomes[[1]]), so.amount, c = 80000,   cluster = ibge.id))
rdrobust.purchases.2.outcome.2 <- with(purchases.bandwidth.2, rdbwselect(get(outcomes[[2]]), so.amount, c = 80000,   cluster = ibge.id))
rdrobust.purchases.2.outcome.3 <- with(purchases.bandwidth.2, rdbwselect(get(outcomes[[3]]), so.amount, c = 80000,   cluster = ibge.id))
rdrobust.purchases.2.outcome.4 <- with(purchases.bandwidth.2, rdbwselect(get(outcomes[[4]]), so.amount, c = 80000,   cluster = ibge.id))
rdrobust.purchases.2.outcome.5 <- with(purchases.bandwidth.2, rdbwselect(get(outcomes[[5]]), so.amount, c = 80000,   cluster = ibge.id))
rdrobust.purchases.2.outcome.6 <- with(purchases.bandwidth.2, rdbwselect(get(outcomes[[6]]), so.amount, c = 80000,   cluster = ibge.id))

rdrobust.purchases.3.outcome.1 <- with(purchases.bandwidth.3, rdbwselect(get(outcomes[[1]]), so.amount, c = 650000,  cluster = ibge.id))
rdrobust.purchases.3.outcome.2 <- with(purchases.bandwidth.3, rdbwselect(get(outcomes[[2]]), so.amount, c = 650000,  cluster = ibge.id))
rdrobust.purchases.3.outcome.3 <- with(purchases.bandwidth.3, rdbwselect(get(outcomes[[3]]), so.amount, c = 650000,  cluster = ibge.id))
rdrobust.purchases.3.outcome.4 <- with(purchases.bandwidth.3, rdbwselect(get(outcomes[[4]]), so.amount, c = 650000,  cluster = ibge.id))
rdrobust.purchases.3.outcome.5 <- with(purchases.bandwidth.3, rdbwselect(get(outcomes[[5]]), so.amount, c = 650000,  cluster = ibge.id))
rdrobust.purchases.3.outcome.6 <- with(purchases.bandwidth.3, rdbwselect(get(outcomes[[6]]), so.amount, c = 650000,  cluster = ibge.id))

rdrobust.works.1.outcome.1     <- with(works.bandwidth.1,     rdbwselect(get(outcomes[[1]]), so.amount, c = 15000,   cluster = ibge.id))
rdrobust.works.1.outcome.2     <- with(works.bandwidth.1,     rdbwselect(get(outcomes[[2]]), so.amount, c = 15000,   cluster = ibge.id))
rdrobust.works.1.outcome.3     <- with(works.bandwidth.1,     rdbwselect(get(outcomes[[3]]), so.amount, c = 15000,   cluster = ibge.id))
rdrobust.works.1.outcome.4     <- with(works.bandwidth.1,     rdbwselect(get(outcomes[[4]]), so.amount, c = 15000,   cluster = ibge.id))
rdrobust.works.1.outcome.5     <- with(works.bandwidth.1,     rdbwselect(get(outcomes[[5]]), so.amount, c = 15000,   cluster = ibge.id))
rdrobust.works.1.outcome.6     <- with(works.bandwidth.1,     rdbwselect(get(outcomes[[6]]), so.amount, c = 15000,   cluster = ibge.id))

rdrobust.works.2.outcome.1     <- with(works.bandwidth.2,     rdbwselect(get(outcomes[[1]]), so.amount, c = 150000,  cluster = ibge.id))
rdrobust.works.2.outcome.2     <- with(works.bandwidth.2,     rdbwselect(get(outcomes[[2]]), so.amount, c = 150000,  cluster = ibge.id))
rdrobust.works.2.outcome.3     <- with(works.bandwidth.2,     rdbwselect(get(outcomes[[3]]), so.amount, c = 150000,  cluster = ibge.id))
rdrobust.works.2.outcome.4     <- with(works.bandwidth.2,     rdbwselect(get(outcomes[[4]]), so.amount, c = 150000,  cluster = ibge.id))
rdrobust.works.2.outcome.5     <- with(works.bandwidth.2,     rdbwselect(get(outcomes[[5]]), so.amount, c = 150000,  cluster = ibge.id))
rdrobust.works.2.outcome.6     <- with(works.bandwidth.2,     rdbwselect(get(outcomes[[6]]), so.amount, c = 150000,  cluster = ibge.id))

rdrobust.works.3.outcome.1     <- with(works.bandwidth.3,     rdbwselect(get(outcomes[[1]]), so.amount, c = 1500000, cluster = ibge.id))
rdrobust.works.3.outcome.2     <- with(works.bandwidth.3,     rdbwselect(get(outcomes[[2]]), so.amount, c = 1500000, cluster = ibge.id))
rdrobust.works.3.outcome.3     <- with(works.bandwidth.3,     rdbwselect(get(outcomes[[3]]), so.amount, c = 1500000, cluster = ibge.id))
rdrobust.works.3.outcome.4     <- with(works.bandwidth.3,     rdbwselect(get(outcomes[[4]]), so.amount, c = 1500000, cluster = ibge.id))
rdrobust.works.3.outcome.5     <- with(works.bandwidth.3,     rdbwselect(get(outcomes[[5]]), so.amount, c = 1500000, cluster = ibge.id))
rdrobust.works.3.outcome.6     <- with(works.bandwidth.3,     rdbwselect(get(outcomes[[6]]), so.amount, c = 1500000, cluster = ibge.id))

summary(rdrobust.purchases.1.outcome.1)
summary(rdrobust.purchases.1.outcome.2)
summary(rdrobust.purchases.1.outcome.3)
summary(rdrobust.purchases.1.outcome.4)
summary(rdrobust.purchases.1.outcome.5)
summary(rdrobust.purchases.1.outcome.6)
summary(rdrobust.purchases.2.outcome.1)
summary(rdrobust.purchases.2.outcome.2)
summary(rdrobust.purchases.2.outcome.3)
summary(rdrobust.purchases.2.outcome.4)
summary(rdrobust.purchases.2.outcome.5)
summary(rdrobust.purchases.2.outcome.6)
summary(rdrobust.purchases.3.outcome.1)
summary(rdrobust.purchases.3.outcome.2)
summary(rdrobust.purchases.3.outcome.3)
summary(rdrobust.purchases.3.outcome.4)
summary(rdrobust.purchases.3.outcome.5)
summary(rdrobust.purchases.3.outcome.6)
summary(rdrobust.works.1.outcome.1)
summary(rdrobust.works.1.outcome.2)
summary(rdrobust.works.1.outcome.3)
summary(rdrobust.works.1.outcome.4)
summary(rdrobust.works.1.outcome.5)
summary(rdrobust.works.1.outcome.6)
summary(rdrobust.works.2.outcome.1)
summary(rdrobust.works.2.outcome.2)
summary(rdrobust.works.2.outcome.3)
summary(rdrobust.works.2.outcome.4)
summary(rdrobust.works.2.outcome.5)
summary(rdrobust.works.2.outcome.6)
summary(rdrobust.works.3.outcome.1)
summary(rdrobust.works.3.outcome.2)
summary(rdrobust.works.3.outcome.3)
summary(rdrobust.works.3.outcome.4)
summary(rdrobust.works.3.outcome.5)
summary(rdrobust.works.3.outcome.6)


# Test 2: IK  = Imbens and Kalyanaraman (2012)
# We loop over all outcomes for the optimal bandwidth
for (i in seq(outcomes)) {

  # Create RDDdata objects for each triad proc.type-cutoff-outcome (2x3x6)
  RDDdata.purchases.1 <- RDDdata(y = get(outcomes[i]), x = so.amount,
    cutpoint = 8000,    data = purchases.bandwidth.1)
  RDDdata.purchases.2 <- RDDdata(y = get(outcomes[i]), x = so.amount,
    cutpoint = 80000,   data = purchases.bandwidth.2)
  RDDdata.purchases.3 <- RDDdata(y = get(outcomes[i]), x = so.amount,
    cutpoint = 650000,  data = purchases.bandwidth.3)
  RDDdata.works.1     <- RDDdata(y = get(outcomes[i]), x = so.amount,
    cutpoint = 15000,   data = works.bandwidth.1)
  RDDdata.works.2     <- RDDdata(y = get(outcomes[i]), x = so.amount,
    cutpoint = 150000,  data = works.bandwidth.2)
  RDDdata.works.3     <- RDDdata(y = get(outcomes[i]), x = so.amount,
    cutpoint = 1500000, data = works.bandwidth.3)

  # Assign new object names before ending loop
  for (x in seq(1, 3)) {
    assign(
      paste0(
        "RDD.pcutoff", x, ".outcome", i),
        get(paste0("RDDdata.purchases.", x)
      )
    )
    assign(
      paste0(
        "RDD.wcutoff", x, ".outcome", i),
        get(paste0("RDDdata.works.", x)
      )
    )
  }
  rm(list = objects(pattern = "RDDdata\\.(purchases|works)\\."))
}

# Create empty vectors for later calculation of mean bandwidth
p.cutoff1.bw <- double(length = 6)
p.cutoff2.bw <- double(length = 6)
p.cutoff3.bw <- double(length = 6)
w.cutoff1.bw <- double(length = 6)
w.cutoff2.bw <- double(length = 6)
w.cutoff3.bw <- double(length = 6)

# Fill in vectors
for (i in seq(1, 6)) {

  # Each element is one bandwidth calculation
  p.cutoff1.bw[[i]] <- RDDbw_IK(get(paste0("RDD.pcutoff1", ".outcome", i)))
  p.cutoff2.bw[[i]] <- RDDbw_IK(get(paste0("RDD.pcutoff2", ".outcome", i)))
  p.cutoff3.bw[[i]] <- RDDbw_IK(get(paste0("RDD.pcutoff3", ".outcome", i)))
  w.cutoff1.bw[[i]] <- RDDbw_IK(get(paste0("RDD.wcutoff1", ".outcome", i)))
  w.cutoff2.bw[[i]] <- RDDbw_IK(get(paste0("RDD.wcutoff2", ".outcome", i)))
  w.cutoff3.bw[[i]] <- RDDbw_IK(get(paste0("RDD.wcutoff3", ".outcome", i)))

}

# Mean bandwidth
mean(p.cutoff1.bw)
mean(p.cutoff2.bw)
mean(p.cutoff3.bw)
mean(w.cutoff1.bw)
mean(w.cutoff2.bw)
mean(w.cutoff3.bw)

# Test 3: CV  = Ludwig and Miller (2007)
