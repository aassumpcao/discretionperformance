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
    education = max(education),
    health    = max(health)
  ) %>%
  with(., table(education, health)) %>%
  xtable()

#---------------------------#
# Table: Summary Statistics #
#---------------------------#
# Produce statistics (and their labels) for summary statistics table
# SO stats first
so.statistics <- c("so.amount", "infraction.count", "corruption.binary",
  "corruption.share", "corruption.amount", "mismanagement.binary",
  "mismanagement.share", "mismanagement.amount")
so.statistics.labels <- c("Amount (in R$)", "Infraction Count",
  "Corruption Index I (Binary)",
  "Corruption Index II (Share of Total Infractions)",
  "Corruption Index III (Amount)", "Mismanagement Index I (Binary)",
  "Mismanagement Index II (Share of Total Infractions)",
  "Mismanagement Index III (Amount)")

# Then municipal stats second
mun.statistics <- setdiff(c(names(mun.data), names(mun.election)),
  c("ibge.id", "mun.election"))
mun.statistics.labels <- c("Urban Population (Share)", "Female (Share)",
  "Illiteracy Rate", "GDP", "Gini Index", "Human Development Index",
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
  select(ibge.id, mun.statistics) %>%
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
  label            = "descriptivestatistics",
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
  label            = "descriptivestatistics",
  table.placement  = "!htbp"
)

#------------------------------------------------------------------------------#
################################### Analysis ###################################
#------------------------------------------------------------------------------#
