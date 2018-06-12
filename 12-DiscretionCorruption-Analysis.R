#------------------------------------------------------------------------------#
# Estimating the Effect of Discretionary Spending on Corruption:
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
# This third script produces the analysis in the Appendix on service order clas-
# sification. We use a program for Stata authored by Assumpcao (2018) to classi-
# fy the service orders and produce quality measures of the classification pro-
# cedure.
#
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
################################### Packages ###################################
#------------------------------------------------------------------------------#
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
library(xtable)

#------------------------------------------------------------------------------#
################################### Analysis ###################################
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
so.statistics <- c("so.amount", "infraction.count", "corruption")
mun.statistics <- c("")
