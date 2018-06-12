#------------------------------------------------------------------------------#
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
library(RStata)      # Version 1.1.1
library(Hmisc)       # Version 4.1.1
library(rdd)         # Version 0.99.1
library(papaja)      # Version 0.1.0.9709
library(extrafont)   # Version 0.17
library(tikzDevice)  # Version 0.11

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