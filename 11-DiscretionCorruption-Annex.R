#------------------------------------------------------------------------------#
# Appendix Script
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
library(tidyverse) # Version 1.2.1
library(haven)     # Version 1.1.1
library(lubridate) # Version 1.7.4
library(readxl)    # Version 1.1.0
library(psych)     # Version 1.8.4
library(magrittr)  # Version 1.5
library(stargazer) # Version 5.2.1
library(lfe)       # Version 2.6-2291
library(RStata)    # Version 1.1.1
library(Hmisc)     # Version 4.1.1

#------------------------------------------------------------------------------#
################################## Wrangling ###################################
#------------------------------------------------------------------------------#
# Set options for running Stata from R
options(
  "RStata.StataPath" = "/Applications/Stata/StataSE.app/Contents/MacOS/stata-se"
)
options("RStata.StataVersion" = 15)

# Run do-file
stata("11-DiscretionCorruption-Annex.do")

# In the do-file above, I defined the keywords that should be matched in order
# for a service order to be classified as general procurement (purchases) or
# public works. Theses are the two procurement types in Law 8,666/93.
so.data.tagged <- read_dta("sodata_tagged.dta")
load("irregularities_cgu.Rda")

# Here I bring in variables that contain expenditure infractions in order to
# create classification counterfactuals to check whether the keywords criteria
# works.
irregularities.cgu %<>%
  distinct(so.id, .keep_all = T) %>%
  rename(so.id = `Row Labels`) %>%
  rename_at(vars(2:40), funs(paste("infraction.", ., sep = ""))) %>%
  mutate_at(
    vars(num_range("infraction.", 0:36), num_range("infraction.", 98:99)),
    funs(ifelse(is.na(.), 0, 1))
  ) %>%
  select(-c(41:42))

# Three vectors for the data transformation below
procurement.vector  <- paste0("infraction.", c(4:10, 30:31))
SOtext.vector       <- c("purchases", "works")
transfertext.vector <- c("tpurchases", "tworks")

# Create the data for classification checks
appendix.data <-
  left_join(irregularities.cgu, so.data.tagged, by = c("so.id" = "soID")) %>%
    select(so.id, -contains("description"), num_range("infraction.", 4:10),
      infraction.19, num_range("infraction.", 30:31), matches("purchases|works")
    ) %>%
    mutate(
      so.procurement.bycode = ifelse(rowSums(.[,procurement.vector]) > 0, 1, 0),
      so.procurement.bySOtext = ifelse(rowSums(.[,SOtext.vector]) > 0, 1, 0),
      so.procurement.bytransfertext = ifelse(rowSums(.[,transfertext.vector])>0,
                                             1, 0
                                      ),
      so.purchases.bycode         = so.procurement.bycode,
      so.purchases.bySOtext       = purchases,
      so.purchases.bytransfertext = tpurchases,
      so.works.bycode             = ifelse(
                                      so.procurement.bycode == 1 |
                                      infraction.19 == 1,
                                      1,
                                      0
                                    ),
      so.works.bySOtext           = works,
      so.works.bytransfertext     = tworks
    ) %>%
    select(contains("so."))

rm(list = objects(pattern = "so.data|cgu|vector"))

#------------------------------------------------------------------------------#
################################### Analysis ###################################
#------------------------------------------------------------------------------#
#-----------------------#
# All procurement types #
#-----------------------#
# 1. How does the classification differ when using transfer text?
with(
  appendix.data, table(so.procurement.bySOtext, so.procurement.bytransfertext)
)

# Correlation between SO flagged in SO text and Transfer text
with(
  appendix.data,
  cor(
    so.procurement.bySOtext,
    so.procurement.bytransfertext,
    use = "complete.obs"
  )
)

# 2. How does it change when comparing to procurement codes?
with(
  appendix.data, table(so.procurement.bySOtext, so.procurement.bycode)
)

# Correlation between SO flagged in SO text and containing procurement text
with(
  appendix.data,
  cor(
    so.procurement.bySOtext,
    so.procurement.bycode,
    use = "complete.obs"
  )
)

#----------------#
# Purchases type #
#----------------#
no.works <- appendix.data[appendix.data$so.works.bySOtext != 1,]

# 1. How does the classification differ when using transfer text?
with(no.works, table(so.purchases.bySOtext, so.purchases.bytransfertext))

# Correlation between SO flagged in SO text and Transfer text
with(
  no.works,
  cor(
    so.purchases.bySOtext,
    so.purchases.bytransfertext,
    use = "complete.obs"
  )
)

# 2. How does it change when comparing to procurement codes?
with(no.works, table(so.purchases.bySOtext, so.purchases.bycode))

# Correlation between SO flagged in SO text and containing procurement text
with(
  no.works,
  cor(
    so.purchases.bySOtext,
    so.purchases.bycode,
    use = "complete.obs"
  )
)

#------------------#
# Public woks type #
#------------------#
with(appendix.data, table(so.works.bySOtext, so.works.bytransfertext))

# Correlation between SO flagged in SO text and Transfer text
with(
  appendix.data,
  cor(
    so.works.bySOtext,
    so.works.bytransfertext,
    use = "complete.obs"
  )
)

# 2. How does it change when comparing to procurement codes?
with(appendix.data, table(so.works.bySOtext, so.works.bycode))

# Correlation between SO flagged in SO text and containing procurement text
with(
  appendix.data,
  cor(
    so.works.bySOtext,
    so.works.bycode,
    use = "complete.obs"
  )
)