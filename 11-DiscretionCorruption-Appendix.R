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
library(VennDiagram) # Version 1.6.203
library(extrafont)   # Version 0.17
library(tikzDevice)  # Version 0.11

# Parameters for Latex font
loadfonts(device = "postscript")


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
# so.data.tagged <- read_dta("sodata_tagged.dta")
load("irregularities_cgu.Rda")
load("so.data.Rda")
load("sodata_tagged.Rda")

# Here I bring in variables that contain expenditure infractions in order to
# create classification counterfactuals to check whether the keywords criteria
# works.
irregularities.cgu %<>%
  filter(!is.na(`Row Labels`)) %>%
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
granttext.vector <- c("tpurchases", "tworks")

# Create the data for classification checks
appendix.data <-
  left_join(irregularities.cgu, so.data.tagged, by = c("so.id" = "soID")) %>%
    select(so.id, -contains("description"), num_range("infraction.", 4:10),
      infraction.19, num_range("infraction.", 30:31), matches("purchases|works")
    ) %>%
    mutate(
      so.procurement.bycode = ifelse(rowSums(.[,procurement.vector]) > 0, 1, 0),
      so.procurement.bySOtext = ifelse(rowSums(.[,SOtext.vector]) > 0, 1, 0),
      so.procurement.bygranttext = ifelse(rowSums(.[,granttext.vector])>0,
                                             1, 0
                                      ),
      so.purchases.bycode         = so.procurement.bycode,
      so.purchases.bySOtext       = purchases,
      so.purchases.bygranttext = tpurchases,
      so.works.bycode             = ifelse(
                                      so.procurement.bycode == 1 |
                                      infraction.19 == 1,
                                      1,
                                      0
                                    ),
      so.works.bySOtext           = works,
      so.works.bygranttext     = tworks
    ) %>%
    select(contains("so."))

# Add so.amount variable for DCdensity test in Appendix B
appendix.data %<>%
  left_join(so.data, by = c("so.id" = "so.id")) %>%
  select(1:10, 25)

rm(list = objects(pattern = "so.data|cgu|vector"))

#------------------------------------------------------------------------------#
################################## Appendix A ##################################
#------------------------------------------------------------------------------#
#--------------#
# Venn Diagram #
#--------------#
area1 <- nrow(subset(appendix.data, so.purchases.bySOtext == 1))
area2 <- nrow(subset(appendix.data, so.works.bySOtext     == 1))
area3 <- nrow(subset(appendix.data,
                     so.purchases.bySOtext == 1 & so.works.bySOtext== 1
              )
         )

# Producing the graphical object
venn.plot <-
  draw.pairwise.venn(
    area1      = area1,
    area2      = area2,
    cross.area = area3,
    category   = c("Purchases \n Service Orders", "Works \n Service Orders"),
    cat.pos    = c(180, 180),
    cat.cex    = c(2, 2),
    cat.default.pos = c("outer"),
    cat.dist   = .05,
    fontface   = 1,
    cex        = c(3, 3, 3),
    fill       = c("white", "grey")
  )

# Producing the graphical device
png(paste0(getwd(),"/article/venn.png"))
grid.draw(venn.plot)
dev.off()


#-----------------------#
# All procurement types #
#-----------------------#
# 1. How does the classification differ when using grant text?
with(
  appendix.data, table(so.procurement.bySOtext, so.procurement.bygranttext)
)

# Correlation between SO flagged in SO text and grant text
with(
  appendix.data,
  cor(
    so.procurement.bySOtext,
    so.procurement.bygranttext,
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

# 1. How does the classification differ when using grant text?
with(no.works, table(so.purchases.bySOtext, so.purchases.bygranttext))

# Correlation between SO flagged in SO text and grant text
with(
  no.works,
  cor(
    so.purchases.bySOtext,
    so.purchases.bygranttext,
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
with(appendix.data, table(so.works.bySOtext, so.works.bygranttext))

# Correlation between SO flagged in SO text and grant text
with(
  appendix.data,
  cor(
    so.works.bySOtext,
    so.works.bygranttext,
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

#------------------------------------------------------------------------------#
################################## Appendix B ##################################
#------------------------------------------------------------------------------#
# In appendix B, we run the McCrary (2008) tests to guarantee that there are no
# manipulation issues around cutoffs in Law 8,666/93. In the lines below we sub-
# set the data to calculate manipulation at each cutoff.
purchases.cutoff1  <-
  select(appendix.data, c(1,6,9,11)) %>%
  filter(so.amount >=    4000 & so.amount <=   12000 & so.works.bySOtext != 1)
purchases.cutoff2  <-
  select(appendix.data, c(1,6,9,11)) %>%
  filter(so.amount >=   40000 & so.amount <=  120000 & so.works.bySOtext != 1)
purchases.cutoff3  <-
  select(appendix.data, c(1,6,9,11)) %>%
  filter(so.amount >=  500000 & so.amount <=  750000 & so.works.bySOtext != 1)
works.cutoff1 <-
  select(appendix.data, c(1,6,9,11)) %>%
  filter(so.amount >=    7500 & so.amount <=   22000)
works.cutoff2 <-
  select(appendix.data, c(1,6,9,11)) %>%
  filter(so.amount >=   75000 & so.amount <=  225000)
works.cutoff3 <-
  select(appendix.data, c(1,6,9,11)) %>%
  filter(so.amount >= 1200000 & so.amount <= 1800000)

# Run t-tests on manipulation
purchases.manipulation1 <- with(purchases.cutoff1,DCdensity(so.amount,    8000))
purchases.manipulation2 <- with(purchases.cutoff2,DCdensity(so.amount,   80000))
purchases.manipulation3 <- with(purchases.cutoff3,DCdensity(so.amount,  650000))
works.manipulation1     <- with(works.cutoff1,    DCdensity(so.amount,   15000))
works.manipulation2     <- with(works.cutoff2,    DCdensity(so.amount,  150000))
works.manipulation3     <- with(works.cutoff3,    DCdensity(so.amount, 1500000))

# Save cutoff plots
# Plot 1
with(purchases.cutoff1, DCdensity(so.amount,    8000))
abline(v = 8000)
title(
  main = "Purchases Cutoff 1",
  xlab = paste0(
    "SO Amount \n ",
    "(p-value: ",
    sprintf("%0.3f", purchases.manipulation1),
    "; n = ",
    nrow(purchases.cutoff1),
    ")",
    collapse = ""
  )
)

purchases.plot1 <- recordPlot()

# Plot 2
with(purchases.cutoff2, DCdensity(so.amount,   80000))
abline(v = 80000)
title(
  main = "Purchases Cutoff 2",
  xlab = paste0(
    "SO Amount \n ",
    "(p-value: ",
    sprintf("%0.3f", purchases.manipulation2),
    "; n = ",
    nrow(purchases.cutoff2),
    ")",
    collapse = ""
  )
)
purchases.plot2 <- recordPlot()

# Plot 3
with(purchases.cutoff3, DCdensity(so.amount,  650000))
abline(v = 650000)
title(
  main = "Purchases Cutoff 3",
  xlab = paste0(
    "SO Amount \n ",
    "(p-value: ",
    sprintf("%0.3f", purchases.manipulation3),
    "; n = ",
    nrow(purchases.cutoff3),
    ")",
    collapse = ""
  )
)
purchases.plot3 <- recordPlot()

# Plot 4
with(works.cutoff1,     DCdensity(so.amount,   15000))
abline(v = 15000)
title(
  main = "Works Cutoff 1",
  xlab = paste0(
    "SO Amount \n ",
    "(p-value: ",
    sprintf("%0.3f", works.manipulation1),
    "; n = ",
    nrow(works.cutoff1),
    ")",
    collapse = ""
  )
)
works.plot1     <- recordPlot()

# Plot 5
with(works.cutoff2,     DCdensity(so.amount,  150000))
abline(v = 150000)
title(
  main = "Works Cutoff 2",
  xlab = paste0(
    "SO Amount \n ",
    "(p-value: ",
    sprintf("%0.3f", works.manipulation2),
    "; n = ",
    nrow(works.cutoff2),
    ")",
    collapse = ""
  )
)
works.plot2     <- recordPlot()

# Plot 6
with(works.cutoff3,     DCdensity(so.amount, 1500000))
abline(v = 1500000)
title(
  main = "Works Cutoff 3",
  xlab = paste0(
    "SO Amount \n ",
    "(p-value: ",
    sprintf("%0.3f", works.manipulation3),
    "; n = ",
    nrow(works.cutoff3),
    ")",
    collapse = ""
  )
)
works.plot3     <- recordPlot()
