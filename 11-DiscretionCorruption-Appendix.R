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
      so.procurement.bycode      = ifelse(
                                     rowSums(.[,procurement.vector]) > 0, 1, 0
                                   ),
      so.procurement.bySOtext    = ifelse(
                                     rowSums(.[,SOtext.vector]) > 0, 1, 0
                                   ),
      so.procurement.bygranttext = ifelse(
                                     rowSums(.[,granttext.vector])>0,
                                          1, 0
                                   ),
      so.purchases.bycode        = so.procurement.bycode,
      so.purchases.bySOtext      = purchases,
      so.purchases.bygranttext   = tpurchases,
      so.works.bycode            = ifelse(
                                     so.procurement.bycode == 1 |
                                     infraction.19 == 1,
                                     1,
                                     0
                                   ),
      so.works.bySOtext          = works,
      so.works.bygranttext       = tworks
    ) %>%
    select(contains("so."))

# Add so.amount variable for DCdensity test in Appendix B
appendix.data %<>%
  left_join(so.data, by = c("so.id" = "so.id")) %>%
  select(1:10, 25)

save(appendix.data, file = "appendix.data.Rda")

rm(list = objects(pattern = "cgu|vector"))

#------------------------------------------------------------------------------#
################################## Appendix A ##################################
#------------------------------------------------------------------------------#
## Inline chunk for number of observations in analysis
format(nrow(so.data), big.mark = ",")

#----------------------#
# FIGURE: Venn Diagram #
#----------------------#
area1 <- nrow(subset(appendix.data, so.purchases.bySOtext == 1))
area2 <- nrow(subset(appendix.data, so.works.bySOtext     == 1))
area3 <- nrow(
           subset(
             appendix.data, so.purchases.bySOtext == 1 & so.works.bySOtext== 1
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

# Producing the graphical device for LaTeX
png(paste0(getwd(),"/article/venn.png"))
grid.draw(venn.plot)
dev.off()

# # Command for markdown inclusion
# knitr::include_graphics("venn.png")

#---------------------#
# TABLE: Search Terms #
#---------------------#
# IN MARKDOWN / LATEX ONLY #

#-----------------------#
# TABLE: Search Results #
#-----------------------#
# Load search terms
purchases.terms <- c("aquisi", "execu", "equipame", "ve[íi]culo", "despesa",
  "aplica[çc]", "medicamento(.)*peaf", "compra", "recurso(.)*financ",
  "unidade(.)*m[óo]ve(.)*sa[úu]de", "pnate", "transporte(.)*escola",
  "desenv(.)*ensino", "kit", "siafi", "implementa[çc]", "adquir", "pme(.)*2004",
  "aparelhamento", "Total")

works.terms <- c("co(ns|sn)tru", "obra", "implant", "infra(.)*estrut", "amplia",
  "abasteci(.)*d(.)*[áa]gua", "reforma",
  "(melhoria|adequa)+(.)*(f[íi]sica|escolar|habitac|sanit[áa]ria)+", "esgot",
  "adutora|dessaliniz|reservat[óo]", "sanit[áa]ri[ao]", "poço", "aperfei[çc]oa",
  "saneamento", "res[íi]duo(.)*s[óo]lido", "conclus[ãa]o", "Total")

#--------------------------#
# TABLE: Purchases Results #
#--------------------------#
purchases.results <-
  read.delim(
    paste0(getwd(), "/tables/appendix_tab2.txt"),
    colClasses = "character"
  ) %>%
  select(-X, -X.1) %>%
  transmute(
    `Total Finds`        = as.numeric(c1),
    `Average Find`       = as.numeric(c2),
    `Average Length`     = as.numeric(c3),
    `Average Position`   = as.numeric(c4),
    `Average TF-IDF`     = as.numeric(c5),
    `Means test p-value` = as.numeric(c6)
  )
# rownames(procurement.results) <- works.terms

# # Command for inclusion in latex / markdown
# print(
#   xtable(purchases.results, label = "purchasesresults", align = rep("r", 7),
#     digits = c(0, 0, rep(3, 5)), caption = "Purchases Search Results"),
#   file              = paste0(getwd(), "/tables/appendix_tab2.tex"),
#   floating          = TRUE,
#   table.placement   = "!htbp",
#   caption.placement = "top",
#   NA.string         = ".",
#   print.results     = TRUE
# )

#----------------------#
# TABLE: Works Results #
#----------------------#
works.results <-
  read.delim(
    paste0(getwd(), "/tables/appendix_tab3.txt"),
    colClasses = "character"
  ) %>%
  select(-X, -X.1) %>%
  transmute(
    `Total Finds`        = as.numeric(c1),
    `Average Find`       = as.numeric(c2),
    `Average Length`     = as.numeric(c3),
    `Average Position`   = as.numeric(c4),
    `Average TF-IDF`     = as.numeric(c5),
    `Means test p-value` = as.numeric(c6)
  )
# rownames(procurement.results) <- works.terms

# # Command for inclusion in latex / markdown
# print(
#   xtable(works.results, label = "worksresults", align = rep("r", 7),
#     digits = c(0, 0, rep(3, 5)), caption = "Works Search Results"),
#   file              = paste0(getwd(), "/tables/appendix_tab3.tex"),
#   floating          = TRUE,
#   table.placement   = "!htbp",
#   caption.placement = "top",
#   NA.string         = ".",
#   print.results     = TRUE
# )

#---------------------------------------------#
# TABLE: SO description vs. Grant description #
#---------------------------------------------#
## Inline number of SO that have descriptions both from SO text and Grant text
nrow(
  appendix.data %>%
    filter(!is.na(so.procurement.bySOtext) & !is.na(so.procurement.bygranttext))
)

# RE-WRITE TABLE 4



#--------------------------------------------#
# TABLE: SO description vs. Procurement Code #
#--------------------------------------------#
no.works <- appendix.data[appendix.data$so.works.bySOtext != 1, ]

# RE-WRITE TABLE 5

table(appendix.data$so.procurement.bySOtext, appendix.data$so.purchases.bycode)

table(appendix.data$so.procurement.bySOtext, appendix.data$so.works.bycode)


#------------------------------------------------------------------------------#
################################## Appendix B ##################################
#------------------------------------------------------------------------------#
# In appendix B, we run the McCrary (2008) tests to guarantee that there are no
# manipulation issues around cutoffs in Law 8,666/93. In the lines below we sub-
# set the data to calculate manipulation at each cutoff.
manipulation.data <- filter(appendix.data, so.procurement.bySOtext == 1)

purchases.cutoff1  <-
  select(manipulation.data, c(1,6,9,11)) %>%
  filter(so.amount >=    4000 & so.amount <=   12000 & so.works.bySOtext != 1)
purchases.cutoff2  <-
  select(manipulation.data, c(1,6,9,11)) %>%
  filter(so.amount >=   40000 & so.amount <=  120000 & so.works.bySOtext != 1)
purchases.cutoff3  <-
  select(manipulation.data, c(1,6,9,11)) %>%
  filter(so.amount >=  500000 & so.amount <=  750000 & so.works.bySOtext != 1)
works.cutoff1 <-
  select(manipulation.data, c(1,6,9,11)) %>%
  filter(so.amount <=   30000 & so.purchases.bySOtext!=1)
works.cutoff2 <-
  select(manipulation.data, c(1,6,9,11)) %>%
  filter(so.amount >=   75000 & so.amount <=  225000 & so.purchases.bySOtext!=1)
works.cutoff3 <-
  select(manipulation.data, c(1,6,9,11)) %>%
  filter(so.amount >=  750000 & so.amount <= 2500000 & so.purchases.bySOtext!=1)

# Run t-tests on manipulation
purchases.manipulation1 <- with(purchases.cutoff1,DCdensity(so.amount,    8000))
purchases.manipulation2 <- with(purchases.cutoff2,DCdensity(so.amount,   80000))
purchases.manipulation3 <- with(purchases.cutoff3,DCdensity(so.amount,  650000))
works.manipulation1     <- with(works.cutoff1,    DCdensity(so.amount,   15000))
works.manipulation2     <- with(works.cutoff2,    DCdensity(so.amount,  150000))
works.manipulation3     <- with(works.cutoff3,    DCdensity(so.amount, 1500000))

# Save cutoff plots
# Plot 1
png(filename = paste0(getwd(), "/article/purchasesmanipulation1.png"))
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
dev.off()

# Plot 2
png(filename = paste0(getwd(), "/article/purchasesmanipulation2.png"))
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
dev.off()

# Plot 3
png(filename = paste0(getwd(), "/article/purchasesmanipulation3.png"))
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
dev.off()

# Plot 4
png(filename = paste0(getwd(), "/article/worksmanipulation1.png"))
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
dev.off()

# Plot 5
png(filename = paste0(getwd(), "/article/worksmanipulation2.png"))
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
dev.off()

# Plot 6
png(filename = paste0(getwd(), "/article/worksmanipulation3.png"))
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
dev.off()
