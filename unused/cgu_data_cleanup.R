# Andre Assumpcao
# aassumpcao@unc.edu
# CGU audit cleanup file

######################
# Packages needed ####
library(tidyverse)
library(readxl)
library(poliscidata)
library(labelled)
setwd("/Users/aassumpcao/OneDrive - University of North Carolina at Chapel Hill/Documents/Research/2012 Discretion and Corruption/")

#####################
# Load dataset
cgu.data <- read_xlsx("base_cgu_13mar2018.xlsx",
                      sheet = "Dados",
                      range = "A1:R37843",
                      col_names = T,
                      col_types = "text",
                      trim_ws = T)
# Define variable labels
var_label(cgu.data) <- list(cod_ibge = "IBGE id", cod_TSE = "TSE id", UF = "State", mun = "Municipality", draw = "Draw",
                            MEC = "Ministry of Education", MS = "Ministry of Health", MDS = "Ministry of Social Development",
                            id_irregularity = "Irregularity ID", policy = "Policy", subpolicy = "Subpolicy", amount = "Amount",
                            SO = "Service Order", SO_number = "Number of SO", code = "Audit code", audit_start = "Audit Start Date",
                            audit_finish = "Audit Finish Date", objFiscalizacao = "Description of Audit")

