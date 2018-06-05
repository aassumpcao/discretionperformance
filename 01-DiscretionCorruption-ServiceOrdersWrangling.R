#------------------------------------------------------------------------------#
# (TITLE TO BE DETERMINED)
# Service Order Data wrangling script
# Prepared by:
# Andre Assumpcao
# aassumpcao@unc.edu
#------------------------------------------------------------------------------#
rm(list=ls())

#------------------------------------------------------------------------------#
# README:
#
# This first script file wrangles data at the service order level. What we do
# here is to fill in all service order data from multiple data sources. There is
# information on service order amount, audit year, policy area category, audit
# lottery, textual description of data from two sources, and problems found by
# auditors. It creates the main dataset for analysis in the paper.
#
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
################################### Packages ###################################
#------------------------------------------------------------------------------#
library(tidyverse)  # Version 1.2.1
library(haven)      # Version 1.1.1
library(lubridate)  # Version 1.7.4
library(readxl)     # Version 1.1.0
library(psych)      # Version 1.8.4
library(magrittr)   # Version 1.5
library(stargazer)  # Version 5.2.1
library(lfe)        # Version 2.6-2291

#------------------------------------------------------------------------------#
################################ Data Wrangling ################################
#------------------------------------------------------------------------------#
# Load the following datasets
load("base_cgu.Rda")               # Audit reports coded by CEPESP-FGV
load("corruption_discretion.Rda")  # List of service orders from lCEPESP-FGV
load("irregularities_cgu.Rda")     # List of irregularities by service order
load("cgu_convenios.Rda")          # Federal grants to Brazilian municipalities
load("Sorteio8a31.Rda")            # CGU list of funds audited


#--------------------#
### Mutating Joins ###
#--------------------#
# CEPESP coded 14,521 CGU service orders in health and education
glimpse(corruption_discretion)

# 1. Merge service orders with additional CGU audit information
so.data <- Sorteio8a31 %>%
  select(
    numero, instrumento_transferencia, convenio, periodo_realizacao_inicio_1,
    periodo_realizacao_final, funcional_programatica
  ) %>%
  left_join(corruption_discretion, ., by = c("so_ID" = "numero"))

# 2. Merge service orders with audit report data from CEPESP
so.data <- base_cgu %>%
  select(
    SO_number, cod_ibge, draw, policy, subpolicy, amount, MEC, MS,
    objFiscalizacao
  ) %>%
  distinct(SO_number, .keep_all = T) %>%
  left_join(so.data, ., by = c("so_ID" = "SO_number")) %>%
  left_join(., irregularities.cgu, by = c("so_ID" = "Row Labels")) %>%
  rename_at(vars(13:53), funs(paste("infraction.", ., sep = ""))) %>%
  mutate_at(
    vars(num_range("infraction.", 0:36), num_range("infraction.", 98:99)),
    funs(ifelse(is.na(.), 0, 1))
  ) %>%
  mutate( # This work fixes the missing IDs for public policy
    policy     = str_pad(policy, 4, pad = "0"),
    subpolicy  = ifelse(is.na(funcional_programatica), subpolicy, NA),
    subpolicy1 = sub("^(\\d\\d\\d\\d)(\\dE\\d\\d)$", "\\1\\.\\2", subpolicy),
    subpolicy2 = ifelse(nchar(subpolicy) > 9,
                        str_pad(
                          sprintf("%.4f", as.numeric(subpolicy)), 9, pad = "0"
                        ),
                        NA
                 ),
    subpolicy1 = ifelse(is.na(subpolicy2), subpolicy1, subpolicy2),
    subpolicy1 = str_pad(subpolicy1, 9, pad = "0"),
    subpolicy1 = sub("00119\\.386", "0119.3860", subpolicy1),
    subpolicy1 = str_remove(subpolicy1, "\\."),
    # Individual problems with program and action IDs
    subpolicy1 = sub("13760E31", "10610E36", subpolicy1),
    subpolicy1 = sub("12208585", "12200906", subpolicy1),
    subpolicy1 = sub("01223863", "00043863", subpolicy1),
    subpolicy1 = sub("00050593", "12930593", subpolicy1),
    subpolicy1 = sub("12143863", "00043863", subpolicy1),
    subpolicy1 = sub("01197652", "01227652", subpolicy1),
    subpolicy1 = sub("00675612", "00674519", subpolicy1),
    subpolicy1 = sub("12143861", "01193861", subpolicy1),
    subpolicy1 = sub("10490357", "00080357", subpolicy1)
  )

# 1. Merge missing policy and subpolicy information
so.data <- Sorteio8a31 %>%
  select(funcional_programatica) %>%
  mutate(subpolicy1 = substr(funcional_programatica, 6, 13)) %>%
  distinct(subpolicy1, .keep_all = T) %>%
  left_join(so.data, ., by = c("subpolicy1", "subpolicy1")) %>%
  mutate(funcional_programatica = ifelse(is.na(funcional_programatica.x),
                                         funcional_programatica.y,
                                         funcional_programatica.x
                                  ),
         funcional_programatica = sub("360000001",
                                      "36000000100000000",
                                      funcional_programatica
                                  )
  ) %>%
  select(-funcional_programatica.x, -funcional_programatica.y)

# 3. Merge service orders with Federal grants information
so.data <- cgu_convenios %>%
  filter(TipoEnteConvenente != "'E' (estadual)") %>%
  select(NúmeroConvênio, ObjetoConvênio, ValorLiberado) %>%
  mutate(
    ValorLiberado = parse_double(ValorLiberado,
                                      locale = locale(decimal_mark = ",")
                         )
  ) %>%
  left_join(so.data, ., by = c("convenio" = "NúmeroConvênio"))

# 4. Replace SOs with missing amounts in audit reports from amounts from the
# Federal grants data AND break up the classification of policy programs and
# activities
so.data %<>%
  mutate(
    so.amount = ifelse(is.na(amount) & !is.na(ValorLiberado)
                     & ValorLiberado != 0, ValorLiberado, amount
                )
  ) %>%
  separate(# Format is: ##.###.####.####.####
           # 1. ##   = função      (= function)
           # 2. ###  = subfunção   (= subfunction)
           # 3. #### = programa    (= program)
           # 4. #### = ação        (= action)
           # 5. #### = localizador (= locator)
    funcional_programatica,
    into = c("fun", "subfun", "program", "action", "locator"),
    sep  = c(2, 5, 9, 13)
  ) %>%
  select(-locator, -contains("policy"))


#------------------#
### Columns work ###
#------------------#
# 5. Drop useless columns (corruption)
names(so.data)
so.data %<>% select(-convenio, -amount, -`(blank)`, -ValorLiberado)

# 6. Rename and rearrange columns from largest to smallest aggregation level
so.data %<>%
  rename(
    so.id                = so_ID,
    transfer.id          = instrumento_transferencia,
    audit.start          = periodo_realizacao_inicio_1,
    audit.end            = periodo_realizacao_final,
    ibge.id              = cod_ibge,
    lottery.id           = draw,
    education            = MEC,
    health               = infraction.MS,
    so.description       = infraction.objFiscalizacao,
    infraction.count     = "Grand Total",
    so.function          = fun,
    so.subfunction       = subfun,
    so.program           = program,
    so.subprogram        = action,
    transfer.description = ObjetoConvênio
  ) %>%
  select(
    ibge.id, transfer.id, transfer.description, education, health, lottery.id,
    audit.start, audit.end, contains("so"), contains("infraction")
  )

# 7. Create corruption and mismanagement outcomes (== 7 outcomes total)
# 7.1. corruption.binary    == outcome 1
# 7.2. corruption.share     == outcome 2
# 7.3. corruption.amount    == outcome 3
# 7.4. corruption.count     == outcome 4
# 7.5. mismanagement.binary == outcome 5
# 7.6. mismanagement.share  == (not an outcome, just used for calculating 7.7)
# 7.7. mismanagement.amount == outcome 7
# 7.8. mismanagement.count  == outcome 8

# Create infraction vectors
corruption.vector    <- c("infraction.4", "infraction.5", "infraction.6",
                          "infraction.8", "infraction.9", "infraction.11",
                          "infraction.12", "infraction.13", "infraction.31")
mismanagement.vector <- c("infraction.1", "infraction.2", "infraction.3",
                          "infraction.7",  "infraction.10", "infraction.14",
                          "infraction.15", "infraction.16", "infraction.17",
                          "infraction.18", "infraction.19", "infraction.20",
                          "infraction.21", "infraction.22", "infraction.23",
                          "infraction.24", "infraction.25", "infraction.26",
                          "infraction.27", "infraction.28", "infraction.29",
                          "infraction.30", "infraction.32", "infraction.33",
                          "infraction.34", "infraction.35", "infraction.36")
infraction.vector    <- c(corruption.vector, mismanagement.vector)

# Create outcomes from infraction vectors
so.data %<>%
  mutate(
    # Infraction count (all infractions except for 0, 98, 99) = NOT AN OUTCOME
    infraction.count     = rowSums(.[,infraction.vector]),

    # Binary outcomes (== 1 if there are such infractions)
    corruption.binary    = ifelse(rowSums(.[,corruption.vector]) > 0, 1, 0),
    mismanagement.binary = ifelse(rowSums(.[,mismanagement.vector]) > 0, 1, 0),

    # Infractions as share of total infractions
    corruption.share     = rowSums(.[,corruption.vector]) / infraction.count,
    mismanagement.share  = rowSums(.[,mismanagement.vector]) / infraction.count,

    # Amount as a share of total
    corruption.amount    = ifelse(corruption.binary == 1 & !is.na(so.amount),
                             corruption.share * so.amount, 0
                           ),
    mismanagement.amount = ifelse(mismanagement.binary == 1 & !is.na(so.amount),
                             mismanagement.share * so.amount, 0
                           ),

    # Count of such infractions)
    corruption.count     = rowSums(.[,corruption.vector]),
    mismanagement.count  = rowSums(.[,mismanagement.vector])
  )

#------------------#
#### Values work ###
#------------------#
# 8. Create subprogram description

# 9. Run lubridate
# Manually fill service orders with missing dates
no.date <- tibble(
  so.id       = c(169349, 219129, 197090, 197370, 197582, 197863,
                  198033, 198251, 197071, 197555, 197866, 198035,
                  198107),
  audit.start = ymd(c("2005-08-29", "2005-10-28", "2007-07-30", "2007-07-30",
                  "2007-07-30", "2007-07-30", "2007-07-30", "2007-07-30",
                  "2007-08-02", "2007-08-02", "2007-08-02", "2007-08-02",
                  "2007-08-02")),
  audit.end   = ymd(c("2005-09-02", "2005-12-26", "2007-09-27", "2007-09-27",
                  "2007-09-27", "2007-09-27", "2007-09-27", "2007-09-27",
                  "2007-09-14", "2007-09-14", "2007-09-14", "2007-09-14",
                  "2007-09-14"))
           )

# Fill in missing values for date from municipal audit lottery
date.fill <- so.data %>%
  select(ibge.id, lottery.id, so.id, contains("audit")) %>%
  unite("unique.id", c("ibge.id", "lottery.id"), sep = "", remove = F) %>%
  group_by(unique.id) %>%
  fill(audit.start, audit.end, .direction = "up") %>%
  fill(audit.start, audit.end, .direction = "down") %>%
  full_join(., no.date, by = c ("so.id" = "so.id")) %>%
  mutate(
    audit.start = ifelse(is.na(audit.start.x) & !is.na(audit.start.y),
                         as.character(audit.start.y),
                         as.character(audit.start.x)
                  ),
    audit.end   = ifelse(is.na(audit.end.x) & !is.na(audit.end.y),
                         as.character(audit.end.y),
                         as.character(audit.end.x)
                  )
  ) %>%
  ungroup() %>%
  select(so.id, audit.start, audit.end)

# Pull imputed dates to final SO dataset
so.data %<>%
  left_join(date.fill, by = c("so.id" = "so.id")) %>%
  mutate(
    audit.start.x = as.character(audit.start.y),
    audit.end.x   = as.character(audit.end.y)
  ) %>%
  rename(
    audit.start   = audit.start.x,
    audit.end   = audit.end.x
  ) %>%
  select(-contains(".y"))

# Remove fill data
rm(no.date, date.fill)

# 10. Fill in missing textual descriptions and reformat dates
so.data %<>%
  mutate(
    nchar.so.description = nchar(so.description),
    audit.start = ymd(audit.start),
    audit.end   = ymd(audit.end)
  ) %>%
  group_by(so.subprogram) %>%
  arrange(so.subprogram, desc(nchar.so.description)) %>%
  fill(so.description, .direction = "down") %>%
  ungroup() %>%
  select(-nchar.so.description)

# 11. To log or not to log amount variable
names(so.data)

#--------------------------#
### Back to columns work ###
#--------------------------#
# # 12. Define SO as public works or purchases and type based on Law 8,666/93
# so.data.tagged <- read_dta("soData_tagged.dta")
load("soData_tagged.Rda")

so.data <- so.data.tagged %>%
  select(soID, contains("purchases"), contains("works")) %>%
  left_join(so.data, ., by = c("so.id" = "soID")) %>%
  mutate(
    so.type        = ifelse(
                       works == 1, 2,
                       ifelse(works == 0 & purchases == 1, 1, 0)
                     ),
    transfer.type  = ifelse(
                       tworks == 1, 2,
                       ifelse(tworks == 0 & tpurchases == 1, 1, 0)
                     ),
    so.procurement = 0,
    so.procurement = ifelse(so.type == 1 & so.amount > 8000,  1, so.procurement
                     ),
    so.procurement = ifelse(so.type == 1 & so.amount > 80000, 2, so.procurement
                     ),
    so.procurement = ifelse(so.type == 1 & so.amount > 650000,3, so.procurement
                     ),
    so.procurement = ifelse(
                            so.type == 2 & so.amount > 15000,  1, so.procurement
                     ),
    so.procurement = ifelse(
                            so.type == 2 & so.amount > 150000, 2, so.procurement
                     ),
    so.procurement = ifelse(
                            so.type == 2 & so.amount > 1500000,3, so.procurement
                     )
  ) %>%
  select(-contains("purchases"), -contains("works")) %>%
  select(
    c(1:3), transfer.type, c(4:15), so.type, so.procurement,
    contains("infraction."), contains("corruption.")
  )
# Remove unnecessary dataset
rm(so.data.tagged)

#------------------#
##### Rows work ####
#------------------#
# 14. Drop observations
so.data %<>%
  filter(
    # Drop four types of observations: (i) lottery > 31, for which we can't
    # know whether auditors investigated all SOs in municipality; (ii) SO for
    # which monetary amount is missing, for which we can't determine procurement
    # type; (iii) SO for which no procurement was carried out; (iv) observations
    # for which we cannot identify its description.
    lottery.id < 32  & !is.na(so.amount) & !is.na(so.description) & so.type != 0
  )

# # Narrow into variables used for identification of SO
# so.data %>%
#   select(so.id, transfer.description, so.description) %>%
#   rename(
#     soID = so.id,
#     transferDescription = transfer.description,
#     soDescription = so.description
#   ) %>%
#   write_delim(
#     # Save to .csv to run textfind in Stata
#     "soData.csv",
#     delim = "#",
#     na = ".",
#     col_names = T
#   )

save(so.data, file = "so.data.Rda")