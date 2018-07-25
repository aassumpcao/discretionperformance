
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

tibble(model =,
       term = dimnames(nonp.corruption.1.purchases.1$coef)[[1]],
       estimate = as.list(nonp.corruption.1.purchases.1$coef),
       std.error = ,
       statistic = ,
       p.value =

  )




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