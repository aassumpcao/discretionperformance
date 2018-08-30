p.balance.1 %>% unlist() %>% as.tibble(row.names = c(so.statistics, mun.statistics))
?rownames

names(covs)

?rdmc
mun.covariates
analysis.data %$% hist(mun.corruption)


rm(a, b, discussion.table, discussion.database)

class(covs$model.1)

purchases.cutoff.1 %$% table(so.procurement)

bandwidth.table
bandwidth.means

bandwidth.min
bandwidth.max
bandwidth.avg

covs
data.table::transpose(bandwidth.table[1:6,c(1,4)])

bandwidth.means <- map(as.list(bandwidth.table[1:6,]), data.table::transpose)

as.list(bandwidth.table[1:6,])

data.table::transpose(bandwidth.table[1:6,])

as.character(sample.size)

names(purchases.cutoff.1)

analysis.data %$% table(so.procurement)

covs
dev.off()

str(graph.1)

lines(graph.1[["genvars"]][,c("rdplot_ci_l", "rdplot_ci_r")], lty = "dashed", col = "red")
geom_errorbar(aes(ymax = graph.1[["genvars"]][1:16, c("rdplot_ci_l", "rdplot_ci_r")],
                  ymin = graph.1[["genvars"]][17:29, c("rdplot_ci_l", "rdplot_ci_r")]))

fake.1
fake.2
fake.3

graph.1[["genvars"]][1:16, c("rdplot_ci_l", "rdplot_ci_r")]

names(discussion.data)

summarize
??replace_na
106+292
(43876+44177+38117)/3

w.balance.1
names(purchases.cutoff.1)
Q
mun.statistics

# First we pull bandwidths for all outcomes in purchases and works at cutoff 1
bandwidth.table <- readRDS("bandwidth.table.Rds")

class(bandwidth.list)

# Subsetting purchases sample to corruption and mismanagement bandwidths for
# outcome III (amount potentially lost to corruption)
welfare.corruption.p <- purchases.data %>%
  filter(bandwidthRange(so.amount, 8000, unlist(bandwidth.table[1, 3])))
welfare.mismanagement.p <- purchases.data %>%
  filter(bandwidthRange(so.amount, 8000, unlist(bandwidth.table[1, 6])))

# Subsetting works sample to corruption and mismanagement bandwidths for
# outcome III (amount potentially lost to corruption)
welfare.corruption.w <- works.data %>%
  filter(bandwidthRange(so.amount, 15000, unlist(bandwidth.table[4, 3])))
welfare.mismanagement.w <- works.data %>%
  filter(bandwidthRange(so.amount, 15000, unlist(bandwidth.table[4, 6])))

# Pull average amount potentially lost to corruption and mismanagement
avg.1 <- as.vector(summary(welfare.corruption.p$corruption.amount))
avg.2 <- as.vector(summary(welfare.mismanagement.p$mismanagement.amount))
avg.3 <- as.vector(summary(welfare.corruption.w$corruption.amount))
avg.4 <- as.vector(summary(welfare.mismanagement.w$mismanagement.amount))

# Create empty table
welfare.table <- tibble(Cost = rep(NA, 15), Type = rep(NA, 15),
  `Avg. Lost (in R$)` = rep(NA, 15), `# Obs.` = rep(NA, 15),
  `Total (in R$)` = rep(NA, 15))

# Fill in table with standard entries
welfare.table[1:4, 1]     <- rep(c("Corruption", "Mismanagement"), 2)
welfare.table[1:4, 2]     <- rep(c("Purchases", "Works"), each = 2)
welfare.table[7, 1]       <- "Benefits"
welfare.table[8, 1:2]     <- c("Works", "Mismanagement")
welfare.table[5, 1]       <- "Total Cost"
welfare.table[9, 1]       <- "Total Benefit"
welfare.table[11, 1]      <- "Welfare Effect"
welfare.table[12:15, 1]   <- c("Cost (in R$) in the absence of discretion benefit",
                               "Cost Reduction (in %)",
                               "Works Cost (in R$) in the absence of discretion benefit",
                               "Works Cost Reduction (in %)")

# Fill in with relevant COST statistics
welfare.table[1, 3:5] <- c(avg.1[4], nrow(welfare.corruption.p),
  avg.1[4]*nrow(welfare.corruption.p))
welfare.table[2, 3:5] <- c(avg.2[4], nrow(welfare.mismanagement.p),
  avg.2[4]*nrow(welfare.mismanagement.p))
welfare.table[3, 3:5] <- c(avg.3[4], nrow(welfare.corruption.w),
  avg.3[4]*nrow(welfare.corruption.w))
welfare.table[4, 3:5] <- c(avg.4[4], nrow(welfare.mismanagement.w),
  avg.4[4]*nrow(welfare.mismanagement.w))
welfare.table[5, 5]   <- sum(unlist(lapply(welfare.table[1:4, 5], as.numeric)))

# Fill in with relevant BENEFIT statistics (Mismanagement III, Cutoff)
welfare.table[8, 3:5] <- c(-4611, nrow(welfare.mismanagement.w),
  -4611*nrow(welfare.mismanagement.w))

# Fill in with relevant welfare effect statistics
welfare.table[12, 5]  <- sum(unlist(lapply(welfare.table[1:4, 5], as.numeric)),
                             -as.numeric(welfare.table[8, 5]))
welfare.table[13, 5]  <- abs(welfare.table[8, 5]) / welfare.table[12, 5]
welfare.table[14, 5]  <- sum(unlist(lapply(welfare.table[3:4, 5], as.numeric)),
                             -as.numeric(welfare.table[8, 5]))
welfare.table[15, 5]  <- abs(welfare.table[8, 5]) / welfare.table[14, 5]

# Format numerical entries in table
welfare.table[,3]           <- as.integer(unlist(welfare.table[, 3]))
welfare.table[c(1:12,14),5] <- as.integer(unlist(welfare.table[c(1:12, 14), 5]))
welfare.table[c(13, 15), 5] <- 100*welfare.table[c(13, 15), 5]

welfare.table %>%
  xtable(caption = "Welfare Effects from Discretion at Cutoff 1",
         label   = "tab:welfare",
         align   = "llrrrr"
  ) %>%
  print.xtable(type              = "latex",
               file              = "./article/tab_welfare.tex",
               table.placement   = "!htbp",
               caption.placement = "top",
               size              = "scriptsize",
               hline.after       = c(rep(-1, 2), 2, 6, 9, 10, 13, rep(15, 2)),
               include.rownames  = FALSE
  )


?xtable