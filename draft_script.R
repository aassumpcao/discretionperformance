p.balance.1 %>% unlist() %>% as.tibble(row.names = c(so.statistics, mun.statistics))
?rownames

names(covs)

class(covs$model.1)

covs

str(p.balance.1)
str(c)

nrow(purchases.cutoff.1["so.procurement" > 0,])
?collapse
class(purchases.cutoff.1$so.procurement)

covs

unlist(non.cumulative.mismanagement.4.cutoff.1)$H3

bandwidth.table
bandwidth.means
Q

works.cutoff.1 %$% table(so.procurement)
works.cutoff.2 %$% table(so.procurement)
works.cutoff.3 %$% table(so.procurement)
purchases.cutoff.2 %$% table(so.procurement)
fonts()
?png
mm.1
mm.2
mm.3
names(purchases.cutoff.2)
format(bandwidth.table[4, 4], digits = 5)
as.integer(bandwidth.table[4, 4])

as.integer(bandwidth.table[4, 6])+1

bandwidth.table[4,4]

rdms
so.covariates

mm.1 <- 43876
mm.2 <- 44177
mm.3 <- 38117

a <- lm(mismanagement.binary ~ factor(so.procurement),
        data = filter(analysis.data,
                      so.type == 2 & (so.amount >= 15000 + 43876)
               )
     )

summary(a)
summary(b)

fake.1
fake.2
fake.3

a <- filter(analysis.data, so.type != 2)
b <- a %$%
  rdrobust(y = mismanagement.binary, x = so.amount, c = 15000, p = 1, q = 2,
    # h = fake.bandwidth[i], b = fake.bandwidth[i],
    level = 90, cluster = a$ibge.id, all = TRUE
  )


appendix.data %$% table(so.works.bygranttext)

summary(!is.na(falsification.data$so.description))

purchases.cutoff.1 %$% table(!is.na(so.description))

View(falsification.data$so.description)
names(falsification.data)
names(appendix.data)
fake.bandwidth
?join
filter(falsification.data, so.type == 0) %$% table(so.procurement)

falsification.data %$% table(so.type)

names(falsification.data)

falsification.data %$% View(so.amount)
rm(obj)
?rename
x <- fake.3
fake.label
unlist(fake.3$b)

names(appendix.data)
names(irregularities.cgu)
names(analysis.data)
names(falsification.data)
left_join(irregularities.cgu, so.data.tagged, by = c("so.id" = "soID"))

?join

names()
fake.1
fake.2
fake.3