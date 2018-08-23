p.balance.1 %>% unlist() %>% as.tibble(row.names = c(so.statistics, mun.statistics))
?rownames

names(covs)


rm(a, b, discussion.table, discussion.database)

class(covs$model.1)

summarize
??replace_na


discussion.right <- analysis.data %>%
  group_by(so.type, so.procurement) %>%
  summarize(avg.infractions = mean(infraction.count))

discussion.left <- analysis.data %>%
  mutate(so.year = year(audit.end)) %>%
  select(-audit.end) %>%
  group_by(so.year, so.type, so.procurement) %>%
  summarize(total.infractions = mean(infraction.count)) %>%
  filter(!is.na(so.year))

discussion.data <- left_join(discussion.left, discussion.right,
                             by = c("so.type" = "so.type",
                                    "so.procurement" = "so.procurement")
                   )

discussion.data %>%
  ggplot(aes(y = total.infractions, x = so.year)) +
    geom_col() +
    facet_grid(so.type ~ so.procurement,
               labeller = labeller(so.type = c(`1`= "Purchases", `2`= "Works")),
               switch = "both") +
    geom_hline(yintercept = discussion.right$avg.infractions,
               linetype   = "dashed") +
    ylab("Procurement Types") + xlab("Procurement Categories") +
    scale_x_continuous(breaks = c(2004:2010)) +
    theme(text = element_text(family = "LM Roman 10", size = 12))


discussion.1
View(discussion.1)

covs

analysis.data %$% table(infraction.count)

names(analysis.data)

??freq

?table

ggplot(analysis.data, aes(y = infraction.count, x = so.year)) +
  geom_col() +
  facet_grid(so.type ~ so.procurement)

?n()
?sum

analysis.data %$% table(audit.end)

?geom

analysis.data %$% table()

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