mun.covariates
?lapply


outcome.labels

analysis.data %$% table(so.type)

analysis.data %<>% filter(so.type == 2)
?grep


se.1 <- starprep(corruption.binary.models[[1]], clusters = ols.purchases$ibge.id, alpha = .1)
se.2 <- starprep(corruption.binary.models[[2]], clusters = ols.purchases$ibge.id, alpha = .1)
se.3 <- starprep(corruption.binary.models[[3]], clusters = ols.works$ibge.id, alpha = .1)
se.4 <- starprep(corruption.binary.models[[4]], clusters = ols.works$ibge.id, alpha = .1)


