###################################################################
# rdmulti: analysis of RD designs with multiple cutoffs or scores
# Illustration file
# !version 0.1 11-Apr-2018
# Authors: Matias Cattaneo, Rocio Titiunik, Gonzalo Vazquez-Bare
###################################################################

setwd('M:/Research/RD projects/Multicutoffs/rdmulti/R version')

library(rdrobust)

source('rdmc.R')
source('rdmcplot.R')
source('rdms.R')


###################################################################
# Multiple cutoffs
###################################################################

data = read.csv('simdata_multic.csv')
Y = data$y
X = data$x
C = data$c

aux = rdmc(Y,X,C)
aux = rdmc(Y,X,C,pooled.opt=paste('h=20','p=2',sep=','),verbose=TRUE)
aux = rdmc(Y,X,C,hvec=c(11,12))

pdf('rdmcplot.pdf')
aux = rdmcplot(Y,X,C)
dev.off()

pdf('rdmcplot2.pdf')
aux = rdmcplot(Y,X,C,noscatter=TRUE)
dev.off()

pdf('rdmcplot3.pdf')
aux = rdmcplot(Y,X,C,hvec=c(11,12),pvec=c(1,1))
dev.off()


###################################################################
# Cumulative cutoffs
###################################################################

data = read.csv('simdata_cumul.csv')
Y = data$y
X = data$x
cvec = c(data$c[1],data$c[2])

aux = rdms(Y,X,cvec)
aux = rdms(Y,X,cvec,hvec=c(11,8))
aux = rdms(Y,X,cvec,range.l=c(33,32.5),range.r=c(32.5,100))
cutoff = cvec[1]*(X<=49.5) + cvec[2]*(X>49.5)
aux = rdmc(Y,X,cutoff)

pdf('rdmc_cumul.pdf')
aux = rdmcplot(Y,X,cutoff)
dev.off()


###################################################################
# Bivariate score
###################################################################

data = read.csv('simdata_multis.csv')
Y = data$y
X1 = data$x1
X2 = data$x2
zvar = data$t
cvec = c(data$c1[1],data$c1[2],data$c1[3])
cvec2 = c(data$c2[1],data$c2[2],data$c2[3])

aux = rdms(Y,X1,cvec,X2,zvar,cvec2)
aux = rdms(Y,X1,cvec,X2,zvar,cvec2,hvec=c(15,13,17))
xnorm = apply(cbind(abs(.5-X1),abs(.5-X2)),1,min)*(2*zvar-1)
aux = rdms(Y,X1,cvec,X2,zvar,cvec2,xnorm=xnorm)

