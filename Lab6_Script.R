# Lab 6 Script: Chi-Square and Correlation
# The University of Texas at San Antonio
# URP-5393: Urban Planning Methods II


#---- Objectives ----
# In this Lab you will learn to:

# 1. Learn to apply Chi-Square tests on categorical data
# 2. Learn to calculate Correlation coefficients


#---- 1. Learn to apply Chi-Square tests on categorical data ----

library(data.table)
#install.packages("foreign")
library(foreign)

hts <- data.table(read.spss("datasets/HTS.household.10regions.sav",to.data.frame = T))

hts.tab.obs<-table(hts$sf,hts$anywalk)
class(hts.tab.obs)

prop.table(hts.tab.obs,margin = 1)

hts.cs<-chisq.test(x = hts$sf,y = hts$anywalk,correct = F) # chi-squared test
hts.cs

attributes(hts.cs)


# ---- Simulated hypothesis testing plot ----


curve(expr = dchisq(x = x, df = 1), from = 0, to = 30)
abline(h=0,col='blue')
points(x=hts.cs$statistic,y=0,col='red')

##upper value from Chi-Squared Dist (1-alpha) with alpha=0.05
upper95 <- qchisq(p = .95,df= 1)

#create vector of x values
x_upper95 <- seq(upper95, 30)

#create vector of chi-square density values
p_upper95 <- dchisq(x_upper95, df = 1)

#fill in portion of the density plot for upper 95% value to end of plot
polygon(x = c(x_upper95, rev(x_upper95)), y = c(p_upper95, rep(0, length(p_upper95))), col = adjustcolor('red', alpha=0.3), border = NA)

# ---- symmetric measures for the chi-Squared test
#install.packages('vcd')
library(vcd)
assocstats(hts.tab.obs)

#---- 2. Learn to calculate Correlation coefficients ----

uza <- data.table(read.spss("datasets/UZA.sav",to.data.frame = TRUE))

#pearson Correlation coefficient between log of road lane mile per 100 pop and log of daily vehicle miles traveled

cor(uza$lnlm,uza$lnvmt) # lanes -> vehicle usage?

uza[,cor.test(lnlm,lnvmt)]  #data.table

cor.test(uza$lnlm,uza$lnvmt)  # H0: Correlation = 0; H1: Correlation !=0


# partial correlation
install.packages("ggm")
library(ggm)
names(uza)
uza_par<-uza[,c("lnlm","lnvmt","lnfuel")]
uza_pcor<-pcor(c(1,2,3),cov(uza_par)) #partial correlation coefficient
uza_pcor

dim(uza)
pcor.test(r = uza_pcor,q = 1,n = 157)

cor.test(uza$vmt,uza$pop000,method="spearman") #Spearman correlation coefficient
