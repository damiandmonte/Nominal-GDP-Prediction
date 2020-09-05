# install.packages("ggpubr")
# install.packages("readxl")
# install.packages("plyr")
# install.packages("readxl")
# install.packages("caret")
# install.packages("AppliedPredictiveModeling")
# install.packages("installr")
# install.packages("ggplot2")
# install.packages("lattice")
# install.packages("glmnet")
# install.packages("kernlab")
# install.packages ("e1071", dependencies = TRUE)
# install.packages("randomForest")
# install.packages ("BBmisc")
# install.packages ("tidyr")
# install.packages("factoextra")
# install.packages("PerformanceAnalytics")
# install.packages("corrplot")

library(plyr)
library(ggplot2)
library(lattice)
library(installr)
library("readxl")
library("plyr")
library("readxl")
library(dplyr)
library(tidyverse)
library(data.table)
library("ggpubr")
library(caret)
library(rpart)
library(AppliedPredictiveModeling)
library(glmnet)
library(kernlab)
library(ggpubr)
library("randomForest")
library("BBmisc")
library(e1071)
library(factoextra)
library("PerformanceAnalytics")
library(corrplot)

#setwd("F:/MCS/Data Analytics/Project/")
#getwd()

EDA_DF_val <- data.frame(year, ngdp)
df_all_stock <- read.csv("Stock_Market_Data.csv")
df_bond_rates <- read.csv("Bond Rates.csv")
df_corp_profs <- read.csv("Corporate Profits.csv")
my_data <- read_excel("housing.xlsx")
my_data1111 <- read_excel("manufacturing.xlsx")
my_data11 <- read_excel("labour productivity.xlsx")

year <- c(2009,2010,2011,2012,2013,
          2014,2015,2016,2017,2018)
#https://www.google.com/publicdata/explore?ds=d5bncppjof8f9_&met_y=ny_gdp_mktp_cd&idim=country:CAN:MEX:IND&hl=en&dl=en#!ctype=l&strail=false&bcs=d&nselm=h&met_y=ny_gdp_mktp_cd&scale_y=lin&ind_y=false&rdim=country&idim=country:CAN&ifdim=country&hl=en_US&dl=en&ind=false
ngdp <- c(1.37,1.61,1.7886,1.8243,1.8426,
          1.7993,1.5596,1.5358,1.653,1.71)
#https://databank.worldbank.org/reports.aspx?source=2&series=BX.GSR.GNFS.CD&country=CAN
population <- c(33628895.0, 34004889.0, 34339328.0, 34714222.0, 35082954.0,
                35437435.0, 35702908.0, 36109487.0, 36540268.0, 37058856.0)
#https://data.worldbank.org/indicator/FR.INR.RINR?locations=CA
RealInterestRate<- c(4.8,-0.3,-0.1,1.8,1.2,
                     1.0,3.7,1.9,0.13,2.219)
#https://tradingeconomics.com/canada/travel-services-percent-of-service-imports-bop-wb-data.html
TravelServicesImports<- c(29.1,30.5,31.1,31.3,31.1,
                          31.0,29.9,28.7,29.4,29.8)
#https://data.worldbank.org/indicator/CM.MKT.LDOM.NO?locations=CA
TotalDomesticCompanies<- c(3727,3771,3980,4030,3810,
                           3948,3799,3368,3278,3330)
#https://data.worldbank.org/indicator/SM.POP.REFG.OR?locations=CA&view=chart
RefPop<-c(99,90,109,123,99,
          95,80,84,75,84)
#https://data.worldbank.org/indicator/BX.TRF.PWKR.CD.DT
RemRec<-c(1196444724.41024,1199157750.27888, 1227344978.04621, 1254600489.98476, 1336538605.70778,
          1351304689.29191, 1274255368.12748, 1289324860.09108,1326198147.54178, 1351563682.71848)
#https://data.worldbank.org/indicator/BM.TRF.PWKR.CD.DT
RemPaid<-c(4662098288.9915, 5290453957.41713, 5557756827.74173, 5629382995.17341, 5702499908.42515,
           5948213016.80636, 5114157603.63363, 5255722464.64191,6480421657.83641, 6629192670.76703)
#https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=2310005701
RailwaysPass<-c(1372.3,1346.3,1356.7,1341.8,1330.5,
                1300,1322.4,1380.87,1535, 1596.59)
#https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=2310025301
AirPass<-c(52583516,63277409.3753385,66078011.9547392,70467400.7928687,71526725.918431,
           75528607,80228302,85406425,91404001, 89380000)
IntAirPass<-c(1198381,1234528.61925597,1245743.61579713,1280198.18557714,1263297.33686341,
              1290419,1322033,1359442,1443818,1475062.58618088)
#https://www.statista.com/statistics/412804/euro-to-canadian-dollar-average-annual-exchange-rate/ 
eur_cad <- c(1.59,1.37,1.38,1.28,1.37,
             1.47,1.42,1.47,1.464915,1.53054)
#https://www.ofx.com/en-ca/forex-news/historical-exchange-rates/ 
usd_cad <- c(1.141433,1.030533,0.989323,0.999997,1.030084,
             1.104347,1.279163,1.325521,1.297846,1.296654)
#https://tradingeconomics.com/canada/ease-of-doing-business
eob <- c(8,12,13,17,19,
         13,20,22,18,22)  #Ease of Business
#https://www.statista.com/statistics/271247/inflation-rate-in-canada/
inflation <- c(0.3,1.78,2.91,1.52,0.94,
               1.91,1.13,1.43,1.6,2.24)
#https://www.bankofcanada.ca/rates/price-indexes/bcpi/
cpi <- c(461.54,560.17,658.76,620.66,618.71,
         605.86,386.72,351.74,130.4,133.4)  #commodity Price Index
#https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1810000501
cupi <- c(114.4,116.5,119.9,121.7,122.8,
          125.2,126.6,128.4,130.8,133.4) #consumer price index
#https://data.oecd.org/tax/tax-on-personal-income.htm
tpi <- c(2.04,12.04,12.15,12.30,12.86,
         12.97,12.62,12.14,12.10,12.11) #Tax on personal income
#https://data.oecd.org/gga/general-government-deficit.htm
gov_deficit <- c(-3.88,-4.74,-3.31,-2.52,-1.49,
                 0.17,-0.06,-0.45,-0.13,-0.40)
#https://data.oecd.org/tax/tax-revenue.htm#indicator-chart
trev <- c(32.35,31.01,30.80,31.18,31.13,
          31.27,32.82,33.17,32.81,32.99) #Tax Revenue
#https://www150.statcan.gc.ca/n1/daily-quotidien/170913/dq170913a-eng.htm
income_growth <- c(-5.86,5.46,4.55,1.27,2.66,
                   1.99,-1.58,0.85,2.6,0)
#https://www150.statcan.gc.ca/n1/en/subjects/travel_and_tourism
tourist <- c(15737,16219,16014,16344,16059,
            16537,17971,19971,20883,21134)
#https://data.worldbank.org/indicator/ST.INT.ARVL?locations=CA
#https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1410028703
unemployment <- c(8.3,8.1,7.5,7.3,7.1,
                  6.9,6.9,7,6.3,5.8)
#https://www.statcan.gc.ca/eng/subjects-start/housing
housing_mkt<- c(10756375,11146943,11519574,11715862,11994309,
                12356841,12443428,12565782,12471449,12000105)
#https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3610020801
lab_prod<-c(53.8,54.6,55.6,55.8,56.7,
            58.1,58.1,58.3,59.58,59.58)

#https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1010012401 
#select the value of shares traded in the respective year and sum them
df_all_stock_2006 <- df_all_stock %>% 
  filter(grepl( "Toronto Stock Exchange, value of shares traded",Stock.market.statistics),
         grepl( "2006-",ï..REF_DATE)) 
tval_2006 <- sum(df_all_stock_2006$VALUE)

df_all_stock_2007 <- df_all_stock %>% 
  filter(grepl( "Toronto Stock Exchange, value of shares traded",Stock.market.statistics),
         grepl( "2007-",ï..REF_DATE)) 
tval_2007 <- sum(df_all_stock_2007$VALUE)

df_all_stock_2008 <- df_all_stock %>% 
  filter(grepl( "Toronto Stock Exchange, value of shares traded",Stock.market.statistics),
         grepl( "2008-",ï..REF_DATE)) 
tval_2008 <- sum(df_all_stock_2008$VALUE)

df_all_stock_2009 <- df_all_stock %>% 
  filter(grepl( "Toronto Stock Exchange, value of shares traded",Stock.market.statistics),
         grepl( "2009-",ï..REF_DATE)) 
tval_2009 <- sum(df_all_stock_2009$VALUE)

df_all_stock_2010 <- df_all_stock %>% 
  filter(grepl( "Toronto Stock Exchange, value of shares traded",Stock.market.statistics),
         grepl( "2010-",ï..REF_DATE)) 
tval_2010 <- sum(df_all_stock_2010$VALUE)

df_all_stock_2011 <- df_all_stock %>% 
  filter(grepl( "Toronto Stock Exchange, value of shares traded",Stock.market.statistics),
         grepl( "2011-",ï..REF_DATE)) 
tval_2011 <- sum(df_all_stock_2011$VALUE)

df_all_stock_2012 <- df_all_stock %>% 
  filter(grepl( "Toronto Stock Exchange, value of shares traded",Stock.market.statistics),
         grepl( "2012-",ï..REF_DATE)) 
tval_2012 <- sum(df_all_stock_2012$VALUE)

df_all_stock_2013 <- df_all_stock %>% 
  filter(grepl( "Toronto Stock Exchange, value of shares traded",Stock.market.statistics),
         grepl( "2013-",ï..REF_DATE)) 
tval_2013 <- sum(df_all_stock_2013$VALUE)

df_all_stock_2014 <- df_all_stock %>% 
  filter(grepl( "Toronto Stock Exchange, value of shares traded",Stock.market.statistics),
         grepl( "2014-",ï..REF_DATE)) 
tval_2014 <- sum(df_all_stock_2014$VALUE)

df_all_stock_2015 <- df_all_stock %>% 
  filter(grepl( "Toronto Stock Exchange, value of shares traded",Stock.market.statistics),
         grepl( "2015-",ï..REF_DATE)) 
tval_2015 <- sum(df_all_stock_2015$VALUE)

df_all_stock_2016 <- df_all_stock %>% 
  filter(grepl( "Toronto Stock Exchange, value of shares traded",Stock.market.statistics),
         grepl( "2016-",ï..REF_DATE)) 
tval_2016 <- sum(df_all_stock_2016$VALUE)

df_all_stock_2017 <- df_all_stock %>% 
  filter(grepl( "Toronto Stock Exchange, value of shares traded",Stock.market.statistics),
         grepl( "2017-",ï..REF_DATE)) 
tval_2017 <- sum(df_all_stock_2017$VALUE)

tval_2018 <- 1665138 

# create a column of all the summed values  
tval <- c(tval_2009,tval_2010,tval_2011,tval_2012,tval_2013,tval_2014,tval_2015,
          tval_2016,tval_2017,tval_2018)

#select the volume of shares traded in the respective year and sum them
df_all_stock_v2006 <- df_all_stock %>% 
  filter(grepl( "Toronto Stock Exchange, volume of shares traded",Stock.market.statistics),
         grepl( "2006-",ï..REF_DATE)) 
tvol_2006 <- sum(df_all_stock_v2006$VALUE)

df_all_stock_v2007 <- df_all_stock %>% 
  filter(grepl( "Toronto Stock Exchange, volume of shares traded",Stock.market.statistics),
         grepl( "2007-",ï..REF_DATE)) 
tvol_2007 <- sum(df_all_stock_v2007$VALUE)

df_all_stock_v2008 <- df_all_stock %>% 
  filter(grepl( "Toronto Stock Exchange, volume of shares traded",Stock.market.statistics),
         grepl( "2008-",ï..REF_DATE)) 
tvol_2008 <- sum(df_all_stock_v2008$VALUE)

df_all_stock_v2009 <- df_all_stock %>% 
  filter(grepl( "Toronto Stock Exchange, volume of shares traded",Stock.market.statistics),
         grepl( "2009-",ï..REF_DATE)) 
tvol_2009 <- sum(df_all_stock_v2009$VALUE)

df_all_stock_v2010 <- df_all_stock %>% 
  filter(grepl( "Toronto Stock Exchange, volume of shares traded",Stock.market.statistics),
         grepl( "2010-",ï..REF_DATE)) 
tvol_2010 <- sum(df_all_stock_v2010$VALUE)

df_all_stock_v2011 <- df_all_stock %>% 
  filter(grepl( "Toronto Stock Exchange, volume of shares traded",Stock.market.statistics),
         grepl( "2011-",ï..REF_DATE)) 
tvol_2011 <- sum(df_all_stock_v2011$VALUE)

df_all_stock_v2012 <- df_all_stock %>% 
  filter(grepl( "Toronto Stock Exchange, volume of shares traded",Stock.market.statistics),
         grepl( "2012-",ï..REF_DATE)) 
tvol_2012 <- sum(df_all_stock_v2012$VALUE)

df_all_stock_v2013 <- df_all_stock %>% 
  filter(grepl( "Toronto Stock Exchange, volume of shares traded",Stock.market.statistics),
         grepl( "2013-",ï..REF_DATE)) 
tvol_2013 <- sum(df_all_stock_v2013$VALUE)

df_all_stock_v2014 <- df_all_stock %>% 
  filter(grepl( "Toronto Stock Exchange, volume of shares traded",Stock.market.statistics),
         grepl( "2014-",ï..REF_DATE)) 
tvol_2014 <- sum(df_all_stock_v2014$VALUE)

df_all_stock_v2015 <- df_all_stock %>% 
  filter(grepl( "Toronto Stock Exchange, volume of shares traded",Stock.market.statistics),
         grepl( "2015-",ï..REF_DATE)) 
tvol_2015 <- sum(df_all_stock_v2015$VALUE)

df_all_stock_v2016 <- df_all_stock %>% 
  filter(grepl( "Toronto Stock Exchange, volume of shares traded",Stock.market.statistics),
         grepl( "2016-",ï..REF_DATE)) 
tvol_2016 <- sum(df_all_stock_v2016$VALUE)

df_all_stock_v2017 <- df_all_stock %>% 
  filter(grepl( "Toronto Stock Exchange, volume of shares traded",Stock.market.statistics),
         grepl( "2017-",ï..REF_DATE)) 
tvol_2017 <- sum(df_all_stock_v2017$VALUE)

tvol_2018 <-87861.5 

#create a column of all the values
tvol <- c(tvol_2009,
          tvol_2010,
          tvol_2011,
          tvol_2012,
          tvol_2013,
          tvol_2014,
          tvol_2015,
          tvol_2016,
          tvol_2017,
          tvol_2018)

#https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1010013901
#select the bond data from year 2009 to 2018
colnames(df_bond_rates)
df_bond <- df_bond_rates %>% 
  filter(grepl( "Selected Government of Canada benchmark bond yields: 10 years",Rates),
         grepl( "2009-|2010-|2011-|2012-|2013-|2014-|2015-|2016-|2017-|2018-",ï..REF_DATE)) 
df_bond

# For each year select values of all 12 months and find average of those values.
df_bond_2009 <- df_bond %>% filter(grepl("2009",ï..REF_DATE)) 
df_bond_2009 <- sum(df_bond_2009$VALUE) 
bon_val_2009 <- (df_bond_2009/12)
bon_val_2009

df_bond_2010 <- df_bond %>% filter(grepl("2010",ï..REF_DATE)) 
df_bond_2010 <- sum(df_bond_2010$VALUE) 
bon_val_2010 <- (df_bond_2010/12)

df_bond_2011 <- df_bond %>% filter(grepl("2011",ï..REF_DATE)) 
df_bond_2011 <- sum(df_bond_2011$VALUE) 
bon_val_2011 <- (df_bond_2011/12)

df_bond_2012 <- df_bond %>% filter(grepl("2012",ï..REF_DATE)) 
df_bond_2012 <- sum(df_bond_2012$VALUE) 
bon_val_2012 <- (df_bond_2012/12)

df_bond_2013 <- df_bond %>% filter(grepl("2013",ï..REF_DATE)) 
df_bond_2013 <- sum(df_bond_2013$VALUE) 
bon_val_2013 <- (df_bond_2013/12)

df_bond_2014 <- df_bond %>% filter(grepl("2014",ï..REF_DATE)) 
df_bond_2014 <- sum(df_bond_2014$VALUE) 
bon_val_2014 <- (df_bond_2014/12)

df_bond_2015 <- df_bond %>% filter(grepl("2015",ï..REF_DATE)) 
df_bond_2015 <- sum(df_bond_2015$VALUE) 
bon_val_2015 <- (df_bond_2015/12)

df_bond_2016 <- df_bond %>% filter(grepl("2016",ï..REF_DATE)) 
df_bond_2016 <- sum(df_bond_2016$VALUE) 
bon_val_2016 <- (df_bond_2016/12)

df_bond_2017 <- df_bond %>% filter(grepl("2017",ï..REF_DATE)) 
df_bond_2017 <- sum(df_bond_2017$VALUE) 
bon_val_2017 <- (df_bond_2017/12)

df_bond_2018 <- df_bond %>% filter(grepl("2018",ï..REF_DATE)) 
df_bond_2018 <- sum(df_bond_2018$VALUE) 
bon_val_2018 <- (df_bond_2018/12)

#create a column of the average bond value of each year. 
bond_10yr <- c(bon_val_2009, bon_val_2010, bon_val_2011, bon_val_2012,
               bon_val_2013, bon_val_2014,bon_val_2015,bon_val_2016,bon_val_2017,bon_val_2018)
bond_10yr

#=======================================================================================================
#DATAFRAME WITH ALL VARIABLES
#=======================================================================================================

#Create a RAW datafrrame
raw_df <- data.frame(year,ngdp,population,RealInterestRate,TotalDomesticCompanies,
                     TravelServicesImports,RefPop,RemRec,RemPaid,RailwaysPass,AirPass,
                     IntAirPass,tval, tvol,bond_10yr,eur_cad, usd_cad,eob, inflation,
                     cupi,cpi,gov_deficit,trev, income_growth,tourist,unemployment,
                     housing_mkt,lab_prod)

#Create a training set
raw_train = raw_df[1:8,]

#create a validtion set
raw_valid = raw_df[9:10,]

#====================================================================================================
#Shapiro Wilk Normality Test to find whether data is normalized or not

mydata<-raw_df
kk<-Map(function(x)cbind(shapiro.test(x)$statistic,shapiro.test(x)$p.value),mydata)
myout<-ldply(kk)
names(myout)<-c("var","W","p.value")
myout

#Variables which are not normalized
nn_vars <- filter(myout, p.value < 0.05)
nn_vars

#===================================================================================================
#Standardization of Data and Normal Distribution

#min_max_norm <- function(x) {
#  ((x - min(x)) / (max(x) - min(x)) * 100)
#}

#zVar <- function (myVar) {
#  (myVar - mean(myVar)) / sd(myVar)
#}

# dnorm function is probability density function for a standard normal distribution
#1/sqrt(2*pi)*exp(-0.5*x^2)

normal_distrbution <- function(col){
  dnorm(col, mean(col), sd(col))
}

minmax_norm <- as.data.frame(lapply(raw_df, normal_distrbution))

#==================================================================================================
#Plotting Histograms

hist(minmax_norm$ngdp,col="darkmagenta")
hist(minmax_norm$population,col="darkmagenta")
hist(minmax_norm$RealInterestRate,col="darkmagenta")
hist(minmax_norm$TotalDomesticCompanies,col="darkmagenta")
hist(minmax_norm$TravelServicesImports,col="darkmagenta")
hist(minmax_norm$RefPop,col="darkmagenta")
hist(minmax_norm$RemRec,col="darkmagenta")
hist(minmax_norm$RemPaid,col="darkmagenta")
hist(minmax_norm$RailwaysPass,col="darkmagenta")
hist(minmax_norm$AirPass,col="darkmagenta")
hist(minmax_norm$IntAirPass,col="darkmagenta")
hist(minmax_norm$tval,col="darkmagenta")
hist(minmax_norm$tvol,col="darkmagenta")
hist(minmax_norm$bond_10yr,col="darkmagenta")
hist(minmax_norm$eur_cad,col="darkmagenta")
hist(minmax_norm$usd_cad,col="darkmagenta")
hist(minmax_norm$lab_prod,col="darkmagenta")
hist(minmax_norm$eob,col="darkmagenta")
hist(minmax_norm$inflation,col="darkmagenta")
hist(minmax_norm$cupi,col="darkmagenta")
hist(minmax_norm$cpi,col="darkmagenta")
hist(minmax_norm$gov_deficit,col="darkmagenta")
hist(minmax_norm$trev,col="darkmagenta")
hist(minmax_norm$income_growth,col="darkmagenta")
hist(minmax_norm$tourist,col="darkmagenta")
hist(minmax_norm$unemployment,col="darkmagenta")
hist(minmax_norm$housing_mkt,col="darkmagenta")
hist(minmax_norm$lab_prod,col="darkmagenta")
#======================================================================================
#Plotting the features

plot(minmax_norm$year,minmax_norm$ngdp) 
plot(minmax_norm$year,minmax_norm$population)
plot(minmax_norm$year,minmax_norm$RealInterestRate)
plot(minmax_norm$year,minmax_norm$TotalDomesticCompanies)
plot(minmax_norm$year,minmax_norm$TravelServicesImports) 
plot(minmax_norm$year,minmax_norm$EnergyUse)
plot(minmax_norm$year,minmax_norm$AgrLand)
plot(minmax_norm$year,minmax_norm$RefPop)
plot(minmax_norm$year,minmax_norm$RemRec)
plot(minmax_norm$year,minmax_norm$RemPaid)
plot(minmax_norm$year,minmax_norm$RailwaysPass)
plot(minmax_norm$year,minmax_norm$AirPass)  
plot(minmax_norm$year,minmax_norm$IntAirPass)
plot(minmax_norm$year,minmax_norm$tval)
plot(minmax_norm$year,minmax_norm$tvol)
plot(minmax_norm$year,minmax_norm$bond_10yr)
plot(minmax_norm$year,minmax_norm$eur_cad)
plot(minmax_norm$year,minmax_norm$usd_cad)
plot(minmax_norm$year,minmax_norm$lab_prod)
plot(minmax_norm$year,minmax_norm$eob)
plot(minmax_norm$year,minmax_norm$inflation)
plot(minmax_norm$year,minmax_norm$cupi)
plot(minmax_norm$year,minmax_norm$cpi)
plot(minmax_norm$year,minmax_norm$gov_deficit)
plot(minmax_norm$year,minmax_norm$trev)
plot(minmax_norm$year,minmax_norm$income_growth)
plot(minmax_norm$year,minmax_norm$tourist)
plot(minmax_norm$year,minmax_norm$unemployment)
plot(minmax_norm$year,minmax_norm$housing_mkt)
plot(minmax_norm$year,minmax_norm$lab_prod)

#=================================================================================
#Preparing test and train before the univariate analysis

minmax_norm_train <- minmax_norm[1:8,] 
minmax_norm_test <- minmax_norm[9:10,] 

#=================================================================================
#Analysis

colnames(minmax_norm)

#Create a correlation Matrix
x <- cor(minmax_norm)

#print the Matrix
print(x)

#The function chart.Correlation()[ in the package PerformanceAnalytics], can be used to display a chart of a correlation matrix.
my_data_ <- minmax_norm
chart.Correlation(my_data_, histogram=TRUE, pch=19)

#View Summary of the matrix
summary(x[upper.tri(x)])

#Using findCorrelation function to find the highly correlated variables
highlyCor <- 
  findCorrelation(
    x,
    cutoff = 0.9,
    verbose = FALSE,
    names = TRUE,
    exact = ncol(x) < 100
  )

highlyCor

#Remove Highly Correlated Variables
minmax_norm_ua <- minmax_norm[ , !(names(minmax_norm) %in% highlyCor)]
colnames(minmax_norm_ua)

#===========================================================================================
#Create Train and Test Data after analysis and removal of highly correlated variables

minmax_norm_ua_train <- minmax_norm_ua[1:8,]
minmax_norm_ua_test<- minmax_norm_ua[9:10,]

#=============================================================================================================
#Remove the colnames with high correlation from the raw dataset and create a new dataset for PCA
raw_data_uni <- raw_df[ , !(names(raw_df) %in% highlyCor)]
colnames(raw_data_uni)

raw_train_uni <- raw_df[1:8,]
raw_valid_uni <- raw_df[9:10,]

#==============================================================================================================
#Principal Component Analysis on RAW UNIVARIATE DATA-----we are not using normalized data set created earlier 
#as prcomp does the normalization 

colnames(raw_train_uni)
tngdp <- raw_train_uni$ngdp

# Compute PCA
# apply PCA - scale. = TRUE is highly
pca_2 = prcomp(raw_train_uni[,-1:-2],scale. = T)

#Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component.
fviz_eig(pca_2)

#Graph of variables. Positive correlated variables point to the same side of the plot. 
#Negative correlated variables point to opposite sides of the graph.
fviz_pca_var(pca_2,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
#Biplot of years and variables
fviz_pca_biplot(pca_2, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

# print method
print(pca_2)

# plot method
plot(pca_2, type = "l")

# summary method
summary(pca_2)

# loadings
loadings_2 <- as.data.frame(pca_2$x)
Matrix_2 <- pca_2$rotation

#Standard Deviation
std_dev <- pca_2$sdev
pr_comp_var <- std_dev^2
pr_comp_var

#Find variances of each PCA
prop_var_ex <- (pr_comp_var/sum(pr_comp_var))
prop_var_ex

#Select laodings and bind with ngdp
loadings4 <- loadings_2[1:7]
pca_train4 <- cbind(loadings4,tngdp)
pca_train4

#Create test PCA data
pca_test <- raw_valid_uni[-1:-2]
pca_test2 <- predict(pca_2, newdata = pca_test)
pca_test2 <- as.data.frame(pca_test2)
pca_test3 <- pca_test2[1:7]
Y_test <- raw_valid_uni$ngdp
pca_test4 <- cbind(pca_test3,Y_test)

#==============================================================================================================
#Train Linear Model from the PCA selected values
lin_mod_uni <- lm(tngdp ~ PC1 + PC2 + PC4 + PC6 + PC7,data=pca_train4)
summary(lin_mod_uni)

#predict using the test PCA
predict_uni <- predict(lin_mod_uni,pca_test3)
predict_uni

#========================================================================================================
# LINEAR REGRESSION MODEL - With 10 fold Cross Validation

# Set up a 10-fold cross validation
tc <- trainControl(method = "cv", number = 10)
# Include the setup in your model
lin_mod_cv <- lm(tngdp ~ PC1 + PC2 + PC4 + PC6 + PC7,data=pca_train4,trControl=tc)
summary(lin_mod_cv)

predict_cv <- predict(lin_mod_cv,pca_test3)
predict_cv  #Exact Same Results as linear

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

# Calculate error
error <- raw_valid_uni$ngdp - predict_cv

# Example of invocation of functions
rmse(error)
mae(error)

#Calculate individual year accuracy
accuracy_each <- 100 - ((abs(raw_valid_uni$ngdp - predict_cv) / abs(raw_valid_uni$ngdp)) * 100)
accuracy_each

#calculate overall Accuracy
accuracy <- 100 - mean((abs(raw_valid_uni$ngdp - predict_cv) / abs(raw_valid_uni$ngdp)) * 100)
accuracy


#==============================================================================================================
#Feature Selection using Random Forest

# Random Forest condidering all features
colnames(minmax_norm_ua_train)
str(minmax_norm_ua_train)
rf <- randomForest(minmax_norm_ua_train$ngdp ~ .,
                   data = minmax_norm_ua_train,
                   ntree = 1000, 
                   mtry = 20, 
                   importance = TRUE)

# Find the importance of each feature according to the data leaf node impurity
imp = importance(rf, type=2, class=NULL,scale=TRUE)
imp <- data.frame(predictors=rownames(imp),imp)

# sort them according to their importance
imp.sort <- arrange(imp,desc(IncNodePurity))
imp.sort$predictors <- factor(imp.sort$predictors,levels=imp.sort$predictors)
imp.sort

# > imp.sort
# predictors IncNodePurity
# 1                    trev    0.20708287
# 2                 usd_cad    0.16530453
# 3             housing_mkt    0.15779639
# 4  TotalDomesticCompanies    0.15265170
# 5                     cpi    0.13772201
# 6                    tvol    0.12753961
# 7               bond_10yr    0.12609777
# 8              IntAirPass    0.12475134
# 9        RealInterestRate    0.12294165
# 10                    eob    0.10581954
# 11                 RemRec    0.09784834
# 12            gov_deficit    0.09610515
# 13                eur_cad    0.09356500
# 14           RailwaysPass    0.08922714
# 15                RemPaid    0.07504624
# 16              inflation    0.06294615
# 17                   tval    0.05723689
# 18  TravelServicesImports    0.04136437
# 19                tourist    0.03227438
# 20                 RefPop    0.01238447

#============================================================================================================
#Random Forest Model to predict the NGDP

summary(minmax_norm_ua_train)

# Train a Random Forest
rfs <- randomForest(minmax_norm_ua_train$ngdp ~ RemRec +
                      RealInterestRate + TravelServicesImports +
                      bond_10yr + gov_deficit + TotalDomesticCompanies ,            
                    data = minmax_norm_ua_train,
                    ntree = 1000, 
                    mtry = 6, 
                    importance = TRUE)

# Print the model output                             
print(rfs)

#predict
rf_pred_tr <- predict(rfs, raw_valid_uni)
rf_pred_tr

# Plot the model trained in the previous exercise
plot(rfs)

# Calculate error
error <- raw_valid_uni$ngdp - rf_pred_tr
error

# Find RMSE and MAE
rmse(error)
mae(error)

#calculate Accuracy
accuracy_each <- 100 - ((abs(raw_valid_uni$ngdp - rf_pred_tr) / abs(raw_valid_uni$ngdp)) * 100)
accuracy_each

#calculate overall Accuracy
accuracy <- 100 - mean((abs(raw_valid_uni$ngdp - rf_pred_tr) / abs(raw_valid_uni$ngdp)) * 100)
accuracy

#===========================================================================================================
#Random Forest Model ----HIGH ACCURATE OUTPUT --BUT with RAW data in few variables
raw_train_tmp <- raw_train

#normalize 3 variables 
lp_norm <- dnorm(raw_train_tmp$lab_prod, mean(raw_train_tmp$lab_prod), 
                 sd(raw_train_tmp$lab_prod))
lp_norm
ri_norm <- dnorm(raw_train_tmp$RealInterestRate, mean(raw_train_tmp$RealInterestRate),
                 sd(raw_train_tmp$RealInterestRate))
ri_norm
rp_norm <- dnorm(raw_train_tmp$RemPaid, mean(raw_train_tmp$RemPaid),
                 sd(raw_train_tmp$RemPaid))
rp_norm

raw_train_tmp$RealInterestRate <- ri_norm
raw_train_tmp$lab_prod <- lp_norm
raw_train_tmp$RemPaid <- rp_norm

#train the model
rf_ <- randomForest(ngdp ~ tval+RealInterestRate+
                     inflation+RailwaysPass+lab_prod+gov_deficit+
                      housing_mkt+bond_10yr+RemPaid+RemRec,
                   data = raw_train_tmp,
                   ntree = 1000,
                   importance = TRUE)
#print the model
print(rf_)

#plot the model
plot(rf_)

#predict using test set
rf_pred_ <- predict(rf_, newdata= raw_valid[-2])
rf_pred_

#calculate the error
error <- raw_valid_uni$ngdp - rf_pred_
error

# Find RMSE and MAE
rmse(error)
mae(error)

#calculate Accuracy
accuracy_each <- 100 - ((abs(raw_valid_uni$ngdp - rf_pred_) / abs(raw_valid$ngdp)) * 100)
accuracy_each

#calculate overall Accuracy
accuracy <- 100 - mean((abs(raw_valid_uni$ngdp - rf_pred_) / abs(raw_valid$ngdp)) * 100)
accuracy

#=======================================================================================================
# SVM MODELLING  

#svm_tune <- tune(svm, train.x=raw_train_uni, train.y=raw_valid_uni, 
#                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
#print(svm_tune)

# Best model determined by CV
gamma.best <- 1e-5; cost.best <- 1e+4; epsilon.best <- 0.01

#train the SVM model
svm_model <- svm(ngdp ~., data = raw_train_uni,type = "eps-regression",kernel = "radial",
                 cost = cost.best, gamma = gamma.best, epsilon = epsilon.best)

#print the statistics
print(svm_model)

#predict using test data
pred <- predict(svm_model,raw_valid_uni)
pred

#calculate the error
error <- raw_valid_uni$ngdp - pred
error

# calculate the RMSE and MAE
rmse(error)
mae(error)

#Calculate the Accuracy
accuracy_each <- 100 - ((abs(raw_valid_uni$ngdp - pred) / abs(raw_valid$ngdp)) * 100)
accuracy_each

#calculate overall Accuracy
accuracy <- 100 - mean((abs(raw_valid_uni$ngdp - pred) / abs(raw_valid$ngdp)) * 100)
accuracy

#========================================================================================================
#Elastic Net Regression Model 
#This is on the raw dataset(without highly correlated variables) as scaling is done the the train fucntion
# Set training control
train_cont_elr <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           verboseIter = TRUE)

x <- model.matrix(ngdp~., raw_train_uni)[,-1:-2]
y <- raw_train_uni$ngdp

# Train the model
elastic_reg <- train(ngdp ~  .,
                     data = raw_train_uni,
                     method = "glmnet",
                     preProcess = c("center", "scale"),
                     tuneLength = 10,
                     trControl = train_cont_elr)

# Check the statistics
summary(elastic_reg)

# View the best tune
elastic_reg$bestTune

#View the co-efficents
coef(elastic_reg$finalModel, elastic_reg$bestTune$lambda)

#predict using test data
pred_test_elr <- predict(elastic_reg, raw_valid_uni)
pred_test_elr

#calculate the error
error <- raw_valid_uni$ngdp - pred_test_elr
error

# Calcuate the RMSE and MAE
rmse(error)
mae(error)

#calculate Accuracy
accuracy_each <- 100 - ((abs(raw_valid_uni$ngdp - pred_test_elr) / abs(raw_valid$ngdp)) * 100)
accuracy_each

#calculate overall Accuracy
accuracy <- 100 - mean((abs(raw_valid_uni$ngdp - pred_test_elr) / abs(raw_valid$ngdp)) * 100)
accuracy

#========================================================================================================