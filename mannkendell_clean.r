#Read in the correrct .csv file
chla<-read.csv('C:/Users/nmillette/Documents/2017 (AOML)/AOML postdoc/Work/Biscayne Bay data/biscayne.csv',header=T)

#Need to download EnvStats function for this specific version of the
#Seasonal Mann-Kendall test
library(EnvStats)

#There is only one line of code to run the actual test, but you need to make
#sure that your data in the excel file is the right format. The year and month
#need to be their own, seperate, column. This test allows for gaps in data, so I
#always made sure that even months missing data were included. I have never run
#the test with every month for every year not included. 
SMK<-kendallSeasonalTrendTest(s5~month+year, data=chla)

#Includes tau, slope, intercept output. Tau is similar to R2.
SMK$estimate

#Upper and Lower confidence intervals
SMK$interval$limits

#Actually Chi-square and z value. The z value is for whether the overall trend
#is signifiant
SMK$p.value

#List of all the possible outputs you can look up for the seasonal mann-kendall
#test
summary(SMK)

#List of how many datapoints were used to look at the trend for each month
SMK$sample.size



