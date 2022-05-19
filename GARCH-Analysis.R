library(rugarch)
library(xts)#for timeseries object coercion
library(quantmod)
library(ggplot2)
library(broom)
library(tidyverse)
library(dplyr)
data = read.csv("C:/Users/Blaisonyl/Downloads/datalabcovid.csv")
#data2 = read.csv("C:/Users/Blaisonyl/Downloads/ChiaraCOVID.csv")

#Turn data into a time series object
#Create a time series object

data_cleaned <-data[2:828, ]
data_cleaned$Date <- as.Date(data_cleaned$Date, "%m/%d/%Y") # Note the Y instead of y. Our interest is a 4digit year char.
#data_cleaned["date.ts"] <- as.Date(data_cleaned$Date, "%m/%d/%Y")



data_ts <- xts(data_cleaned$Total_new_cases, data_cleaned$Date)
class(data_ts) #Verify timeseries object
plot(data_ts)#roughly check the nature of our dataset
colnames(data_ts) <- c('cases') #help with tidying the dataset
data_ts <- tidy(data_ts) #clean the house for plotting

#Plot the data
ggplot(data_ts, aes(x = index, y = value, color = series))+ 
  geom_line() + 
  theme_bw() +
  labs(title = "Daily cases of COVID from January 2020 to April 2022", x = "Date", y = "Cases")


##Due to the scale of the graph, we might be tempted to think its a constant
##Trend till end of 2021, for this reason, i thought to look closely
##at 2020 - 2021 data to appreciate the underlying trends


##2020 data###
data2020 <- data_cleaned[1:348, ]
data2020_ts <- xts(data2020 $Total_new_cases, data2020 $Date)
colnames(data2020_ts) <- c('cases')
data2020_ts <- tidy(data2020_ts)
ggplot(data2020_ts, aes(x = index, y = value, color = series))+ 
  geom_line() + 
  theme_bw() +
  labs(title = "Daily cases of COVID from January 2020 to December 2020", x = "Date", y = "Cases")

##2021-2022 data###
data2021_2022 <- data_cleaned[349:713, ]
data2021_2022_ts <- xts(data2021_2022 $Total_new_cases, data2021_2022$Date)
colnames(data2021_2022_ts) <- c('cases')
data2021_2022_ts <- tidy(data2021_2022_ts)
ggplot(data2021_2022_ts, aes(x = index, y = value, color = series))+ 
  geom_line() + 
  theme_bw() +
  labs(title = "Daily cases of COVID from January 2021 to December 2021", x = "Date", y = "Cases")


##2020-2022 data###
data2020_2022 <- data_cleaned[1:713, ]
data2020_2022_ts <- xts(data2020_2022 $Total_new_cases, data2020_2022$Date)
colnames(data2020_2022_ts) <- c('cases')
data2020_2022_ts <- tidy(data2020_2022_ts)
ggplot(data2020_2022_ts, aes(x = index, y = value, color = series))+ 
  geom_line() + 
  theme_bw() +
  labs(title = "Daily cases of COVID from January 2020 to January 2022", x = "Date", y = "Cases")

##As we can see there are underlying differences in trends 



#GARCH model
#Note, We will do a couple of GARCH models and chose which to use based on the lowest AKAIKE value
#In the next lines i attemt to change the armaorder, variance model and the distribution.model arguments
#to obtain the model with the best fit 

#GARCH model for current day and error term of current day
#AIM to Chose the GARCH model with lowest AIC number AKA AKAIKE


Garch_1 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                             mean.model = list(armaOrder = c(0, 0)), 
                             distribution.model = "std")

garch_result<- ugarchfit(spec = Garch_1, data = data_ts$value, solver = "hybrid")
garch_result

#The model doesnt converge, so we try other parameters

Garch_01 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                        distribution.model = "std", fixed.pars = list(omega = 0))

garch_result<- ugarchfit(spec = Garch_01, data = data_ts$value, solver = "hybrid")
garch_result #Big AKAIKE

#GARCH model for previous day and error term of previous day
Garch_2 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                      mean.model = list(armaOrder = c(1, 1)), 
                      distribution.model = "std")

garch_result <- ugarchfit(spec = Garch_2, data = data_ts$value, solver = "hybrid")
garch_result

#with more parameters
Garch_02 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                        mean.model = list(armaOrder = c(1, 1), include.mean = TRUE), 
                        distribution.model = "std", fixed.pars = list(omega = 0))

garch_result<- ugarchfit(spec = Garch_02, data = data_ts$value, solver = "hybrid")
garch_result  ##13.401 best

#This i think is the best garch model. and the best till the end of the script. 





#GARCH model for previous 14 days and error term of previous 14 days
#Chose 14 days because of covid incubation period
Garch_3 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                      mean.model = list(armaOrder = c(14, 14)), 
                      distribution.model = "std")

garch_result       <- ugarchfit(spec = Garch_3, data = data_ts$value, solver = "hybrid")
garch_result
#error
#with more parameters
Garch_03 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                        mean.model = list(armaOrder = c(14, 14), include.mean = TRUE), 
                        distribution.model = "std", fixed.pars = list(omega = 0))

garch_result<- ugarchfit(spec = Garch_03, data = data_ts$value, solver = "hybrid")
garch_result #error

#GARCH model for previous 30 days and error term of previous 30 days

Garch_4 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                      mean.model = list(armaOrder = c(30, 30)), 
                      distribution.model = "std")

garch_result       <- ugarchfit(spec = Garch_4, data = data_ts$value, solver = "hybrid")
garch_result
#error
#morre parameters
Garch_04 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                        mean.model = list(armaOrder = c(30, 30), include.mean = TRUE), 
                        distribution.model = "std", fixed.pars = list(omega = 0))

garch_result<- ugarchfit(spec = Garch_04, data = data_ts$value, solver = "hybrid")
garch_result ## too long

#WE repeat the above GARCH model on the gaussian distribution

Garch_11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                      mean.model = list(armaOrder = c(0, 0)), 
                      distribution.model = "norm")

garch_result<- ugarchfit(spec = Garch_11, data = data_ts$value, solver = "hybrid")
garch_result

#more paramters
Garch_011 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                        distribution.model = "norm", fixed.pars = list(omega = 0))

garch_result<- ugarchfit(spec = Garch_011, data = data_ts$value, solver = "hybrid")
garch_result #Akaike too hgh

#The model doesnt converge, so we try other parameters

#GARCH model for previous day and error term of previous day
Garch_21 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                      mean.model = list(armaOrder = c(1, 1)), 
                      distribution.model = "norm")

garch_result <- ugarchfit(spec = Garch_21, data = data_ts$value, solver = "hybrid")
garch_result

#more paramters

Garch_021 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                        mean.model = list(armaOrder = c(1, 1), include.mean = TRUE), 
                        distribution.model = "norm", fixed.pars = list(omega = 0))

garch_result<- ugarchfit(spec = Garch_021, data = data_ts$value, solver = "hybrid")
garch_result #high AKAIKE


#GARCH model for previous 14 days and error term of previous 14 days
#Chose 14 days because of covid incubation period
Garch_31 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                      mean.model = list(armaOrder = c(14, 14)), 
                      distribution.model = "norm")

garch_result       <- ugarchfit(spec = Garch_31, data = data_ts$value, solver = "hybrid")
garch_result

Garch_031 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                        mean.model = list(armaOrder = c(14, 14), include.mean = TRUE), 
                        distribution.model = "norm", fixed.pars = list(omega = 0))

garch_result<- ugarchfit(spec = Garch_031, data = data_ts$value, solver = "hybrid")
garch_result #error

#GARCH model for previous 30 days and error term of previous 30 days

Garch_41 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                      mean.model = list(armaOrder = c(30, 30) ), 
                      distribution.model = "norm")

garch_result       <- ugarchfit(spec = Garch_41, data = data_ts$value, solver = "hybrid")
garch_result


#lets use a normal distribution on iGARCH and see


Garch_10 <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1, 1)), 
                       mean.model = list(armaOrder = c(1, 1), include.mean = TRUE), 
                       distribution.model = "norm", fixed.pars = list(omega = 0))

garch_result<- ugarchfit(spec = Garch_10, data = data_ts$value, solver = "hybrid")
garch_result
#the iGARCH model worked 
#SO far i will recommend using this as it the only that has performed

#previous day
Garch_101 <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1, 1)), 
                       mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                       distribution.model = "norm", fixed.pars = list(omega = 0))

garch_result<- ugarchfit(spec = Garch_101, data = data_ts$value, solver = "hybrid")
garch_result


#14 days
Garch_102 <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1, 1)), 
                        mean.model = list(armaOrder = c(14, 14), include.mean = TRUE), 
                        distribution.model = "norm", fixed.pars = list(omega = 0))

garch_result<- ugarchfit(spec = Garch_102, data = data_ts$value, solver = "hybrid")
garch_result
#error

#30 days
Garch_103 <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1, 1)), 
                        mean.model = list(armaOrder = c(30, 30), include.mean = TRUE), 
                        distribution.model = "norm", fixed.pars = list(omega = 0))

garch_result<- ugarchfit(spec = Garch_103, data = data_ts$value, solver = "hybrid")
garch_result #didnt work

##IGNORE####

#TEst-trial and error
Garch_104 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                        mean.model = list(armaOrder = c(1, 1), include.mean = TRUE), 
                        distribution.model = "norm", fixed.pars = list(omega = 0))

garch_result<- ugarchfit(spec = Garch_104, data = data_ts$value, solver = "hybrid")
garch_result
