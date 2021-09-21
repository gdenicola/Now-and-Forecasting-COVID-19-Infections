library(tidyverse)
library(ggpubr)
library(RColorBrewer)

##create doa vector

doa <- (as.POSIXlt("2020-12-09",tz="GMT"))
#a week in seconds
interval <- 7*24*60*60
doa_vector <- doa
nmodels <- 32

#construct vector of DOAs
for (i in (1:(nmodels - 1))) {
  diff = i*interval
  current_doa <- doa - diff
  print(current_doa)
  doa_vector <- c(doa_vector, as.character(current_doa))
}

doa_vector = rev(doa_vector)
doa_vector = as.character(doa_vector)
doa_vector = strtrim(doa_vector,10)
doa_vector <- as.POSIXct(doa_vector,tz = "GMT")


#Plotting the MSE and MRE


#WARNING: the paths need to be changed to your own!
#the data is available in the Sync and Share folder provided


#loading the dataframes
forecast_errors_full <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/forecast_errors_full.Rds")
forecast_errors_no_randoms <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/forecast_errors_no_randoms.Rds")
forecast_errors_no_randoms_no_AR <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/forecast_errors_no_randoms_no_AR.Rds")
forecast_errors_noAR <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/forecast_errors_noAR.Rds")
forecast_errors_noAR_d <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/forecast_errors_noAR_d.Rds")
forecast_errors_noAR_t <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/forecast_errors_noAR_t.Rds")
forecast_errors_noshortterm <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/forecast_errors_noshortterm.Rds")
forecast_errors_noshortterm_noAR <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/forecast_errors_noshortterm_noAR.Rds")

forecast_errors_21 <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/forecast_errors_21.Rds")
forecast_errors_28 <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/forecast_errors_28.Rds")
forecast_errors_35 <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/forecast_errors_35.Rds")
forecast_errors_42 <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/forecast_errors_42.Rds")
forecast_errors_nogender <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/forecast_errors_nogender.Rds")


forenowcast_errors_full <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/forenowcast_errors_full.Rds")
forenowcast_errors_no_randoms <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/forenowcast_errors_no_randoms.Rds")
forenowcast_errors_no_randoms_no_AR <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/forenowcast_errors_no_randoms_no_AR.Rds")
forenowcast_errors_noAR <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/forenowcast_errors_noAR.Rds")
forenowcast_errors_noAR_d <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/forenowcast_errors_noAR_d.Rds")
forenowcast_errors_noAR_t <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/forenowcast_errors_noAR_t.Rds")
forenowcast_errors_noshortterm <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/forenowcast_errors_noshortterm.Rds")
forenowcast_errors_noshortterm_noAR <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/forenowcast_errors_noshortterm_noAR.Rds")

forenowcast_errors_21 <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/forenowcast_errors_21.Rds")
forenowcast_errors_28 <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/forenowcast_errors_28.Rds")
forenowcast_errors_35 <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/forenowcast_errors_35.Rds")
forenowcast_errors_42 <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/forenowcast_errors_42.Rds")
forenowcast_errors_nogender <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/forenowcast_errors_nogender.Rds")


nowcast_errors_full <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/nowcast_errors_full.Rds")
nowcast_errors_no_randoms <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/nowcast_errors_no_randoms.Rds")
nowcast_errors_no_randoms_no_AR <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/nowcast_errors_no_randoms_no_AR.Rds")
nowcast_errors_noAR <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/nowcast_errors_noAR.Rds")
nowcast_errors_noAR_d <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/nowcast_errors_noAR_d.Rds")
nowcast_errors_noAR_t <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/nowcast_errors_noAR_t.Rds")
nowcast_errors_noshortterm <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/nowcast_errors_noshortterm.Rds")
nowcast_errors_noshortterm_noAR <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/nowcast_errors_noshortterm_noAR.Rds")

nowcast_errors_21 <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/nowcast_errors_21.Rds")
nowcast_errors_28 <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/nowcast_errors_28.Rds")
nowcast_errors_35 <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/nowcast_errors_35.Rds")
nowcast_errors_42 <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/nowcast_errors_42.Rds")
nowcast_errors_nogender <- readRDS("C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/data/nowcast_errors_nogender.Rds")



mre_forecast_21 <- NULL
for (i in 1:32) {
  mre_forecast_21 <- c(mre_forecast_21,mean(forecast_errors_21[[i]]$error.per.100k))
}


mre_forecast_28 <- NULL
for (i in 1:32) {
  mre_forecast_28 <- c(mre_forecast_28,mean(forecast_errors_28[[i]]$error.per.100k))
}

mre_forecast_35 <- NULL
for (i in 1:32) {
  mre_forecast_35 <- c(mre_forecast_35,mean(forecast_errors_35[[i]]$error.per.100k))
}

mre_forecast_nogender <- NULL
for (i in 1:32) {
  mre_forecast_nogender <- c(mre_forecast_nogender,mean(forecast_errors_nogender[[i]]$error.per.100k))
}

mre_forecast_full <- NULL
for (i in 1:32) {
  mre_forecast_full <- c(mre_forecast_full,mean(forecast_errors_full[[i]]$error.per.100k))
}

mre_forecast_norandoms <- NULL
for (i in 1:32) {
  mre_forecast_norandoms <- c(mre_forecast_norandoms,mean(forecast_errors_no_randoms[[i]]$error.per.100k))
}

mre_forecast_norandoms_noAR <- NULL
for (i in 1:32) {
  mre_forecast_norandoms_noAR <- c(mre_forecast_norandoms_noAR,mean(forecast_errors_no_randoms_no_AR[[i]]$error.per.100k))
}

mre_forecast_noAR <- NULL
for (i in 1:32) {
  mre_forecast_noAR <- c(mre_forecast_noAR,mean(forecast_errors_noAR[[i]]$error.per.100k))
}

mre_forecast_noAR_t <- NULL
for (i in 1:32) {
  mre_forecast_noAR_t <- c(mre_forecast_noAR_t,mean(forecast_errors_noAR_t[[i]]$error.per.100k))
}

mre_forecast_noAR_d <- NULL
for (i in 1:32) {
  mre_forecast_noAR_d <- c(mre_forecast_noAR_d,mean(forecast_errors_noAR_d[[i]]$error.per.100k))
}

mre_forecast_noshortterm <- NULL
for (i in 1:32) {
  mre_forecast_noshortterm <- c(mre_forecast_noshortterm,mean(forecast_errors_noshortterm[[i]]$error.per.100k))
}

mre_forecast_noshortterm_noAR <- NULL
for (i in 1:32) {
  mre_forecast_noshortterm_noAR <- c(mre_forecast_noshortterm_noAR,mean(forecast_errors_noshortterm_noAR[[i]]$error.per.100k))
}

mre_forecast_df <- cbind.data.frame(Week = 1:32, doa = as.Date(doa_vector),mre_forecast_full,mre_forecast_noAR,mre_forecast_noAR_d,
                                   mre_forecast_noAR_t,mre_forecast_norandoms,mre_forecast_norandoms_noAR,
                                   mre_forecast_noshortterm,mre_forecast_noshortterm_noAR)

#testing for larger training windows
mre_forecast_df_window <- cbind.data.frame(Week = 1:32, doa = as.Date(doa_vector),mre_forecast_21,mre_forecast_28,mre_forecast_35)

#testing without gender
mre_forecast_df_gender <- cbind.data.frame(Week = 1:32, doa = as.Date(doa_vector),mre_forecast_noAR_t,mre_forecast_nogender)

#put in long format
mre_forecast_df_long <- pivot_longer(mre_forecast_df,
                                    cols = 3:10)
names(mre_forecast_df_long) <- c("Week","Date", "Model Variant","MRE")

#for window
mre_forecast_df_long_window <- pivot_longer(mre_forecast_df_window,
                                     cols = 3:5)
names(mre_forecast_df_long_window) <- c("Week","Date", "Number of days in training set","MRE")

#for gender
mre_forecast_df_long_gender <- pivot_longer(mre_forecast_df_gender,
                                            cols = 3:4)
names(mre_forecast_df_long_gender) <- c("Week","Date", "Model Variant","MRE")

#turn into absolute balue
mre_forecast_df_long$`Absolute MRE`=abs(mre_forecast_df_long$MRE)

mre_forecast_df_long$`Model Variant`=as.factor(mre_forecast_df_long$`Model Variant`)
levels(mre_forecast_df_long$`Model Variant`) <- c("Full","No AR", "No AR_d","No AR_t (chosen)   ", "No REs", "No REs and no AR","No short term RE","No short term RE, no AR")
mre_forecast_df_long$`Model Variant`<- factor(mre_forecast_df_long$`Model Variant`,levels = c("Full", "No REs", "No REs and no AR","No short term RE","No short term RE, no AR","No AR", "No AR_d","No AR_t (chosen)   "))

#for window
mre_forecast_df_long_window$`Absolute MRE`=abs(mre_forecast_df_long_window$MRE)

mre_forecast_df_long_window$`Number of days in training set`=as.factor(mre_forecast_df_long_window$`Number of days in training set`)
levels(mre_forecast_df_long_window$`Number of days in training set`) <- c("21","28", "35")
mre_forecast_df_long_window$`Number of days in training set`<- factor(mre_forecast_df_long_window$`Number of days in training set`,levels = c("21","28", "35"))

#for gender
mre_forecast_df_long_gender$`Absolute MRE`=abs(mre_forecast_df_long_gender$MRE)

mre_forecast_df_long_gender$`Model Variant`=as.factor(mre_forecast_df_long_gender$`Model Variant`)
levels(mre_forecast_df_long_gender$`Model Variant`) <- c("base","no gender")
mre_forecast_df_long_gender$`Model Variant`<- factor(mre_forecast_df_long_gender$`Model Variant`,levels = c("base","no gender"))
#NOWCAST

mre_nowcast_21 <- NULL
for (i in 1:32) {
  mre_nowcast_21 <- c(mre_nowcast_21,mean(nowcast_errors_21[[i]]$error.per.100k))
}


mre_nowcast_28 <- NULL
for (i in 1:32) {
  mre_nowcast_28 <- c(mre_nowcast_28,mean(nowcast_errors_28[[i]]$error.per.100k))
}

mre_nowcast_35 <- NULL
for (i in 1:32) {
  mre_nowcast_35 <- c(mre_nowcast_35,mean(nowcast_errors_35[[i]]$error.per.100k))
}

mre_nowcast_nogender <- NULL
for (i in 1:32) {
  mre_nowcast_nogender <- c(mre_nowcast_nogender,mean(nowcast_errors_nogender[[i]]$error.per.100k))
}


mre_nowcast_full <- NULL
for (i in 1:32) {
  mre_nowcast_full <- c(mre_nowcast_full,mean(nowcast_errors_full[[i]]$error.per.100k))
}

mre_nowcast_norandoms <- NULL
for (i in 1:32) {
  mre_nowcast_norandoms <- c(mre_nowcast_norandoms,mean(nowcast_errors_no_randoms[[i]]$error.per.100k))
}

mre_nowcast_norandoms_noAR <- NULL
for (i in 1:32) {
  mre_nowcast_norandoms_noAR <- c(mre_nowcast_norandoms_noAR,mean(nowcast_errors_no_randoms_no_AR[[i]]$error.per.100k))
}

mre_nowcast_noAR <- NULL
for (i in 1:32) {
  mre_nowcast_noAR <- c(mre_nowcast_noAR,mean(nowcast_errors_noAR[[i]]$error.per.100k))
}

mre_nowcast_noAR_t <- NULL
for (i in 1:32) {
  mre_nowcast_noAR_t <- c(mre_nowcast_noAR_t,mean(nowcast_errors_noAR_t[[i]]$error.per.100k))
}

mre_nowcast_noAR_d <- NULL
for (i in 1:32) {
  mre_nowcast_noAR_d <- c(mre_nowcast_noAR_d,mean(nowcast_errors_noAR_d[[i]]$error.per.100k))
}

mre_nowcast_noshortterm <- NULL
for (i in 1:32) {
  mre_nowcast_noshortterm <- c(mre_nowcast_noshortterm,mean(nowcast_errors_noshortterm[[i]]$error.per.100k))
}

mre_nowcast_noshortterm_noAR <- NULL
for (i in 1:32) {
  mre_nowcast_noshortterm_noAR <- c(mre_nowcast_noshortterm_noAR,mean(nowcast_errors_noshortterm_noAR[[i]]$error.per.100k))
}

mre_nowcast_df <- cbind.data.frame(Week = 1:32, doa = as.Date(doa_vector),mre_nowcast_full,mre_nowcast_noAR,mre_nowcast_noAR_d,
                                       mre_nowcast_noAR_t,mre_nowcast_norandoms,mre_nowcast_norandoms_noAR,
                                       mre_nowcast_noshortterm,mre_nowcast_noshortterm_noAR)

#testing for larger training windows
mre_nowcast_df_window <- cbind.data.frame(Week = 1:32, doa = as.Date(doa_vector),mre_nowcast_21,mre_nowcast_28,mre_nowcast_35)

#testing without gender
mre_nowcast_df_gender <- cbind.data.frame(Week = 1:32, doa = as.Date(doa_vector),mre_nowcast_noAR_t,mre_nowcast_nogender)

#put in long format
mre_nowcast_df_long <- pivot_longer(mre_nowcast_df,
                                        cols = 3:10)
names(mre_nowcast_df_long) <- c("Week","Date", "Model Variant","MRE")

#for window
mre_nowcast_df_long_window <- pivot_longer(mre_nowcast_df_window,
                                            cols = 3:5)
names(mre_nowcast_df_long_window) <- c("Week","Date", "Number of days in training set","MRE")

#for gender
mre_nowcast_df_long_gender <- pivot_longer(mre_nowcast_df_gender,
                                            cols = 3:4)
names(mre_nowcast_df_long_gender) <- c("Week","Date", "Model Variant","MRE")



#turn into absolute balue
mre_nowcast_df_long$`Absolute MRE`=abs(mre_nowcast_df_long$MRE)

mre_nowcast_df_long$`Model Variant`=as.factor(mre_nowcast_df_long$`Model Variant`)
levels(mre_nowcast_df_long$`Model Variant`) <- c("Full","No AR", "No AR_d","No AR_t (chosen)   ", "No REs", "No REs and no AR","No short term RE","No short term RE, no AR")
mre_nowcast_df_long$`Model Variant`<- factor(mre_nowcast_df_long$`Model Variant`,levels = c("Full", "No REs", "No REs and no AR","No short term RE","No short term RE, no AR","No AR", "No AR_d","No AR_t (chosen)   "))

#for window
mre_nowcast_df_long_window$`Absolute MRE`=abs(mre_nowcast_df_long_window$MRE)

mre_nowcast_df_long_window$`Number of days in training set`=as.factor(mre_nowcast_df_long_window$`Number of days in training set`)
levels(mre_nowcast_df_long_window$`Number of days in training set`) <- c("21","28", "35")
mre_nowcast_df_long_window$`Number of days in training set`<- factor(mre_nowcast_df_long_window$`Number of days in training set`,levels = c("21","28", "35"))

#for gender
mre_nowcast_df_long_gender$`Absolute MRE`=abs(mre_nowcast_df_long_gender$MRE)

mre_nowcast_df_long_gender$`Model Variant`=as.factor(mre_nowcast_df_long_gender$`Model Variant`)
levels(mre_nowcast_df_long_gender$`Model Variant`) <- c("base","no gender")
mre_nowcast_df_long_gender$`Model Variant`<- factor(mre_nowcast_df_long_gender$`Model Variant`,levels = c("base","no gender"))







#forenowcast

mre_forenowcast_21 <- NULL
for (i in 1:32) {
  mre_forenowcast_21 <- c(mre_forenowcast_21,mean(forenowcast_errors_21[[i]]$error.per.100k))
}


mre_forenowcast_28 <- NULL
for (i in 1:32) {
  mre_forenowcast_28 <- c(mre_forenowcast_28,mean(forenowcast_errors_28[[i]]$error.per.100k))
}

mre_forenowcast_35 <- NULL
for (i in 1:32) {
  mre_forenowcast_35 <- c(mre_forenowcast_35,mean(forenowcast_errors_35[[i]]$error.per.100k))
}

mre_forenowcast_nogender <- NULL
for (i in 1:32) {
  mre_forenowcast_nogender <- c(mre_forenowcast_nogender,mean(forenowcast_errors_nogender[[i]]$error.per.100k))
}

mre_forenowcast_full <- NULL
for (i in 1:32) {
  mre_forenowcast_full <- c(mre_forenowcast_full,mean(forenowcast_errors_full[[i]]$error.per.100k))
}

mre_forenowcast_norandoms <- NULL
for (i in 1:32) {
  mre_forenowcast_norandoms <- c(mre_forenowcast_norandoms,mean(forenowcast_errors_no_randoms[[i]]$error.per.100k))
}

mre_forenowcast_norandoms_noAR <- NULL
for (i in 1:32) {
  mre_forenowcast_norandoms_noAR <- c(mre_forenowcast_norandoms_noAR,mean(forenowcast_errors_no_randoms_no_AR[[i]]$error.per.100k))
}

mre_forenowcast_noAR <- NULL
for (i in 1:32) {
  mre_forenowcast_noAR <- c(mre_forenowcast_noAR,mean(forenowcast_errors_noAR[[i]]$error.per.100k))
}

mre_forenowcast_noAR_t <- NULL
for (i in 1:32) {
  mre_forenowcast_noAR_t <- c(mre_forenowcast_noAR_t,mean(forenowcast_errors_noAR_t[[i]]$error.per.100k))
}

mre_forenowcast_noAR_d <- NULL
for (i in 1:32) {
  mre_forenowcast_noAR_d <- c(mre_forenowcast_noAR_d,mean(forenowcast_errors_noAR_d[[i]]$error.per.100k))
}

mre_forenowcast_noshortterm <- NULL
for (i in 1:32) {
  mre_forenowcast_noshortterm <- c(mre_forenowcast_noshortterm,mean(forenowcast_errors_noshortterm[[i]]$error.per.100k))
}

mre_forenowcast_noshortterm_noAR <- NULL
for (i in 1:32) {
  mre_forenowcast_noshortterm_noAR <- c(mre_forenowcast_noshortterm_noAR,mean(forenowcast_errors_noshortterm_noAR[[i]]$error.per.100k))
}

mre_forenowcast_df <- cbind.data.frame(Week = 1:32, doa = as.Date(doa_vector),mre_forenowcast_full,mre_forenowcast_noAR,mre_forenowcast_noAR_d,
                                    mre_forenowcast_noAR_t,mre_forenowcast_norandoms,mre_forenowcast_norandoms_noAR,
                                    mre_forenowcast_noshortterm,mre_forenowcast_noshortterm_noAR)

#testing for larger training windows
mre_forenowcast_df_window <- cbind.data.frame(Week = 1:32, doa = as.Date(doa_vector),mre_forenowcast_21,mre_forenowcast_28,mre_forenowcast_35)

#testing without gender
mre_forenowcast_df_gender <- cbind.data.frame(Week = 1:32, doa = as.Date(doa_vector),mre_forenowcast_noAR_t,mre_forenowcast_nogender)

#put in long format
mre_forenowcast_df_long <- pivot_longer(mre_forenowcast_df,
                                     cols = 3:10)
names(mre_forenowcast_df_long) <- c("Week","Date", "Model Variant","MRE")

#for window
mre_forenowcast_df_long_window <- pivot_longer(mre_forenowcast_df_window,
                                            cols = 3:5)
names(mre_forenowcast_df_long_window) <- c("Week","Date", "Number of days in training set","MRE")

#for gender
mre_forenowcast_df_long_gender <- pivot_longer(mre_forenowcast_df_gender,
                                            cols = 3:4)
names(mre_forenowcast_df_long_gender) <- c("Week","Date", "Model Variant","MRE")

#turn into absolute balue
mre_forenowcast_df_long$`Absolute MRE`=abs(mre_forenowcast_df_long$MRE)

mre_forenowcast_df_long$`Model Variant`=as.factor(mre_forenowcast_df_long$`Model Variant`)
levels(mre_forenowcast_df_long$`Model Variant`) <- c("Full","No AR", "No AR_d","No AR_t (chosen)   ", "No REs", "No REs and no AR","No short term RE","No short term RE, no AR")
mre_forenowcast_df_long$`Model Variant`<- factor(mre_forenowcast_df_long$`Model Variant`,levels = c("Full", "No REs", "No REs and no AR","No short term RE","No short term RE, no AR","No AR", "No AR_d","No AR_t (chosen)   "))

#for window
mre_forenowcast_df_long_window$`Absolute MRE`=abs(mre_forenowcast_df_long_window$MRE)

mre_forenowcast_df_long_window$`Number of days in training set`=as.factor(mre_forenowcast_df_long_window$`Number of days in training set`)
levels(mre_forenowcast_df_long_window$`Number of days in training set`) <- c("21","28", "35")
mre_forenowcast_df_long_window$`Number of days in training set`<- factor(mre_forenowcast_df_long_window$`Number of days in training set`,levels = c("21","28", "35"))

#for gender
mre_forenowcast_df_long_gender$`Absolute MRE`=abs(mre_forenowcast_df_long_gender$MRE)

mre_forenowcast_df_long_gender$`Model Variant`=as.factor(mre_forenowcast_df_long_gender$`Model Variant`)
levels(mre_forenowcast_df_long_gender$`Model Variant`) <- c("base","no gender")
mre_forenowcast_df_long_gender$`Model Variant`<- factor(mre_forenowcast_df_long_gender$`Model Variant`,levels = c("base","no gender"))


#####plot removing the absolute value
#plot lines nowcast
pdf(file = "C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/Comparisons/Non-Absolute MRE Nowcast.pdf",width = 14, height = 11)
ggplot(data = mre_nowcast_df_long, aes(Date,`MRE`,col=`Model Variant`,size=`Model Variant`)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey", size=1.5) +
  geom_line() +
  scale_color_brewer(palette = "Greens") +
  scale_size_manual(values = c(0.8,0.8,0.8,0.8,0.8,0.8,0.8,2.5)) +
  theme_pubr(base_size = 40) +
  labs(x="Fitting Date",y="MRPE") +
  #ggtitle("Nowcasts") +
  scale_x_date(date_labels="%b",date_breaks  ="1 month") +
  scale_y_continuous(limits = c(-54,54),breaks = seq(-60,50,20)) +
  theme(plot.title = element_text(size = 60, face = "bold")) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))
  #scale_y_continuous(limits = c(0,54),breaks = seq(0,50,10)) +
  # theme(legend.justification = c(0.99, 0.99), legend.position = c(0.36, 0.99), 
  #       legend.text.align = 0, legend.box.background = element_rect(colour = "grey"),
  #       legend.background = element_blank()) 
  #theme(legend.key.size = unit(0.1, "cm"))
#theme(legend.title = element_blank())
dev.off()


#plot lines forecast
pdf(file = "C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/Comparisons/Non-Absolute MRE Forecast.pdf",width = 14, height = 11)
ggplot(data = mre_forecast_df_long, aes(Date,`MRE`,col=`Model Variant`,size=`Model Variant`)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey", size=1.5) +
  geom_line() +
  #ggtitle("Forecasts") +
  scale_color_brewer(palette = "Reds") +
  scale_size_manual(values = c(0.8,0.8,0.8,0.8,0.8,0.8,0.8,2.5)) +
  theme_pubr(base_size = 40) +
  labs(x="Fitting Date",y="MRPE") +
  scale_y_continuous(limits = c(-54,54),breaks = seq(-60,50,20)) +
  theme(plot.title = element_text(size = 60, face = "bold")) +  
  theme(legend.position = "none") +
scale_x_date(date_labels="%b",date_breaks  ="1 month") 
  # theme(legend.justification = c(0.99, 0.99), legend.position = c(0.36, 0.99), 
  #       legend.text.align = 0, legend.box.background = element_rect(colour = "grey"),
  #       legend.background = element_blank()) 
#theme(legend.title = element_blank()) 
dev.off()



#plot lines forenowcast
pdf(file = "C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/Comparisons/Non-Absolute MRE Forenowcast.pdf",width = 14, height = 11)
ggplot(data = mre_forenowcast_df_long, aes(Date,`MRE`,col=`Model Variant`,size=`Model Variant`)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey", size=1.5) +
  geom_line() +
  scale_color_brewer(palette = "Blues") +
  scale_size_manual(values = c(0.8,0.8,0.8,0.8,0.8,0.8,0.8,2.5)) +
  theme_pubr(base_size = 40) +
  labs(x="Fitting Date",y="MRPE") +
  #ggtitle("Forenowcasts") +
  scale_x_date(date_labels="%b",date_breaks  ="1 month") +
  scale_y_continuous(limits = c(-54,54),breaks = seq(-60,50,20)) +
  # theme(legend.justification = c(0.99, 0.99), legend.position = c(0.36, 0.5),
  #       legend.text.align = 0, legend.box.background = element_rect(colour = "grey"),
  #       legend.background = element_blank())
#theme(legend.key.size = unit(0.1, "cm"))
#theme(legend.title = element_blank())
theme(plot.title = element_text(size = 60, face = "bold")) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 1))
dev.off()



#FORECAST

#plotting for different training window
pdf(file = "C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/Comparisons/Non-Absolute MRE Forecast window.pdf",width = 14, height = 11)
ggplot(data = mre_forecast_df_long_window, aes(Date,`MRE`,col=`Number of days in training set`,size=`Number of days in training set`)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey", size=1.5) +
  geom_line() +
  ggtitle("Forecasts") +
  #scale_color_brewer() +
  scale_size_manual(values = c(2,0.8,0.8,0.8,0.8,0.8,0.8,2)) +
  theme_pubr(base_size = 40) +
  labs(x="Fitting Date",y="MRPE") +
  scale_y_continuous(limits = c(-32,32),breaks = seq(-60,50,10)) +
  scale_x_date(date_labels="%b",date_breaks  ="1 month") 
# theme(legend.justification = c(0.99, 0.99), legend.position = c(0.36, 0.99), 
#       legend.text.align = 0, legend.box.background = element_rect(colour = "grey"),
#       legend.background = element_blank()) 
#theme(legend.title = element_blank()) 

dev.off()


#plotting for nogender: 
#plot lines forecast
pdf(file = "C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/Comparisons/Non-Absolute MRE Forecast nogender.pdf",width = 14, height = 11)
ggplot(data = mre_forecast_df_long_gender, aes(Date,`MRE`,col=`Model Variant`,size=`Model Variant`)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey", size=1.5) +
  geom_line() +
  ggtitle("Forecasts") +
  #scale_color_brewer(palette = "Reds") +
  scale_size_manual(values = c(2,0.8,0.8,0.8,0.8,0.8,0.8,2)) +
  theme_pubr(base_size = 40) +
  labs(x="Fitting Date",y="MRPE") +
  scale_y_continuous(limits = c(-32,32),breaks = seq(-60,50,10)) +
  scale_x_date(date_labels="%b",date_breaks  ="1 month") 
# theme(legend.justification = c(0.99, 0.99), legend.position = c(0.36, 0.99), 
#       legend.text.align = 0, legend.box.background = element_rect(colour = "grey"),
#       legend.background = element_blank()) 
#theme(legend.title = element_blank()) 

dev.off()


#FORENOWCAST

#plotting for different training window
pdf(file = "C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/Comparisons/Non-Absolute MRE Forenowcast window.pdf",width = 14, height = 11)
ggplot(data = mre_forenowcast_df_long_window, aes(Date,`MRE`,col=`Number of days in training set`,size=`Number of days in training set`)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey", size=1.5) +
  geom_line() +
  ggtitle("Forenowcasts") +
  #scale_color_brewer() +
  scale_size_manual(values = c(2,0.8,0.8,0.8,0.8,0.8,0.8,2)) +
  theme_pubr(base_size = 40) +
  labs(x="Fitting Date",y="MRPE") +
  scale_y_continuous(limits = c(-32,32),breaks = seq(-60,50,10)) +
  scale_x_date(date_labels="%b",date_breaks  ="1 month") 
# theme(legend.justification = c(0.99, 0.99), legend.position = c(0.36, 0.99), 
#       legend.text.align = 0, legend.box.background = element_rect(colour = "grey"),
#       legend.background = element_blank()) 
#theme(legend.title = element_blank()) 

dev.off()


#plotting for nogender: 
pdf(file = "C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/Comparisons/Non-Absolute MRE Forenowcast nogender.pdf",width = 14, height = 11)
ggplot(data = mre_forenowcast_df_long_gender, aes(Date,`MRE`,col=`Model Variant`,size=`Model Variant`)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey", size=1.5) +
  geom_line() +
  ggtitle("Forenowcasts") +
  #scale_color_brewer(palette = "Reds") +
  scale_size_manual(values = c(2,0.8,0.8,0.8,0.8,0.8,0.8,2)) +
  theme_pubr(base_size = 40) +
  labs(x="Fitting Date",y="MRPE") +
  scale_y_continuous(limits = c(-32,32),breaks = seq(-60,50,10)) +
  scale_x_date(date_labels="%b",date_breaks  ="1 month") 
# theme(legend.justification = c(0.99, 0.99), legend.position = c(0.36, 0.99), 
#       legend.text.align = 0, legend.box.background = element_rect(colour = "grey"),
#       legend.background = element_blank()) 
#theme(legend.title = element_blank()) 

dev.off()



#NOWCAST

#plotting for different trainign window
pdf(file = "C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/Comparisons/Non-Absolute MRE Nowcast window.pdf",width = 14, height = 11)
ggplot(data = mre_nowcast_df_long_window, aes(Date,`MRE`,col=`Number of days in training set`,size=`Number of days in training set`)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey", size=1.5) +geom_line() +
  ggtitle("Nowcasts") +
  #scale_color_brewer() +
  scale_size_manual(values = c(2,0.8,0.8,0.8,0.8,0.8,0.8,2)) +
  theme_pubr(base_size = 40) +
  labs(x="Fitting Date",y="MRPE") +
  scale_y_continuous(limits = c(-32,32),breaks = seq(-60,50,10)) +
  scale_x_date(date_labels="%b",date_breaks  ="1 month") 
# theme(legend.justification = c(0.99, 0.99), legend.position = c(0.36, 0.99), 
#       legend.text.align = 0, legend.box.background = element_rect(colour = "grey"),
#       legend.background = element_blank()) 
#theme(legend.title = element_blank()) 

dev.off()


#plotting for nogender: 
#plot lines forecast
pdf(file = "C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/Comparisons/Non-Absolute MRE Nowcast nogender.pdf",width = 14, height = 11)
ggplot(data = mre_nowcast_df_long_gender, aes(Date,`MRE`,col=`Model Variant`,size=`Model Variant`)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey", size=1.5) +
  geom_line() +
  ggtitle("Nowcasts") +
  #scale_color_brewer(palette = "Reds") +
  scale_size_manual(values = c(2,0.8,0.8,0.8,0.8,0.8,0.8,2)) +
  theme_pubr(base_size = 40) +
  labs(x="Fitting Date",y="MRPE") +
  scale_y_continuous(limits = c(-32,32),breaks = seq(-60,50,10)) +
  scale_x_date(date_labels="%b",date_breaks  ="1 month") 
# theme(legend.justification = c(0.99, 0.99), legend.position = c(0.36, 0.99), 
#       legend.text.align = 0, legend.box.background = element_rect(colour = "grey"),
#       legend.background = element_blank()) 
#theme(legend.title = element_blank()) 

dev.off()

#############################################################
#REPEAT ALL PLOTS USING ABSOLUTE ERRORS INSTEAD OF ERRORS

mre_forecast_21 <- NULL
for (i in 1:32) {
  mre_forecast_21 <- c(mre_forecast_21,mean(abs(forecast_errors_21[[i]]$error.per.100k)))
}

mre_forecast_28 <- NULL
for (i in 1:32) {
  mre_forecast_28 <- c(mre_forecast_28,mean(abs(forecast_errors_28[[i]]$error.per.100k)))
}

mre_forecast_35 <- NULL
for (i in 1:32) {
  mre_forecast_35 <- c(mre_forecast_35,mean(abs(forecast_errors_35[[i]]$error.per.100k)))
}

mre_forecast_nogender <- NULL
for (i in 1:32) {
  mre_forecast_nogender <- c(mre_forecast_nogender,mean(abs(forecast_errors_nogender[[i]]$error.per.100k)))
}





mre_forecast_full <- NULL
for (i in 1:32) {
  mre_forecast_full <- c(mre_forecast_full,mean(abs(forecast_errors_full[[i]]$error.per.100k)))
}

mre_forecast_norandoms <- NULL
for (i in 1:32) {
  mre_forecast_norandoms <- c(mre_forecast_norandoms,mean(abs(forecast_errors_no_randoms[[i]]$error.per.100k)))
}

mre_forecast_norandoms_noAR <- NULL
for (i in 1:32) {
  mre_forecast_norandoms_noAR <- c(mre_forecast_norandoms_noAR,mean(abs(forecast_errors_no_randoms_no_AR[[i]]$error.per.100k)))
}

mre_forecast_noAR <- NULL
for (i in 1:32) {
  mre_forecast_noAR <- c(mre_forecast_noAR,mean(abs(forecast_errors_noAR[[i]]$error.per.100k)))
}

mre_forecast_noAR_t <- NULL
for (i in 1:32) {
  mre_forecast_noAR_t <- c(mre_forecast_noAR_t,mean(abs(forecast_errors_noAR_t[[i]]$error.per.100k)))
}

mre_forecast_noAR_d <- NULL
for (i in 1:32) {
  mre_forecast_noAR_d <- c(mre_forecast_noAR_d,mean(abs(forecast_errors_noAR_d[[i]]$error.per.100k)))
}

mre_forecast_noshortterm <- NULL
for (i in 1:32) {
  mre_forecast_noshortterm <- c(mre_forecast_noshortterm,mean(abs(forecast_errors_noshortterm[[i]]$error.per.100k)))
}

mre_forecast_noshortterm_noAR <- NULL
for (i in 1:32) {
  mre_forecast_noshortterm_noAR <- c(mre_forecast_noshortterm_noAR,mean(abs(forecast_errors_noshortterm_noAR[[i]]$error.per.100k)))
}

mre_forecast_df <- cbind.data.frame(Week = 1:32, doa = as.Date(doa_vector),mre_forecast_full,mre_forecast_noAR,mre_forecast_noAR_d,
                                    mre_forecast_noAR_t,mre_forecast_norandoms,mre_forecast_norandoms_noAR,
                                    mre_forecast_noshortterm,mre_forecast_noshortterm_noAR)

#testing for larger training windows
mre_forecast_df_window <- cbind.data.frame(Week = 1:32, doa = as.Date(doa_vector),mre_forecast_21,mre_forecast_28,mre_forecast_35)

#testing without gender
mre_forecast_df_gender <- cbind.data.frame(Week = 1:32, doa = as.Date(doa_vector),mre_forecast_noAR_t,mre_forecast_nogender)

#put in long format
mre_forecast_df_long <- pivot_longer(mre_forecast_df,
                                     cols = 3:10)
names(mre_forecast_df_long) <- c("Week","Date", "Model Variant","MRE")

#for window
mre_forecast_df_long_window <- pivot_longer(mre_forecast_df_window,
                                            cols = 3:5)
names(mre_forecast_df_long_window) <- c("Week","Date", "Number of days in training set","MRE")

#for gender
mre_forecast_df_long_gender <- pivot_longer(mre_forecast_df_gender,
                                            cols = 3:4)
names(mre_forecast_df_long_gender) <- c("Week","Date", "Model Variant","MRE")

#turn into absolute balue
mre_forecast_df_long$`Absolute MRE`=abs(mre_forecast_df_long$MRE)

mre_forecast_df_long$`Model Variant`=as.factor(mre_forecast_df_long$`Model Variant`)
levels(mre_forecast_df_long$`Model Variant`) <- c("Full","No AR", "No AR_d","No AR_t (chosen)   ", "No REs", "No REs and no AR","No short term RE","No short term RE, no AR")
mre_forecast_df_long$`Model Variant`<- factor(mre_forecast_df_long$`Model Variant`,levels = c("Full", "No REs", "No REs and no AR","No short term RE","No short term RE, no AR","No AR", "No AR_d","No AR_t (chosen)   "))

#for window
mre_forecast_df_long_window$`Absolute MRE`=abs(mre_forecast_df_long_window$MRE)

mre_forecast_df_long_window$`Number of days in training set`=as.factor(mre_forecast_df_long_window$`Number of days in training set`)
levels(mre_forecast_df_long_window$`Number of days in training set`) <- c("21","28", "35")
mre_forecast_df_long_window$`Number of days in training set`<- factor(mre_forecast_df_long_window$`Number of days in training set`,levels = c("21","28", "35"))

#for gender
mre_forecast_df_long_gender$`Absolute MRE`=abs(mre_forecast_df_long_gender$MRE)

mre_forecast_df_long_gender$`Model Variant`=as.factor(mre_forecast_df_long_gender$`Model Variant`)
levels(mre_forecast_df_long_gender$`Model Variant`) <- c("base","no gender")
mre_forecast_df_long_gender$`Model Variant`<- factor(mre_forecast_df_long_gender$`Model Variant`,levels = c("base","no gender"))


#NOWCAST

mre_nowcast_21 <- NULL
for (i in 1:32) {
  mre_nowcast_21 <- c(mre_nowcast_21,mean(abs(nowcast_errors_21[[i]]$error.per.100k)))
}

mre_nowcast_28 <- NULL
for (i in 1:32) {
  mre_nowcast_28 <- c(mre_nowcast_28,mean(abs(nowcast_errors_28[[i]]$error.per.100k)))
}

mre_nowcast_35 <- NULL
for (i in 1:32) {
  mre_nowcast_35 <- c(mre_nowcast_35,mean(abs(nowcast_errors_35[[i]]$error.per.100k)))
}

mre_nowcast_nogender <- NULL
for (i in 1:32) {
  mre_nowcast_nogender <- c(mre_nowcast_nogender,mean(abs(nowcast_errors_nogender[[i]]$error.per.100k)))
}

mre_nowcast_full <- NULL
for (i in 1:32) {
  mre_nowcast_full <- c(mre_nowcast_full,mean(abs(nowcast_errors_full[[i]]$error.per.100k)))
}

mre_nowcast_norandoms <- NULL
for (i in 1:32) {
  mre_nowcast_norandoms <- c(mre_nowcast_norandoms,mean(abs(nowcast_errors_no_randoms[[i]]$error.per.100k)))
}

mre_nowcast_norandoms_noAR <- NULL
for (i in 1:32) {
  mre_nowcast_norandoms_noAR <- c(mre_nowcast_norandoms_noAR,mean(abs(nowcast_errors_no_randoms_no_AR[[i]]$error.per.100k)))
}

mre_nowcast_noAR <- NULL
for (i in 1:32) {
  mre_nowcast_noAR <- c(mre_nowcast_noAR,mean(abs(nowcast_errors_noAR[[i]]$error.per.100k)))
}

mre_nowcast_noAR_t <- NULL
for (i in 1:32) {
  mre_nowcast_noAR_t <- c(mre_nowcast_noAR_t,mean(abs(nowcast_errors_noAR_t[[i]]$error.per.100k)))
}

mre_nowcast_noAR_d <- NULL
for (i in 1:32) {
  mre_nowcast_noAR_d <- c(mre_nowcast_noAR_d,mean(abs(nowcast_errors_noAR_d[[i]]$error.per.100k)))
}

mre_nowcast_noshortterm <- NULL
for (i in 1:32) {
  mre_nowcast_noshortterm <- c(mre_nowcast_noshortterm,mean(abs(nowcast_errors_noshortterm[[i]]$error.per.100k)))
}

mre_nowcast_noshortterm_noAR <- NULL
for (i in 1:32) {
  mre_nowcast_noshortterm_noAR <- c(mre_nowcast_noshortterm_noAR,mean(abs(nowcast_errors_noshortterm_noAR[[i]]$error.per.100k)))
}

mre_nowcast_df <- cbind.data.frame(Week = 1:32, doa = as.Date(doa_vector),mre_nowcast_full,mre_nowcast_noAR,mre_nowcast_noAR_d,
                                   mre_nowcast_noAR_t,mre_nowcast_norandoms,mre_nowcast_norandoms_noAR,
                                   mre_nowcast_noshortterm,mre_nowcast_noshortterm_noAR)

#testing for larger training windows
mre_nowcast_df_window <- cbind.data.frame(Week = 1:32, doa = as.Date(doa_vector),mre_nowcast_21,mre_nowcast_28,mre_nowcast_35)

#testing without gender
mre_nowcast_df_gender <- cbind.data.frame(Week = 1:32, doa = as.Date(doa_vector),mre_nowcast_noAR_t,mre_nowcast_nogender)

#put in long format
mre_nowcast_df_long <- pivot_longer(mre_nowcast_df,
                                    cols = 3:10)
names(mre_nowcast_df_long) <- c("Week","Date", "Model Variant","MRE")

#for window
mre_nowcast_df_long_window <- pivot_longer(mre_nowcast_df_window,
                                           cols = 3:5)
names(mre_nowcast_df_long_window) <- c("Week","Date", "Number of days in training set","MRE")

#for gender
mre_nowcast_df_long_gender <- pivot_longer(mre_nowcast_df_gender,
                                           cols = 3:4)
names(mre_nowcast_df_long_gender) <- c("Week","Date", "Model Variant","MRE")



#turn into absolute balue
mre_nowcast_df_long$`Absolute MRE`=abs(mre_nowcast_df_long$MRE)

mre_nowcast_df_long$`Model Variant`=as.factor(mre_nowcast_df_long$`Model Variant`)
levels(mre_nowcast_df_long$`Model Variant`) <- c("Full","No AR", "No AR_d","No AR_t (chosen)   ", "No REs", "No REs and no AR","No short term RE","No short term RE, no AR")
mre_nowcast_df_long$`Model Variant`<- factor(mre_nowcast_df_long$`Model Variant`,levels = c("Full", "No REs", "No REs and no AR","No short term RE","No short term RE, no AR","No AR", "No AR_d","No AR_t (chosen)   "))

#for window
mre_nowcast_df_long_window$`Absolute MRE`=abs(mre_nowcast_df_long_window$MRE)

mre_nowcast_df_long_window$`Number of days in training set`=as.factor(mre_nowcast_df_long_window$`Number of days in training set`)
levels(mre_nowcast_df_long_window$`Number of days in training set`) <- c("21","28", "35")
mre_nowcast_df_long_window$`Number of days in training set`<- factor(mre_nowcast_df_long_window$`Number of days in training set`,levels = c("21","28", "35"))

#for gender
mre_nowcast_df_long_gender$`Absolute MRE`=abs(mre_nowcast_df_long_gender$MRE)

mre_nowcast_df_long_gender$`Model Variant`=as.factor(mre_nowcast_df_long_gender$`Model Variant`)
levels(mre_nowcast_df_long_gender$`Model Variant`) <- c("base","no gender")
mre_nowcast_df_long_gender$`Model Variant`<- factor(mre_nowcast_df_long_gender$`Model Variant`,levels = c("base","no gender"))






#forenowcast


mre_forenowcast_21 <- NULL
for (i in 1:32) {
  mre_forenowcast_21 <- c(mre_forenowcast_21,mean(abs(forenowcast_errors_21[[i]]$error.per.100k)))
}

mre_forenowcast_28 <- NULL
for (i in 1:32) {
  mre_forenowcast_28 <- c(mre_forenowcast_28,mean(abs(forenowcast_errors_28[[i]]$error.per.100k)))
}





mre_forenowcast_35 <- NULL
for (i in 1:32) {
  mre_forenowcast_35 <- c(mre_forenowcast_35,mean(abs(forenowcast_errors_35[[i]]$error.per.100k)))
}

mre_forenowcast_nogender <- NULL
for (i in 1:32) {
  mre_forenowcast_nogender <- c(mre_forenowcast_nogender,mean(abs(forenowcast_errors_nogender[[i]]$error.per.100k)))
}


mre_forenowcast_full <- NULL
for (i in 1:32) {
  mre_forenowcast_full <- c(mre_forenowcast_full,mean(abs(forenowcast_errors_full[[i]]$error.per.100k)))
}

mre_forenowcast_norandoms <- NULL
for (i in 1:32) {
  mre_forenowcast_norandoms <- c(mre_forenowcast_norandoms,mean(abs(forenowcast_errors_no_randoms[[i]]$error.per.100k)))
}

mre_forenowcast_norandoms_noAR <- NULL
for (i in 1:32) {
  mre_forenowcast_norandoms_noAR <- c(mre_forenowcast_norandoms_noAR,mean(abs(forenowcast_errors_no_randoms_no_AR[[i]]$error.per.100k)))
}

mre_forenowcast_noAR <- NULL
for (i in 1:32) {
  mre_forenowcast_noAR <- c(mre_forenowcast_noAR,mean(abs(forenowcast_errors_noAR[[i]]$error.per.100k)))
}

mre_forenowcast_noAR_t <- NULL
for (i in 1:32) {
  mre_forenowcast_noAR_t <- c(mre_forenowcast_noAR_t,mean(abs(forenowcast_errors_noAR_t[[i]]$error.per.100k)))
}

mre_forenowcast_noAR_d <- NULL
for (i in 1:32) {
  mre_forenowcast_noAR_d <- c(mre_forenowcast_noAR_d,mean(abs(forenowcast_errors_noAR_d[[i]]$error.per.100k)))
}

mre_forenowcast_noshortterm <- NULL
for (i in 1:32) {
  mre_forenowcast_noshortterm <- c(mre_forenowcast_noshortterm,mean(abs(forenowcast_errors_noshortterm[[i]]$error.per.100k)))
}

mre_forenowcast_noshortterm_noAR <- NULL
for (i in 1:32) {
  mre_forenowcast_noshortterm_noAR <- c(mre_forenowcast_noshortterm_noAR,mean(abs(forenowcast_errors_noshortterm_noAR[[i]]$error.per.100k)))
}

mre_forenowcast_df <- cbind.data.frame(Week = 1:32, doa = as.Date(doa_vector),mre_forenowcast_full,mre_forenowcast_noAR,mre_forenowcast_noAR_d,
                                       mre_forenowcast_noAR_t,mre_forenowcast_norandoms,mre_forenowcast_norandoms_noAR,
                                       mre_forenowcast_noshortterm,mre_forenowcast_noshortterm_noAR)

#testing for larger training windows
mre_forenowcast_df_window <- cbind.data.frame(Week = 1:32, doa = as.Date(doa_vector),mre_forenowcast_21,mre_forenowcast_28,mre_forenowcast_35)

#testing without gender
mre_forenowcast_df_gender <- cbind.data.frame(Week = 1:32, doa = as.Date(doa_vector),mre_forenowcast_noAR_t,mre_forenowcast_nogender)

#put in long format
mre_forenowcast_df_long <- pivot_longer(mre_forenowcast_df,
                                        cols = 3:10)
names(mre_forenowcast_df_long) <- c("Week","Date", "Model Variant","MRE")

#for window
mre_forenowcast_df_long_window <- pivot_longer(mre_forenowcast_df_window,
                                               cols = 3:5)
names(mre_forenowcast_df_long_window) <- c("Week","Date", "Number of days in training set","MRE")

#for gender
mre_forenowcast_df_long_gender <- pivot_longer(mre_forenowcast_df_gender,
                                               cols = 3:4)
names(mre_forenowcast_df_long_gender) <- c("Week","Date", "Model Variant","MRE")

#turn into absolute balue
mre_forenowcast_df_long$`Absolute MRE`=abs(mre_forenowcast_df_long$MRE)

mre_forenowcast_df_long$`Model Variant`=as.factor(mre_forenowcast_df_long$`Model Variant`)
levels(mre_forenowcast_df_long$`Model Variant`) <- c("Full","No AR", "No AR_d","No AR_t (chosen)   ", "No REs", "No REs and no AR","No short term RE","No short term RE, no AR")
mre_forenowcast_df_long$`Model Variant`<- factor(mre_forenowcast_df_long$`Model Variant`,levels = c("Full", "No REs", "No REs and no AR","No short term RE","No short term RE, no AR","No AR", "No AR_d","No AR_t (chosen)   "))

#for window
mre_forenowcast_df_long_window$`Absolute MRE`=abs(mre_forenowcast_df_long_window$MRE)

mre_forenowcast_df_long_window$`Number of days in training set`=as.factor(mre_forenowcast_df_long_window$`Number of days in training set`)
levels(mre_forenowcast_df_long_window$`Number of days in training set`) <- c("21","28", "35")
mre_forenowcast_df_long_window$`Number of days in training set`<- factor(mre_forenowcast_df_long_window$`Number of days in training set`,levels = c("21","28", "35"))

#for gender
mre_forenowcast_df_long_gender$`Absolute MRE`=abs(mre_forenowcast_df_long_gender$MRE)

mre_forenowcast_df_long_gender$`Model Variant`=as.factor(mre_forenowcast_df_long_gender$`Model Variant`)
levels(mre_forenowcast_df_long_gender$`Model Variant`) <- c("base","no gender")
mre_forenowcast_df_long_gender$`Model Variant`<- factor(mre_forenowcast_df_long_gender$`Model Variant`,levels = c("base","no gender"))


#plot lines nowcast
pdf(file = "C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/Comparisons/Absolute MRE Nowcast2.pdf",width = 14, height = 11)
ggplot(data = mre_nowcast_df_long, aes(Date,`Absolute MRE`,col=`Model Variant`,size=`Model Variant`)) +
  geom_line() +
  scale_color_brewer(palette = "Greens") +
  ggtitle("Nowcasts") +
  scale_size_manual(values = c(0.8,0.8,0.8,0.8,0.8,0.8,0.8,2.5)) +
  theme_pubr(base_size = 40) +
  labs(x="Fitting Date",y="MARPE") +
  scale_y_continuous(limits = c(0,100),breaks = seq(-60,100,20)) +
  theme(plot.title = element_text(size = 60, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  scale_x_date(date_labels="%b",date_breaks  ="1 month") 
# theme(legend.justification = c(0.99, 0.99), legend.position = c(0.36, 0.99), 
#       legend.text.align = 0, legend.box.background = element_rect(colour = "grey"),
#       legend.background = element_blank()) 
#guides(fill=guide_legend(title="New Legend Title"))
  #labs(col='Variant') 

dev.off()

#plot lines forecast
pdf(file = "C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/Comparisons/Absolute MRE Forecast2.pdf",width = 14, height = 11)
ggplot(data = mre_forecast_df_long, aes(Date,`Absolute MRE`,col=`Model Variant`,size=`Model Variant`)) +
  geom_line() +
  ggtitle("Forecasts") +
  scale_color_brewer(palette = "Reds") +
  scale_size_manual(values = c(0.8,0.8,0.8,0.8,0.8,0.8,0.8,2.5)) +
  theme_pubr(base_size = 40) +
  labs(x="Fitting Date",y="MARPE") +
  scale_y_continuous(limits = c(-0,100),breaks = seq(-60,100,20)) +
  scale_x_date(date_labels="%b",date_breaks  ="1 month") +
  theme(plot.title = element_text(size = 60, face = "bold")) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))
# theme(legend.justification = c(0.99, 0.99), legend.position = c(0.36, 0.99), 
#       legend.text.align = 0, legend.box.background = element_rect(colour = "grey"),
#       legend.background = element_blank()) 
#theme(legend.title = element_blank()) 
dev.off()

#plot lines forenowcast
pdf(file = "C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/Comparisons/Absolute MRE Forenowcast2.pdf",width = 14, height = 11)
ggplot(data = mre_forenowcast_df_long, aes(Date,`Absolute MRE`,col=`Model Variant`,size=`Model Variant`)) +
  geom_line() +
  ggtitle("Forenowcasts") +
  scale_color_brewer(palette = "Blues") +
  scale_size_manual(values = c(0.8,0.8,0.8,0.8,0.8,0.8,0.8,2.5)) +
  theme_pubr(base_size = 40) +
  labs(x="Fitting Date",y="MARPE") +
  scale_y_continuous(limits = c(-0,100),breaks = seq(-60,100,20)) +
  scale_x_date(date_labels="%b",date_breaks  ="1 month")  +
  theme(plot.title = element_text(size = 60, face = "bold")) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))
# theme(legend.justification = c(0.99, 0.99), legend.position = c(0.36, 0.99), 
#       legend.text.align = 0, legend.box.background = element_rect(colour = "grey"),
#       legend.background = element_blank()) 
#theme(legend.title = element_blank()) 
dev.off()



#############################################################
#Repeat all plotting with |yhat - y| / yhat as a measure,
#(optionally) remove small observations before the analysis

#removing all districts for which forecasted/100.000 < 2

w <- 2
# (set w <- 0 to keep all observations)


for (i in 1:32) {
  test <- forecast_errors_full[[i]]
  test <- test[test$forecast.per.100k>w,]
  forecast_errors_full[[i]] <- test
}

for (i in 1:32) {
  test <- forecast_errors_no_randoms[[i]]
  test <- test[test$forecast.per.100k>w,]
  forecast_errors_no_randoms[[i]] <- test
}

for (i in 1:32) {
  test <- forecast_errors_no_randoms_no_AR[[i]]
  test <- test[test$forecast.per.100k>w,]
  forecast_errors_no_randoms_no_AR[[i]] <- test
}

for (i in 1:32) {
  test <- forecast_errors_noAR[[i]]
  test <- test[test$forecast.per.100k>w,]
  forecast_errors_noAR[[i]] <- test
}

for (i in 1:32) {
  test <- forecast_errors_noAR_d[[i]]
  test <- test[test$forecast.per.100k>w,]
  forecast_errors_noAR_d[[i]] <- test
}

for (i in 1:32) {
  test <- forecast_errors_noAR_t[[i]]
  test <- test[test$forecast.per.100k>w,]
  forecast_errors_noAR_t[[i]] <- test
}

for (i in 1:32) {
  test <- forecast_errors_noshortterm[[i]]
  test <- test[test$forecast.per.100k>w,]
  forecast_errors_noshortterm[[i]] <- test
}

for (i in 1:32) {
  test <- forecast_errors_noshortterm_noAR[[i]]
  test <- test[test$forecast.per.100k>w,]
  forecast_errors_noshortterm_noAR[[i]] <- test
}







for (i in 1:32) {
  test <- forenowcast_errors_full[[i]]
  test <- test[test$forenowcast.per.100k>w,]
  forenowcast_errors_full[[i]] <- test
}

for (i in 1:32) {
  test <- forenowcast_errors_no_randoms[[i]]
  test <- test[test$forenowcast.per.100k>w,]
  forenowcast_errors_no_randoms[[i]] <- test
}

for (i in 1:32) {
  test <- forenowcast_errors_no_randoms_no_AR[[i]]
  test <- test[test$forenowcast.per.100k>w,]
  forenowcast_errors_no_randoms_no_AR[[i]] <- test
}

for (i in 1:32) {
  test <- forenowcast_errors_noAR[[i]]
  test <- test[test$forenowcast.per.100k>w,]
  forenowcast_errors_noAR[[i]] <- test
}

for (i in 1:32) {
  test <- forenowcast_errors_noAR_d[[i]]
  test <- test[test$forenowcast.per.100k>w,]
  forenowcast_errors_noAR_d[[i]] <- test
}

for (i in 1:32) {
  test <- forenowcast_errors_noAR_t[[i]]
  test <- test[test$forenowcast.per.100k>w,]
  forenowcast_errors_noAR_t[[i]] <- test
}

for (i in 1:32) {
  test <- forenowcast_errors_noshortterm[[i]]
  test <- test[test$forenowcast.per.100k>w,]
  forenowcast_errors_noshortterm[[i]] <- test
}

for (i in 1:32) {
  test <- forenowcast_errors_noshortterm_noAR[[i]]
  test <- test[test$forenowcast.per.100k>w,]
  forenowcast_errors_noshortterm_noAR[[i]] <- test
}







for (i in 1:32) {
  test <- nowcast_errors_full[[i]]
  test <- test[test$nowcast.per.100k>w,]
  nowcast_errors_full[[i]] <- test
}

for (i in 1:32) {
  test <- nowcast_errors_no_randoms[[i]]
  test <- test[test$nowcast.per.100k>w,]
  nowcast_errors_no_randoms[[i]] <- test
}

for (i in 1:32) {
  test <- nowcast_errors_no_randoms_no_AR[[i]]
  test <- test[test$nowcast.per.100k>w,]
  nowcast_errors_no_randoms_no_AR[[i]] <- test
}

for (i in 1:32) {
  test <- nowcast_errors_noAR[[i]]
  test <- test[test$nowcast.per.100k>w,]
  nowcast_errors_noAR[[i]] <- test
}

for (i in 1:32) {
  test <- nowcast_errors_noAR_d[[i]]
  test <- test[test$nowcast.per.100k>w,]
  nowcast_errors_noAR_d[[i]] <- test
}

for (i in 1:32) {
  test <- nowcast_errors_noAR_t[[i]]
  test <- test[test$nowcast.per.100k>w,]
  nowcast_errors_noAR_t[[i]] <- test
}

for (i in 1:32) {
  test <- nowcast_errors_noshortterm[[i]]
  test <- test[test$nowcast.per.100k>w,]
  nowcast_errors_noshortterm[[i]] <- test
}

for (i in 1:32) {
  test <- nowcast_errors_noshortterm_noAR[[i]]
  test <- test[test$nowcast.per.100k>w,]
  nowcast_errors_noshortterm_noAR[[i]] <- test
}


#forecasts
mre_forecast_full <- NULL
for (i in 1:32) {
  mre_forecast_full <- c(mre_forecast_full, mean(abs(forecast_errors_full[[i]]$error.per.100k) / forecast_errors_full[[i]]$forecast.per.100k ))
}

mre_forecast_norandoms <- NULL
for (i in 1:32) {
  mre_forecast_norandoms <- c(mre_forecast_norandoms,mean(abs(forecast_errors_no_randoms[[i]]$error.per.100k) / forecast_errors_no_randoms[[i]]$forecast.per.100k ))
}

mre_forecast_norandoms_noAR <- NULL
for (i in 1:32) {
  mre_forecast_norandoms_noAR <- c(mre_forecast_norandoms_noAR,mean(abs(forecast_errors_no_randoms_no_AR[[i]]$error.per.100k) / forecast_errors_no_randoms_no_AR[[i]]$forecast.per.100k ))
}

mre_forecast_noAR <- NULL
for (i in 1:32) {
  mre_forecast_noAR <- c(mre_forecast_noAR,mean(abs(forecast_errors_noAR[[i]]$error.per.100k) / forecast_errors_noAR[[i]]$forecast.per.100k ))
}

mre_forecast_noAR_t <- NULL
for (i in 1:32) {
  mre_forecast_noAR_t <- c(mre_forecast_noAR_t,mean(abs(forecast_errors_noAR_t[[i]]$error.per.100k) / forecast_errors_noAR_t[[i]]$forecast.per.100k ))
}

mre_forecast_noAR_d <- NULL
for (i in 1:32) {
  mre_forecast_noAR_d <- c(mre_forecast_noAR_d,mean(abs(forecast_errors_noAR_d[[i]]$error.per.100k) / forecast_errors_noAR_d[[i]]$forecast.per.100k ))
}

mre_forecast_noshortterm <- NULL
for (i in 1:32) {
  mre_forecast_noshortterm <- c(mre_forecast_noshortterm,mean(abs(forecast_errors_noshortterm[[i]]$error.per.100k) / forecast_errors_noshortterm[[i]]$forecast.per.100k ))
}

mre_forecast_noshortterm_noAR <- NULL
for (i in 1:32) {
  mre_forecast_noshortterm_noAR <- c(mre_forecast_noshortterm_noAR,mean(abs(forecast_errors_noshortterm_noAR[[i]]$error.per.100k) / forecast_errors_noshortterm_noAR[[i]]$forecast.per.100k ))
}

mre_forecast_df <- cbind.data.frame(Week = 1:32, doa = as.Date(doa_vector),mre_forecast_full,mre_forecast_noAR,mre_forecast_noAR_d,
                                    mre_forecast_noAR_t,mre_forecast_norandoms,mre_forecast_norandoms_noAR,
                                    mre_forecast_noshortterm,mre_forecast_noshortterm_noAR)



#put in long format
mre_forecast_df_long <- pivot_longer(mre_forecast_df,
                                     cols = 3:10)
names(mre_forecast_df_long) <- c("Week","Date", "Model Variant","MRE")



#turn into absolute balue
mre_forecast_df_long$`Absolute MRE`=abs(mre_forecast_df_long$MRE)

mre_forecast_df_long$`Model Variant`=as.factor(mre_forecast_df_long$`Model Variant`)
levels(mre_forecast_df_long$`Model Variant`) <- c("Full","No AR", "No AR_d","No AR_t (chosen)   ", "No REs", "No REs and no AR","No short term RE","No short term RE, no AR")
mre_forecast_df_long$`Model Variant`<- factor(mre_forecast_df_long$`Model Variant`,levels = c("Full", "No REs", "No REs and no AR","No short term RE","No short term RE, no AR","No AR", "No AR_d","No AR_t (chosen)   "))

#forecast
pdf(file = "C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/Comparisons/Absolute relative error forecast.pdf",width = 14, height = 11)
ggplot(data = mre_forecast_df_long, aes(Date,`Absolute MRE`,col=`Model Variant`,size=`Model Variant`)) +
  geom_line() +
  ggtitle("Forecasts") +
  #scale_color_brewer(palette = "Reds") +
  scale_size_manual(values = c(0.8,0.8,0.8,0.8,0.8,0.8,0.8,2.5)) +
  theme_pubr(base_size = 25) +
  labs(x="Fitting Date",y="Mean Relative Absolute Error") +
  scale_y_continuous(limits = c(-0,1.25)) +
  scale_x_date(date_labels="%b",date_breaks  ="1 month") +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  #theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))
# theme(legend.justification = c(0.99, 0.99), legend.position = c(0.36, 0.99), 
#       legend.text.align = 0, legend.box.background = element_rect(colour = "grey"),
#       legend.background = element_blank()) 
#theme(legend.title = element_blank()) 
dev.off()


#forenowcasts
mre_forenowcast_full <- NULL
for (i in 1:32) {
  mre_forenowcast_full <- c(mre_forenowcast_full, mean(abs(forenowcast_errors_full[[i]]$error.per.100k) / forenowcast_errors_full[[i]]$forenowcast.per.100k ))
}

mre_forenowcast_norandoms <- NULL
for (i in 1:32) {
  mre_forenowcast_norandoms <- c(mre_forenowcast_norandoms,mean(abs(forenowcast_errors_no_randoms[[i]]$error.per.100k) / forenowcast_errors_no_randoms[[i]]$forenowcast.per.100k ))
}

mre_forenowcast_norandoms_noAR <- NULL
for (i in 1:32) {
  mre_forenowcast_norandoms_noAR <- c(mre_forenowcast_norandoms_noAR,mean(abs(forenowcast_errors_no_randoms_no_AR[[i]]$error.per.100k) / forenowcast_errors_no_randoms_no_AR[[i]]$forenowcast.per.100k ))
}

mre_forenowcast_noAR <- NULL
for (i in 1:32) {
  mre_forenowcast_noAR <- c(mre_forenowcast_noAR,mean(abs(forenowcast_errors_noAR[[i]]$error.per.100k) / forenowcast_errors_noAR[[i]]$forenowcast.per.100k ))
}

mre_forenowcast_noAR_t <- NULL
for (i in 1:32) {
  mre_forenowcast_noAR_t <- c(mre_forenowcast_noAR_t,mean(abs(forenowcast_errors_noAR_t[[i]]$error.per.100k) / forenowcast_errors_noAR_t[[i]]$forenowcast.per.100k ))
}

mre_forenowcast_noAR_d <- NULL
for (i in 1:32) {
  mre_forenowcast_noAR_d <- c(mre_forenowcast_noAR_d,mean(abs(forenowcast_errors_noAR_d[[i]]$error.per.100k) / forenowcast_errors_noAR_d[[i]]$forenowcast.per.100k ))
}

mre_forenowcast_noshortterm <- NULL
for (i in 1:32) {
  mre_forenowcast_noshortterm <- c(mre_forenowcast_noshortterm,mean(abs(forenowcast_errors_noshortterm[[i]]$error.per.100k) / forenowcast_errors_noshortterm[[i]]$forenowcast.per.100k ))
}

mre_forenowcast_noshortterm_noAR <- NULL
for (i in 1:32) {
  mre_forenowcast_noshortterm_noAR <- c(mre_forenowcast_noshortterm_noAR,mean(abs(forenowcast_errors_noshortterm_noAR[[i]]$error.per.100k) / forenowcast_errors_noshortterm_noAR[[i]]$forenowcast.per.100k ))
}

mre_forenowcast_df <- cbind.data.frame(Week = 1:32, doa = as.Date(doa_vector),mre_forenowcast_full,mre_forenowcast_noAR,mre_forenowcast_noAR_d,
                                    mre_forenowcast_noAR_t,mre_forenowcast_norandoms,mre_forenowcast_norandoms_noAR,
                                    mre_forenowcast_noshortterm,mre_forenowcast_noshortterm_noAR)



#put in long format
mre_forenowcast_df_long <- pivot_longer(mre_forenowcast_df,
                                     cols = 3:10)
names(mre_forenowcast_df_long) <- c("Week","Date", "Model Variant","MRE")



#turn into absolute balue
mre_forenowcast_df_long$`Absolute MRE`=abs(mre_forenowcast_df_long$MRE)

mre_forenowcast_df_long$`Model Variant`=as.factor(mre_forenowcast_df_long$`Model Variant`)
levels(mre_forenowcast_df_long$`Model Variant`) <- c("Full","No AR", "No AR_d","No AR_t (chosen)   ", "No REs", "No REs and no AR","No short term RE","No short term RE, no AR")
mre_forenowcast_df_long$`Model Variant`<- factor(mre_forenowcast_df_long$`Model Variant`,levels = c("Full", "No REs", "No REs and no AR","No short term RE","No short term RE, no AR","No AR", "No AR_d","No AR_t (chosen)   "))

#forenowcast
pdf(file = "C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/Comparisons/Absolute relative error forenowcast.pdf",width = 14, height = 11)
ggplot(data = mre_forenowcast_df_long, aes(Date,`Absolute MRE`,col=`Model Variant`,size=`Model Variant`)) +
  geom_line() +
  ggtitle("Forenowcasts") +
  #scale_color_brewer(palette = "Reds") +
  scale_size_manual(values = c(0.8,0.8,0.8,0.8,0.8,0.8,0.8,2.5)) +
  theme_pubr(base_size = 25) +
  labs(x="Fitting Date",y="Mean Relative Absolute Error") +
  scale_y_continuous(limits = c(-0,1.25)) +
  scale_x_date(date_labels="%b",date_breaks  ="1 month") +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  #theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))
# theme(legend.justification = c(0.99, 0.99), legend.position = c(0.36, 0.99), 
#       legend.text.align = 0, legend.box.background = element_rect(colour = "grey"),
#       legend.background = element_blank()) 
#theme(legend.title = element_blank()) 
dev.off()


#nowcasts
mre_nowcast_full <- NULL
for (i in 1:32) {
  mre_nowcast_full <- c(mre_nowcast_full, mean(abs(nowcast_errors_full[[i]]$error.per.100k) / nowcast_errors_full[[i]]$nowcast.per.100k ))
}

mre_nowcast_norandoms <- NULL
for (i in 1:32) {
  mre_nowcast_norandoms <- c(mre_nowcast_norandoms,mean(abs(nowcast_errors_no_randoms[[i]]$error.per.100k) / nowcast_errors_no_randoms[[i]]$nowcast.per.100k ))
}

mre_nowcast_norandoms_noAR <- NULL
for (i in 1:32) {
  mre_nowcast_norandoms_noAR <- c(mre_nowcast_norandoms_noAR,mean(abs(nowcast_errors_no_randoms_no_AR[[i]]$error.per.100k) / nowcast_errors_no_randoms_no_AR[[i]]$nowcast.per.100k ))
}

mre_nowcast_noAR <- NULL
for (i in 1:32) {
  mre_nowcast_noAR <- c(mre_nowcast_noAR,mean(abs(nowcast_errors_noAR[[i]]$error.per.100k) / nowcast_errors_noAR[[i]]$nowcast.per.100k ))
}

mre_nowcast_noAR_t <- NULL
for (i in 1:32) {
  mre_nowcast_noAR_t <- c(mre_nowcast_noAR_t,mean(abs(nowcast_errors_noAR_t[[i]]$error.per.100k) / nowcast_errors_noAR_t[[i]]$nowcast.per.100k ))
}

mre_nowcast_noAR_d <- NULL
for (i in 1:32) {
  mre_nowcast_noAR_d <- c(mre_nowcast_noAR_d,mean(abs(nowcast_errors_noAR_d[[i]]$error.per.100k) / nowcast_errors_noAR_d[[i]]$nowcast.per.100k ))
}

mre_nowcast_noshortterm <- NULL
for (i in 1:32) {
  mre_nowcast_noshortterm <- c(mre_nowcast_noshortterm,mean(abs(nowcast_errors_noshortterm[[i]]$error.per.100k) / nowcast_errors_noshortterm[[i]]$nowcast.per.100k ))
}

mre_nowcast_noshortterm_noAR <- NULL
for (i in 1:32) {
  mre_nowcast_noshortterm_noAR <- c(mre_nowcast_noshortterm_noAR,mean(abs(nowcast_errors_noshortterm_noAR[[i]]$error.per.100k) / nowcast_errors_noshortterm_noAR[[i]]$nowcast.per.100k ))
}

mre_nowcast_df <- cbind.data.frame(Week = 1:32, doa = as.Date(doa_vector),mre_nowcast_full,mre_nowcast_noAR,mre_nowcast_noAR_d,
                                    mre_nowcast_noAR_t,mre_nowcast_norandoms,mre_nowcast_norandoms_noAR,
                                    mre_nowcast_noshortterm,mre_nowcast_noshortterm_noAR)



#put in long format
mre_nowcast_df_long <- pivot_longer(mre_nowcast_df,
                                     cols = 3:10)
names(mre_nowcast_df_long) <- c("Week","Date", "Model Variant","MRE")



#turn into absolute balue
mre_nowcast_df_long$`Absolute MRE`=abs(mre_nowcast_df_long$MRE)

mre_nowcast_df_long$`Model Variant`=as.factor(mre_nowcast_df_long$`Model Variant`)
levels(mre_nowcast_df_long$`Model Variant`) <- c("Full","No AR", "No AR_d","No AR_t (chosen)   ", "No REs", "No REs and no AR","No short term RE","No short term RE, no AR")
mre_nowcast_df_long$`Model Variant`<- factor(mre_nowcast_df_long$`Model Variant`,levels = c("Full", "No REs", "No REs and no AR","No short term RE","No short term RE, no AR","No AR", "No AR_d","No AR_t (chosen)   "))

#nowcast
pdf(file = "C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/Plots/cases/Prediction Errors/Comparisons/Absolute relative error nowcast.pdf",width = 14, height = 11)
ggplot(data = mre_nowcast_df_long, aes(Date,`Absolute MRE`,col=`Model Variant`,size=`Model Variant`)) +
  geom_line() +
  ggtitle("Nowcasts") +
  #scale_color_brewer(palette = "Reds") +
  scale_size_manual(values = c(0.8,0.8,0.8,0.8,0.8,0.8,0.8,2.5)) +
  theme_pubr(base_size = 25) +
  labs(x="Fitting Date",y="Mean Absolute Relative Error") +
  scale_y_continuous(limits = c(-0,1.25)) +
  scale_x_date(date_labels="%b",date_breaks  ="1 month") +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  #theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))
# theme(legend.justification = c(0.99, 0.99), legend.position = c(0.36, 0.99), 
#       legend.text.align = 0, legend.box.background = element_rect(colour = "grey"),
#       legend.background = element_blank()) 
#theme(legend.title = element_blank()) 
dev.off()


