# set current path to this file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# clear the global environment
rm(list = ls())

# sets local system language to English
Sys.setlocale("LC_ALL","English")

# path to the sync and share folder - 
# replace your own path here!!!
# this path always needs to be saved in the global environment
#path.LRZ = "C:/Users/ru58paj/LRZ Sync+Share/Corona/"                     # Marc
path.LRZ = "C:/Users/ru48fak/LRZ Sync+Share/Corona (Marc Schneble)/"   # Giacomo


# load all functions 
source("Functions/Preprocessing.R")
source("Functions/Cases.R")
source("Functions/Diagnostics.R")

# load packages
library(tidyverse)
library(magrittr)
library(lubridate)
library(mgcv)
library(readxl)
library(rgdal)
library(checkmate)
library(ggplot2)
library(MASS)
library(matrixStats)
library(pROC)
library(ggrepel)
library(ggsn)
library(parallel)

#### new case model ####
doa <- as.POSIXct("2020-09-15", tz = "GMT") #best date for forecasts: 2020-09-15
T.max <- 21
d.max <- 7
recent <- 7
k <- 7
AR.d <- TRUE
AR.t <- FALSE

#fitting the model
model <- fit.case.model.new(doa, T.max, d.max, recent, AR.d, AR.t)

# plot maps
plot.effects.cases(doa)

# plot time effect
plot.time.effect(doa)

# plot predictions with prediction intervals
(g.nowcast <- plot.predictions(model, d.max = 7, k = 7, type = "nowcast", n = 100))
(g.forecast <- plot.predictions(model,  d.max = 7, k = 7, type = "forecast", n = 100))
(g.forenowcasts <- plot.predictions(model, d.max = 7, k = 7, type = "forenowcast", n = 100))


