
fit.case.model.new = function(doa, T.max, d.max, recent, AR.d, AR.t){
  
  # preprocess district data
  districts <- preprocess.districts()

  # gender-age combinations
  age.groups <- c("A00-A04", "A05-A14", "A15-A34", "A35-A59", "A60-A79", "A80+")
  genders <- c("M", "W")
  
  # all preprocessed RKI data sets
  files <- list.files(path= paste0(path.LRZ, "Data/Formatted"))
  # reporting date of each data set
  reporting.dates <- as.POSIXct(sub("\\..*" , "", sub("^[^2]*", "" , files)), hour = 0, tz = "GMT")
  #T.max temporarily increased by one to abtain AR(1) 
  T.max <- T.max + 1
  # the first considered date should not be before min(reporting,dates)
  T.max <- min(T.max, as.numeric(doa - min(reporting.dates)) + 1)
  # only consider data from the last T.max days
  files <- tail(files[which(doa >= reporting.dates)], T.max)
  reporting.dates <- tail(reporting.dates[which(doa >= reporting.dates)], T.max)
  
  # registration dates
  registration.dates <- doa %m+% days(x = seq.int(from = -T.max, to = -1, by = 1))
  weekdays <- weekdays.POSIXt(registration.dates, abbreviate = TRUE)
  
  
  data <- cases.by.date <- vector("list", length(files))
  names(data) <- reporting.dates
  
  data.long <- NULL
  
  t = 0
  for (file in files) {
    t = t+1
    data[[t]] <- read_rds(paste0(path.LRZ, "Data/Formatted/", file)) 
    
    data.t <- data[[t]] %>%filter(cases > 0, date >= max(registration.dates[t] - days(d.max-1), registration.dates[1]), 
                                  age != "unbekannt", gender != "unbekannt") %>% 
      group_by_at(vars(date, districtId, age, gender)) %>% summarise(C.t.d = sum(cases)) %>% ungroup() 
    
    num.days <- min(d.max, t)
    time.points <- seq(max(t-d.max+1, 1), t, 1)
    
    # if not all districts have an observation 
    if (length(unique(data.t$districtId)) != nrow(districts)){
      missing.Ids <- setdiff(districts$districtId, as.numeric(unique(data.t$districtId)))
      data.t <- bind_rows(data.t, tibble(districtId = missing.Ids, date = registration.dates[t], age = "A00-A04", gender = "M", C.t.d = 0))
    }
    
    data.t <- complete(data.t, date, districtId, age, gender, fill = list(C.t.d = 0)) %>%
      mutate(districtId = as.numeric(districtId),
             age = as.factor(age),
             pop = rep(as.vector(t(as.matrix(dplyr::select(districts, 5:16)))), num.days),
             lat = rep(rep(districts$lat, each = length(age.groups)*length(genders)), num.days),
             lon = rep(rep(districts$lon, each = length(age.groups)*length(genders)), num.days),
             density = rep(rep(districts$density, each = length(age.groups)*length(genders)), num.days),
             time = rep(time.points, each = nrow(districts)*length(genders)*length(age.groups)),
             time.recent = pmax(time - (T.max - d.max), 0),
             age.80 = as.factor(1*(age == "A80+")),
             day.recent = 1*(time - (T.max - recent) > 0),
             delay = as.numeric(reporting.dates[t] - date),
             weekday = weekdays[time],
             C.t.d_1 = NA,
             N.t.d = NA
             
      )
    
    data.t.d <- NULL
    

    
    #this version sets C.t.d_1 = 1 + C.t.d at d = d-1 
    for (d in 1:min(d.max, t)){
      data.t.d <- bind_rows(data.t.d, 
                            filter(data.t, delay == d) %>% 
                              mutate(C.t.d_1 = switch(as.character(d), 
                                                      "1" = 1,
                                                      filter(data.long, time == t-d+1, delay == d-1)$C.t.d+1),
                                     N.t.d = switch(as.character(d),
                                                    "1" = filter(data.t, time == t, delay == d)$C.t.d,
                                                    filter(data.t, time == t-d+1, delay == d)$C.t.d - 
                                                      filter(data.long, time == t-d+1, delay == d-1)$C.t.d)))
    }
    
    data.long <- bind_rows(data.long, data.t.d) 
  }
  
  data.long$N.t.d[which(data.long$N.t.d < 0)] <- 0
  #data.long$C.t.d_1 <- pmax(data.long$C.t.d_1, 1)
  
  off <- log(data.long$pop) + log(data.long$C.t.d_1)
  data.long$offy <- off
  
  
  #transform variables into factors for BAM
  data.long <- mutate(data.long,
                      delay = factor(delay),
                      weekday = factor(weekday),
                      day.recent = factor(day.recent))
  
  #set monday as reference category for weekday
  data.long <- mutate(data.long, weekday = relevel(weekday, ref = "Mon"))
  
  #arrange dataset to accomodate for AR(1)  
  data.long <- arrange(data.long, delay, districtId, age, gender, date)  
  #add C.t_1.d   
  data.long <- mutate(data.long, C.t_1.d = if_else(date != min(date),                                                   
                                                   1 + lag(C.t.d),                                                   
                                                   1 ))  
  
  #removing extra day (22nd) from analysis and updating time accordingly  
  data.long <- filter(data.long, date > min(date))  
  data.long <- mutate(data.long, time = time - 1)
  
  #for adding GISD at NUTS2 level 
  
  # coordinates <- read_excel(paste(path.LRZ, "Data/Demographics/coordinates.xlsx", sep = ""))
  # nuts <- read_excel(paste(path.LRZ, "Data/Demographics/GISD_NUTS2_2012.xlsx", sep = ""))
  # nuts <- data.frame(NUTS2 = nuts$NUTS22012, GISD = nuts$GISD_2012)
  # nuts_full <- merge(coordinates,nuts)
  # nuts_2 <- data.frame(nuts = nuts_full$NUTS2,districtId = nuts_full$Id, depr = nuts_full$GISD)
  # data.long <- merge(nuts_2,data.long)
  
  
  #UNCOMMENT FOR FULL MODEL
  form <- N.t.d ~ s(time, bs = "ps", k = 8) + s(lon, lat) +
    s(day.recent, districtId, bs = "re") + delay + weekday +
    age + gender + offset(log(pop) #+depr #add only if chunk above uncommented
    )

  
  if (AR.d) {
    form <- update.formula(form, ~ . + log(C.t.d_1))
  }
  if (AR.t) {
    form <- update.formula(form, ~ . + log(C.t_1.d))
  }

  #COMMENT FOR ALL AGES; LEAVE FOR 80- 
   #data.long <- dplyr::filter(data.long, age == "A80+")
   #data.long$age <- factor(data.long$age)
  
  #model fitting with BAM
  #added options discrete = T and nthreads = 20 to improve performance
  model <- bam(form, 
               discrete = T,
               nthreads = 20,
               data = data.long %>% 
                 mutate(
                   age = relevel(age, ref = "A35-A59"),
                        districtId = factor(districtId)), 
               family = nb)

  
  #attach data to model to return it
  model$data <- data.long
  model$registration.dates <- registration.dates[2:T.max]
  model$doa <- doa
  model$T.max <- T.max - 1
  model$d.max <- d.max
  
  
  saveRDS(model, paste0(path.LRZ, "Output/cases/",
                       doa, ".Rds"))
  
  

  
  return(model)
  
}




#this function simply returns the formatted data in long format

get.data.long = function(num.days = 31, N.models = 1, T.max = 25, d.max = 6, prev = "rslope", prev.days = 5) {
  
  # preprocess district data
  districts <- preprocess.districts()
  
  # gender-age combinations
  age.group <- c("A00-A04", "A05-A14", "A15-A34", "A35-A59", "A60-A79", "A80+")
  genders <- c("M", "W")
  
  files <- list.files(path = paste(path.LRZ, "Data/Formatted", sep = ""))
  files <- files[(length(files) - N.models + 1):length(files)]
  reporting.dates <- as.POSIXct(sub("\\..*" , "", sub("^[^2]*", "" , files)), hour = 0, tz = "GMT")
  
  data.RKI <- cases.by.date <- vector("list", length(files))
  names(data.RKI) <-  reporting.dates
  
  for (i in 1:length(files)) {
    data.RKI[[i]] <- as_tibble(read_rds(paste(path.LRZ, "Data/Formatted/", files[i], sep = ""))) %>% 
      mutate(gender = as.character(gender))
    # 03152 is not a districts any more
    if (length(which(data.RKI[[i]]$districtId == "03152")) > 0) {
      data.RKI[[i]] <- data.RKI[[i]][-which(data.RKI[[i]]$districtId == "03152"), ]
    }
    # if date is not in desired format
    if (!is.element("POSIXct", class(data.RKI[[i]]$date))){
      data.RKI[[i]] <- data.RKI[[i]] %>% mutate(date = as.POSIXct(x = date, format = "%m/%d/%Y", tz = "UTC")) %>%
        mutate(date = as_datetime(x = date))
    }
    # rename age variable of necessary
    if (is.element("age_group", names(data.RKI[[i]]))){
      data.RKI[[i]] <- rename(data.RKI[[i]], age = age_group) %>% mutate(age = as.character(age))
    }
    
    date.t0 <- max(data.RKI[[i]]$date) - days(num.days -1) 
    data.RKI[[i]]$day <- as.numeric((data.RKI[[i]]$date - date.t0)/(86400))
    data.RKI[[i]]$weekday <- weekdays.POSIXt(data.RKI[[i]]$date, abbreviate = TRUE)
    
    weekdays <- weekdays.POSIXt(date.t0 %m+% days(x = seq.int(from = 0, to = num.days-1, by = 1)),
                                abbreviate = TRUE)
    
    data.long <- data.RKI[[i]] %>%filter(cases > 0, day >= 0, age != "unbekannt", gender != "unbekannt") %>% 
      group_by_at(vars(day, districtId, age, gender)) %>% summarise(cases = sum(cases)) %>% ungroup() 
    
    # if not all districts have an observation within the last 30 days
    if (length(unique(data.long$districtId)) != nrow(districts)){
      missing.Ids <- setdiff(districts$districtId, as.numeric(unique(data.long$districtId)))
      missing.Ids <- ifelse(nchar(missing.Ids) == 4,
                            yes = paste0("0", missing.Ids), no = missing.Ids)
      data.long <- bind_rows(data.long, tibble(districtId = missing.Ids, day = 0, age = "A00-A04", gender = "M", cases = 0))
    }
    
    data.long <- complete(data.long, day, districtId, age, gender, fill = list(cases = 0)) %>%
      mutate(districtId = as.numeric(districtId),
             age = as.factor(age),
             day.lag = pmax(day - (num.days - 1 - prev.days), 0),
             pop = rep(as.vector(t(as.matrix(select(districts, 5:16)))), num.days),
             lat = rep(rep(districts$lat, each = length(age.group)*length(genders)), num.days),
             lon = rep(rep(districts$lon, each = length(age.group)*length(genders)), num.days),
             weekday = as.factor(rep(weekdays, each = nrow(districts)*length(age.group)*length(genders)))
      )
    
    data.long$age <- relevel(data.long$age, ref = "A35-A59")
    data.long$weekday <- relevel(data.long$weekday, ref = "Mon")
    
    # offsets
    pop <- data.long$pop
    delay.factor <- delay.factors(date = reporting.dates[i], T.max = T.max, d.max = d.max, response = "cases")$factor
    delay.factor <- c(rep(1, num.days - length(delay.factor)), delay.factor)
    delay.factor <- rep(delay.factor, each = nrow(districts)*length(age.group)*length(genders))
    
    #off = pop/delay.factor
    off <<- pop/delay.factor
    
    
    # adding mean cases of the last 3 days for AR model
    if (prev == "AR"){
      #add variable with average number of infected in the last three day
      data.long$prev.day <- data.long$day-1
      data.long$prev.day2 <- data.long$day-2
      data.long$prev.day3 <- data.long$day-3
      
      data.long$prev.cases <- rep(0, times = length(data.long$day))
      data.long$prev.cases2 <- rep(0, times = length(data.long$day))
      data.long$prev.cases3 <- rep(0, times = length(data.long$day))
      
      
      for (l in unique(data.long$gender)) {
        for (j in unique(data.long$age)) {
          for (k in unique(data.long$districtId)) {
            positions <- match(data.long$prev.day[data.long$gender==l&data.long$districtId==k&data.long$age==j],
                               data.long$day[data.long$gender==l&data.long$districtId==k&data.long$age==j])
            data.long$prev.cases[data.long$gender==l&data.long$districtId==k&data.long$age==j] <- data.long$cases[data.long$gender==l&data.long$districtId==k&data.long$age==j][positions]
            
            positions2 <- match(data.long$prev.day2[data.long$gender==l&data.long$districtId==k&data.long$age==j],
                                data.long$day[data.long$gender==l&data.long$districtId==k&data.long$age==j])
            data.long$prev.cases2[data.long$gender==l&data.long$districtId==k&data.long$age==j] <- data.long$cases[data.long$gender==l&data.long$districtId==k&data.long$age==j][positions2]
            
            positions3 <- match(data.long$prev.day3[data.long$gender==l&data.long$districtId==k&data.long$age==j],
                                data.long$day[data.long$gender==l&data.long$districtId==k&data.long$age==j])
            data.long$prev.cases3[data.long$gender==l&data.long$districtId==k&data.long$age==j] <- data.long$cases[data.long$gender==l&data.long$districtId==k&data.long$age==j][positions3]
          }
        }
      }
      
      data.long$prev.cases[is.na(data.long$prev.cases)] <- 0
      data.long$prev.cases2[is.na(data.long$prev.cases2)] <- 0
      data.long$prev.cases3[is.na(data.long$prev.cases3)] <- 0
      
      data.long$prev.cases.mean <- rowMeans(cbind(data.long$prev.cases,
                                                  data.long$prev.cases2,
                                                  data.long$prev.cases3))
      
    }
  }
  return(data.long)
}



#internal function for the prediction: gets the realized infection numbers
get.cases <- function(doa, k, d.max, type) {
  
  date.observed <- doa + days(d.max)*(type == "nowcast" | type == "forecast") + days(d.max+k)*(type == "forenowcast")
  files <- list.files(path= paste0(path.LRZ, "Data/Formatted")) 
  file <- files[grep(as.character(date.observed), files)]
  
  min.date <- if_else(type == "nowcast", doa - days(d.max), doa)
  max.date <- if_else(type == "nowcast", doa - days(1), doa + days(k-1))
  
  data <- read_rds(paste0(path.LRZ, "Data/Formatted/", file)) %>%
    filter(date >= min.date, date <= max.date, age != "unbekannt", gender != "unbekannt") %>% 
    group_by(districtId) %>% 
    summarise(observed = sum(cases)) %>% 
    ungroup()  
  
  districts <- left_join(preprocess.districts(), data, by = "districtId") %>%
    replace_na(list(observed = 0))
  
  return(districts)
}

#this next function produces the dataset needed for nowcasting, 
# adding missing rows to predict

nowcast.data = function(data = model$data, T.max = 21, d.max = 7) { 
  #get data from last 7 days
  nowcast_data <- filter(data, time > (T.max - d.max))
  
  #add missing rows to include all delays
  nowcast_data <- complete(nowcast_data, delay, gender, age, districtId, date)
  
  #add populations and coordinates
  nowcast_data=arrange(nowcast_data,districtId)
  nowcast_data <- fill(nowcast_data, lon, lat, pop)
  
  #add weekday based on date
  nowcast_data <- mutate(nowcast_data, weekday = wday(date, label = TRUE))
  
  #add time and time.recent based on date and inputs
  nowcast_data <- mutate(nowcast_data, time = as.numeric(T.max - d.max + 1 + (date - min(date))/(60*60*24)) )
  nowcast_data <- mutate(nowcast_data, time.recent = time - (T.max - d.max))
  nowcast_data <- mutate(nowcast_data, day.recent = 1)
  
  return(nowcast_data)
}

#careful: this dataset still has NAs in C.t.d, C.t.d_1, N.T.d and 
#offy (needed for nowcasting)!
#They will be added in the one-step-ahead nowcasting function.


#function that nowcasts cases up to d.max
nowcast.cases = function(nowcast_data = nowcast_data, model = model, d.max=d.max) {
  
  for (i in 1:d.max) {
    
    #fill in C.t_1.d to be able to perform nowcast with AR(1)
    nowcast_data <- arrange(nowcast_data,delay,districtId, age,gender,date)  
    #add C.t_1.d   
    nowcast_data <- mutate(nowcast_data, C.t_1.d = if_else(is.na(C.t_1.d),                                                   
                                                    1 + lag(C.t.d),                                                   
                                                    C.t_1.d))  
    
    #fill in C.t.d_1 and offset to be able to perform 1 step ahead nowcast
    nowcast_data <- arrange(nowcast_data,districtId,age,gender,time,delay)
    nowcast_data <- mutate(nowcast_data, C.t.d_1 = 
                             if_else(condition = (delay == 1), 
                                     true = 1, 
                                     false = lag(C.t.d) + 1 ) 
    )
    
    #control that there are no zeros 
    nowcast_data <- mutate(nowcast_data, C.t.d_1 = 
                             if_else(condition = (C.t.d_1 < 1),
                                     true = 1,
                                     false = C.t.d_1)
    )
    #now compute the offset
    nowcast_data <- mutate(nowcast_data, offy = log(pop) + log(C.t.d_1))
    
    #finally we can perform the first step of nowcasts
    #all rows with NA predictions will predict NA, so nowcasts only go one-step ahead
    nowcast_data <- mutate(nowcast_data, 
                           nowcast = predict.bam(model, nowcast_data, type = "response"))
    
    #update N.t.d with nowcast
    nowcast_data <- mutate(nowcast_data, N.t.d = 
                             if_else(condition = is.na(N.t.d),
                                     true = nowcast,
                                     false = N.t.d)
    )
    #now naturally update C.t.d in a similar way
    nowcast_data <- mutate(nowcast_data, C.t.d = 
                             if_else(condition = is.na(C.t.d),
                                     true = lag(C.t.d)+N.t.d,
                                     false = C.t.d)
    )
    
  }
  return(nowcast_data)  
}

#function that forecasts cases for the next k days with delay up to d.max
forecast.cases = function(k = 7, 
                          data = nowcasts, 
                          model = model, T.max = T.max, d.max = d.max) {
  
  forecasts = NULL
  
  #select last nowcasted day for the first C.t_1.d
  data_last_day <- filter(data, time == T.max)
  
  #we will predict cases for the next k days
  for (j in 1:k) {
  
  #select last day
  forecast_data <- filter(data, time == T.max)
  
  #set unknown quantities to NA
  forecast_data <- mutate(forecast_data, C.t.d = NA)
  forecast_data <- mutate(forecast_data, C.t.d_1 = NA)
  forecast_data <- mutate(forecast_data, N.t.d = NA)
  forecast_data <- mutate(forecast_data, offy = NA)
  forecast_data <- mutate(forecast_data, forecast = NA)
  forecast_data <- mutate(forecast_data, C.t_1.d = data_last_day$C.t.d + 1)
  
  
  
  
  #update weekday (not time, as we want the spline to be constant in prediction step)
  forecast_data <- mutate(forecast_data, weekday = wday(date + 60*60*24, label = T))
  
  #now forecast:
  
  for (i in 1:d.max) {
    
    #fill in C.t.d_1 and offset to be able to perform 1 step ahead nowcast
    forecast_data <- arrange(forecast_data,districtId,age,gender,time,delay)
    forecast_data <- mutate(forecast_data, C.t.d_1 = 
                              if_else(condition = (delay == 1), 
                                      true = 1, 
                                      false = as.double(lag(C.t.d)) + 1)
    )
    
    #control that there are no zeros
    forecast_data <- mutate(forecast_data, C.t.d_1 = 
                              if_else(condition = (C.t.d_1 < 1),
                                      true = 1,
                                      false = C.t.d_1)
    )
    #now compute the offset
    forecast_data <- mutate(forecast_data, offy = log(pop) + log(C.t.d_1))
    
    #finally we can perform the first step of forecasts
    #all rows with NA predictions will predict NA, so nowcasts only go one-step ahead
    forecast_data <- mutate(forecast_data, 
                            forecast = predict.bam(model, forecast_data, type = "response"))
    
    #update N.t.d with nowcast
    forecast_data <- mutate(forecast_data, N.t.d = 
                              if_else(condition = is.na(N.t.d),
                                      true = forecast,
                                      false = as.double(N.t.d))
    )
    #now naturally update C.t.d in a similar way
    forecast_data <- mutate(forecast_data, C.t.d = 
                              if_else(condition = is.na(C.t.d),
                                      true = if_else(condition = (delay==1),
                                                     true = N.t.d,
                                                     false = lag(C.t.d)+N.t.d ),
                                      false = as.double(C.t.d))
    )
  }
  
  #after prediction is done, update date (NOT time)
  forecast_data <- mutate(forecast_data, date = date + 60*60*24)
  
  #update data last day to grab C.t_1.d
  data_last_day <- forecast_data
  
  #update nowcasts for next cycle
  data <- forecast_data
  
  #append existing forecasts
  forecasts <- rbind(forecasts,forecast_data)
  
  }
  
  forecasts <- mutate(forecasts, nowcast = forecast)
  return(forecasts)  
}

#internal function for performing predictions
forecast.cases.diagonal = function(k = 7, 
                                   nowcastz = nowcasts, 
                                   forecastz = forenowcasts, 
                                   modelz = model, doaz = doa, 
                                   d.maxz = d.max, T.maxz = T.max) {
  
  nowcastz <- mutate(nowcastz, forecast = nowcast)
  data <- rbind(nowcastz, forecastz)
  
  data = group_by(data, districtId, delay, date)
  predicted = summarize(data, sum(forecast))
  predicted = arrange(predicted, date)
  
  predicted = mutate(predicted, 
                     time = as.numeric(as.factor(date)))
  
  #we will sum up the diagonal forecasts for the next k days
  diag_pred_all = tibble(districtId = unique(data$districtId))
  for (j in 1:k) {
    predicted = filter(predicted, time!=1)
    predicted = mutate(predicted, time = time - 1)
    predicted = mutate(predicted, 
                       tplusd = time + as.numeric(delay))
    
    diag_pred = filter(predicted, tplusd == 8)
    diag_pred = group_by(diag_pred, districtId)
    diag_pred = summarize(diag_pred, forecast = sum(`sum(forecast)`))
    diag_pred_all = cbind(diag_pred_all,diag_pred$forecast)
  }
  diag_pred_all$forecast=rowSums(diag_pred_all[,2:8]) 
  diag_pred_all = tibble(districtId = diag_pred_all$districtId,
                         forecast = diag_pred_all$forecast)
  
  return(diag_pred_all)
  
}


#contains all predicted cases with reporting date 
# from doa to doa + 7 

#now let's get real cases
get.cases.diagonal = function(k = 7, 
                              doaz = doa, 
                              d.maxz = d.max, T.maxz = T.max) {
  #newdata <- fit.case.model.new(doaz + days(k), T.maxz, d.maxz)
  #data <- newdata$data
  
  # preprocess district data
  doaz = doaz + days(k)
  doa = doaz
  districts <- preprocess.districts()
  
  # gender-age combinations
  age.groups <- c("A00-A04", "A05-A14", "A15-A34", "A35-A59", "A60-A79", "A80+")
  genders <- c("M", "W")
  
  # all preprocessed RKI data sets
  files <- list.files(path= paste0(path.LRZ, "Data/Formatted"))
  # reporting date of each data set
  reporting.dates <- as.POSIXct(sub("\\..*" , "", sub("^[^2]*", "" , files)), hour = 0, tz = "GMT")
  # the first considered date should not be before min(reporting,dates)
  T.max <- min(T.max, as.numeric(doa - min(reporting.dates)) + 1)
  # only consider data from the last T.max days
  files <- tail(files[which(doa >= reporting.dates)], T.max)
  reporting.dates <- tail(reporting.dates[which(doa >= reporting.dates)], T.max)
  
  # registration dates
  registration.dates <- doa %m+% days(x = seq.int(from = -T.max, to = -1, by = 1))
  weekdays <- weekdays.POSIXt(registration.dates, abbreviate = TRUE)
  
  
  data <- cases.by.date <- vector("list", length(files))
  names(data) <- reporting.dates
  
  data.long <- NULL
  
  t = 0
  for (file in files) {
    t = t+1
    data[[t]] <- read_rds(paste0(path.LRZ, "Data/Formatted/", file)) 
    
    data.t <- data[[t]] %>%filter(cases > 0, date >= max(registration.dates[t] - days(d.max-1), registration.dates[1]), 
                                  age != "unbekannt", gender != "unbekannt") %>% 
      group_by_at(vars(date, districtId, age, gender)) %>% summarise(C.t.d = sum(cases)) %>% ungroup() 
    
    num.days <- min(d.max, t)
    time.points <- seq(max(t-d.max+1, 1), t, 1)
    
    # if not all districts have an observation 
    if (length(unique(data.t$districtId)) != nrow(districts)){
      missing.Ids <- setdiff(districts$districtId, as.numeric(unique(data.t$districtId)))
      data.t <- bind_rows(data.t, tibble(districtId = missing.Ids, date = registration.dates[t], age = "A00-A04", gender = "M", C.t.d = 0))
    }
    
    data.t <- complete(data.t, date, districtId, age, gender, fill = list(C.t.d = 0)) %>%
      mutate(districtId = as.numeric(districtId),
             age = as.factor(age),
             pop = rep(as.vector(t(as.matrix(dplyr::select(districts, 5:16)))), num.days),
             lat = rep(rep(districts$lat, each = length(age.groups)*length(genders)), num.days),
             lon = rep(rep(districts$lon, each = length(age.groups)*length(genders)), num.days),
             density = rep(rep(districts$density, each = length(age.groups)*length(genders)), num.days),
             time = rep(time.points, each = nrow(districts)*length(genders)*length(age.groups)),
             time.recent = pmax(time - (T.max - 7), 0),
             age.80 = as.factor(1*(age == "A80+")),
             day.recent = 1*(time - (T.max - 7) > 0),
             delay = as.numeric(reporting.dates[t] - date),
             weekday = weekdays[time],
             C.t.d_1 = NA,
             N.t.d = NA
      )
    
    data.t.d <- NULL
    
    
    #this version sets C.t.d_1 = 1 + C.t.d at d = d-1 
    for (d in 1:min(d.max, t)){
      data.t.d <- bind_rows(data.t.d, 
                            filter(data.t, delay == d) %>% 
                              mutate(C.t.d_1 = switch(as.character(d), 
                                                      "1" = 1,
                                                      filter(data.long, time == t-d+1, delay == d-1)$C.t.d+1),
                                     N.t.d = switch(as.character(d),
                                                    "1" = filter(data.t, time == t, delay == d)$C.t.d,
                                                    filter(data.t, time == t-d+1, delay == d)$C.t.d - 
                                                      filter(data.long, time == t-d+1, delay == d-1)$C.t.d)))
    }
    
    data.long <- bind_rows(data.long, data.t.d) 
  }
  
  data.long$N.t.d[which(data.long$N.t.d < 0)] <- 0
  #data.long$C.t.d_1 <- pmax(data.long$C.t.d_1, 1)
  
  off <- log(data.long$pop) + log(data.long$C.t.d_1)
  data.long$offy <- off
  
  districts <- mutate(districts, 
                      land = data[[t]] %>% group_by(districtId, land) %>% 
                        summarize(sum(cases)) %>% dplyr::select(land, districtId) %>% pull(land))
  
  data.long <- left_join(data.long, districts %>% dplyr::select(districtId, land), by = "districtId") %>%
    mutate(land = factor(land))
  
  #transform variables into factors for BAM
  data.long <- mutate(data.long,delay = factor(delay),
                      weekday = factor(weekday),
                      day.recent = factor(day.recent))
  
  doa <- doa - days(k)
  data <- filter(data.long,date > doa - days(k + 1))
  
  data = group_by(data, districtId, delay, date)
  real = summarize(data, sum(N.t.d))
  real = arrange(real, date)
  real = mutate(real, time = as.numeric(as.factor(date)))
  
  #we will sum up the diagonal reported for the k days starting from doa
  diag_real_all = tibble(districtId = unique(data$districtId))
  for (p in 1:k) {
    real = filter(real, time!=1)
    real = mutate(real, time = time - 1)
    real = mutate(real, 
                  tplusd = time + as.numeric(delay))
    
    diag_real = filter(real, tplusd == 8)
    diag_real = group_by(diag_real, districtId)
    diag_real = summarize(diag_real, observed = sum(`sum(N.t.d)`))
    diag_real_all = cbind(diag_real_all,diag_real$observed)
  }
  diag_real_all$observed=rowSums(diag_real_all[,2:8]) 
  diag_real_all = tibble(districtId = diag_real_all$districtId,
                         observed = diag_real_all$observed)
  
  diag_real_all <- left_join(preprocess.districts(), 
                             diag_real_all, by = "districtId") %>% 
    replace_na(list(observed = 0))
  
  return(diag_real_all)
  
  # diag_all <- tibble(districtId = diag_real_all$districtId,
  #                    forecast = diag_pred_all$forecast,
  #                    observed = diag_real_all$observed)
  # 
  # return(diag_all) 
  
}

#newer version
forecast.cases.diagonal.new = function(k = 7, 
                                   nowcastz = nowcasts, 
                                   forecastz = forenowcasts, 
                                   modelz = model, doaz = doa, 
                                   d.maxz = d.max, T.maxz = T.max) {
  
  nowcastz <- mutate(nowcastz, forecast = nowcast)
  data <- rbind(nowcastz, forecastz)
  
  #data = group_by(data, districtId, delay, date)
  #predicted = summarize(data, sum(forecast))
  predicted = arrange(data, date)
  
  predicted = mutate(predicted, 
                     time2 = as.numeric(as.factor(date)))
  
  #we will sum up the diagonal forecasts for the next k days
  final_pred = NULL
  cum_pred_all = tibble(districtId = unique(data$districtId))
  for (j in 1:k) {
    predicted = filter(predicted, time2!=1)
    predicted = mutate(predicted, time2 = time2 - 1)
    predicted = mutate(predicted, 
                       tplusd = time2 + as.numeric(delay))
    
    diag_pred = filter(predicted, tplusd == 8)
    final_pred = rbind(final_pred,diag_pred)
    cum_pred = group_by(diag_pred, districtId)
    cum_pred = summarize(cum_pred, forecast = sum(forecast))
    cum_pred_all = cbind(cum_pred_all,cum_pred$forecast)
  }
  cum_pred_all$forecast=rowSums(cum_pred_all[,2:8]) 
  cum_pred_all = tibble(districtId = cum_pred_all$districtId,
                         forecast = cum_pred_all$forecast)
  output = list(final_pred,cum_pred_all)
  
  return(final_pred)
  
}

#function for performing predictions
predict.case.model <- function(model, k){  
  
  # district data
  districts <- preprocess.districts()
  
  # take data from the last d.max days before doa
  data <- model$data %>% filter(time > model$T.max - model$d.max)
  ###data <- dplyr::filter(data, age != "")
  
  # add data for the next k days
  data <- bind_rows(data, data[1:k, ] %>% dplyr::select(time, districtId, age, gender, delay) %>%
                      mutate(time = (max(data$time)+1):(max(data$time+k)))) %>%
    complete(time, districtId, age, gender, delay) %>%
    arrange(time, delay, districtId, age, gender) %>%
    mutate(pop = rep(as.vector(t(as.matrix(dplyr::select(districts, 5:16)))), #this needs to be 5:16 instead of 14 for al ages
                      length(unique(time))*length(unique(delay))),
           lat = rep(rep(districts$lat, each = length(unique(age))*length(unique(gender))), 
                     length(unique(time))*length(unique(delay))),
           lon = rep(rep(districts$lon, each = length(unique(age))*length(unique(gender))), 
                     length(unique(time))*length(unique(delay))),
           day.recent = 1*(time - (model$T.max - model$d.max) > 0),
           date = model$doa - days(model$T.max) + days(time-1),
           weekday = factor(weekdays.POSIXt(date, abbreviate = TRUE),
                            levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
           age = relevel(age, ref = "A35-A59"),
           pred = N.t.d)
  
  
  # filling the full diagonals (red area) with the predicted values
  for (j in (model$T.max+2):(model$T.max+k+1)) {
    # determine AR components of the diagonal
    data.j <- filter(data, time + as.numeric(delay) == j) %>%
      mutate(C.t_1.d = 1+filter(data, time + as.numeric(delay) == j-1) %>% pull(C.t.d),
             C.t.d_1 = 1+c(filter(data, time + as.numeric(delay) == j-1, as.numeric(delay) != 7) %>% pull(C.t.d),
                         rep(0, nrow(districts)*6*2))) #this was 6
    
    # predict the diagonal
    data.j <- mutate(data.j,  pred = predict.bam(model, 
                                newdata = data.j %>% 
                                  mutate(time = pmin(time, model$T.max)),
                                type = "response"))
    
    # update the C.t.d
    data.j <- mutate(data.j, C.t.d = c(filter(data, time + as.numeric(delay) == j-1, as.numeric(delay) != 7) %>% pull(C.t.d), 
                       rep(0, nrow(districts)*6*2)) + pred) #this was 6
    
    # save in the complete data
    data[which(data$time + as.numeric(data$delay) == j), ] <- data.j
  }
  
  # filling the rest of the blue diagonal
  d <- 0
  for (j in (model$T.max+k+2):(model$T.max+k+d.max)) {
    
    d <- d+1
    # determine AR components of the diagonal
    data.j <- filter(data, time + as.numeric(delay) == j) %>%
      mutate(C.t_1.d = 1+filter(data, time + as.numeric(delay) == j-1, as.numeric(delay) > d) %>% pull(C.t.d),
             C.t.d_1 = 1+filter(data, time + as.numeric(delay) == j-1, as.numeric(delay) != 7) %>% pull(C.t.d))
    
    # predict the diagonal
    data.j <- mutate(data.j,  pred = predict.bam(model, 
                                                 newdata = data.j %>% 
                                                   mutate(time = pmin(time, model$T.max)),
                                                 type = "response"))
    # update the C.t.d
    data.j <- mutate(data.j, C.t.d = filter(data, time + as.numeric(delay) == j-1, as.numeric(delay) != 7) %>% pull(C.t.d) + pred)
    
    # save in the complete data
    data[which(data$time + as.numeric(data$delay) == j), ] <- data.j
  }
  
  #single age group? if so, uncomment and fill the desired age
  #data <- filter(data, age == "A00-A04")
  
  data.nowcast <- filter(data, time <= model$T.max) 
  data.forecast <- filter(data, time+as.numeric(delay) > model$T.max+1, 
                          time+as.numeric(delay) <= model$T.max+k+1) 
  data.forenowcast <- filter(data, time > model$T.max)
  
  #single age group? if so, uncomment and fill the desired age
  #data <- filter(data, age == "A80+")

  
  
  pred.nowcast <- data.nowcast %>% group_by(districtId) %>% summarize(nowcast = sum(pred)) %>% ungroup() %>% pull(nowcast)
  pred.forecast <- data.forecast %>% group_by(districtId) %>% summarize(forecast = sum(pred)) %>% ungroup() %>% pull(forecast)
  pred.forenowcast <- data.forenowcast %>% group_by(districtId) %>% summarize(forenowcast = sum(pred)) %>% ungroup() %>% pull(forenowcast)
  
  predictions <- tibble(
    name = rep(districts$name, 3),
    districtId = rep(districts$districtId, 3),
    pred = c(pred.nowcast, pred.forecast, pred.forenowcast),
    kind = c(rep("nowcast", nrow(districts)), rep("forecast", nrow(districts)), rep("forenowcast", nrow(districts)))
  )
  
  return(predictions)
}  


predict.case.model.single <- function(model, k){  
  
  # district data
  districts <- preprocess.districts()
  
  # take data from the last d.max days before doa
  data <- model$data %>% filter(time > model$T.max - model$d.max)
  ###data <- dplyr::filter(data, age != "")
  
  # add data for the next k days
  data <- bind_rows(data, data[1:k, ] %>% dplyr::select(time, districtId, age, gender, delay) %>%
                      mutate(time = (max(data$time)+1):(max(data$time+k)))) %>%
    complete(time, districtId, age, gender, delay) %>%
    arrange(time, delay, districtId, age, gender) %>%
    mutate(pop = rep(as.vector(t(as.matrix(dplyr::select(districts, 15:16)))), #this needs to be 5:16 instead of 14 for al ages
                     length(unique(time))*length(unique(delay))),
           lat = rep(rep(districts$lat, each = length(unique(age))*length(unique(gender))), 
                     length(unique(time))*length(unique(delay))),
           lon = rep(rep(districts$lon, each = length(unique(age))*length(unique(gender))), 
                     length(unique(time))*length(unique(delay))),
           day.recent = 1*(time - (model$T.max - model$d.max) > 0),
           date = model$doa - days(model$T.max) + days(time-1),
           weekday = factor(weekdays.POSIXt(date, abbreviate = TRUE),
                            levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
           #age = relevel(age, ref = "A35-A59"),
           pred = N.t.d)
  
  
  # filling the full diagonals (red area) with the predicted values
  for (j in (model$T.max+2):(model$T.max+k+1)) {
    # determine AR components of the diagonal
    data.j <- filter(data, time + as.numeric(delay) == j) %>%
      mutate(C.t_1.d = 1+filter(data, time + as.numeric(delay) == j-1) %>% pull(C.t.d),
             C.t.d_1 = 1+c(filter(data, time + as.numeric(delay) == j-1, as.numeric(delay) != 7) %>% pull(C.t.d),
                           rep(0, nrow(districts)*1*2))) #this was 6
    
    # predict the diagonal
    data.j <- mutate(data.j,  pred = predict.bam(model, 
                                                 newdata = data.j %>% 
                                                   mutate(time = pmin(time, model$T.max)),
                                                 type = "response"))
    
    # update the C.t.d
    data.j <- mutate(data.j, C.t.d = c(filter(data, time + as.numeric(delay) == j-1, as.numeric(delay) != 7) %>% pull(C.t.d), 
                                       rep(0, nrow(districts)*1*2)) + pred) #this was 6
    
    # save in the complete data
    data[which(data$time + as.numeric(data$delay) == j), ] <- data.j
  }
  
  # filling the rest of the blue diagonal
  d <- 0
  for (j in (model$T.max+k+2):(model$T.max+k+d.max)) {
    
    d <- d+1
    # determine AR components of the diagonal
    data.j <- filter(data, time + as.numeric(delay) == j) %>%
      mutate(C.t_1.d = 1+filter(data, time + as.numeric(delay) == j-1, as.numeric(delay) > d) %>% pull(C.t.d),
             C.t.d_1 = 1+filter(data, time + as.numeric(delay) == j-1, as.numeric(delay) != 7) %>% pull(C.t.d))
    
    # predict the diagonal
    data.j <- mutate(data.j,  pred = predict.bam(model, 
                                                 newdata = data.j %>% 
                                                   mutate(time = pmin(time, model$T.max)),
                                                 type = "response"))
    # update the C.t.d
    data.j <- mutate(data.j, C.t.d = filter(data, time + as.numeric(delay) == j-1, as.numeric(delay) != 7) %>% pull(C.t.d) + pred)
    
    # save in the complete data
    data[which(data$time + as.numeric(data$delay) == j), ] <- data.j
  }
  
  data.nowcast <- filter(data, time <= model$T.max) 
  data.forecast <- filter(data, time+as.numeric(delay) > model$T.max+1, 
                          time+as.numeric(delay) <= model$T.max+k+1) 
  data.forenowcast <- filter(data, time > model$T.max)
  
  pred.nowcast <- data.nowcast %>% group_by(districtId) %>% summarize(nowcast = sum(pred)) %>% ungroup() %>% pull(nowcast)
  pred.forecast <- data.forecast %>% group_by(districtId) %>% summarize(forecast = sum(pred)) %>% ungroup() %>% pull(forecast)
  pred.forenowcast <- data.forenowcast %>% group_by(districtId) %>% summarize(forenowcast = sum(pred)) %>% ungroup() %>% pull(forenowcast)
  
  predictions <- tibble(
    name = rep(districts$name, 3),
    districtId = rep(districts$districtId, 3),
    pred = c(pred.nowcast, pred.forecast, pred.forenowcast),
    kind = c(rep("nowcast", nrow(districts)), rep("forecast", nrow(districts)), rep("forenowcast", nrow(districts)))
  )
  
  return(predictions)
}  

predict.case.model.sim <- function(model, k, n = 100, alpha = 0.1){  
 
  # district data
  districts <- preprocess.districts()
  
  # take data from the last d.max days before doa
  data <- model$data %>% filter(time > model$T.max - model$d.max)
  
  # dispersion parameter
  theta <- model$family$getTheta(TRUE)
  
  # add data for the next k days
  data <- bind_rows(data, data[1:k, ] %>% dplyr::select(time, districtId, age, gender, delay) %>%
                      mutate(time = (max(data$time)+1):(max(data$time+k)))) %>%
    complete(time, districtId, age, gender, delay) %>%
    arrange(time, delay, districtId, age, gender) %>%
    mutate(pop = rep(as.vector(t(as.matrix(dplyr::select(districts, 5:16)))), 
                     length(unique(time))*length(unique(delay))),
           lat = rep(rep(districts$lat, each = length(unique(age))*length(unique(gender))), 
                     length(unique(time))*length(unique(delay))),
           lon = rep(rep(districts$lon, each = length(unique(age))*length(unique(gender))), 
                     length(unique(time))*length(unique(delay))),
           day.recent = 1*(time - (model$T.max - model$d.max) > 0),
           date = model$doa - days(model$T.max) + days(time-1),
           weekday = factor(weekdays.POSIXt(date, abbreviate = TRUE),
                            levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
           age = relevel(age, ref = "A35-A59"),
           pred = N.t.d,
           sim = N.t.d)
  
  sample.nowcast <- matrix(0, n, nrow(districts))
  sample.forecast <- matrix(0, n, nrow(districts))
  sample.forenowcast <- matrix(0, n, nrow(districts))
  
  for (i in 1:n) {
    print(i)
    data.i <- data
    
    # filling the full diagonals (red area) with the predicted values
    for (j in (model$T.max+2):(model$T.max+k+1)) {
      # determine AR components of the diagonal
      data.j <- filter(data.i, time + as.numeric(delay) == j) %>%
        mutate(C.t_1.d = 1+filter(data.i, time + as.numeric(delay) == j-1) %>% pull(C.t.d),
               C.t.d_1 = 1+c(filter(data.i, time + as.numeric(delay) == j-1, as.numeric(delay) != 7) %>% pull(C.t.d),
                             rep(0, nrow(districts)*6*2)))
      
      # determine expected values of the diagonal and simulate from the nb distribution
      data.j <- mutate(data.j,  pred = predict.bam(model, 
                                                   newdata = data.j %>% 
                                                     mutate(time = pmin(time, model$T.max)),
                                                   type = "response"),
                       sim = rnegbin(n = nrow(data.j), mu = pred, theta = theta))
      
      # update the C.t.d
      data.j <- mutate(data.j, C.t.d = c(filter(data.i, time + as.numeric(delay) == j-1, as.numeric(delay) != 7) %>% pull(C.t.d), 
                                         rep(0, nrow(districts)*6*2)) + sim)
      
      # save in the complete data
      data.i[which(data.i$time + as.numeric(data.i$delay) == j), ] <- data.j
    }
    
    # filling the rest of the blue diagonal
    d <- 0
    for (j in (model$T.max+k+2):(model$T.max+k+d.max)) {
      
      d <- d+1
      # determine AR components of the diagonal
      data.j <- filter(data.i, time + as.numeric(delay) == j) %>%
        mutate(C.t_1.d = 1+filter(data.i, time + as.numeric(delay) == j-1, as.numeric(delay) > d) %>% pull(C.t.d),
               C.t.d_1 = 1+filter(data.i, time + as.numeric(delay) == j-1, as.numeric(delay) != 7) %>% pull(C.t.d))
      
      # predict the diagonal
      data.j <- mutate(data.j,  pred = predict.bam(model, 
                                                   newdata = data.j %>% 
                                                     mutate(time = pmin(time, model$T.max)),
                                                   type = "response"),
                       sim = rnegbin(n = nrow(data.j), mu = pred, theta = theta))
      
      # update the C.t.d
      data.j <- mutate(data.j, C.t.d = filter(data.i, time + as.numeric(delay) == j-1, as.numeric(delay) != 7) %>% pull(C.t.d) + sim)
      
      # save in the complete data
      data.i[which(data.i$time + as.numeric(data.i$delay) == j), ] <- data.j
    }
    
    data.nowcast <- filter(data.i, time <= model$T.max) 
    data.forecast <- filter(data.i, time+as.numeric(delay) > model$T.max+1, 
                            time+as.numeric(delay) <= model$T.max+k+1) 
    data.forenowcast <- filter(data.i, time > model$T.max)
    
    sample.nowcast[i, ] <- data.nowcast %>% group_by(districtId) %>% summarize(nowcast = sum(sim)) %>% ungroup() %>% pull(nowcast)
    sample.forecast[i, ] <- data.forecast %>% group_by(districtId) %>% summarize(forecast = sum(sim)) %>% ungroup() %>% pull(forecast)
    sample.forenowcast[i, ] <- data.forenowcast %>% group_by(districtId) %>% summarize(forenowcast = sum(sim)) %>% ungroup() %>% pull(forenowcast)
    
  }

  results.nowcast <- tibble(
    name = districts$name,
    districtId = districts$districtId,
    estimate = colMeans(sample.nowcast),
    lower = colQuantiles(sample.nowcast, probs = alpha/2),
    upper = colQuantiles(sample.nowcast, probs = 1-alpha/2)
  )
  
  results.forecast <- tibble(
    name = districts$name,
    districtId = districts$districtId,
    estimate = colMeans(sample.forecast),
    lower = colQuantiles(sample.forecast, probs = alpha/2),
    upper = colQuantiles(sample.forecast, probs = 1-alpha/2)
  )
  
  results.forenowcast <- tibble(
    name = districts$name,
    districtId = districts$districtId,
    estimate = colMeans(sample.forenowcast),
    lower = colQuantiles(sample.forenowcast, probs = alpha/2),
    upper = colQuantiles(sample.forenowcast, probs = 1-alpha/2)
  )
  
  results <- bind_rows(results.nowcast, results.forecast, results.forenowcast) %>%
    mutate(kind = c(rep("nowcast", nrow(districts)), 
                    rep("forecast", nrow(districts)), 
                    rep("forenowcast", nrow(districts))))
  
  return(results)
  
}

predict.case.model.parallel <- function(model, k, n, alpha = 0.1){
  
  # district data
  districts <- preprocess.districts()
  
  # take data from the last d.max days before doa
  data <- model$data %>% filter(time > model$T.max - model$d.max)
  
  # dispersion parameter
  theta <- model$family$getTheta(TRUE)
  
  # add data for the next k days
  data <- bind_rows(data, data[1:k, ] %>% dplyr::select(time, districtId, age, gender, delay) %>%
                      mutate(time = (max(data$time)+1):(max(data$time+k)))) %>%
    complete(time, districtId, age, gender, delay) %>%
    arrange(time, delay, districtId, age, gender) %>%
    mutate(pop = rep(as.vector(t(as.matrix(dplyr::select(districts, 5:16)))), 
                     length(unique(time))*length(unique(delay))),
           lat = rep(rep(districts$lat, each = length(unique(age))*length(unique(gender))), 
                     length(unique(time))*length(unique(delay))),
           lon = rep(rep(districts$lon, each = length(unique(age))*length(unique(gender))), 
                     length(unique(time))*length(unique(delay))),
           day.recent = 1*(time - (model$T.max - model$d.max) > 0),
           date = model$doa - days(model$T.max) + days(time-1),
           weekday = factor(weekdays.POSIXt(date, abbreviate = TRUE),
                            levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
           age = relevel(age, ref = "A35-A59"),
           pred = N.t.d,
           sim = N.t.d)
  
  t.start <- Sys.time()
  no.cores = detectCores() - 2
  print(no.cores)
  cl = makeCluster(no.cores)
  
  clusterEvalQ(cl, library(tidyverse))
  clusterEvalQ(cl, library(mgcv))
  clusterEvalQ(cl, library(MASS))
  
  samples <- parLapply(cl = cl, 1:n, predict.diagonals, model = model, data = data, T.max = T.max, d.max = d.max, k = k, theta = theta, districts = districts)
  
  stopCluster(cl)
  print(Sys.time() - t.start)
  
  ind.nowcast <- which(ceiling(1:length(unlist(samples))/412) %% 3 == 1)
  ind.forecast <- which(ceiling(1:length(unlist(samples))/412) %% 3 == 2)
  ind.forenowcast <- which(ceiling(1:length(unlist(samples))/412) %% 3 == 0)
  
  sample.nowcast <- matrix(unlist(samples)[ind.nowcast], ncol = nrow(districts), byrow = TRUE)
  sample.forecast <- matrix(unlist(samples)[ind.forecast], ncol = nrow(districts), byrow = TRUE)
  sample.forenowcast <- matrix(unlist(samples)[ind.forenowcast], ncol = nrow(districts), byrow = TRUE)
  
  results.nowcast <- tibble(
    name = districts$name,
    districtId = districts$districtId,
    mean = colMeans(sample.nowcast),
    lower = colQuantiles(sample.nowcast, probs = alpha/2),
    upper = colQuantiles(sample.nowcast, probs = 1-alpha/2)
  )
  
  results.forecast <- tibble(
    name = districts$name,
    districtId = districts$districtId,
    mean = colMeans(sample.forecast),
    lower = colQuantiles(sample.forecast, probs = alpha/2),
    upper = colQuantiles(sample.forecast, probs = 1-alpha/2)
  )
  
  results.forenowcast <- tibble(
    name = districts$name,
    districtId = districts$districtId,
    mean = colMeans(sample.forenowcast),
    lower = colQuantiles(sample.forenowcast, probs = alpha/2),
    upper = colQuantiles(sample.forenowcast, probs = 1-alpha/2)
  )
  
  results <- bind_rows(results.nowcast, results.forecast, results.forenowcast) %>%
    mutate(kind = c(rep("nowcast", nrow(districts)), 
                    rep("forecast", nrow(districts)), 
                    rep("forenowcast", nrow(districts))))
  
}

predict.diagonals <- function(m, model, data, T.max, d.max, k, theta, districts){
  
  # filling the full diagonals (red area) with the predicted values
  for (j in (T.max+2):(T.max+k+1)) {
    # determine AR components of the diagonal
    data.j <- filter(data, time + as.numeric(delay) == j) %>%
      mutate(C.t_1.d = 1+filter(data, time + as.numeric(delay) == j-1) %>% pull(C.t.d),
             C.t.d_1 = 1+c(filter(data, time + as.numeric(delay) == j-1, as.numeric(delay) != 7) %>% pull(C.t.d),
                           rep(0, nrow(districts)*6*2)))
    
    # determine expected values of the diagonal and simulate from the nb distribution
    data.j <- mutate(data.j,  pred = predict.bam(model, 
                                                 newdata = data.j %>% 
                                                   mutate(time = pmin(time, T.max)),
                                                 type = "response"),
                     sim = rnegbin(n = nrow(data.j), mu = pred, theta = theta))
    
    # update the C.t.d
    data.j <- mutate(data.j, C.t.d = c(filter(data, time + as.numeric(delay) == j-1, as.numeric(delay) != 7) %>% pull(C.t.d), 
                                       rep(0, nrow(districts)*6*2)) + sim)
    
    # save in the complete data
    data[which(data$time + as.numeric(data$delay) == j), ] <- data.j
  }
  
  # filling the rest of the blue diagonal
  d <- 0
  for (j in (T.max+k+2):(T.max+k+d.max)) {
    
    d <- d+1
    # determine AR components of the diagonal
    data.j <- filter(data, time + as.numeric(delay) == j) %>%
      mutate(C.t_1.d = 1+filter(data, time + as.numeric(delay) == j-1, as.numeric(delay) > d) %>% pull(C.t.d),
             C.t.d_1 = 1+filter(data, time + as.numeric(delay) == j-1, as.numeric(delay) != 7) %>% pull(C.t.d))
    
    # predict the diagonal
    data.j <- mutate(data.j,  pred = predict.bam(model, 
                                                 newdata = data.j %>% 
                                                   mutate(time = pmin(time, T.max)),
                                                 type = "response"),
                     sim = rnegbin(n = nrow(data.j), mu = pred, theta = theta))
    
    # update the C.t.d
    data.j <- mutate(data.j, C.t.d = filter(data, time + as.numeric(delay) == j-1, as.numeric(delay) != 7) %>% pull(C.t.d) + sim)
    
    # save in the complete data
    data[which(data$time + as.numeric(data$delay) == j), ] <- data.j
  }
  
  data.nowcast <- filter(data, time <= T.max) 
  data.forecast <- filter(data, time+as.numeric(delay) > T.max+1, 
                          time+as.numeric(delay) <= T.max+k+1) 
  data.forenowcast <- filter(data, time > T.max)
  
  nowcast <- data.nowcast %>% group_by(districtId) %>% summarize(nowcast = sum(sim)) %>% ungroup() %>% pull(nowcast)
  forecast <- data.forecast %>% group_by(districtId) %>% summarize(forecast = sum(sim)) %>% ungroup() %>% pull(forecast)
  forenowcast <- data.forenowcast %>% group_by(districtId) %>% summarize(forenowcast = sum(sim)) %>% ungroup() %>% pull(forenowcast)
  
  return(list(nowcast = nowcast, forecast = forecast, forenowcast = forenowcast))
}

#to fit the model without gender
fit.case.model.new.nogender = function(doa, T.max, d.max, recent, AR.d, AR.t){
  
  # preprocess district data
  districts <- preprocess.districts()
  districts$pop.0.4 <- districts$pop.m.0.4 + districts$pop.w.0.4
  districts$pop.5.14 <- districts$pop.m.5.14 + districts$pop.w.5.14
  districts$pop.15.34 <- districts$pop.m.15.34 + districts$pop.w.15.34
  districts$pop.35.59 <- districts$pop.m.35.59 + districts$pop.w.35.59
  districts$pop.60.79 <- districts$pop.m.60.79 + districts$pop.w.60.79
  districts$pop.80 <- districts$pop.m.80 + districts$pop.w.80
  
  
  
  # gender-age combinations
  age.groups <- c("A00-A04", "A05-A14", "A15-A34", "A35-A59", "A60-A79", "A80+")
  #genders <- c("M", "W")
  
  # all preprocessed RKI data sets
  files <- list.files(path= paste0(path.LRZ, "Data/Formatted"))
  # reporting date of each data set
  reporting.dates <- as.POSIXct(sub("\\..*" , "", sub("^[^2]*", "" , files)), hour = 0, tz = "GMT")
  #T.max temporarily increased by one to abtain AR(1) 
  T.max <- T.max + 1
  # the first considered date should not be before min(reporting,dates)
  T.max <- min(T.max, as.numeric(doa - min(reporting.dates)) + 1)
  # only consider data from the last T.max days
  files <- tail(files[which(doa >= reporting.dates)], T.max)
  reporting.dates <- tail(reporting.dates[which(doa >= reporting.dates)], T.max)
  
  # registration dates
  registration.dates <- doa %m+% days(x = seq.int(from = -T.max, to = -1, by = 1))
  weekdays <- weekdays.POSIXt(registration.dates, abbreviate = TRUE)
  
  
  data <- cases.by.date <- vector("list", length(files))
  names(data) <- reporting.dates
  
  data.long <- NULL
  
  t = 0
  for (file in files) {
    t = t+1
    data[[t]] <- read_rds(paste0(path.LRZ, "Data/Formatted/", file)) 
    
    data.t <- data[[t]] %>%filter(cases > 0, date >= max(registration.dates[t] - days(d.max-1), registration.dates[1]), 
                                  age != "unbekannt") %>% 
      group_by_at(vars(date, districtId, age)) %>% summarise(C.t.d = sum(cases)) %>% ungroup() 
    
    num.days <- min(d.max, t)
    time.points <- seq(max(t-d.max+1, 1), t, 1)
    
    # if not all districts have an observation 
    if (length(unique(data.t$districtId)) != nrow(districts)){
      missing.Ids <- setdiff(districts$districtId, as.numeric(unique(data.t$districtId)))
      data.t <- bind_rows(data.t, tibble(districtId = missing.Ids, date = registration.dates[t], age = "A00-A04", C.t.d = 0))
    }
    
    data.t <- complete(data.t, date, districtId, age, fill = list(C.t.d = 0)) %>%
      mutate(districtId = as.numeric(districtId),
             age = as.factor(age),
             pop = rep(as.vector(t(as.matrix(dplyr::select(districts, 21:26)))), num.days),
             lat = rep(rep(districts$lat, each = length(age.groups)), num.days),
             lon = rep(rep(districts$lon, each = length(age.groups)), num.days),
             density = rep(rep(districts$density, each = length(age.groups)), num.days),
             time = rep(time.points, each = nrow(districts)*length(age.groups)),
             time.recent = pmax(time - (T.max - d.max), 0),
             age.80 = as.factor(1*(age == "A80+")),
             day.recent = 1*(time - (T.max - recent) > 0),
             delay = as.numeric(reporting.dates[t] - date),
             weekday = weekdays[time],
             C.t.d_1 = NA,
             N.t.d = NA
            
      )
    
    data.t.d <- NULL
    
    
    #this version sets C.t.d_1 = 1 + C.t.d at d = d-1 
    for (d in 1:min(d.max, t)){
      data.t.d <- bind_rows(data.t.d, 
                            filter(data.t, delay == d) %>% 
                              mutate(C.t.d_1 = switch(as.character(d), 
                                                      "1" = 1,
                                                      filter(data.long, time == t-d+1, delay == d-1)$C.t.d+1),
                                     N.t.d = switch(as.character(d),
                                                    "1" = filter(data.t, time == t, delay == d)$C.t.d,
                                                    filter(data.t, time == t-d+1, delay == d)$C.t.d - 
                                                      filter(data.long, time == t-d+1, delay == d-1)$C.t.d)))
    }
    
    data.long <- bind_rows(data.long, data.t.d) 
  }
  
  data.long$N.t.d[which(data.long$N.t.d < 0)] <- 0
  #data.long$C.t.d_1 <- pmax(data.long$C.t.d_1, 1)
  
  off <- log(data.long$pop) + log(data.long$C.t.d_1)
  data.long$offy <- off
  
  #districts <- mutate(districts, 
  #                    land = data[[t]] %>% group_by(districtId, land) %>% 
  #                      summarize(sum(cases)) %>% dplyr::select(land, districtId) %>% pull(land))
  
  #data.long <- left_join(data.long, districts %>% dplyr::select(districtId, land), by = "districtId") %>%
  #  mutate(land = factor(land))
  
  #transform variables into factors for BAM
  data.long <- mutate(data.long,
                      delay = factor(delay),
                      weekday = factor(weekday),
                      day.recent = factor(day.recent))
  
  #set monday as reference category for weekday
  data.long <- mutate(data.long, weekday = relevel(weekday, ref = "Mon"))
  
  #arrange dataset to accomodate for AR(1)  
  data.long <- arrange(data.long, delay, districtId, age, date)  
  #add C.t_1.d   
  data.long <- mutate(data.long, C.t_1.d = if_else(date != min(date),                                                   
                                                   1 + lag(C.t.d),                                                   
                                                   1 ))  
  
  #removing extra day (22nd) from analysis and updating time accordingly  
  data.long <- filter(data.long, date > min(date))  
  data.long <- mutate(data.long, time = time - 1)
  
  #UNCOMMENT FOR FULL MODEL
  form <- N.t.d ~ s(time, bs = "ps", k = 8) + s(lon, lat) +
    s(day.recent, districtId, bs = "re") + delay + weekday +
    age + offset(log(pop))
  
  
  if (AR.d) {
    form <- update.formula(form, ~ . + log(C.t.d_1))
  }
  if (AR.t) {
    form <- update.formula(form, ~ . + log(C.t_1.d))
  }
  
  
  #model fitting with BAM
  #added options discrete = T and nthreads = 20 to improve performance
  model <- bam(form, 
               discrete = T,
               nthreads = 20,
               data = data.long %>% 
                 mutate(
                   age = relevel(age, ref = "A35-A59"),
                   districtId = factor(districtId)), 
               family = nb)
  
  
  
  
  #attach data to model to return it
  model$data <- data.long
  model$registration.dates <- registration.dates[2:T.max]
  model$doa <- doa
  model$T.max <- T.max - 1
  model$d.max <- d.max
  
  
  saveRDS(model, paste0(path.LRZ, "Output/cases/",
                        doa, ".Rds"))
  
  
  
  
  return(model)
  
}


predict.case.model.nogender <- function(model, k){  
  
  # district data
  districts <- preprocess.districts()
  districts$pop.0.4 <- districts$pop.m.0.4 + districts$pop.w.0.4
  districts$pop.5.14 <- districts$pop.m.5.14 + districts$pop.w.5.14
  districts$pop.15.34 <- districts$pop.m.15.34 + districts$pop.w.15.34
  districts$pop.35.59 <- districts$pop.m.35.59 + districts$pop.w.35.59
  districts$pop.60.79 <- districts$pop.m.60.79 + districts$pop.w.60.79
  districts$pop.80 <- districts$pop.m.80 + districts$pop.w.80
  
  # take data from the last d.max days before doa
  data <- model$data %>% filter(time > model$T.max - model$d.max)
  ###data <- dplyr::filter(data, age != "")
  
  # add data for the next k days
  data <- bind_rows(data, data[1:k, ] %>% dplyr::select(time, districtId, age, delay) %>%
                      mutate(time = (max(data$time)+1):(max(data$time+k)))) %>%
    complete(time, districtId, age, delay) %>%
    arrange(time, delay, districtId, age) %>%
    mutate(pop = rep(as.vector(t(as.matrix(dplyr::select(districts, 21:26)))), #this needs to be 5:16 instead of 14 for al ages
                     length(unique(time))*length(unique(delay))),
           lat = rep(rep(districts$lat, each = length(unique(age))), 
                     length(unique(time))*length(unique(delay))),
           lon = rep(rep(districts$lon, each = length(unique(age))), 
                     length(unique(time))*length(unique(delay))),
           day.recent = 1*(time - (model$T.max - model$d.max) > 0),
           date = model$doa - days(model$T.max) + days(time-1),
           weekday = factor(weekdays.POSIXt(date, abbreviate = TRUE),
                            levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
           age = relevel(age, ref = "A35-A59"),
           pred = N.t.d)
  
  
  # filling the full diagonals (red area) with the predicted values
  for (j in (model$T.max+2):(model$T.max+k+1)) {
    # determine AR components of the diagonal
    data.j <- filter(data, time + as.numeric(delay) == j) %>%
      mutate(C.t_1.d = 1+filter(data, time + as.numeric(delay) == j-1) %>% pull(C.t.d),
             C.t.d_1 = 1+c(filter(data, time + as.numeric(delay) == j-1, as.numeric(delay) != 7) %>% pull(C.t.d),
                           rep(0, nrow(districts)*6))) #this was 6
    
    # predict the diagonal
    data.j <- mutate(data.j,  pred = predict.bam(model, 
                                                 newdata = data.j %>% 
                                                   mutate(time = pmin(time, model$T.max)),
                                                 type = "response"))
    
    # update the C.t.d
    data.j <- mutate(data.j, C.t.d = c(filter(data, time + as.numeric(delay) == j-1, as.numeric(delay) != 7) %>% pull(C.t.d), 
                                       rep(0, nrow(districts)*6)) + pred) #this was 6
    
    # save in the complete data
    data[which(data$time + as.numeric(data$delay) == j), ] <- data.j
  }
  
  # filling the rest of the blue diagonal
  d <- 0
  for (j in (model$T.max+k+2):(model$T.max+k+d.max)) {
    
    d <- d+1
    # determine AR components of the diagonal
    data.j <- filter(data, time + as.numeric(delay) == j) %>%
      mutate(C.t_1.d = 1+filter(data, time + as.numeric(delay) == j-1, as.numeric(delay) > d) %>% pull(C.t.d),
             C.t.d_1 = 1+filter(data, time + as.numeric(delay) == j-1, as.numeric(delay) != 7) %>% pull(C.t.d))
    
    # predict the diagonal
    data.j <- mutate(data.j,  pred = predict.bam(model, 
                                                 newdata = data.j %>% 
                                                   mutate(time = pmin(time, model$T.max)),
                                                 type = "response"))
    # update the C.t.d
    data.j <- mutate(data.j, C.t.d = filter(data, time + as.numeric(delay) == j-1, as.numeric(delay) != 7) %>% pull(C.t.d) + pred)
    
    # save in the complete data
    data[which(data$time + as.numeric(data$delay) == j), ] <- data.j
  }
  
  #single age group? if so, uncomment and fill the desired age
  #data <- filter(data, age == "A00-A04")
  
  data.nowcast <- filter(data, time <= model$T.max) 
  data.forecast <- filter(data, time+as.numeric(delay) > model$T.max+1, 
                          time+as.numeric(delay) <= model$T.max+k+1) 
  data.forenowcast <- filter(data, time > model$T.max)
  
  #single age group? if so, uncomment and fill the desired age
  #data <- filter(data, age == "A80+")
  
  
  
  pred.nowcast <- data.nowcast %>% group_by(districtId) %>% summarize(nowcast = sum(pred)) %>% ungroup() %>% pull(nowcast)
  pred.forecast <- data.forecast %>% group_by(districtId) %>% summarize(forecast = sum(pred)) %>% ungroup() %>% pull(forecast)
  pred.forenowcast <- data.forenowcast %>% group_by(districtId) %>% summarize(forenowcast = sum(pred)) %>% ungroup() %>% pull(forenowcast)
  
  predictions <- tibble(
    name = rep(districts$name, 3),
    districtId = rep(districts$districtId, 3),
    pred = c(pred.nowcast, pred.forecast, pred.forenowcast),
    kind = c(rep("nowcast", nrow(districts)), rep("forecast", nrow(districts)), rep("forenowcast", nrow(districts)))
  )
  
  return(predictions)
} 

predict.case.model.parallel.nogender <- function(model, k, n, alpha = 0.1){
  
  # district data
  districts <- preprocess.districts()
  districts$pop.0.4 <- districts$pop.m.0.4 + districts$pop.w.0.4
  districts$pop.5.14 <- districts$pop.m.5.14 + districts$pop.w.5.14
  districts$pop.15.34 <- districts$pop.m.15.34 + districts$pop.w.15.34
  districts$pop.35.59 <- districts$pop.m.35.59 + districts$pop.w.35.59
  districts$pop.60.79 <- districts$pop.m.60.79 + districts$pop.w.60.79
  districts$pop.80 <- districts$pop.m.80 + districts$pop.w.80
  
  # take data from the last d.max days before doa
  data <- model$data %>% filter(time > model$T.max - model$d.max)
  
  # dispersion parameter
  theta <- model$family$getTheta(TRUE)
  
  # add data for the next k days
  data <- bind_rows(data, data[1:k, ] %>% dplyr::select(time, districtId, age, delay) %>%
                      mutate(time = (max(data$time)+1):(max(data$time+k)))) %>%
    complete(time, districtId, age, delay) %>%
    arrange(time, delay, districtId, age) %>%
    mutate(pop = rep(as.vector(t(as.matrix(dplyr::select(districts, 21:26)))), 
                     length(unique(time))*length(unique(delay))),
           lat = rep(rep(districts$lat, each = length(unique(age))), 
                     length(unique(time))*length(unique(delay))),
           lon = rep(rep(districts$lon, each = length(unique(age))), 
                     length(unique(time))*length(unique(delay))),
           day.recent = 1*(time - (model$T.max - model$d.max) > 0),
           date = model$doa - days(model$T.max) + days(time-1),
           weekday = factor(weekdays.POSIXt(date, abbreviate = TRUE),
                            levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
           age = relevel(age, ref = "A35-A59"),
           pred = N.t.d,
           sim = N.t.d)
  
  t.start <- Sys.time()
  no.cores = detectCores() - 2
  print(no.cores)
  cl = makeCluster(no.cores)
  
  clusterEvalQ(cl, library(tidyverse))
  clusterEvalQ(cl, library(mgcv))
  clusterEvalQ(cl, library(MASS))
  
  samples <- parLapply(cl = cl, 1:n, predict.diagonals.nogender, model = model, data = data, T.max = T.max, d.max = d.max, k = k, theta = theta, districts = districts)
  
  stopCluster(cl)
  print(Sys.time() - t.start)
  
  ind.nowcast <- which(ceiling(1:length(unlist(samples))/412) %% 3 == 1)
  ind.forecast <- which(ceiling(1:length(unlist(samples))/412) %% 3 == 2)
  ind.forenowcast <- which(ceiling(1:length(unlist(samples))/412) %% 3 == 0)
  
  sample.nowcast <- matrix(unlist(samples)[ind.nowcast], ncol = nrow(districts), byrow = TRUE)
  sample.forecast <- matrix(unlist(samples)[ind.forecast], ncol = nrow(districts), byrow = TRUE)
  sample.forenowcast <- matrix(unlist(samples)[ind.forenowcast], ncol = nrow(districts), byrow = TRUE)
  
  results.nowcast <- tibble(
    name = districts$name,
    districtId = districts$districtId,
    mean = colMeans(sample.nowcast),
    lower = colQuantiles(sample.nowcast, probs = alpha/2),
    upper = colQuantiles(sample.nowcast, probs = 1-alpha/2)
  )
  
  results.forecast <- tibble(
    name = districts$name,
    districtId = districts$districtId,
    mean = colMeans(sample.forecast),
    lower = colQuantiles(sample.forecast, probs = alpha/2),
    upper = colQuantiles(sample.forecast, probs = 1-alpha/2)
  )
  
  results.forenowcast <- tibble(
    name = districts$name,
    districtId = districts$districtId,
    mean = colMeans(sample.forenowcast),
    lower = colQuantiles(sample.forenowcast, probs = alpha/2),
    upper = colQuantiles(sample.forenowcast, probs = 1-alpha/2)
  )
  
  results <- bind_rows(results.nowcast, results.forecast, results.forenowcast) %>%
    mutate(kind = c(rep("nowcast", nrow(districts)), 
                    rep("forecast", nrow(districts)), 
                    rep("forenowcast", nrow(districts))))
  
}


predict.diagonals.nogender <- function(m, model, data, T.max, d.max, k, theta, districts){
  
  # filling the full diagonals (red area) with the predicted values
  for (j in (T.max+2):(T.max+k+1)) {
    # determine AR components of the diagonal
    data.j <- filter(data, time + as.numeric(delay) == j) %>%
      mutate(C.t_1.d = 1+filter(data, time + as.numeric(delay) == j-1) %>% pull(C.t.d),
             C.t.d_1 = 1+c(filter(data, time + as.numeric(delay) == j-1, as.numeric(delay) != 7) %>% pull(C.t.d),
                           rep(0, nrow(districts)*6)))
    
    # determine expected values of the diagonal and simulate from the nb distribution
    data.j <- mutate(data.j,  pred = predict.bam(model, 
                                                 newdata = data.j %>% 
                                                   mutate(time = pmin(time, T.max)),
                                                 type = "response"),
                     sim = rnegbin(n = nrow(data.j), mu = pred, theta = theta))
    
    # update the C.t.d
    data.j <- mutate(data.j, C.t.d = c(filter(data, time + as.numeric(delay) == j-1, as.numeric(delay) != 7) %>% pull(C.t.d), 
                                       rep(0, nrow(districts)*6)) + sim)
    
    # save in the complete data
    data[which(data$time + as.numeric(data$delay) == j), ] <- data.j
  }
  
  # filling the rest of the blue diagonal
  d <- 0
  for (j in (T.max+k+2):(T.max+k+d.max)) {
    
    d <- d+1
    # determine AR components of the diagonal
    data.j <- filter(data, time + as.numeric(delay) == j) %>%
      mutate(C.t_1.d = 1+filter(data, time + as.numeric(delay) == j-1, as.numeric(delay) > d) %>% pull(C.t.d),
             C.t.d_1 = 1+filter(data, time + as.numeric(delay) == j-1, as.numeric(delay) != 7) %>% pull(C.t.d))
    
    # predict the diagonal
    data.j <- mutate(data.j,  pred = predict.bam(model, 
                                                 newdata = data.j %>% 
                                                   mutate(time = pmin(time, T.max)),
                                                 type = "response"),
                     sim = rnegbin(n = nrow(data.j), mu = pred, theta = theta))
    
    # update the C.t.d
    data.j <- mutate(data.j, C.t.d = filter(data, time + as.numeric(delay) == j-1, as.numeric(delay) != 7) %>% pull(C.t.d) + sim)
    
    # save in the complete data
    data[which(data$time + as.numeric(data$delay) == j), ] <- data.j
  }
  
  data.nowcast <- filter(data, time <= T.max) 
  data.forecast <- filter(data, time+as.numeric(delay) > T.max+1, 
                          time+as.numeric(delay) <= T.max+k+1) 
  data.forenowcast <- filter(data, time > T.max)
  
  nowcast <- data.nowcast %>% group_by(districtId) %>% summarize(nowcast = sum(sim)) %>% ungroup() %>% pull(nowcast)
  forecast <- data.forecast %>% group_by(districtId) %>% summarize(forecast = sum(sim)) %>% ungroup() %>% pull(forecast)
  forenowcast <- data.forenowcast %>% group_by(districtId) %>% summarize(forenowcast = sum(sim)) %>% ungroup() %>% pull(forenowcast)
  
  return(list(nowcast = nowcast, forecast = forecast, forenowcast = forenowcast))
}



prediction.intervals <- function(model, data, districts, type, n = 1000, alpha = 0.05){
  
  theta <- model$family$getTheta(TRUE)
  lower <- upper <- rep(0, nrow(districts))
  sample <- matrix(NA, n, nrow(districts)) 
  for (j in 1:n) {
    sample[j, ] <- data %>% filter(determ == 0) %>% 
      mutate(sim = rnegbin(length(which(determ == 0)), mu = pred[which(determ == 0)], theta = theta)) %>%
      group_by(districtId) %>% 
      summarize(sim = sum(sim)) %>% 
      ungroup() %>% 
      pull(sim)
    if (type == "nowcast"){
      sample[j, ] <- sample[j, ] + data %>% filter(determ == 1) %>% 
        group_by(districtId) %>%
        summarize(obs = sum(N.t.d)) %>%
        ungroup() %>%
        pull(obs)
    }
  }
  Q <- colQuantiles(sample, probs = c(alpha/2, 1-alpha/2))
  districts <- mutate(districts, lower.per.100k = Q[, 1]/pop*100000, upper.per.100k = Q[, 2]/pop*100000)
  
  return(districts)
}

probability.threshold <- function(model, data, n = 1000, threshold = 35){
  
  districts <- preprocess.districts()
  
  theta <- model$family$getTheta(TRUE)
  lower <- upper <- rep(0, nrow(districts))
  sample <- matrix(NA, n, nrow(districts)) 
  for (j in 1:n) {
    sample[j, ] <- data %>% mutate(sim = rnegbin(nrow(data), mu = pred, theta = theta)) %>%
      group_by(districtId) %>% summarize(sim = sum(sim)) %>% ungroup() %>% pull(sim)
  }
  
  P <- rep(0, nrow(districts))
  for (j in 1:nrow(districts)) {
    P[j] <- mean(1*(sample[, j]/districts$pop[j]*100000 > threshold)) 
  }
  
  return(P)
}

get.metrics <- function(model, data, metric, type, k, threshold = 35){
  assert_choice(metric, choices = c("MSE", "AUC"))
  assert_choice(type, choices = c("nowcast", "forecast", "forenowcast", 
                                  "AR-nowcast", "AR-forecast", "AR-forenowcast"))
  
  data.type <- switch(type,
                      "nowcast" = filter(data, time <= model$T.max),
                      "forecast" = filter(data, time+as.numeric(delay) > model$T.max+1, 
                                          time+as.numeric(delay) <= model$T.max+k+1),
                      "forenowcast" = filter(data, time > model$T.max),
                      data)
  
  if (substring(type, 1, 2) == "AR"){
    type = substring(type, first = 4)
  }
  
  # get district table with observed cases
  if (type == "nowcast" | type == "forenowcast" | type == "AR-nowcast" | type == "AR-forenowcast"){
    observed <- get.cases(model$doa, k, d.max, type)
  }
  if (type == "forecast" | type == "AR-forecast"){
    observed <- get.cases.diagonal(k,doa,d.max,T.max)
  }
  
  observed <- observed %>% 
    mutate(districtId = as.factor(districtId)) %>% 
    dplyr::select(districtId, name, observed, pop)
  
  by.district <- left_join(observed,
                           data.type %>% group_by(districtId) %>% summarize(predicted = sum(pred)) %>% ungroup(), 
                           by = "districtId") %>%
    mutate(observed.per.100k = observed/pop*100000,
           predicted.per.100k = predicted/pop*100000)
  
  if (metric == "AUC"){
    by.district <- mutate(by.district, obs.above.th = 1*(observed.per.100k >= threshold), 
                          pred.above.th = probability.threshold(model, data.type))
    AUC <- as.numeric(auc(response = by.district$obs.above.th, predictor = by.district$pred.above.th))
    return(AUC)
  }
  
  if (metric == "MSE"){
    MSE <- mean((by.district$observed.per.100k - by.district$predicted.per.100k)^2)
    return(MSE)
  }
  
}

model.selection.recent <- function(range.doa, d.max, k, T.max, range.recent){
  # set up table which is filled with the optimal "recent" in terms of MSE and AUC for every doa
  best.recent <- tibble(doa = range.doa, nowcast = NA, forecast = NA, forenowcast = NA)
  for (i in 1:length(range.doa)) {
    doa <- range.doa[i]
    
    metrics <- list()
    metrics$forenowcast <- metrics$forecast <- metrics$nowcast <- tibble(recent = range.recent, MSE = NA, AUC = NA)
    for (recent in range.recent) {
      
      # fit model and predict the NA values
      message(paste0("Fit model for doa = ", doa, " and recent = ", recent))
      model <- fit.case.model.new(doa, T.max, d.max, recent, FALSE, FALSE)
      data.predicted <- predict.case.model(model, k)
      
      # metrics for nowcast
      metrics$nowcast$MSE[which(metrics$nowcast$recent == recent)] <- get.metrics(model, data.predicted, "MSE", type = "nowcast", k = k)
      metrics$nowcast$AUC[which(metrics$nowcast$recent == recent)] <- get.metrics(model, data.predicted, "AUC", type = "nowcast", k = k)
      
      # metrics for forecast
      metrics$forecast$MSE[which(metrics$forecast$recent == recent)] <- get.metrics(model, data.predicted, "MSE", type = "forecast", k = k)
      metrics$forecast$AUC[which(metrics$forecast$recent == recent)] <- get.metrics(model, data.predicted, "AUC", type = "forecast", k = k)
      
      # metrics for forenowcast
      metrics$forenowcast$MSE[which(metrics$forenowcast$recent == recent)] <- get.metrics(model, data.predicted, "MSE", type = "forenowcast", k = k)
      metrics$forenowcast$AUC[which(metrics$forenowcast$recent == recent)] <- get.metrics(model, data.predicted, "AUC", type = "forenowcast", k = k)
    }
    print(metrics)
    # find optimal "recent" for the doa
    best.recent$nowcast[which(best.recent$doa == doa)] <- range.recent[order(metrics$nowcast$MSE, metrics$nowcast$AUC, method = "radix", decreasing = c(FALSE, TRUE))[1]]
    best.recent$forecast[which(best.recent$doa == doa)] <- range.recent[order(metrics$forecast$MSE, metrics$forecast$AUC, method = "radix", decreasing = c(FALSE, TRUE))[1]]
    best.recent$forenowcast[which(best.recent$doa == doa)] <- range.recent[order(metrics$forenowcast$MSE, metrics$forenowcast$AUC, method = "radix", decreasing = c(FALSE, TRUE))[1]]
    
  }
  return(best.recent)
}

model.selection.AR <- function(range.doa, d.max, recent, k, T.max){
  
  best.AR <- tibble(doa = range.doa, nowcast = NA, forecast = NA, forenowcast = NA)
  
  for (i in 1:length(range.doa)) {
    doa <- range.doa[i]
    
    metrics <- list()
    metrics$forenowcast <- metrics$forecast <- metrics$nowcast <- 
      tibble(AR.d = c(FALSE, FALSE, TRUE, TRUE), 
             AR.t = c(FALSE, TRUE, FALSE, TRUE),
             MSE = NA, AUC = NA)
    
    for (j in 1:4) {
      AR.t <- metrics$nowcast$AR.d[j]
      AR.d <- metrics$nowcast$AR.t[j]
      model <- fit.case.model.new(doa, T.max, d.max, recent, AR.d, AR.t)
      
      nowcast_data <- nowcast.data(model$data, T.max, d.max)
      nowcasts <- nowcast.cases(nowcast_data, model, d.max)
      forenowcasts <- forecast.cases(k = 7, nowcasts, model, T.max, d.max)
      forecasts <- forecast.cases.diagonal.new(k, nowcasts, forenowcasts, model, doa, d.max, T.max)
      
      
      # metrics for nowcast
      metrics$nowcast$MSE[j] <- get.metrics(model, nowcasts %>% mutate(fit = nowcast), "MSE", type = "AR-nowcast", k = k)
      metrics$nowcast$AUC[j] <- get.metrics(model, nowcasts %>% mutate(fit = nowcast), "AUC", type = "AR-nowcast", k = k)
      
      # metrics for forecast
      metrics$forecast$MSE[j] <- get.metrics(model, forecasts %>% mutate(fit = forecast), "MSE", type = "AR-forecast", k = k)
      metrics$forecast$AUC[j] <- get.metrics(model, forecasts %>% mutate(fit = forecast), "AUC", type = "AR-forecast", k = k)
      
      # metrics for forenowcast
      metrics$forenowcast$MSE[j] <- get.metrics(model, forenowcasts %>% mutate(fit = forecast), "MSE", type = "AR-forenowcast", k = k)
      metrics$forenowcast$AUC[j] <- get.metrics(model, forenowcasts %>% mutate(fit = forecast), "AUC", type = "AR-forenowcast", k = k)
      
    }
    
    # find optimal "recent" for the doa
    best.AR$nowcast[which(best.AR$doa == doa)] <- order(metrics$nowcast$MSE, metrics$nowcast$AUC, method = "radix", decreasing = c(FALSE, TRUE))[1]
    best.AR$forecast[which(best.AR$doa == doa)] <- order(metrics$forecast$MSE, metrics$forecast$AUC, method = "radix", decreasing = c(FALSE, TRUE))[1]
    best.AR$forenowcast[which(best.AR$doa == doa)] <- order(metrics$forenowcast$MSE, metrics$forenowcast$AUC, method = "radix", decreasing = c(FALSE, TRUE))[1]
    
  }
  
  return(best.AR)
}