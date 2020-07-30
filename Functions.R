# This function reads the raw RKI data, gives suitable column names
# and saves the table as tibble
# Caution: The style of the RKI datasets has changed over time and probably
# will change again, i.e. new columns are added or columns are rearranged

# Input:
# - all: if TRUE, all raw data available are being read and saved,
# if FALSE (default), only datasets which have not been read yet
# will be read. Setting "all" to TRUE should be avoided.

# Output: none

read.RKI <- function(all = FALSE){
  
  # get all filenames and the respective dates from the original RKI datasets
  files.RKI <- list.files(path= paste0(path.LRZ, "Data/RKI"))
  dates.RKI <- as.POSIXct(sub("\\..*" , "", sub("^[^2]*", "" , files.RKI)), 
                          hour = 0, tz = "GMT")
  
  # get all filenames and the respective dates from the data that already has been preprocessed
  files.read <- list.files(path= paste0(path.LRZ, "Data/Formatted"))
  dates.read <- as.POSIXct(sub("\\..*" , "", sub("^[^2]*", "" , files.read)), 
                           hour = 0, tz = "GMT")
  
  if (!all){
    # only preprocess if data has not been preprocessed yet
    files.RKI <- files.RKI[which(!is.element(dates.RKI, dates.read))]
  }
  
  for (file in files.RKI) {
    # read RKI dataset and get reporting data
    data.RKI <- read_csv(file = paste0(path.LRZ, "Data/RKI/", file))
    reporting.date <- as.POSIXct(sub("\\..*" , "", sub("^[^2]*", "" , file)), 
                                 hour = 0, tz = "GMT")
    
    # number of columns in the RKI dataset
    cols <- as.character(ncol(data.RKI))
    
    # label columns names depending on cols
    names(data.RKI) <- switch(cols,
                              "18" = c("id", "landId","land","district","age","gender",
                                       "cases","deaths","date","districtId",
                                       "updated","newcase","newdeath","date.desease",
                                       "cured","newcure", "is.beginning.desease", "age2"),
                              "17" =  c("landId","land","district","age","gender",
                                        "cases","deaths","id","date","districtId",
                                        "updated","newcase","newdeath","date.desease",
                                        "cured","newcure", "is.beginning.desease"),
                              "14" = c("landId","land","district","age","gender",
                                       "cases","deaths","id","date","districtId",
                                       "updated","newcase","newdeath","date.desease"))
    
    # save prepocessed dataset
    saveRDS(data.RKI, file = paste0(path.LRZ, "Data/Formatted/cases_GermanTemporal_", 
                                    as.character(reporting.date), ".rds"))
  }
}

# This function formats the RKI data read in by read.RKI() to have the data in a
# common style

# Input: none

# Output: none

format.RKI <- function(){
  
  files.preprocessed <- list.files(path= paste0(path.LRZ, "Data/Formatted"))
  dates.preprocessed <- as.POSIXct(sub("\\..*" , "", sub("^[^2]*", "" , files.preprocessed)), 
                                   hour = 0, tz = "GMT")
  
  for (file in files.preprocessed) {
    data <- as_tibble(read_rds(paste0(path.LRZ, "Data/Formatted/", file))) %>% 
      mutate(gender = as.character(gender))
    
    # change data format of districtId from character to numeric
    if (is.character(data$districtId)){
      data$districtId <- as.numeric(data$districtId)
    }
    
    # 3152 is not a districts any more
    if (length(which(data$districtId == 3152)) > 0) {
      data.RKI <- data.RKI[-which(data.RKI$districtId == 3152), ]
    }
    
    # if date is not in desired format
    if (!is.element("POSIXct", class(data$date))){
      data <- data %>% mutate(date = as.POSIXct(x = date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%Y/%m/%d"),
                                                tz = "UTC")) %>% 
        mutate(date = as_datetime(x = date))
    }
    
    # rename age variable of necessary
    if (is.element("age_group", names(data))){
      data <- rename(data, age = age_group) %>% mutate(age = as.character(age))
    }
    
    saveRDS(data, file = paste0(path.LRZ, "Data/Formatted/", file))
  }
}

# This function creates a data frame that with one row for each district. 
# The columns contain 
# - the district names and Ids, 
# - the gender/age group specific population sizes
# - the coordinates of the centroids of the districts
# - the population density of the districts (not used in the analyses)

# Input: none

# Output: the data frame described above

preprocess.districts <- function(){
  
  # read population and coordinates of districts
  coordinates <- read_excel(paste(path.LRZ, "Data/Demographics/coordinates.xlsx", sep = ""))
  population <- read_excel(paste(path.LRZ, "Data/Demographics/population.xlsx", sep = ""))
  pop.density <- read_excel(paste(path.LRZ, "Data/Demographics/population_total.xlsx", sep = ""))
  
  districts <- tibble(districtId = as.numeric(population$districtId[seq(1, nrow(population), 2)]),
                      pop = round(population$gesamt[seq(1, nrow(population), 2)]), 
                      pop.m = round(population$gesamt[seq(2, nrow(population), 2)]), 
                      pop.f = pop - pop.m,
                      pop.m.0.4 = round(rowSums(population[seq(2, nrow(population), 2), 5:9])),
                      pop.w.0.4 = round(rowSums(population[seq(1, nrow(population), 2), 5:9])) -
                        round(rowSums(population[seq(2, nrow(population), 2), 5:9])),
                      pop.m.5.14 = round(rowSums(population[seq(2, nrow(population), 2), 10:19])),
                      pop.w.5.14 = round(rowSums(population[seq(1, nrow(population), 2), 10:19])) - 
                        round(rowSums(population[seq(2, nrow(population), 2), 10:19])),
                      pop.m.15.34 = round(rowSums(population[seq(2, nrow(population), 2), 20:39])),
                      pop.w.15.34 = round(rowSums(population[seq(1, nrow(population), 2), 20:39])) - 
                        round(rowSums(population[seq(2, nrow(population), 2), 20:39])),
                      pop.m.35.59 = round(rowSums(population[seq(2, nrow(population), 2), 40:64])),
                      pop.w.35.59 = round(rowSums(population[seq(1, nrow(population), 2), 40:64])) - 
                        round(rowSums(population[seq(2, nrow(population), 2), 40:64])),
                      pop.m.60.79 = round(rowSums(population[seq(2, nrow(population), 2), 65:84])),
                      pop.w.60.79 = round(rowSums(population[seq(1, nrow(population), 2), 65:84])) - 
                        round(rowSums(population[seq(2, nrow(population), 2), 65:84])),
                      pop.m.80 = round(rowSums(population[seq(2, nrow(population), 2), 85:95])),
                      pop.w.80 = round(rowSums(population[seq(1, nrow(population), 2), 85:95])) - 
                        round(rowSums(population[seq(2, nrow(population), 2), 85:95]))) 
  
  # add coordinate information
  districts <- districts[order(districts$districtId), ] %>% 
    mutate(name = coordinates$name, 
           lon = as.numeric(coordinates$longitude), 
           lat = as.numeric(coordinates$latitude), 
           density = pop.density$perkm2)
  
  return(districts)
}


fit.model = function(doa, T.max, d.max, recent, AR.d, AR.t){
  
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
  
  form <- N.t.d ~ s(time, bs = "ps", k = 8) + s(lon, lat) +
    s(day.recent, districtId, bs = "re") + delay + weekday + 
    age + gender + offset(log(pop))
  
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
                 mutate(age = relevel(age, ref = "A35-A59"),
                        districtId = factor(districtId)), 
               family = nb)
  
  #attach data to model to return it
  model$data <- data.long
  model$registration.dates <- registration.dates[2:T.max]
  model$doa <- doa
  model$T.max <- T.max - 1
  model$d.max <- d.max
  
  
  saveRDS(model, paste0(path.LRZ, "Output/cases/", doa, ".Rds"))
  
  return(model)
}

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

get.cases.diagonal = function(k = 7, 
                              doaz = doa, 
                              d.maxz = d.max, T.maxz = T.max) {

  
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
  
  
}

predict.cases <- function(model, k){  
  
  # district data
  districts <- preprocess.districts()
  
  # take data from the last d.max days before doa
  data <- model$data %>% filter(time > model$T.max - model$d.max)
  
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
           pred = N.t.d)
  
  
  # filling the full diagonals (red area) with the predicted values
  for (j in (model$T.max+2):(model$T.max+k+1)) {
    # determine AR components of the diagonal
    data.j <- filter(data, time + as.numeric(delay) == j) %>%
      mutate(C.t_1.d = 1+filter(data, time + as.numeric(delay) == j-1) %>% pull(C.t.d),
             C.t.d_1 = 1+c(filter(data, time + as.numeric(delay) == j-1, as.numeric(delay) != 7) %>% pull(C.t.d),
                           rep(0, nrow(districts)*6*2)))
    
    # predict the diagonal
    data.j <- mutate(data.j,  pred = predict.bam(model, 
                                                 newdata = data.j %>% 
                                                   mutate(time = pmin(time, model$T.max)),
                                                 type = "response"))
    
    # update the C.t.d
    data.j <- mutate(data.j, C.t.d = c(filter(data, time + as.numeric(delay) == j-1, as.numeric(delay) != 7) %>% pull(C.t.d), 
                                       rep(0, nrow(districts)*6*2)) + pred)
    
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

prediction.intervals <- function(model, k, n, alpha = 0.1){
  
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

plot.map <- function(data, type, limits, date, state_borders,
                     plot_title, legend_title, caption, pointers = NULL) {
  
  # Checking of inputs:
  assert_choice(x = type, choices = c("s_2", "u_r0", "u_r1"))
  
  districts <- preprocess.districts()
  
  # ggplot theme:
  theme <- theme_classic() +
    theme(text = element_text(size = 40), 
          legend.text = element_text(size = 40),
          plot.title = element_text(hjust = 0.5, size =40, face = "bold"),
          strip.text.y = element_text(size = 40), legend.text.align = 0,
          strip.placement = "outside", strip.background = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.key.size = unit(2.5, "cm"))
  
  fill <- sym(type)
  
  plot <- ggplot() +
    ggtitle(plot_title) +
    labs(caption = caption) +
    geom_polygon(data = data,
                 mapping = aes(x = long, y = lat, group = group,
                               fill = !!fill),
                 col = "azure1") +
    geom_polygon(data = data[data$type %in% c("Kreisfreie Stadt",
                                              "Stadtkreis"), ],
                 mapping = aes(x = long, y = lat, group = group, fill = !!fill),
                 col = "white") +
    geom_polygon(data = state_borders,
                 mapping = aes(x = long, y = lat, group = group),
                 col = "grey20", size = 0.75, fill = NA) + 
    geom_polygon(data = state_borders[state_borders$state %in% c("Berlin",
                                                                 "Bremen"), ],
                 mapping = aes(x = long, y = lat, group = group),
                 col = "grey20", fill = NA) +
    scale_fill_gradient2(legend_title, low = "steelblue", high = "firebrick2", limits = limits) + 
    north(data, location = "topleft", scale = 0.15) +
    scalebar(data, transform = TRUE, dist_unit = "km", dist = 100, model = "WGS84",
             location = "topright") + 
    theme 
  
  if (!is.null(pointers)){
    plot <- plot + geom_segment(data = pointers, 
                                aes(x = startpoint.lon, y = startpoint.lat, 
                                    xend = endpoint.lon, yend = endpoint.lat),
                                arrow = arrow(), size = 1.5) + 
      geom_label(data = pointers, aes(x = startpoint.lon, 
                                      y = startpoint.lat, label = name),
                 nudge_y = -0.2, label.size = 2, size = 10, color = "black",
                 label.padding = unit(0.55, "lines"), fill = "lightcyan")
  }
  
  return(plot)
}


plot.predictions <- function(model, k = 7, d.max = 7, type = "nowcast", n = 1000) {
  
  assert_choice(type, choices = c("nowcast", "forecast", "forenowcast"))
  
  predictions <- left_join(predict.cases(model, k), 
                           prediction.intervals(model, k, n = n),
                           by = c("name", "districtId", "kind")) %>%
    filter(kind == type)
  
  
  # get district table with observed cases
  if (type == "forenowcast"){
    observed <- get.cases(model$doa, k, d.max, type)
  }
  if (type == "forecast"){
    observed <- get.cases.diagonal(k,doa,d.max,T.max)
  }
  if (type == "nowcast"){
    observed <- get.cases(model$doa, k, d.max, type)
  }
  
  observed <- observed %>% 
    dplyr::select(districtId, name, observed, pop)
  
  districts <- full_join(observed, 
                         predictions, 
                         by = "districtId") %>% 
    mutate(abs_error = abs(observed - pred),
           observed.per.100k = observed/pop*100000,
           !!paste0(type, ".per.100k") := pred/pop*100000,
           lower.per.100k = lower/pop*100000,
           upper.per.100k = upper/pop*100000)
  
  y.lab <- switch(type, 
                  "nowcast" = "Nowcasted cases per 100 000 inhabitants",
                  "forecast" = "Forecasted cases per 100 000 inhabitants",
                  "forenowcast" = "Forenowcasted cases per 100 000 inhabitants")
  
  title <-switch(type,
                 "nowcast" = paste0("Finally observed vs. nowcasted cases \nfor registration dates from ", as.character(model$doa - days(d.max)), " to ", as.character(model$doa - days(1))),
                 "forecast" = paste0("Finally observed vs. forecasted cases \nfor reporting dates from ", as.character(model$doa), " to ", as.character(model$doa + days(6))),  
                 "forenowcast" = paste0("Finally observed vs. forenowcasted cases \nfor registration dates from ", as.character(model$doa), " to ", as.character(model$doa + days(6))))
  
  subtitle <-switch(type,
                    "nowcast" = paste0("Registration dates from ", 
                                       as.character(model$doa - days(d.max)), " to ", as.character(model$doa - days(1)),
                                       "\nModel includes registration dates from ", min(model$registration.dates), " until ", max(model$registration.dates)),
                    "forecast" = paste0("Reporting dates from ",
                                        as.character(model$doa), " to ", as.character(model$doa + days(6)),
                                        "\nModel includes registration dates from ", min(model$registration.dates), " until ", max(model$registration.dates)),
                    "forenowcast" = paste0("Registration dates from ",
                                           as.character(model$doa), " to ", as.character(model$doa + days(6)),
                                           "\nModel includes registration dates from ", min(model$registration.dates), " until ", max(model$registration.dates)))
  
  pdf(file = paste0(path.LRZ, "Plots/cases/", type, "/", doa, ".pdf")) 
  g <- ggplot(data = districts, aes(observed.per.100k, !!sym(paste0(type, ".per.100k")))) +
    geom_abline(intercept = 0, slope =  1, col = "grey60", linetype = "dashed", size = 1.2) +    
    geom_errorbar(data = districts, col = "grey20", aes(x = observed.per.100k, ymin = lower.per.100k, ymax = upper.per.100k), width = 0.7, alpha = 0.7) +
    geom_point(color = "black", shape = 3, size = 1.5) +
    scale_x_continuous(limits = c(0, max(districts$observed.per.100k,districts$upper.per.100k)+10)) + 
    scale_y_continuous(limits = c(0, max(districts$observed.per.100k,districts$upper.per.100k)+10)) + 
    geom_text(aes(label=ifelse((observed.per.100k>30)|(!!sym(paste0(type, ".per.100k"))>30),as.character(name.y),'')),hjust=0,vjust=-0.7, size=5 ) + 
    labs(x = "Finally observed cases per 100 000 inhabitants", y = y.lab, 
         title = title) + 
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, size = 18), 
                       plot.subtitle = element_text(hjust = 0.5),
                       axis.title = element_text(size = 18)) 
  print(g)
  dev.off()
  
  return(g)
}