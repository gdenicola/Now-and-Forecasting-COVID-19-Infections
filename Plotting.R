# This function is used inside other functions and contains the main 
# commands for plotting the maps

# Input:
# - data: a data frame which contains all the information on the district borders
# as well as the  spatial effects for each district
# - type: a character vector of length one containing one of the following values
# "m_2", "u_r", "u_rt", "fitted.deaths.per100k", "u_r0", "u_r1", 
# "u_r0_u80", "u_r1_u80", "u_r0_a80", "u_r1_a80"
# - limits: a vector of length two specifying the limits of the legend
# - state_borders: a data frame which contains the state borders of Germany
# - plot_title: the title printed at the top of the map
# - legend_title: the title printed at the top of the legend

# Output: a ggplot object

plot.map <- function(data, type, limits, date, state_borders,
                     plot_title, legend_title, caption, pointers = NULL) {
  
  # Checking of inputs:
  assert_choice(x = type, choices = c("m_2", "u_r", "u_rt", "fitted.deaths.per100k",
                                      "u_r0", "u_r1", "forecast.per.100k", "error.per.100k",
                                      "u_r0_u80", "u_r1_u80", "u_r0_a80", "u_r1_a80"))
  
  districts <- preprocess.districts()
  
  #BASE WAS ALL 45
  
  # ggplot theme:
  theme <- theme_classic() +
    theme(text = element_text(size = 55), 
          legend.text = element_text(size = 55),
          plot.title = element_text(hjust = 0.5, size =44, face = "bold"),
          strip.text.y = element_text(size = 55), legend.text.align = 0,
          strip.placement = "outside", strip.background = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.key.size = unit(2.5, "cm")) #was 2.5
  
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

# This function creates the plots of the two spatial effects 
# included in the quasi-Poisson model and saves the plots in the LRZ folder

# Input: 
# - doa: day of analysis
# - re: which random effects should be used?
# re = "joint" (default) performs the analysis of Section 4
# re = "sep" performs the analysis of Section 6.2
# - nowcast: which nowcasting results should be used for the offset?
# nowcast = "estimate" (default) uses the estimate
# nowcast = "lower" uses the alpha/2 quantile
# nowcast = "upper" uses the 1-alpha/2 quantile

# Output: none

plot.effects.cases <- function(doa){
  
  # district and state boundaries for plotting
  district_borders = read_rds(paste0(path.LRZ, "Data/Maps/district_borders.Rds"))
  state_borders = read_rds(paste0(path.LRZ, "Data/Maps/state_borders.Rds"))
  
  # get desired models
  files <- list.files(path= paste0(path.LRZ, "Output/cases"))
  file <- files[grep(paste(doa), files)]
  model <- read_rds(paste0(path.LRZ, "Output/cases/", file))  
  
  # initilaize table with district effects
  districts <- preprocess.districts()
  district.effects <- tibble(district = districts$name, 
                             districtId = districts$districtId,                            
                             m_2 = 0) 
  
  #district.effects$u_r <- model$lme$coefficients$random$districtId[, 1]
  
  
  
  #random effects (uncomment and modify once model is fitted with bam)
  u <- as.vector(plot.gam(model, select = 0)[[3]]$fit)
  district.effects$u_r0 <- u[1:412]
  district.effects$u_r1 <- u[413:824]
  
  
  
  # spatial effect
  pd <- (plot.gam(model, select = 0)[[2]]$fit)
  x <- plot.gam(model, select = 0)[[2]]$x
  y <- plot.gam(model, select = 0)[[2]]$y
  fitted.matrix <- matrix(pd, nrow = length(y), byrow = TRUE)[length(x):1, ]
  
  # # spatial effect
  #pd <- plot.gam(model$gam, select = 0)[[2]]
  # fitted.matrix <- matrix(pd$fit, nrow = length(pd$y), byrow = TRUE)[length(pd$x):1, ]
  
  for (j in 1:nrow(district.effects)) {
    distance = matrix(0, length(y), length(x))
    for (lat in 1:nrow(distance)) {
      for (lon in 1:ncol(distance)) {
        distance[lat, lon] = sum(sqrt((districts$lat[j] - y[length(y) - lat + 1])^2 + 
                                        (districts$lon[j] - x[lon])^2))
      }
    }
    ind = which(min(distance[which(!is.na(fitted.matrix)) ]) == distance, arr.ind = TRUE)
    district.effects$m_2[j] = fitted.matrix[ind]
  }
  
  # Matching of effects with dataset that contains the district borders
  data <- full_join(district_borders, district.effects)
  
  theme_set(theme_bw() + theme(panel.grid = element_blank(), 
                               plot.title = element_text(hjust = 0.5)))
  
  pointers.spatial <- data.frame(name = c("North-Rhine \nWestphalia", "Berlin"), startpoint.lon = c(6.7, 13.5), startpoint.lat = c(48.5, 50.1),
                                 endpoint.lon = c(districts$lon[110], districts$lon[325]),
                                 endpoint.lat = c(districts$lat[110], districts$lat[325]))
  
  # plot spatial effect
  directory <- paste0(path.LRZ, "Plots/cases/SpatialEffect/")
  png(file = paste0(directory, "/", doa, ".png"), 
      width = 1300, height = 1550, units = "px")
  print(plot.map(data, type = "m_2", limits = range(district.effects$m_2), #c(-2,1)
                 date = doa, state_borders = state_borders,
                 plot_title = "Spatial distribution of COVID-19 infections (log-scale)",
                 #plot_title = "Estimates of smooth spatial effect",
                 legend_title = expression(widehat(m)[2](s[r])),
                 caption = paste0("Based on data reported by the RKI on ", doa, ".\nModel includes registration dates from\n",
                                  min(model$registration.dates), " until ", max(model$registration.dates), ".")
                 #,pointers = pointers.spatial #REACTIVATE FOR PAPER
  )) 
  dev.off()
  
  
  
  # plot long term random intercept 
  directory <- paste0(path.LRZ, "Plots/cases/RandomInterceptLong/")
  png(file = paste0(directory, "/", doa, ".png"), 
      width = 1300, height = 1550, units = "px")
  print(plot.map(data, type = "u_r0", limits =  c(-1.3,2.1), #was  limits = range(district.effects$u_r0)
                 date = doa, state_borders = state_borders, 
                 plot_title = "Estimates of long-term random intercept",
                 legend_title = expression(widehat(u)[r0]),
                 caption = paste0("Based on data reported on ", doa, ".\nModel includes registration dates from\n",
                                  min(model$registration.dates), " until ", max(model$registration.dates), ".")))
  dev.off()
  
  pointers.shortterm <- data.frame(name = "Weimarer Land", startpoint.lon = 13.7, startpoint.lat = 50.1,
                                   #endpoint.lon = districts$lon[406], endpoint.lat = districts$lat[406])
                                   endpoint.lon = 11.49355, endpoint.lat = 51.02157)
  # plot short term random intercept
  directory <- paste0(path.LRZ, "Plots/cases/RandomInterceptShort/")
  png(file = paste0(directory, "/", doa, ".png"), 
      width = 1300, height = 1550, units = "px")
  print(plot.map(data, type = "u_r1", limits = range(district.effects$u_r1), #limits =  c(-1.3,2.1)
                 date = doa, state_borders = state_borders, 
                 plot_title = "Estimates of short-term random intercept",
                 legend_title = expression(widehat(u)[r1]),
                 caption = paste0("Based on data reported on ", doa, ".\nModel includes registration dates from\n",
                                  min(model$registration.dates), " until ", max(model$registration.dates), ".")
                 ,
                 pointers = pointers.shortterm))
  dev.off()
}

# This function creates the plot of cases in the reference group

# Input:
# - doa: day of analysis

# Output: none

plot.time.effect <- function(doa){
  
  theme_set(theme_bw() + theme(panel.grid = element_blank(), 
                               plot.title = element_text(hjust = 0.5)))
  
  # get the required model objects
  files <- list.files(path= paste0(path.LRZ, "Output/cases"))
  file <- files[grep(as.character(doa), files)]
  model <- read_rds(paste0(path.LRZ, "Output/cases/", file))  
  
  # data frame for plotting
  x <- plot.gam(model, select = 0)[[1]]$x
  y <- plot.gam(model, select = 0)[[1]]$fit
  se <- plot.gam(model, select = 0)[[1]]$se
  se.mult <- plot.gam(model, select = 0)[[1]]$se.mult
  
  df <- data.frame(x = x, y = y)  
  
  # confidence bands
  boundaries <- data.frame(x = x,
                           lower = df$y - se,
                           upper = df$y + se)
  
  
  pdf(file = paste0(path.LRZ, "Plots/cases/TimeEffect/", doa, ".pdf"), 
      width = 6, height = 4) 
  g <- ggplot(df, aes(x = x)) + geom_line(aes(y = y)) + 
    theme_bw(base_size = 16) + #was 18
    geom_ribbon(data = boundaries, aes(ymin = lower, ymax = upper), alpha = 0.3) +
    theme(legend.justification = c(0.99, 0.99), legend.position = c(0.99, 0.99)) + 
    theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5)) +
    labs(x = "Registration Date", y = expression(paste(hat(s)[1](t)))) + 
    scale_x_continuous(breaks = seq(1,model$T.max,by = 2), 
                       labels = as.character(substring(model$registration.dates[seq(1,length(model$registration.dates),by=2)], 6))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  print(g)
  dev.off()
}


#this function plots the smooth and random effects of the model

# three different options for age
# age = "u80"         --> age groups below 80
# age = "a80"         --> age group 80+
# age = "all"         --> all age groups

plot.effects = function(response, age, prev = "", nowcast = "", num.days = 31, latest = 1){
  
  # get desired models 
  files <- list.files(path= paste0(path.LRZ, "Output/", response))
  files <- tail(files[intersect(grep(paste(age), files), intersect(grep(paste(nowcast), files), grep(paste(prev), files)))], latest)
  
  # find commom scale for all plots
  time.effect.values <- spatial.effect.values <- intercept.values <- slope.values <- NULL
  for (file in files) {
    model = read_rds(paste0(path.LRZ, "Output/", response, "/", file))  
    
    time.effect.values <- c(time.effect.values, as.vector(plot.gam(model$gam, select = 0)[[1]]$fit))
    spatial.effect.values <- c(spatial.effect.values, as.vector(plot.gam(model$gam, select = 0)[[2]]$fit))
    intercept.values = c(intercept.values, as.numeric(model$lme$coefficients$random$districtId[, 1]))
    if (prev == "rslope" | age == "sep80"){
      slope.values = c(slope.values, as.numeric(model$lme$coefficients$random$districtId[, 2]))
    }
  }
  limits.time.effect <- range(time.effect.values) 
  limits.spatial.effect <- range(spatial.effect.values[which(!is.na(spatial.effect.values))]) 
  limits.intercept = range(intercept.values)
  if (prev == "rslope"){
    limits.slope = range(slope.values) 
  }
  if (age == "sep80"){
    limits.intercept = range(c(intercept.values, intercept.values + slope.values))
  }
  
  # district boundaries for plotting
  data_borders = preprocess.district.plots()
  plot_data = data_borders$plot_data
  state_borders = data_borders$state_borders
  
  for (file in files) {
    print(file)
    # read model
    model = read_rds(paste0(path.LRZ, "Output/", response, "/", file))  
    date.publication <- sub("\\..*" , "", sub("^[^2]*", "" , file))
    if (!is.null(model$registration.dates)){
      registration.dates <- model$registration.dates
    } else {
      registration.dates <- date(date.publication) %m+% days(x = seq.int(from = -num.days, to = -1, by = 1))
    }
    
    
    # fitted values of the spatial/random effect at the centroids coordinates #
    pd = plot.gam(model$gam, select = 0)[[2]]
    fitted.matrix = matrix(pd$fit, nrow = length(pd$y), byrow = TRUE)[length(pd$x):1, ]
    
    # spatial effect
    districts = preprocess.districts()
    district.effects = tibble(district = districts$name, districtId = districts$districtId,
                              random_intercept = as.numeric(model$lme$coefficients$random$districtId[, 1]),
                              spatial = rep(0, nrow(districts)))
    if(prev == "rslope"){
      district.effects <- mutate(district.effects, random_slope = as.numeric(model$lme$coefficients$random$districtId[, 2]))
    } else if (age == "sep80"){
      district.effects <- mutate(district.effects, random_slope = as.numeric(model$lme$coefficients$random$districtId[, 1] + model$lme$coefficients$random$districtId[, 2]))
    } else {
      district.effects <- mutate(district.effects, random_slope = 0)
    } 
    
    for (j in 1:nrow(district.effects)) {
      distance = matrix(0, length(pd$y), length(pd$x))
      for (lat in 1:nrow(distance)) {
        for (lon in 1:ncol(distance)) {
          distance[lat, lon] = sum(sqrt((districts$lat[j] - pd$y[length(pd$y) - lat + 1])^2 + 
                                          (districts$lon[j] - pd$x[lon])^2))
        }
      }
      ind = which(min(distance[which(!is.na(fitted.matrix)) ]) == distance, arr.ind = TRUE)
      district.effects$spatial[j] = fitted.matrix[ind]
    }
    district.effects$sum = district.effects$random_intercept + district.effects$spatial
    
    district.effects = district.effects[order(as.numeric(district.effects$random_intercept), decreasing = TRUE), ]
    #district.effects[, 3:6] = round(district.effects[, 3:6], 2)
    
    
    # Plots 
    # Import and preparation of effects data:
    district.effects <- district.effects %>%
      mutate(districtID = ifelse(nchar(districtId) == 4,
                                 yes = paste0("0", districtId), no = districtId),
             spatial = as.numeric(spatial),
             random_intercept = as.numeric(random_intercept),
             random_slope = as.numeric(random_slope)) %>%
      select(-districtId)
    
    # Matching of both datasets:
    plot_data_effects <- full_join(plot_data, district.effects)
    
    
    # plot day effect
    pd <- plot.gam(model$gam, select = 0)[[1]]
    s.t <- data.frame(x = pd$x, y = exp(pd$fit), lower = exp(pd$fit - pd$se), upper = exp(pd$fit + pd$se))
    
    directory <- paste0(path.LRZ, "Plots/", response, "/DayEffect/", age, "_", prev)
    dir.create(directory, showWarnings = FALSE)
    
    pdf(file = paste0(directory, "/", date.publication, ".pdf"), width = 6, height = 4) 
    g <- ggplot(s.t, aes(x = x)) + geom_line(aes(y = y)) + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) + 
      labs(x = "Registration Date", y = expression(exp(widehat(m)[1](t)))) + 
      scale_x_continuous(breaks = seq(0, max(s.t$x), 2), labels = as.character(substring(registration.dates, 6))[1+seq(0, max(s.t$x), 2)]) + 
      theme_set(theme_bw() + theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    print(g)
    dev.off()  
    
    # plot spatial effect
    directory <- paste0(path.LRZ, "Plots/", response, "/SpatialEffect/", age, "_", prev)
    dir.create(directory, showWarnings = FALSE)
    pdf(file = paste0(directory, "/", date.publication, ".pdf"), width = 7, height = 7.5) 
    print(plot.map(plot_data_effects, type = "spatial", limits = limits.spatial.effect, date = date.publication, state_borders = state_borders, plot_title = "Estimates of Smooth Spatial Effect"))
    dev.off()
    
    
    # plot random intercept
    directory <- paste0(path.LRZ, "Plots/", response, "/RandomIntercept/", age, "_", prev)
    dir.create(directory, showWarnings = FALSE)
    if (age == "sep80"){
      # combined plot for deaths and separate intercept for age groups <80 and 80+
      pdf(file = paste0(directory, "/", date.publication, ".pdf"), width = 14, height = 7.5)
      print(grid.arrange(plot.map(plot_data_effects, type = "random_intercept", limits = limits.intercept, date = date.publication, state_borders = state_borders, plot_title = "Estimates of Spatial Random Intercept \n Age Group <80"),
                         plot.map(plot_data_effects, type = "random_slope", limits = limits.intercept, date = date.publication, state_borders = state_borders, plot_title = "Estimates of Spatial Random Intercept \n Age Group 80+"),
                         nrow = 1, ncol = 2))
      dev.off()
    } else {
      # single plot if response is cases or no age separation when response is death
      pdf(file = paste0(directory, "/", date.publication, ".pdf"), width = 7, height = 7.5)
      print(plot.map(plot_data_effects, type = "random_intercept", limits = limits.intercept, date = date.publication, state_borders = state_borders, plot_title = "Estimates of Spatial Random Intercept"))
      dev.off()
    }
    
    if (prev == "rslope"){
      directory <- paste0(path.LRZ, "Plots/", response, "/RandomSlope/", age, "_", prev)
      dir.create(directory, showWarnings = FALSE)
      pdf(file = paste0(directory, "/", date.publication, ".pdf"), width = 7, height = 7.5) 
      print(plot.map(plot_data_effects, type = "random_slope", limits = limits.slope, date = date.publication, state_borders = state_borders, plot_title = "Estimates of Spatial Random Slope"))
      dev.off()
    }
  }
}

#######this function plots the errors by district on the map
errors.by.district <- data.frame(districtId = g.forenowcasts$data$districtId, 
                                 error.per.100k = g.forenowcasts$data$error.per.100k)

#plot of errors by district on the map
plot.errors <- function(errors.by.district, doa){
  
  # district and state boundaries for plotting
  district_borders = read_rds(paste0(path.LRZ, "Data/Maps/district_borders.Rds"))
  state_borders = read_rds(paste0(path.LRZ, "Data/Maps/state_borders.Rds"))
  
  # Matching of both datasets:
  plot_data_effects <- full_join(district_borders, errors.by.district)
  
  png(file = paste0(path.LRZ, "Plots/Fitted/Forenowcast", doa, ".png"), 
      width = 1300, height = 1550, units = "px") 
  print(plot.map(plot_data_effects, type = "error.per.100k", 
                 limits = range(errors.by.district$error.per.100k), 
                 date = doa, state_borders = state_borders, 
                 plot_title = "Forenowcast Errors per 100 000 Inhabitants",
                 caption = paste0("Based on data reported by the RKI on ", doa, ".\nModel includes registration dates from\n",
                                  min(model$registration.dates), " until ", max(model$registration.dates), "."),
                 legend_title = expression(RPE[r])))
  dev.off()
}

#plot.errors(errors.by.district,doa) #uncomment for plotting



#IMPORTANT:
#data_new MUST have EXACTLY k + d.max more days observed with respect to forecasts

#example: if you want to compare forecasts vs real data for the week 08.05 to 14.05, the
#doa must be 22.05 for data_new and 08.05 for forecasts (and the model of course)

#if you want to plot nowcasting results, set k=0 and use 15.05 as doa for "forecasts" 
#(and for model estimation of course) while keeping the same 22.05 for data_new.


plot.forecasts = function(forecastz = forecasts, data_newx = data_new, T.max = 21, k = 7, d.max = 7) {
  
  data_labels = read_xlsx(paste(path.LRZ,"Data/Demographics/coordinates.xlsx", sep = ""))
  
  new = filter(data_new, time < T.max - k + 1)
  new = filter(new, time > T.max - d.max - k)
  
  #forecasts_old = forecasts
  
  new = group_by(new, districtId) 
  real = summarize(new, sum(N.t.d))
  
  forecasts = group_by(forecasts, districtId)
  predicted = summarize(forecasts, sum(nowcast))
  
  pred = tibble(districtId = predicted$districtId,
                observed = real$`sum(N.t.d)`,
                predicted = predicted$`sum(nowcast)`,
                name = data_labels$name)
  pred = mutate(pred, abs_error = abs(observed-predicted) )
  
  plot = ggplot(data = pred, aes(predicted, observed)) +
    geom_point(color = "black", shape = 3, size= 1.5) +
    ggtitle("Observed vs Forecasted cases in the week from 08.05 to 14.05 with data up to 07.05") + 
    theme_bw() + 
    #xlim(0, 100) + ylim(0,100)
    geom_abline(intercept = 0, slope =  1,col = "grey", linetype = "dashed") +
    geom_text(aes(label=ifelse(observed>75,as.character(name),'')),hjust=0,vjust=0, size=3 )
  
  return(plot)
}

plot.forecasts.new <- function(model, data, k = 7, d.max = 7, type = "nowcast") {
  
  assert_choice(type, choices = c("nowcast", "forecast", "forenowcast"))
  
  data <- predict.case.model(model, k)
  data <- switch(type,
                 "nowcast" = filter(data, time <= model$T.max),
                 "forecast" = filter(data, time+as.numeric(delay) > model$T.max+1, 
                                     time+as.numeric(delay) <= model$T.max+k+1),
                 "forenowcast" = filter(data, time > model$T.max)) %>%
    mutate(determ = 0)
  
  # get district table with observed cases
  if (type == "forenowcast"){
    observed <- get.cases(model$doa, k, d.max, type)
  }
  if (type == "forecast"){
    observed <- get.cases.diagonal(k,doa,d.max,T.max)
  }
  if (type == "nowcast"){
    observed <- get.cases(model$doa, k, d.max, type)
    data$determ[which(!is.na(data$N.t.d))] <- 1
  }
  #if (type == "forenowcast"){
  #  data <- mutate(data, forenowcast = forecast)
  #}
  observed <- observed %>% 
    mutate(districtId = as.factor(districtId)) %>% 
    dplyr::select(districtId, name, observed, pop)
  
  districts <- full_join(observed, 
                         data %>% group_by(districtId) %>% summarize(!!type := sum(pred)) %>% ungroup(), 
                         by = "districtId") %>% 
    mutate(abs_error = abs(observed - !!sym(type)),
           observed.per.100k = observed/pop*100000,
           !!paste0(type, ".per.100k") := !!sym(type)/pop*100000)
  
  districts <- prediction.intervals(model, data, districts, type)
  
  y.lab <- switch(type, 
                  "nowcast" = "Nowcasted cases per 100 000 inhabitants",
                  "forecast" = "Forecasted cases per 100 000 inhabitants",
                  "forenowcast" = "Fore-nowcasted cases per 100 000 inhabitants")
  
  title <-switch(type,
                 "nowcast" = ("Observed vs. Nowcasted Cases"),
                 "forecast" = ("Observed vs. Forecasted Cases"),
                 "forenowcast" = ("Observed vs. Fore-Nowcasted Cases"))
  
  subtitle <-switch(type,
                    "nowcast" = paste0("Registration dates from ", 
                                       as.character(model$doa - days(d.max)), " to ", as.character(model$doa - days(1)),
                                       "\nPredictions performed with data up to ", as.character(model$doa - days(1))),
                    "forecast" = paste0("Reporting dates from ",
                                        as.character(model$doa), " to ", as.character(model$doa + days(6)),
                                        "\nPredictions performed with data up to ", as.character(model$doa - days(1))),
                    "forenowcast" = paste0("Registration dates from ",
                                           as.character(model$doa), " to ", as.character(model$doa + days(6)),
                                           "\nPredictions performed with data up to ", as.character(model$doa - days(1))))
  
  pdf(file = paste0(path.LRZ, "Plots/cases/", type, "/", doa, ".pdf")
      #,width = 6, height = 6
  ) 
  g <- ggplot(data = districts, aes(observed.per.100k, !!sym(paste0(type, ".per.100k")))) +
    geom_abline(intercept = 0, slope =  1, col = "grey60", linetype = "dashed", size = 1.2) +    
    geom_segment(aes(x = observed.per.100k, y = lower.per.100k, xend = observed.per.100k, yend = upper.per.100k), color = "grey50", size = 0.6, data = districts) +
    geom_point(color = "black", shape = 3, size = 1.5) +
    scale_x_continuous(limits = c(0, max(districts$observed.per.100k,districts$upper.per.100k)+10)) + 
    scale_y_continuous(limits = c(0, max(districts$observed.per.100k,districts$upper.per.100k)+10)) + 
    geom_text(aes(label=ifelse((observed.per.100k>500)|(!!sym(paste0(type, ".per.100k"))>700),as.character(name),'')),hjust=0,vjust=-0.7, size=3 ) + 
    labs(x = "Observed cases per 100 000 inhabitants", y = y.lab, 
         title = title, subtitle = subtitle) + 
    theme_bw() + theme(plot.title = element_text(hjust = 0.5), 
                       plot.subtitle = element_text(hjust = 0.5)) #+
  #xlim(0, 100) + ylim(0,100) #+
  #theme(aspect.ratio = 1)
  
  # squarePlot <- function(plt){
  #   return(plt+coord_equal()+
  #            expand_limits(x=ggplot_build(plt)$panel$ranges[[1]]$y.range,
  #                          y=ggplot_build(plt)$panel$ranges[[1]]$x.range))
  #   g <- squarePlot(g)
  #}
  print(g)
  dev.off()
  
  return(g)
}


plot.predictions <- function(model, k = 7, d.max = 7, type = "nowcast", n = 1000) {
  
  assert_choice(type, choices = c("nowcast", "forecast", "forenowcast"))
  
  predictions <- left_join(predict.case.model(model, k), 
                           predict.case.model.parallel(model, k, n = n),
                           by = c("name", "districtId", "kind")) %>%
    filter(kind == type)
  
  
  # get district table with observed cases
  if (type == "forenowcast"){
    observed <- get.cases(model$doa, k, d.max, type)
  }
  if (type == "forecast"){
    #doa <- as.POSIXct("2020-10-20", tz = "GMT") #ADD FOR PRESS RELEASE PLOT
    observed <- get.cases.diagonal(k,model$doa,d.max,T.max)
  }
  if (type == "nowcast"){
    observed <- get.cases(model$doa, k, d.max, type)
  }
  
  observed <- observed %>% 
    dplyr::select(districtId, name, observed, pop)
  
  districts <- full_join(observed, 
                         predictions, 
                         by = "districtId") %>% 
    mutate(error = (observed - pred),
           observed.per.100k = observed/pop*100000,
           !!paste0(type, ".per.100k") := pred/pop*100000,
           lower.per.100k = lower/pop*100000,
           upper.per.100k = upper/pop*100000,
           error.per.100k = observed.per.100k - pred/pop*100000
    )
  
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
  
  pdf(file = paste0(path.LRZ, "Plots/cases/", type, "/", doa, ".pdf")
      #,width = 6, height = 6
  ) 
  g <- ggplot(data = districts, aes(observed.per.100k, !!sym(paste0(type, ".per.100k")))) +
    geom_abline(intercept = 0, slope =  1, col = "grey60", linetype = "dashed", size = 1.2) +    
    geom_errorbar(data = districts, col = "grey20", aes(x = observed.per.100k, ymin = lower.per.100k, ymax = upper.per.100k), width = 0.7, alpha = 0.7) +
    geom_point(color = "black", shape = 3, size = 1.5) +
    scale_x_continuous(limits = c(0, max(districts$observed.per.100k,districts$upper.per.100k)+10)) + 
    scale_y_continuous(limits = c(0, max(districts$observed.per.100k,districts$upper.per.100k)+10)) + 
    #geom_text(aes(label=ifelse((observed.per.100k>350)&(observed.per.100k<400)|(!!sym(paste0(type, ".per.100k"))>740),as.character(name.y),'')),hjust=1.05,vjust=-0.7, size=6 ) + 
    geom_text(aes(label=ifelse((observed.per.100k>7500)|(!!sym(paste0(type, ".per.100k"))>740),as.character(name.y),'')),hjust=-0.1,vjust=-0.7, size=6 ) + 
    labs(x = "Finally observed cases per 100 000 inhabitants", y = y.lab, 
         title = title) + 
    theme_bw(base_size = 16) + theme(plot.title = element_text(hjust = 0.5,size = 17, face = "bold"), 
                                     plot.subtitle = element_text(hjust = 0.5),
                                     axis.title = element_text(size = 18)) #+
  #xlim(0, 100) + ylim(0,100) #+
  #theme(aspect.ratio = 1)
  
  # squarePlot <- function(plt){
  #   return(plt+coord_equal()+
  #            expand_limits(x=ggplot_build(plt)$panel$ranges[[1]]$y.range,
  #                          y=ggplot_build(plt)$panel$ranges[[1]]$x.range))
  #   g <- squarePlot(g)
  #}
  print(g)
  dev.off()
  
  return(g)
}


plot.predictions.nogender <- function(model, k = 7, d.max = 7, type = "nowcast", n = 1000) {
  
  assert_choice(type, choices = c("nowcast", "forecast", "forenowcast"))
  
  predictions <- left_join(predict.case.model.nogender(model, k), 
                           predict.case.model.parallel.nogender(model, k, n = n),
                           by = c("name", "districtId", "kind")) %>%
    filter(kind == type)
  
  
  # get district table with observed cases
  if (type == "forenowcast"){
    observed <- get.cases(model$doa, k, d.max, type)
  }
  if (type == "forecast"){
    #doa <- as.POSIXct("2020-10-20", tz = "GMT") #ADD FOR PRESS RELEASE PLOT
    observed <- get.cases.diagonal(k,model$doa,d.max,T.max)
  }
  if (type == "nowcast"){
    observed <- get.cases(model$doa, k, d.max, type)
  }
  
  observed <- observed %>% 
    dplyr::select(districtId, name, observed, pop)
  
  districts <- full_join(observed, 
                         predictions, 
                         by = "districtId") %>% 
    mutate(error = (observed - pred),
           observed.per.100k = observed/pop*100000,
           !!paste0(type, ".per.100k") := pred/pop*100000,
           lower.per.100k = lower/pop*100000,
           upper.per.100k = upper/pop*100000,
           error.per.100k = observed.per.100k - pred/pop*100000
    )
  
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
  
  pdf(file = paste0(path.LRZ, "Plots/cases/", type, "/", doa, ".pdf")
      #,width = 6, height = 6
  ) 
  g <- ggplot(data = districts, aes(observed.per.100k, !!sym(paste0(type, ".per.100k")))) +
    geom_abline(intercept = 0, slope =  1, col = "grey60", linetype = "dashed", size = 1.2) +    
    geom_errorbar(data = districts, col = "grey20", aes(x = observed.per.100k, ymin = lower.per.100k, ymax = upper.per.100k), width = 0.7, alpha = 0.7) +
    geom_point(color = "black", shape = 3, size = 1.5) +
    scale_x_continuous(limits = c(0, max(districts$observed.per.100k,districts$upper.per.100k)+10)) + 
    scale_y_continuous(limits = c(0, max(districts$observed.per.100k,districts$upper.per.100k)+10)) + 
    geom_text(aes(label=ifelse((observed.per.100k>71)|(!!sym(paste0(type, ".per.100k"))>740),as.character(name.y),'')),hjust=-0.05,vjust=-0.7, size=5 ) + 
    geom_text(aes(label=ifelse((observed.per.100k>1000)|(!!sym(paste0(type, ".per.100k"))>50),as.character(name.y),'')),hjust=1.05,vjust=-0.7, size=5 ) + 
    labs(x = "Finally observed cases per 100 000 inhabitants", y = y.lab, 
         title = title) + 
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, size = 18), 
                       plot.subtitle = element_text(hjust = 0.5),
                       axis.title = element_text(size = 18)) #+
  #xlim(0, 100) + ylim(0,100) #+
  #theme(aspect.ratio = 1)
  
  # squarePlot <- function(plt){
  #   return(plt+coord_equal()+
  #            expand_limits(x=ggplot_build(plt)$panel$ranges[[1]]$y.range,
  #                          y=ggplot_build(plt)$panel$ranges[[1]]$x.range))
  #   g <- squarePlot(g)
  #}
  print(g)
  dev.off()
  
  return(g)
}

