

# Plot the maps ----

# district and state boundaries for plotting
district_borders = read_rds(paste0(path.LRZ, "Data/Maps/district_borders.Rds"))
state_borders = read_rds(paste0(path.LRZ, "Data/Maps/state_borders.Rds"))

# get desired models
files <- list.files(path= paste0(path.LRZ, "Output"))
file <- files[grep(paste(doa), files)]
model <- read_rds(paste0(path.LRZ, "Output/", file))  

# initilaize table with district effects
districts <- preprocess.districts()
district.effects <- tibble(district = districts$name, 
                           districtId = districts$districtId,                            
                           s_2 = 0) 

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
  district.effects$s_2[j] = fitted.matrix[ind]
}

# Matching of effects with dataset that contains the district borders
data <- full_join(district_borders, district.effects)

theme_set(theme_bw() + theme(panel.grid = element_blank(), 
                             plot.title = element_text(hjust = 0.5)))

pointers.spatial <- data.frame(name = c("North-Rhine \nWestphalia", "Berlin"), startpoint.lon = c(6.7, 13.5), startpoint.lat = c(48.5, 50.1),
                               endpoint.lon = c(districts$lon[110], districts$lon[325]),
                               endpoint.lat = c(districts$lat[110], districts$lat[325]))

# plot spatial effect
directory <- paste0(path.LRZ, "Plots/SpatialEffect")
png(file = paste0(directory, "/", doa, ".png"), 
    width = 1300, height = 1550, units = "px")
print(plot.map(data, type = "s_2", limits = range(district.effects$s_2), 
               date = doa, state_borders = state_borders, 
               plot_title = "Estimates of smooth spatial effect",
               legend_title = expression(widehat(s)[2](s[r])),
               caption = paste0("Based on data reported on ", doa, ".\nModel includes registration dates from\n",
                                min(model$registration.dates), " until ", max(model$registration.dates), "."),
               pointers = pointers.spatial)) 
dev.off()



# plot long term random intercept 
directory <- paste0(path.LRZ, "Plots/RandomInterceptLong")
png(file = paste0(directory, "/", doa, ".png"), 
    width = 1300, height = 1550, units = "px")
print(plot.map(data, type = "u_r0", limits = range(district.effects$u_r0), 
               date = doa, state_borders = state_borders, 
               plot_title = "Estimates of long-term random intercept",
               legend_title = expression(widehat(u)[r0]),
               caption = paste0("Based on data reported on ", doa, ".\nModel includes registration dates from\n",
                                min(model$registration.dates), " until ", max(model$registration.dates), ".")))
dev.off()

pointers.shortterm <- data.frame(name = "Gütersloh", startpoint.lon = 6.7, startpoint.lat = 48.5,
                                 endpoint.lon = districts$lon[99], endpoint.lat = districts$lat[99])

# plot short term random intercept
directory <- paste0(path.LRZ, "Plots/RandomInterceptShort")
png(file = paste0(directory, "/", doa, ".png"), 
    width = 1300, height = 1550, units = "px")
print(plot.map(data, type = "u_r1", limits = range(district.effects$u_r1), 
               date = doa, state_borders = state_borders, 
               plot_title = "Estimates of short-term random intercept",
               legend_title = expression(widehat(u)[r1]),
               caption = paste0("Based on data reported on ", doa, ".\nModel includes registration dates from\n",
                                min(model$registration.dates), " until ", max(model$registration.dates), "."),
               pointers = pointers.shortterm))
dev.off()

# plot time effect ----

theme_set(theme_bw() + theme(panel.grid = element_blank(), 
                             plot.title = element_text(hjust = 0.5)))

# get the required model objects
files <- list.files(path= paste0(path.LRZ, "Output"))
file <- files[grep(as.character(doa), files)]
model <- read_rds(paste0(path.LRZ, "Output/", file))  

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


pdf(file = paste0(path.LRZ, "Plots/TimeEffect/", doa, ".pdf"), 
    width = 6, height = 4) 
g <- ggplot(df, aes(x = x)) + geom_line(aes(y = y)) + 
  geom_ribbon(data = boundaries, aes(ymin = lower, ymax = upper), alpha = 0.3) +
  theme(legend.justification = c(0.99, 0.99), legend.position = c(0.99, 0.99)) + 
  theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Registration Date", y = expression(paste(hat(s)[1](t)))) + 
  scale_x_continuous(breaks = 1:model$T.max, 
                     labels = as.character(substring(model$registration.dates, 6))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
print(g)
dev.off()

# Plot the prediction errors 
###EXECUTE THIS PART AFTER MAIN#####

#save the predictions
preds.nowcast = g.nowcast$data
preds.forecast = g.forecast$data
preds.forenowcast = g.forenowcasts$data

#error per 100k
preds.nowcast = mutate(preds.nowcast,
                       abs_rel_err = -(nowcast.per.100k - observed.per.100k) 
)

preds.forecast = mutate(preds.forecast,
                        abs_rel_err = -(forecast.per.100k - observed.per.100k) 
)

preds.forenowcast = mutate(preds.forenowcast,
                           abs_rel_err = -(forenowcast.per.100k - observed.per.100k) 
)

#sort districts by population (ascending)
preds.nowcast = arrange(preds.nowcast,pop,districtId)
preds.forecast = arrange(preds.forecast,pop,districtId)
preds.forenowcast = arrange(preds.forenowcast,pop,districtId)

#####NOWCAST PLOT#######
#absolute relative error 

e = ggplot(data=preds.nowcast,aes(x=1:412,y=abs_rel_err,label=name.x)) + 
  geom_abline(intercept = 0, slope =  0, col = "grey60", linetype = "dashed", size = 1.2) +
  geom_point(size=2) +
  geom_text(size=7,aes( label=ifelse((abs_rel_err<=-2.5),name.x,"")),hjust=0.7, vjust=1.2) +
  geom_text(size=7,aes( label=ifelse((abs_rel_err>=5),name.x,"")),hjust=0.7, vjust=-0.8) +
  
  labs(x = "District", y = "Error per 100k",
       title = "Prediction error per 100k inhabitants by district for Nowcasts", subtitle = NULL)+   
  ylim(-25,25) +
  theme_bw(base_size = 26) +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())

pdf(file = paste0(path.LRZ, "Plots/Residuals/Nowcast/", doa, ".pdf"), width = 16, height = 9)
print(e)
dev.off()


#####FORECAST PLOT#######
#absolute relative error 
i = ggplot(data=preds.forecast,aes(x=1:412,y=abs_rel_err,label=name.x)) + 
  geom_abline(intercept = 0, slope =  0, col = "grey60", linetype = "dashed", size = 1.2) +
  geom_point(size=2) +
  geom_text(size=7,aes( label=ifelse((abs_rel_err>=11),name.x,"")),hjust=0.7, vjust=-0.7) +
  geom_text(size=7,aes( label=ifelse((abs_rel_err<=-11),name.x,"")),hjust=0.7, vjust=1.5) +
  labs(x = "District", y = "Error per 100k",
       title = "Prediction error per 100k inhabitants by district for Forecasts", subtitle = NULL)+   
  ylim(-25,25) +
  theme_bw(base_size = 26) +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())

pdf(file = paste0(path.LRZ, "Plots/Residuals/Forecast/", doa, ".pdf"), width = 16, height = 9)
print(i)
dev.off()


#####FORENOWCAST PLOT#######
#absolute relative error 
a = ggplot(data=preds.forenowcast,aes(x=1:412,y=abs_rel_err,label=name.x)) +
  geom_abline(intercept = 0, slope =  0, col = "grey60", linetype = "dashed", size = 1.2) +
  geom_point(size=2) +
  geom_text(size=7,aes( label=ifelse((abs_rel_err>=11),name.x,"")),hjust=0.7, vjust=-0.7) +
  geom_text(size=7,aes( label=ifelse((abs_rel_err<=-16),name.x,"")),hjust=0.7, vjust=1.5) +
  geom_text(size=7,aes( label=ifelse((abs_rel_err<=-11&abs_rel_err>-16),name.x,"")),hjust=0.7, vjust=-0.7) +
  
  labs(x = "District", y = "Error per 100k",
       title = "Prediction error per 100k inhabitants by district for Forenowcasts", subtitle = NULL)+   
  ylim(-25,25) +
  theme_bw(base_size = 26) +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())

pdf(file = paste0(path.LRZ, "Plots/Residuals/Forenowcast/", doa, ".pdf"), width = 16, height = 9)
print(a)
dev.off()



###compute coverage of prediction intervals

#nowcasts - 84%
preds.nowcast = mutate(preds.nowcast,
                       covered = if_else(
                         (observed >= lower) & (observed <= upper),
                         true = 1,
                         false = 0
                       )
)

(nowcast.coverage <- mean(preds.nowcast$covered))

#forecasts - 75%
preds.forecast = mutate(preds.forecast,
                        covered = if_else(
                          (observed >= lower) & (observed <= upper),
                          true = 1,
                          false = 0
                        )
)

(forecast.coverage <- mean(preds.forecast$covered))

#forenowcasts - 75%
preds.forenowcast = mutate(preds.forenowcast,
                           covered = if_else(
                             (observed >= lower) & (observed <= upper),
                             true = 1,
                             false = 0
                           )
)

#check coverage of the intervals:
(nowcast.coverage <- mean(preds.nowcast$covered))
(forecast.coverage <- mean(preds.forecast$covered))
(forenowcast.coverage <- mean(preds.forenowcast$covered))


####PLOTS FOR THE APPENDIX########


#a week in seconds
interval <- 7*24*60*60
doa_vector <- doa
nmodels <- 10

#construct vector of DOAs
for (i in (1:(nmodels - 1))) {
  diff = i*interval
  current_doa <- doa - diff
  print(current_doa)
  doa_vector <- c(doa_vector, current_doa)
}

doa_vector = rev(doa_vector)


#fit the 10 models
models = list()
coefs = vector()
ses = vector()

for (i in 1:length(doa_vector)) {
  model <- fit.model(doa_vector[i],
                              T.max, d.max, recent, 
                              AR.d = TRUE, 
                              AR.t = TRUE)
  s <- summary(model)
  print(s)
  models = c(models,s)
  coefs = c(coefs,s$p.coeff)
  ses = c(ses, s$se[1:21])
}

#create dataframes with coefficients and standard errors
names = names(coefs[1:21])
splits = split(as.numeric(coefs), ceiling(seq_along(coefs)/21))
coef_df = (cbind(splits$`1`,splits$`2`,splits$`3`,splits$`4`,splits$`5`,splits$`6`,splits$`7`,splits$`8`,splits$`9`,splits$`10`))
coef_df = t(coef_df)
coef_df = as.data.frame(coef_df)
rownames(coef_df) = as.character(doa_vector)
colnames(coef_df) = names

splits = split(as.numeric(ses), ceiling(seq_along(ses)/21))
ses_df = (cbind(splits$`1`,splits$`2`,splits$`3`,splits$`4`,splits$`5`,splits$`6`,splits$`7`,splits$`8`,splits$`9`,splits$`10`,splits$`11`,splits$`12`,splits$`13`))
ses_df = t(ses_df)
ses_df = as.data.frame(ses_df)
rownames(ses_df) = as.character(doa_vector)
colnames(ses_df) = names

#create full dataframe with intervals
uppers = coef_df+1.96*ses_df
lowers = coef_df-1.96*ses_df

names(uppers) = paste(names(coef_df),"upper")
names(lowers) = paste(names(coef_df),"lower")


coef_intervals = cbind(coef_df,uppers,lowers)
coef_intervals$dates = as.Date(rownames(coef_intervals))

#one can save progress up to here to spare time
#saveRDS(coef_intervals, paste0(path.LRZ, "coef_intervals.Rds"))
ncoef = 21
names=names(coef_intervals[1:21])


#plot estimated delay effect over time in same plot
delays_long <- pivot_longer(cbind(coef_intervals[2:7],date = coef_intervals$dates), 
                            cols = 1:6,
                            names_to = "delay", 
                            values_to = "coef"
)

delays_long_upper <- pivot_longer(cbind(coef_intervals[23:28],date = coef_intervals$dates), 
                                  cols = 1:6,
                                  names_to = "delay", 
                                  values_to = "upper"
)

delays_long_lower <- pivot_longer(cbind(coef_intervals[44:59],date = coef_intervals$dates), 
                                  cols = 1:6,
                                  names_to = "delay", 
                                  values_to = "lower"
)

delays_long$upper = delays_long_upper$upper
delays_long$lower =delays_long_lower$lower

pdf(file = paste0(path.LRZ, "Plots/CoefficientsTemporal/delay.pdf"), width = 8.3, height = 5)
ggplot(delays_long, aes(x = date, y = coef, 
                        col = delay)) +
  scale_y_continuous(limits = c(min(delays_long$lower), max(delays_long$upper + 0.7))) +
  geom_abline(intercept = 0, slope =  0, col = "grey40", linetype = "dashed", size = 1.2) +    
  geom_point() +
  geom_line() +
  theme_bw() +
  geom_ribbon(aes(ymin = upper, ymax = lower, fill= delay,col=NULL), 
              alpha= 0.2) +
  labs(x = "Fitting Date", y = paste("Coefficient Estimate"), 
       title = "Effect of delay (in days)", 
       subtitle = NULL, color = "Reporting Delay", fill = "Reporting Delay") +
  #theme_bw(base_size = 26) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.justification = c(0.01, 0.99), legend.position = c(0.01, 0.99),
        legend.direction  = "horizontal") +
  scale_color_hue(labels = c("2 Days", "3 Days", "4 Days", "5 Days", "6 Days", "7 Days")) +
  scale_fill_hue(labels = c("2 Days", "3 Days", "4 Days", "5 Days", "6 Days", "7 Days")) + 
  scale_linetype_manual(name = "1 Day (Reference Category)")
dev.off()


#plot effect of weekdays over time in same plot
weekdays_long <- pivot_longer(cbind(coef_intervals[8:13],date = coef_intervals$dates), 
                              cols = 1:6,
                              names_to = "weekday", 
                              values_to = "coef"
)

weekdays_long_upper <- pivot_longer(cbind(coef_intervals[29:34],date = coef_intervals$dates), 
                                    cols = 1:6,
                                    names_to = "weekday", 
                                    values_to = "upper"
)

weekdays_long_lower <- pivot_longer(cbind(coef_intervals[50:55],date = coef_intervals$dates), 
                                    cols = 1:6,
                                    names_to = "weekday", 
                                    values_to = "lower"
)

weekdays_long$upper = weekdays_long_upper$upper
weekdays_long$lower = weekdays_long_lower$lower

pdf(file = paste0(path.LRZ, "Plots/CoefficientsTemporal/weekday.pdf"), width = 8.3, height = 5)
ggplot(weekdays_long, aes(x = date, y = coef, 
                          col = weekday)) +
  geom_abline(intercept = 0, slope =  0, col = "grey40", linetype = "dashed", size = 1.2) +    
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = upper, ymax = lower, 
                  fill= weekday,col=NULL), alpha= 0.1) +
  theme_bw() +
  labs(x = "Fitting Date", y = paste("Coefficient Estimate"), 
       title = "Weekday effects", 
       subtitle = NULL, color = "Weekday", fill = "Weekday") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.justification = c(0.01, 0.99), legend.position = c(0.01, 0.99),
        legend.direction = "horizontal") + 
  scale_color_hue(labels = c("Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) +
  scale_fill_hue(labels = c("Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) 
dev.off()



#effect of agegroups

agegroupdf = cbind(coef_intervals[14:18],date=coef_intervals$dates)

agegroups_long <- pivot_longer(agegroupdf, 
                               cols = 1:5,
                               names_to = "agegroup", 
                               values_to = "coef"
)

agegroups_long_upper <- pivot_longer(cbind(coef_intervals[35:39],date = coef_intervals$dates), 
                                     cols = 1:5,
                                     names_to = "agegroup", 
                                     values_to = "upper"
)

agegroups_long_lower <- pivot_longer(cbind(coef_intervals[56:60],date = coef_intervals$dates), 
                                     cols = 1:5,
                                     names_to = "agegroup", 
                                     values_to = "lower"
)

agegroups_long$upper = agegroups_long_upper$upper
agegroups_long$lower = agegroups_long_lower$lower

pdf(file = paste0(path.LRZ, "Plots/CoefficientsTemporal/Agegroup.pdf"), width = 8.3, height = 5)
ggplot(agegroups_long, aes(x = date, y = coef, 
                           col = agegroup)) +
  geom_abline(intercept = 0, slope =  0, col = "grey40", linetype = "dashed", size = 1.2) +    
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = upper, ymax = lower, fill= agegroup,col=NULL), 
              alpha= 0.1) +
  labs(x = "Fitting Date", y = paste("Coefficient Estimate"), 
       title = "Effect of age-group", 
       subtitle = NULL, color = "Age Group", fill = "Age Group") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.justification = c(0.99, 0.99), legend.position = c(0.99, 0.99)) +
  ylim(-1.6,1.6) + 
  scale_color_hue(labels = c("0-4 Years", "5-14 Years", "15-34 Years", "60-79 Years", "80+ Years")) +
  scale_fill_hue(labels = c("0-4 Years", "5-14 Years", "15-34 Years", "60-79 Years", "80+ Years")) 
dev.off()

#plot AR components over time
#plot estimated ar effect over time in same plot
ars_long <- pivot_longer(cbind(coef_intervals[20:21],date = coef_intervals$dates), 
                         cols = 1:2,
                         names_to = "AR component", 
                         values_to = "coef"
)

ars_long_upper <- pivot_longer(cbind(coef_intervals[41:42],date = coef_intervals$dates), 
                               cols = 1:2,
                               names_to = "AR component", 
                               values_to = "upper"
)

ars_long_lower <- pivot_longer(cbind(coef_intervals[62:63],date = coef_intervals$dates), 
                               cols = 1:2,
                               names_to = "AR component", 
                               values_to = "lower"
)

ars_long$upper = ars_long_upper$upper
ars_long$lower =ars_long_lower$lower

pdf(file = paste0(path.LRZ, "Plots/CoefficientsTemporal/ar_component.pdf"), width = 8.3, height = 5)
ggplot(ars_long, aes(x = date, y = coef, 
                     col = `AR component`)) +
  geom_abline(intercept = 0, slope =  0, col = "grey40", linetype = "dashed", size = 1.2) +    
  geom_point() +
  geom_line() +
  theme_bw() + 
  geom_ribbon(aes(ymin = upper, ymax = lower, fill= `AR component`,col=NULL), 
              alpha= 0.2) +
  labs(x = "Fitting Date", y = paste("Coefficient Estimate"), 
       title = "Effect of autoregressive components over time", 
       subtitle = NULL, color = "Autoregressive Component", fill = "Autoregressive Component") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.justification = c(0.01, 0.99), legend.position = c(0.01, 0.99),
        legend.direction = "horizontal") + 
  scale_color_hue(labels = c(expression(paste(log(C[t*","*d-1]))), expression(paste(log(C[t-1*","*d]))))) +
  scale_fill_hue(labels = c(expression(paste(log(C[t*","*d-1]))), expression(paste(log(C[t-1*","*d]))))) 
dev.off()



#intercept over time
coef_intervals = mutate(coef_intervals,coefficient = "intercept")
pdf(file = paste0(path.LRZ, "Plots/CoefficientsTemporal/intercept.pdf"), width = 8.3, height = 5)
ggplot(data = coef_intervals, aes(dates,coef_intervals[,1],fill=coefficient)) +
  geom_point(col="navyblue") + 
  geom_line(col="navyblue") +
  geom_ribbon(aes(ymin=coef_intervals[,1+ncoef], 
                  ymax=coef_intervals[,1+2*ncoef]),
              fill="navyblue",alpha=0.1) +
  labs(x = "Fitting Date", y = paste("Coefficient Estimate"), 
       title = "Estimated intercept over time", 
       subtitle = NULL, fill = "", color = "") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
dev.off()


coef_intervals=mutate(coef_intervals,gender="female")

#gender over time
pdf(file = paste0(path.LRZ, "Plots/CoefficientsTemporal/gender.pdf"), width = 8.3, height = 5)
ggplot(data = coef_intervals, aes(dates,coef_intervals[,19],fill=gender)) +
  geom_abline(intercept = 0, slope =  0, col = "grey40", linetype = "dashed", size = 1.2) +    
  geom_point(col="firebrick4") + 
  geom_line(col="firebrick4") +
  geom_ribbon(aes(ymin=coef_intervals[,19+ncoef], 
                  ymax=coef_intervals[,19+2*ncoef]),
              fill="firebrick1",alpha=0.1) +
  theme_bw() + 
  labs(x = "Fitting Date", y = paste("Coefficient Estimate"), 
       title = "Estimated effect of gender (female) over time", 
       subtitle = NULL, color = "Gender", fill = "Gender") +
  ylim(-1.6,1.6) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.justification = c(0.99, 0.99), legend.position = c(0.99, 0.99)) +
  scale_color_hue(labels = c("Female")) +
  scale_fill_hue(labels = c("Female")) 
dev.off()