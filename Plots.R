

# Plot the maps ----

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
print(plot.map(data, type = "s_2", limits = range(district.effects$m_2), 
               date = doa, state_borders = state_borders, 
               plot_title = "Estimates of smooth spatial effect",
               legend_title = expression(widehat(s)[2](s[r])),
               caption = paste0("Based on data reported on ", doa, ".\nModel includes registration dates from\n",
                                min(model$registration.dates), " until ", max(model$registration.dates), "."),
               pointers = pointers.spatial)) 
dev.off()



# plot long term random intercept 
directory <- paste0(path.LRZ, "Plots/cases/RandomInterceptLong/")
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
directory <- paste0(path.LRZ, "Plots/cases/RandomInterceptShort/")
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
  geom_ribbon(data = boundaries, aes(ymin = lower, ymax = upper), alpha = 0.3) +
  theme(legend.justification = c(0.99, 0.99), legend.position = c(0.99, 0.99)) + 
  theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Registration Date", y = expression(paste(hat(s)[1](t)))) + 
  scale_x_continuous(breaks = 1:model$T.max, 
                     labels = as.character(substring(model$registration.dates, 6))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
print(g)
dev.off()
