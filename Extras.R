###EXECUTE AFTER MAIN#####

#compute measures of goodness of prediction for our model
#and plot them

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



###### define threshold
th = 0.001


###check coverage of prediction intervals

#nowcasts 
preds.nowcast = mutate(preds.nowcast,
                       covered = if_else(
                         (observed >= lower) & (observed <= upper),
                         true = 1,
                         false = 0
                       )
                      )

(nowcast.coverage <- mean(preds.nowcast$covered))

#forecasts 
preds.forecast = mutate(preds.forecast,
                       covered = if_else(
                         (observed >= lower) & (observed <= upper),
                         true = 1,
                         false = 0
                       )
)

(forecast.coverage <- mean(preds.forecast$covered))

#forenowcasts 
preds.forenowcast = mutate(preds.forenowcast,
                        covered = if_else(
                          (observed >= lower) & (observed <= upper),
                          true = 1,
                          false = 0
                        )
)

(forenowcast.coverage <- mean(preds.forenowcast$covered))



#####NOWCAST PLOT#######
#absolute relative error 
e = ggplot(data=preds.nowcast,aes(x=1:412,y=abs_rel_err,label=name.x)) + 
  geom_abline(intercept = 0, slope =  0, col = "grey60", linetype = "dashed", size = 1.2) +
  geom_point(size=2) +
  geom_text(size=7,aes( label=ifelse((abs_rel_err<=-9.7),name.x,"")),hjust=0.7, vjust=1.5) +
  geom_text(size=7,aes( label=ifelse((abs_rel_err>=9.9),name.x,"")),hjust=0.7, vjust=-0.8) +
  
  labs(x = "District", y = "Error per 100k",
       title = "Prediction error per 100k inhabitants by district for Nowcasts", subtitle = NULL)+   
  ylim(-20,20) +
  theme_bw(base_size = 26) +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())
e

#####FORECAST PLOT#######
#absolute relative error 
i = ggplot(data=preds.forecast,aes(x=1:412,y=abs_rel_err,label=name.x)) + 
  geom_abline(intercept = 0, slope =  0, col = "grey60", linetype = "dashed", size = 1.2) +
  geom_point(size=2) +
  geom_text(size=7,aes( label=ifelse((abs_rel_err>=52),name.x,"")),hjust=0.3, vjust=-0.7) +
  geom_text(size=7,aes( label=ifelse((abs_rel_err<=-50),name.x,"")),hjust=0.7, vjust=1.5) +
  geom_text(size=7,aes( label=ifelse((abs_rel_err<52&abs_rel_err>49),name.x,"")),hjust=0.8, vjust=-0.7) +
  labs(x = "District", y = "Error per 100k",
       title = "Prediction error per 100k inhabitants by district for Forecasts", subtitle = NULL)+   
  ylim(-75,75) +
  theme_bw(base_size = 26) +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  theme(
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
i

#####FORENOWCAST PLOT#######
#absolute relative error 
a = ggplot(data=preds.forenowcast,aes(x=1:412,y=abs_rel_err,label=name.x)) +
  geom_abline(intercept = 0, slope =  0, col = "grey60", linetype = "dashed", size = 1.2) +
  geom_point(size=2) +
  geom_text(size=7,aes( label=ifelse((abs_rel_err>=53.543),name.x,"")),hjust=0.3, vjust=-0.7) +
  geom_text(size=7,aes( label=ifelse((abs_rel_err<=-50),name.x,"")),hjust=0.7, vjust=1.5) +
  geom_text(size=7,aes( label=ifelse((abs_rel_err<53.543&abs_rel_err>49),name.x,"")),hjust=0.8, vjust=-0.7) +
  
  labs(x = "District", y = "Error per 100k",
       title = "Prediction error per 100k inhabitants by district for Forenowcasts", subtitle = NULL)+   
  ylim(-75,75) +
  theme_bw(base_size = 26) +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())
a


#the following code plots the delay distribution function for times=doa&doa2:
#(Figure 1 in the paper):

doa1 <- as.POSIXct("2020-05-15", tz = "GMT")
doa2 <- as.POSIXct("2020-12-15", tz = "GMT")


model1 <- fit.case.model.new(doa1, T.max, d.max, recent, AR.d, AR.t)
model2 <- fit.case.model.new(doa2, T.max, d.max, recent, AR.d, AR.t)
x <- model1$data
y <- model2$data
v <- rep(0,8)
k <- rep(0,8)
for (i in 0:7) {
  v[i+1] = sum((as.numeric(x$delay)==i)*x$N.t.d)
  k[i+1] = sum((as.numeric(y$delay)==i)*y$N.t.d)
}

v <- v/sum(v)
k <- k/sum(k)
for (i in 2:8) {
  v[i] <- v[i] + v[i-1]
  k[i] <- k[i] + k[i-1]
}
z <- data.frame(f1 = v, f2 = k, d = 0:7)
names(z) <- c("2020-05-15","2020-12-15","Delay")

z_long <- pivot_longer(z,c(1,2))


library(ggpubr)
ggplot(data = z_long, aes(Delay,value,col=name,linetype=name)) +
  geom_step(size = 1.8) +
  theme_bw(base_size = 40) +
  geom_point(aes(color = name),size=5) +
  theme(legend.justification = c(0.01, 0.99), legend.position = c(0.01, 0.99)) +
  #  theme(legend.justification = c(0.01, 0.99), legend.position = c(0.7765, 0.2)) +
  theme(panel.grid = element_blank()) +
  theme(legend.background = element_blank()) +
  #theme(legend.title= element_blank()) +
  scale_x_continuous(breaks = seq(0, 7, 1)) +
  theme(legend.title = element_text(size = 24), 
        legend.text = element_text(size = 20)) +
  labs(y = expression(F[t](d)), color = "Date of analysis", linetype = "Date of analysis")


library(ggpubr)
ggplot(data = z_long, aes(Delay,value,col=name,linetype=name)) +
  geom_step(size = 1.8) +
  theme_bw(base_size = 25) +
  geom_point(aes(color = name)) +
  theme(legend.justification = c(0.01, 0.99), legend.position = c(0.7765, 0.2)) +
  #theme(panel.grid = element_blank()) +
  theme(legend.background = NULL) +
  #theme(legend.title= element_blank()) +
  scale_x_continuous(breaks = seq(0, 7, 1)) +
  labs(y = expression(F[t](d)), color = "Date of analysis", linetype = "Date of analysis")





