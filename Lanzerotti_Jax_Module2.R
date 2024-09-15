#1, setting up
library(tidyverse)
library(features)
setwd()
pseed <-  read_csv("pseed.fin.amps.csv")
  # amplitudes of pectoral fin oscillations 
pseed.bl <- read_csv("pseed.lengths.csv")
  # size of each fish
speeds <-  read_csv("pseed.calibration.csv")
  # calibrated speed in cm/s-1

# joining relational data
  # voltage is the key (common attribute) in pseed+speed (pseed: speed, speeds: vol)
pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  print()

# mutation: adding data to data
pseed2%>%
  select(fish)%>%
  unique()

# merging the tables to pseed2
pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")%>%
  print()

# computing specific speed in BL/s
pseed2 <- pseed2%>%
mutate(bl.s=cm.s/bl)%>%
  print()

# using ggplot to plot specific fin amplitude (amp.bl) against swimming speed (bl.s)
pseed2%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point(alpha=.01)
    # X axis: bl.s values in pseed2
    # Y axis: amp.bl

# looking at the left pelvic fin for only one experiment (identified by first value of date column)
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  ggplot(aes(x=frame,y=amp.bl))+geom_point()

# using customized functions
exp1 <- pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")
f1 <-  features(x = exp1$frame,y=exp1$amp.bl)
fget(f1)
  # there are 24 critical points where the second deriv == 0 
# peaks will have negative curvature, troughs are positive, just not enough decimal places to see that

f2 <-  features(x = exp1$frame,y=exp1$amp.bl*100)
fget(f2)

# pulling out the peaks, choosing the negative critical points
f.tib <- fget(f2)[2:3]%>%
  as_tibble()%>%
  filter(curvature<0)%>%
  mutate(peaks=round(crit.pts,0))%>%
  print()

# new column peaks that has discrete values based on rounding critical points
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  mutate(peak=frame %in% f.tib$peaks)%>%
  ggplot(aes(x=frame,y=amp.bl,col=peak))+geom_point()

# this is max amplitude for only one experiment and there are: 50 experiments 
pseed2%>%
  summarize(n=length(unique(date)))

# crafting our function
find.peaks <- function(x,y,mult=100){
  f <- fget(features(x = x,y=y*mult))[2:3]%>%
  as_tibble()%>%
  filter(curvature<0)%>% 
  mutate(peaks=round(crit.pts,0))
return(f$peaks)
}

#first three experiments
pseed2%>%
  filter(date%in%unique(date)[1:3])%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  ggplot(aes(x=frame,y=amp.bl,alpha=peak,col=peak))+geom_point()+facet_grid(date~fin)

# all of the experiments
pseed.max <- pseed2%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  filter(peak==T) 
pseed.max%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point()+geom_smooth(method="lm")
# amplitude does decrease with speed

  # test it
amp.aov <-  aov(amp.bl~bl.s,pseed.max)
summary(amp.aov)
# p value is much lower than .05, significant relationship indicated

# next steps
pseed.max %>%
  group_by(fish, bl.s) %>%
  summarize(mean.max=mean(amp.bl)) %>%
  ggplot(aes(x=bl.s,y=mean.max,col=fish))+geom_point()+geom_smooth(method="lm")

# pivoting 
pseed2 <- pseed2 %>%
  group_by(date,frame) %>%
  mutate(amp.sum=sum(amp.bl))
pseed2 %>%
  filter(fin=="R")

# make the data wider, include left fin
pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 


# project report

#2, standard error of the mean
compute_se <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(length(x))
}

#3+4, mean max of all

  # amp sum of each frame
pseed2 <- pseed2 %>%
  group_by(fish, date, frame) %>%
  mutate(amp.sum = sum(amp.bl)) %>%
  print()

  # amp sum max, finding peaks 
pseed.sum.max <- pseed2 %>%
  group_by(date, fish, bl.s) %>%
  summarize(amp.sum.max = max(amp.sum, na.rm = TRUE)) %>%
  ungroup() %>% 
  print()

  # mean max 
pseed.sum.max <- pseed.sum.max %>%
  group_by(fish, bl.s) %>%
  summarize(amp.sum.mean = mean(amp.sum.max, na.rm = TRUE),
            amp.sum.se = compute_se(amp.sum.max)) %>%
  ungroup() %>%  
  print()

#5, graph with error bars
pseed.sum.max %>%
  ggplot(aes(x = bl.s, y = amp.sum.mean, color = fish)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = amp.sum.mean - amp.sum.se, ymax = amp.sum.mean + amp.sum.se), width = 0.1) +
  labs(title = "Mean Summed Amplitudes vs. Specific Swimming Speed with SE", x = "Swimming Speed (BL/s)", y = "Mean Summed Amplitude", color = "Fish")

# 6, new tibble for metabolic data 
pseed.met.rate <- read_csv("pseed.met.rate.csv")

# merging data sets 
pseed.merged <- pseed.sum.max %>%
  left_join(pseed.met.rate, by = c("fish", "bl.s")) %>% 
  print()

#7, metabolic power output vs mean max of amp.sum
pseed.merged %>%
  ggplot(aes(x = amp.sum.mean, y = met.rate, color = fish)) +
  geom_point(size = 3)+
  geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Metabolic Power Output vs. Mean Maximum Summed Amplitudes",
       x = "Mean Maximum Summed Amplitude",
       y = "Metabolic Power Output (met.rate)",
       color = "Fish") 

