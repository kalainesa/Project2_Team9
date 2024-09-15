library(tidyverse)
pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  print()

pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")%>%
  print()

pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)%>%
  print()

library(features)

exp1 <- pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")

f1 <-  features(x = exp1$frame,y=exp1$amp.bl)

fget(f1)


f2 <-  features(x = exp1$frame,y=exp1$amp.bl*100)
fget(f2)

f.tib <- fget(f2)[2:3]%>%
  as_tibble()%>%
  filter(curvature<0)%>%
  mutate(peaks=round(crit.pts,0))%>%
  print()



find.peaks <- function(x,y,mult=100){ 
  f <- fget(features(x = x,y=y*mult))[2:3]%>% 
    as_tibble()%>% 
    filter(curvature<0)%>% 
    mutate(peaks=round(crit.pts,0))
  return(f$peaks) 
}


pseed.max <- pseed2%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  filter(peak==T)


amp.aov <-  aov(amp.bl~bl.s,pseed.max)
summary(amp.aov)


pseed2 <- pseed2 %>%
  group_by(date,frame) %>%
  mutate(amp.sum=sum(amp.bl))

pseed2 %>%
  filter(fin=="R")

pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 

SE_M <- function(x) {
  sd(x) / sqrt(length(x))
}

pseed.wide2 <- pseed.wide %>%
  group_by(fish, bl.s, date) %>%
  mutate(peak = frame %in% find.peaks(frame, amp.sum)) %>% 
  filter(peak == T) %>% 
  mutate(amp.sum.LR = amp.sum)  
pseed.wide2

#finds the mean of amp.sum and adds SE  to the summary table
pseed.sum.max <- pseed.wide2%>%
  group_by(fish, bl.s, date) %>%
  summarize(
    amp.sum.mean = mean(amp.sum.LR))
pseed.sum.max

pseed.sum.max <- pseed.sum.max%>%
  group_by(fish, bl.s, date, amp.sum.mean) %>%
  summarize(
    amp.sum.se = SE_M(pseed.sum.max$amp.sum.mean))  
pseed.sum.max

#plot mean amp.sum vs swimming speed with error bars for SE
ggplot(pseed.sum.max, aes(x = bl.s, y = amp.sum.mean, color = fish)) +
  geom_point(size = 3) +  
  geom_errorbar(aes(ymin = amp.sum.mean - amp.sum.se, ymax = amp.sum.mean + amp.sum.se), 
                width = 0.2) +   
  labs(x = "Swimming Speed", y = "Mean Amplitude Sum", color = "Fish") + 
  theme_minimal() + 
  theme(legend.position = "right") 




pseed.met.rate <- read_csv("pseed.met.rate.csv")

pseed.met.rate_selected <- pseed.met.rate %>%
  select(date, met.rate, m.s, cm.s)

pseed.sum.max <- pseed.sum.max %>%
  left_join(pseed.met.rate_selected, by = "date")




ggplot(pseed.sum.max, aes(x = amp.sum.mean, y = met.rate, color = fish)) +
  geom_point(size = 3) +  
  labs(x = "Mean Maximum of Amplitude Sum", y = "Metabolic Power Output", color = "Fish") + 
  theme_minimal() +  
  theme(legend.position = "right")  



