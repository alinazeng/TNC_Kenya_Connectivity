# Random sampling of pixel values
# alinazengziyun@yahoo.com
# March 7 2022


# Libraries needed for formatting and tidying data ----
library(dplyr)
library(tidyr)

# import raw data ----
zone1sample <- read.csv("Input/zone1_samplearea.csv", header = TRUE)
zone1routes <- read.csv("Input/zone1_routes.csv", header = TRUE)
zone2sample <- read.csv("Input/zone2_samplearea.csv", header = TRUE)
zone2routes <- read.csv("Input/zone2_routes.csv", header = TRUE)
zone3sample <- read.csv("Input/zone3_samplearea.csv", header = TRUE)
zone3routes <- read.csv("Input/zone3_routes.csv", header = TRUE)
# 
mean(zone1routes$Value)
# [1] 1346.238
mean(zone2routes$Value)
# [1] 1121.973
mean(zone3routes$Value)
# 1318.70

mean(zone1sample$Value)
# [1] 1307.076               #lmao... mean is probo a terrible indicator

# store in a vector
zone1sample <- zone1sample[,2]
zone2sample <- zone2sample[,2]
zone3sample <- zone3sample[,2]

# randomly draw values from this, repeat for x times

test <- replicate(500, zone1sample %>% sample(504), simplify=F)
test <- replicate(100, zone2sample %>% sample(73), simplify=F)
test <- replicate(250, zone1sample %>% sample(233), simplify=F)

# need an empty df first

stats_zone1 <- data.frame(mean = numeric(), max= numeric(), min = numeric(), ninety_pct = numeric(), sd = numeric(), zone = character())
# test with stats
for( i in 1:length(test) ){
  mean <- mean(test[[i]])
  min <- min(test[[i]])
  max <- max(test[[i]])
  ninety_pct <- quantile(test[[i]], probs = 0.9)
  sd = sd(test[[i]])
  zone <- "zone1"
stats_zone1_add <- data.frame(mean = mean, max= max, min = min, ninety_pct = ninety_pct, sd = sd, zone = zone)
stats_zone1 <- rbind(stats_zone1, stats_zone1_add)
}

#maybe take the sd of the mean too

stats_zone2 <- data.frame(mean = numeric(), max= numeric(), min = numeric(), ninety_pct = numeric(), sd = numeric(),zone = character())
# test with stats
for( i in 1:length(test) ){
  mean <- mean(test[[i]])
  min <- min(test[[i]])
  max <- max(test[[i]])
  ninety_pct <- quantile(test[[i]], probs = 0.9)
  sd <- sd(test[[i]])
  zone <- "zone2"
  stats_zone2_add <- data.frame(mean = mean, max= max, min = min, ninety_pct = ninety_pct, sd = sd, zone = zone)
  stats_zone2 <- rbind(stats_zone2, stats_zone2_add)
}


stats_zone3 <- data.frame(mean = numeric(), max= numeric(), min = numeric(), ninety_pct = numeric(), zone = character())
# test with stats
for( i in 1:length(test) ){
  mean <- mean(test[[i]])
  min <- min(test[[i]])
  max <- max(test[[i]])
  ninety_pct <- quantile(test[[i]], probs = 0.9)
  sd <- sd(test[[i]])
  zone <- "zone3"
  stats_zone3_add <- data.frame(mean = mean, max= max, min = min, ninety_pct = ninety_pct, zone = zone)
  stats_zone3 <- rbind(stats_zone3, stats_zone3_add)
}







# strip plot and see where mean is
ggplot(stats_zone1 , aes(x=zone, y=mean)) +
  geom_jitter(color = "firebrick", size = 1, width = 0.15) +
  labs(x = "Zone", y = "Mean connectivity value") + 
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1))  # Angled labels, so text doesn't overlap						

  
# t-test, non-parametric 
# compare overlap -> so we know how many times we need to run the sampling for
# the less overlap the better
