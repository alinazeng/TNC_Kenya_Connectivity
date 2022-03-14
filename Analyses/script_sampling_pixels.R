# Random sampling of pixel values
# alinazengziyun@yahoo.com
# March 7 2022


# Libraries needed for formatting and tidying data ----
library(dplyr)
library(tidyr)
library(Cairo)
library(ggplot2)

# import raw data ----
zone1sample <- read.csv("Input/zone1_samplearea.csv", header = TRUE)
zone1routes <- read.csv("Input/zone1_routes.csv", header = TRUE)
zone2sample <- read.csv("Input/zone2_samplearea.csv", header = TRUE)
zone2routes <- read.csv("Input/zone2_routes.csv", header = TRUE)
zone3sample <- read.csv("Input/zone3_samplearea.csv", header = TRUE)
zone3routes <- read.csv("Input/zone3_routes.csv", header = TRUE)
# 
mean_routes_zone1 <- mean(zone1routes$Value)
# [1] 1346.238
mean_routes_zone2 <- mean(zone2routes$Value)
# [1] 1121.973
mean_routes_zone3 <- mean(zone3routes$Value)
# 1318.70

mean(zone1sample$Value)
# [1] 1307.076               #lmao... mean is probo a terrible indicator

# store in a vector
zone1sample <- zone1sample[,2]
zone2sample <- zone2sample[,2]
zone3sample <- zone3sample[,2]

# randomly draw values from this, repeat for x times

test_zone1 <- replicate(100, zone1sample %>% sample(504), simplify=F) # can 
test_zone2 <- replicate(100, zone2sample %>% sample(73), simplify=F)
test_zone3 <- replicate(100, zone1sample %>% sample(233), simplify=F)

# need an empty df first
stats_zone1 <- data.frame(mean = numeric(), max= numeric(), min = numeric(), 
                          ninety_pct = numeric(), sd = numeric(), 
                          mean_route = numeric(), max_route = numeric(),
                          min_route = numeric(),  ninety_pct_route  = numeric(),
                          sd_route  = numeric(), zone = character())
for( i in 1:length(test_zone1) ){
  mean <- mean(test_zone1[[i]])
  min <- min(test_zone1[[i]])
  max <- max(test_zone1[[i]])
  ninety_pct <- quantile(test_zone1[[i]], probs = 0.9)
  sd = sd(test_zone1[[i]])
  mean_route <- mean_routes_zone1
  min_route <- min(zone1sample)
  max_route <- max(zone1sample)
  ninety_pct_route <- quantile(zone1sample, probs = 0.9)
  sd_route = sd(zone1sample)
  
  zone <- "zone1"
stats_zone1_add <- data.frame(mean = mean, max= max, min = min, 
                              ninety_pct = ninety_pct, sd = sd, mean_route = mean_route, 
                              min_route = min_route, max_route = max_route, 
                              ninety_pct_route = ninety_pct_route, sd_route = sd_route, zone = zone)
stats_zone1 <- rbind(stats_zone1, stats_zone1_add)
}

#maybe take the sd of the mean too

stats_zone2 <- data.frame(mean = numeric(), max= numeric(), min = numeric(), 
                          ninety_pct = numeric(), sd = numeric(), 
                          mean_route = numeric(), max_route = numeric(),
                          min_route = numeric(),  ninety_pct_route  = numeric(),
                          sd_route  = numeric(), zone = character())
for( i in 1:length(test_zone2 ) ){
  mean <- mean(test_zone2 [[i]])
  min <- min(test_zone2 [[i]])
  max <- max(test_zone2 [[i]])
  ninety_pct <- quantile(test_zone2 [[i]], probs = 0.9)
  sd <- sd(test_zone2 [[i]])
  mean_route <- mean_routes_zone2
  min_route <- min(zone2sample)
  max_route <- max(zone2sample)
  ninety_pct_route <- quantile(zone2sample, probs = 0.9)
  sd_route = sd(zone2sample)
  zone <- "zone2"
  stats_zone2_add <- data.frame(mean = mean, max= max, min = min, 
                                ninety_pct = ninety_pct, sd = sd, mean_route = mean_route, 
                                min_route = min_route, max_route = max_route, 
                                ninety_pct_route = ninety_pct_route, sd_route = sd_route, zone = zone)
  stats_zone2 <- rbind(stats_zone2, stats_zone2_add)
}


stats_zone3 <- data.frame(mean = numeric(), max= numeric(), min = numeric(), 
                          ninety_pct = numeric(),sd = numeric(), mean_route = numeric(), zone = character())
# test with stats
for( i in 1:length(test_zone3) ){
  mean <- mean(test_zone3[[i]])
  min <- min(test_zone3[[i]])
  max <- max(test_zone3[[i]])
  ninety_pct <- quantile(test_zone3[[i]], probs = 0.9)
  sd <- sd(test_zone3[[i]])
  mean_route <- mean_routes_zone3
  min_route <- min(zone3sample)
  max_route <- max(zone3sample)
  ninety_pct_route <- quantile(zone3sample, probs = 0.9)
  sd_route = sd(zone3sample)
  zone <- "zone3"
  stats_zone3_add <- data.frame(mean = mean, max= max, min = min, 
                                ninety_pct = ninety_pct, sd = sd, mean_route = mean_route, 
                                min_route = min_route, max_route = max_route, 
                                ninety_pct_route = ninety_pct_route, sd_route = sd_route, zone = zone)
  stats_zone3 <- rbind(stats_zone3, stats_zone3_add)
}


# combine # use rbind() to combine vertically, otherwise merge()
stats_all_zones <- rbind(stats_zone1,stats_zone3)
stats_all_zones <- rbind(stats_all_zones ,stats_zone2)


# export to csvs
name<-paste("Output/pixel_random_sampling_all_zones.csv",sep="")
write.csv(stats_all_zones,name, row.names = FALSE)



# strip plot and see where mean is
ggplot(stats_all_zones, aes(x=zone, y=mean)) +
  geom_jitter(color = "firebrick", size = 1, width = 0.15) +
  labs(x = "Zone", y = "Mean connectivity value") + 
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1))  # Angled labels, so text doesn't overlap						

# add three lines showing route mean

ggplot(stats_all_zones, aes(x=zone, y=mean)) + 
  geom_boxplot(fill = "goldenrod1", notch = FALSE) + 
  labs(x = "Zone", y = "Mean connectivity value") +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1))  # Angled labels, so text doesn't overlap						


# Multiple histograms
ggplot(stats_all_zones, aes(x = mean)) + 
  geom_histogram(fill = "goldenrod1", col = "black", 
                 binwidth = 0.2, boundary = 0, bins = 15) +
  labs(x = "Mean connectivity value", y = "Frequency") + 
  facet_wrap(~zone, ncol = 1, scales = "free_y", strip.position = "right") +
  theme_classic()


facet by zone
x = status (sample VS routes)

colunn = mean, max, min, sd, 90 percentile, status, routes

# reorganize data and plot side by side


# t-test, non-parametric 
# compare overlap -> so we know how many times we need to run the sampling for
# the less overlap the better
