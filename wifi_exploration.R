# Libraries ........................................................................
pacman::p_load(data.table, tidyverse, caret)

# My Wifi investigation ............................................................
setwd("C:/Users/Violeta/Documents/Ubiqum/3_Deep.Analytics/Wifi")
wifi_train <- fread("trainingData.csv", sep = ',', header = T) 
wifi_valid <- fread("validationData.csv", sep = ',', header = T)
all_equal(wifi_train, wifi_valid) # differ in length? Further investigation

# ...................................................................[TRAINING DATA]
sapply(wifi_train, uniqueN) # returns table with unique observation per column.
    # if lapply, returns list (hard to read when long)
    # with this we understand we have 18 userID, 16 phoneID, 3 buildindID...
    # and different instances for LAT:687 vs LON:679 (!!)

# Check structure ..................................................................
str(wifi_train)
str(wifi_train[,c(521:529)])
wifi_train[,c(523:528)] <- wifi_train[,c(523:528)] %>% mutate_if(is.integer, as.factor) 

# Missing Values? ...................................................................
anyNA(wifi_train)
anyNA(wifi_valid)

library(VIM)
aggr(wifi_train) # not a good dataset to explore this package.

# Duplicates wifi_train? ............................................................

table(duplicated(wifi_train)) # 637 duplicates and 19300 unique. Returns T/F for duplicated obs.
anyDuplicated(wifi_train)     # returns first row it sees duplicated. fromLast = T/F
duplicates <- wifi_train[which(duplicated(wifi_train))] 
duplicates_melt <- duplicates %>% gather("waps", "rssi", 1:520)

ggplot(duplicates_melt) + geom_histogram(aes(rssi), bins = 40)

wifi_duplicated <- duplicates_melt %>% group_by(waps, rssi) %>% tally()

waps_duplicated_100 <- wifi_duplicated %>% filter(rssi == 100)

#-..........................................................[Understanding the Data]

# Removing duplicates:
wifi <- wifi_train %>% filter() %>% distinct()

# Most frequent RSSI recorded -------
wifi_melt <- wifi %>% gather("waps", "rssi", 1:520) %>% mutate_if(is.integer, as.numeric)


ggplot(wifi_melt) + geom_bar(aes(rssi), fill = "maroon") +
  xlim(-110, 10) + xlab("RSSI") + 
  ggtitle("What is the most common RSSI signal recorded?",
          subtitle = "Great signal ranges between -30 and -67") +
  theme_minimal() # geom_histogram gives another count...

wifi_melt %>% filter(rssi != 100) %>% summary() 
# median = -82
# 1Q     = -88
# 3Q     = -72

wifi_melt %>% filter(rssi != 100) %>% filter(rssi > -85) %>% summary()
# median = -74
# 1Q     = -80
# 3Q     = -65
# Filter out good Signal ....................................................                        
strong_signal <- wifi_melt %>% filter(rssi != 100) %>% filter(between(rssi,-85,-30)) %>% 
  group_by(rssi) %>% tally(sort = T) 

ggplot(strong_signal) + geom_col(aes(rssi, n), fill = "blue", alpha = 0.5) + theme_minimal() +
  ggtitle("Distribution of strong sinal regsistry") + ylab("count")

# RSSI per Building .............................................
str(wifi_melt)
wifi_melt %>% filter(rssi !=100) %>% group_by(rssi,BUILDINGID) %>% tally(sort = T) %>%
  ggplot()+ geom_col(aes(rssi,n, fill = BUILDINGID)) + facet_grid(.~BUILDINGID) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() + ggtitle("Recorded signal strength per Building") + ylab("")

# RSSI of a 100 per Building ..............................................
wifi_melt %>% filter(rssi == 100) %>% group_by(rssi, BUILDINGID) %>% tally(sort = T) %>%
  ggplot()+ geom_col(aes(rssi,n, fill = BUILDINGID)) + facet_grid(.~BUILDINGID) +
  scale_fill_brewer(palette = "Set2") + ylab("") +
  theme_minimal() + ggtitle("Recorded signal strength per Building", 
                            subtitle = "Taking a look at records with a 100") 

# WAPS per Building ......................................................
B0 <- wifi_train %>% filter(BUILDINGID == 0) %>% gather("waps", "rssi", 1:520) %>% filter(rssi !=100) 
B1 <- wifi_train %>% filter(BUILDINGID == 1) %>% gather("waps", "rssi", 1:520) %>% filter(rssi !=100)
B2 <- wifi_train %>% filter(BUILDINGID == 2) %>% gather("waps", "rssi", 1:520) %>% filter(rssi !=100)

uniqueN(B0$waps) # 200
uniqueN(B1$waps) # 207
uniqueN(B2$waps) # 203 
# waps are shared between buildings

# I want to plot a heat map for the RSSI signals .........................
wifi_matrix <- wifi_melt[,10:11] %>% filter(rssi !=100)
wifi_matrix <- as.matrix(wifi_melt)

# data too large...reshape?
plotly::plot_ly(x=colnames(wifi_matrix), y=rownames(wifi_matrix), z = wifi_matrix,
                type = "heatmap", colorscale= "Earth")

# Number of Waps recorded per observation:
wifi_train$captured_waps <- apply(ifelse(wifi_train[,1:520] !=100, 1,0), 1, sum)

boxplot(wifi_train$captured_waps, horizontal = T)

ggplot(wifi_train) + geom_bar(aes(captured_waps), fill = "orange") + xlab("Number of WAPS") +
  ggtitle("Detected WAPS per Observation") + theme_minimal()

# User Engagement   ......................................................
ggplot(wifi, aes(BUILDINGID, USERID)) + 
  geom_count(aes(color= ..n..)) + guides(color = 'legend') +
  ggtitle("User Activity per Building") + xlab("Building") + ylab("User") + theme_minimal()

ggplot(wifi) + geom_bar(aes(USERID), fill = 'red') + ggtitle("User Engagement") + xlab("User") + theme_minimal()
phonePlot <- ggplot(wifi) + geom_point(aes(LONGITUDE, LATITUDE, color = PHONEID)) + ggtitle("Phone Used per Building") + theme_minimal()
phoneID <- ggplot(wifi) + geom_bar(aes(PHONEID), fill = "orange") + theme_minimal()
ggplot(wifi) + geom_point(aes(LONGITUDE, LATITUDE, color = USERID)) + ggtitle("User per Building") + theme_minimal()

# User Per Building ......................................................
ggplot(B0) + geom_point(aes(LONGITUDE, LATITUDE, color = USERID)) +
  facet_grid(BUILDINGID ~ FLOOR) + ggtitle("Building 0", subtitle = "Data per Floor") + theme_minimal()

ggplot(B1) + geom_point(aes(LONGITUDE, LATITUDE, color = USERID)) +
  facet_grid(BUILDINGID ~ FLOOR) + ggtitle("Building 1", subtitle = "Data per Floor") + theme_minimal()

ggplot(B2) + geom_point(aes(LONGITUDE, LATITUDE, color = USERID)) +
  facet_grid(BUILDINGID ~ FLOOR) + ggtitle("Building 2", subtitle = "Data per Floor") + theme_minimal()

# How many spaceID -----
table(duplicated(wifi$SPACEID)) # 123 unique SPACEID

wifi %>% filter(BUILDINGID == 0) %>% distinct(SPACEID) # 78 unique SPACEID
wifi %>% filter(BUILDINGID == 1) %>% distinct(SPACEID) # 86 unique SPACEID
wifi %>% filter(BUILDINGID == 2) %>% distinct(SPACEID) # 97 unique SPACEID

# Walked Distance ...............................................................

# Distance recorded per Building (test with 2, floor 3)
loc <- as.data.frame(wifi[,c(521,522)])

# Shifting vectors for latitude and longitude to include end position
shift.vec <- function(vec, shift){
  if (length(vec) <= abs(shift)){
    rep(NA ,length(vec))
  } else {
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec) - shift)]) }
    else {
      c(vec[(abs(shift) + 1):length(vec)], rep(NA, abs(shift)))
    }
  }
}

loc$lat <- shift.vec(loc$LATITUDE, -1)
loc$lon <- shift.vec(loc$LONGITUDE, -1)

# Calculating distances between points (in metres) with the function pointDistance 
# from the 'raster' package.
library(raster)
loc$dist.to.prev <- apply(loc, 1, FUN = function(row) {
  pointDistance(c(as.numeric(as.character(row["lat"])),
                  as.numeric(as.character(row["lon"]))),
                c(as.numeric(as.character(row["LATITUDE"])), 
                  as.numeric(as.character(row["LONGITUDE"]))),
                lonlat = T) # Parameter 'lonlat' has to be TRUE!
})
# distance in km
round(sum(as.numeric(as.character(loc$dist.to.prev)))*0.001, digits = 2)

# .......................................................................[VALIDATION DATA]

wifi_valid[,c(523:528)] <- wifi_valid[,c(523:528)] %>% mutate_if(is.integer, as.factor) 

# Duplicates wifi_valid? ----
table(duplicated(wifi_valid)) 

# The histogram checking the RSSI recorded in wifi_valid
wifi_valid_melt <- wifi_valid %>% gather("waps", "rssi", 1:520)

ggplot() + 
  geom_bar(data = wifi_valid_melt, aes(rssi), fill = 'pink') + 
  geom_bar(data = wifi_melt, aes(rssi), fill = "blue", alpha = 0.1) +
  xlab("RSSI(dBM)") + xlim(-110, 10) + theme_minimal() +
  ggtitle("Recorded signal strength", 
          subtitle = "Comparing Training vs Validation Datasets")

# User Engagement
ggplot(wifi_valid, aes(BUILDINGID, USERID)) + 
  geom_count(aes(color= ..n..)) + guides(color = 'legend') +
  xlab("Building") + ylab("User") + theme_minimal() +
  ggtitle("User Activity per Building", subtitle = "Unique User")

ggplot(wifi_valid) + geom_point(aes(LONGITUDE, LATITUDE, color = USERID)) + 
  ggtitle("User per Building") + theme_minimal()

ggplot(wifi_valid) + geom_point(aes(LONGITUDE, LATITUDE, color = PHONEID)) + 
  ggtitle("Phone Used per Building", 
          subtitle = "One user recorded RSSI strength with 11 phones") + theme_minimal()

# Phone Per Building
wifi_valid %>% filter(BUILDINGID == 0) %>% 
  ggplot() + geom_point(aes(LONGITUDE, LATITUDE, color = PHONEID)) +
  facet_grid(BUILDINGID ~ FLOOR) + ggtitle("Building 0", subtitle = "Data per Floor") + theme_minimal()

wifi_valid %>% filter(BUILDINGID == 1) %>%
  ggplot() + geom_point(aes(LONGITUDE, LATITUDE, color = PHONEID)) +
  facet_grid(BUILDINGID ~ FLOOR) + ggtitle("Building 1", subtitle = "Data per Floor") + theme_minimal()

wifi_valid %>% filter(BUILDINGID == 2) %>%
  ggplot() + geom_point(aes(LONGITUDE, LATITUDE, color = PHONEID)) +
  facet_grid(BUILDINGID ~ FLOOR) + ggtitle("Building 2", subtitle = "Data per Floor") + theme_minimal()