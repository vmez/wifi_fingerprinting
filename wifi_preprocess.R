# Resources ........................................................................
# Caret: https://topepo.github.io/caret/visualizations.html 


# Libraries: preProcess .............................................................
pacman::p_load(tidyverse, data.table, caret, corrplot, RColorBrewer)

# ...................................................................................
setwd("C:/Users/Violeta/Documents/Ubiqum/3_Deep.Analytics/Wifi")
wifi_train <- fread("trainingData.csv", sep = ',', header = T) 
wifi_train[,c(523:528)] <- wifi_train[,c(523:528)] %>% mutate_if(is.integer, as.factor) 

# Remove Duplicates ..................................................................
wifi <- wifi_train %>% filter() %>% distinct()

# Exclude inactive waps ........................................ ....................
#inactive <- wifi_melt %>% group_by(waps, rssi) %>% tally() %>% filter(rssi == 100 & n == 19300)
    # 55 inactive wapps

#wifi[,1:520] %>% summarise_all(var) %>% select_if(function(.) . == 0) %>% names()
    # names of full 100 waps

# Transform 100 to -105 and remove columns with Full-105 (code above does not erase obs)
#for (i in 1:520) {wifi[which(wifi[,i] == 100),i] <- -105}

#wifi <- wifi[, !apply(wifi == -105, 2, all)] 

# Near Zero Variance from Caret .....................................................
nzv_metrics <- nearZeroVar(wifi[,1:520], saveMetrics = T)
obs_nzv <- which(nzv_metrics$zeroVar == T)
wifi <- wifi[, -obs_nzv]

# Convert 100 to -105
for(i in 1:465){ wifi[which(wifi[,i] == 100), i] <- -105}


# DF Logarithmic normalization for comparison purposes

# Correlation plot ..................................................................
  # Dataset only with WAPS + Building + Floor + Lat + Lon
wifi_melt <- wifi %>% gather("waps", "rssi", 1:465) %>% mutate_if(is.integer, as.numeric)
#wifi_melt2 <- within(wifi_melt, rm(waps, USERID, TIMESTAMP, RELATIVEPOSITION, SPACEID))

  # Normalize logscale RSSI
#test <- wifi_melt2
#test$rssi <- sign(test$rssi) * (abs(test$rssi))^(1/3) 
# cube root to deal with negative values but returns negative

  # Flip Data to positive numbers
#test$rssi <- abs(test$rssi) # returns absolute numbers

  # Normalize rssi using a log scale
#test$rssi <- log10(test$rssi)

  # DummyVars to find correlation (limit on dummy prediction 10Gb)
#dmy <- dummyVars(~. , test)
#dmy_wifi <- data.frame(predict(dmy, test)) # takes long time

  # Correlation Plot
#corrplot(cor(dmy_wifi, method = "pearson"), method = "number", order = "FPC", type = "upper", 
#        diag = FALSE, insig = "blank", tl.srt = 45, col=brewer.pal(n=4, name="RdYlBu"))
# lat & B0 .78      
# B0 & Lon .-85
# B2 $ Lon .85
# Lat & Lon -.86
# same correlation after having changed rssi to a positive value.
rm(nzv_metrics, i, obs_nzv)

  # Feature Engneering ......................................................................
# Principle Component Analysis  
values_wifi <- princomp(, scores = T, cor = T)

# Test Cluster Analysis .....................................................................
# Make DF with relevant attributes: 
data <- within(wifi, rm(SPACEID, RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))
#waps <- wifi[,1:465]

  # K-means Cluster
clust <- kmeans(data, 16)
data$groups <- as.factor(clust$cluster)
phoneClust <- ggplot(data) + geom_point(aes(LONGITUDE, LATITUDE, color = groups)) + theme_minimal() +
  ggtitle("Cluster Analysis", subtitle = ("WAPS + LAT + LON + BUILDING + FLOOR"))
    # with no specified iterations, it did 2. The clusters are not defined per building (center = 3)
    # with iter.max at 5, it identified the clusters per building.(center = 3)
    # if center 5, returns 3 iterations, and identifies 5 across three buildings
    # if center 5, with 5 iterations: similar output as above
    # if center 15 (to add to the number of phones used): similar output except for in building 0
gridExtra::grid.arrange(phonePlot, phoneClust)

phoneID_cluster <- ggplot(data) + geom_bar(aes(groups), fill = "blue") + theme_minimal()
gridExtra::grid.arrange(phoneID, phoneID_cluster, 
                        top = "Comparing Phone Captures to 16 Clusters")
#clust_waps <- kmeans(waps, 3)
#waps$groups <- as.factor(clust_waps$cluster)
#ggplot(waps) + geom_bar(aes(groups)) + theme_minimal() +
#  ggtitle("Cluster Analysis", subtitle = ("Only WAPS were used"))
# when cluster just with waps, there is a greater imbalance between the groups.

# Feature (takes a looooooooong time....)
featurePlot(x = wifi_melt[, c(1:4,11)], y = wifi_melt$BUILDINGID,
            plot = "ellipse", auto.key = list(columns = 5))
