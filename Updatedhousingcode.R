library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)
library(readr)
library(tidyr)


#Pulling Data

citypopschange <- read_excel("SUB-IP-EST2023-CUMCHG.xlsx", skip = 3)

#Taking all cities with higher then national average growth

citypopschange <- citypopschange[1:800, ]

#Cleaning the data to make sure it is usable

citypopschange <- citypopschange %>%
  mutate_at(vars(3:6), ~ as.numeric(gsub("%", "", .)))  

clean_data <- na.omit(citypopschange)

scaled_data <- scale(clean_data[, 3:6]) 

#Finding optimal Clustering values with K-means/Elbow method. Find WCSS first then make Graph

wss <- numeric(10)
for (k in 1:10) {
  wss[k] <- kmeans(scaled_data, centers = k, nstart = 25)$tot.withinss
}

plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)", 
     ylab = "Total Within-Cluster Sum of Squares")

optimal_kmeans <- kmeans(scaled_data, centers = 5, nstart = 25)

#Sorting cities through clusters

citypopschange$cluster <- NA
citypopschange[rownames(citypopschange), "cluster"] <- optimal_kmeans$cluster

cluster_counts <- as.data.frame(table(citypopschange$cluster))
colnames(cluster_counts) <- c("Cluster", "Count")
print(cluster_counts)

#Creating summary of each cluster to figure out which groups to use

cluster_summary <- citypopschange %>%
  group_by(cluster) %>%
  summarise(
    avg_percent_change = mean(`Percent`, na.rm = TRUE),
    min_percent_change = min(`Percent`, na.rm = TRUE),
    max_percent_change = max(`Percent`, na.rm = TRUE),
    lowest_pop_raw = min(`Number`, na.rm = TRUE),
    highest_pop_raw = max(`Number`, na.rm =TRUE)
  )

print(cluster_summary)

#Plotting AVG percent change by cluster to choose from

ggplot(cluster_summary, aes(x = factor(cluster), y = avg_percent_change, fill = factor(cluster))) +
  geom_bar(stat = "identity") +  
  theme_minimal() +
  labs(title = "Average Percent Change by Cluster",
       x = "Cluster",
       y = "Average Percent Change (%)",
       fill = "Cluster") +
  theme(legend.position = "none")  

#Creating optimal cities dataframe from gathered info

optimal_cities <- citypopschange %>%
  filter( cluster == 2 | cluster == 3)
colnames(optimal_cities) <- c("Rank","City","2020pop","2023pop","rawpopchange","poppercentchange","cluster")

#Loading in redfin data

Mediandata <- read_tsv("city_market_tracker.tsv000")

#Attempting to split cities into keys

optimal_cities <- optimal_cities %>%
  separate(`City`, into = c("City", "State"), sep = ", ")

#Combining the city and state to create a key and lock to overlap and filter data sets. 
#In theory should create a city-state column as one word which is done in both the census initial data then the 
#second dataset, thereby filtering to only keep matches

clean_names <- function(df) {
  df <- df %>%
    rename_with(str_trim) %>% 
    rename(City = any_of(c("City", "city")),  
           State = any_of(c("State", "state")))
  
  df %>%
    mutate(
      City = str_to_lower(str_trim(str_replace_all(City, "\\s+", ""))), 
      State = str_to_lower(str_trim(str_replace_all(State, "\\s+", "")))
    ) %>%
    mutate(CityStateKey = paste0(City, "_", State))
}

#Attempting to implement with the redfin data

optimal_cities_clean <- clean_names(optimal_cities)
Mediandata_clean <- clean_names(Mediandata)

filtered_houses <- Mediandata_clean %>%
  semi_join(optimal_cities_clean, by = "CityStateKey")

head(filtered_houses)

