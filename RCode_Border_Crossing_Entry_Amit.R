##############   STAT-515 Project on Border Crossing Entry Data  ###############

###                By Amit Chowdary             ########


#1 Plotting a bar graph
library(ggplot2)
library(dplyr)
border_data <- read.csv("C:/Users/Amit/Downloads/Border_Crossing_Entry_Data.csv")
border_data$Date <- as.Date(border_data$Date, format = "%b-%d")
summary_data <- border_data %>%
  group_by(Port.Name, State) %>%
  summarise(TotalEntries = sum(Value))
summary_data_sorted <- summary_data %>%
  arrange(desc(TotalEntries))
top_30_data <- head(summary_data_sorted, 30)
bar_chart <- ggplot(top_30_data, aes(x = reorder(Port.Name, TotalEntries), y = TotalEntries, fill = State)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Number of Entries by Port Name or State (Top 30)",
       x = "Total Entries",
       y = "Port Name or State",
       fill = "State") +
  theme_minimal() +
  coord_flip()
print(bar_chart)








#2 Plotting a Viloin plot 
library(ggplot2)
library(dplyr)
library(scales) 
data <- read.csv("C:/Users/reddy/Downloads/Border_Crossing_Entry_Data.csv")
data$Value <- as.numeric(as.character(data$Value)
data <- na.omit(data)
data$Value <- log1p(data$Value) 
data$MeasureFactor <- factor(data$Measure, levels = unique(data$Measure))
ggplot(data, aes(x = MeasureFactor, y = Value, fill = MeasureFactor)) +
  geom_violin(trim = FALSE, adjust = 0.5, width = 1) +
  scale_fill_brewer(palette = "Set3") +  
  scale_y_continuous(labels = comma) +  
  labs(title = "Violin Plot of Border Crossings by Measure",
       x = "Type of Crossing",
       y = "Log Transformed Number of Crossings") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    plot.title = element_text(hjust = 0.5),           
    legend.title = element_blank()                      
  ) +
  guides(fill = guide_legend(reverse = TRUE)) 







#3 Drawing a Geospatial map 
library(leaflet)
# Read the CSV file
border_data <- read.csv("C:/Users/reddy/Downloads/Border_Crossing_Entry_Data.csv")
border_data$Date <- as.Date(border_data$Date, format = "%b-%d")
m <- leaflet(border_data) %>%
  setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~Longitude,
    lat = ~Latitude,
    clusterOptions = markerClusterOptions(),
    popup = ~paste("Port Name:", Port.Name, "<br>",
                   "State:", State, "<br>",
                   "Measure:", Measure, "<br>",
                   "Value:", Value, "<br>",
                   "Date:", Date)
  )

#Printthemap
m





#4 Showing Principal Component Analysis 
library(readr)
library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
border_data <- read_csv("C:/Users/reddy/Downloads/Border_Crossing_Entry_Data.csv")
border_data <- border_data %>%
  select_if(is.numeric) %>%  
  na.omit()  
print(head(border_data))
pca_result <- prcomp(border_data, scale. = TRUE)  
fviz_pca_biplot(pca_result, label = "var", repel = TRUE,
                ggtheme = theme_minimal(), 
                title = "PCA Biplot with Loading Plots")
explained_variance <- summary(pca_result)$importance[2,]
print(explained_variance)
set.seed(123) 
kmeans_result <- kmeans(pca_result$x[, 1:2], centers = 3)
border_data$cluster <- as.factor(kmeans_result$cluster)
fviz_cluster(list(data = pca_result$x[, 1:2], cluster = kmeans_result$cluster), 
             geom = "point", 
             ellipse = TRUE, 
             star.plot = TRUE, 
             repel = TRUE, 
             ggtheme = theme_minimal(),
             main = "Clusters in PCA Space")



