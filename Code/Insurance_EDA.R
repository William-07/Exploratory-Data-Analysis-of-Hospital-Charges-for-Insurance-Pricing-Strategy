data <- read.csv("/Users/shenglienlee/Desktop/Work/2024_spring/Intro to BA/HW1/Data/Raw/insurance.csv")
#print(data)

#data structure of the each feature
str(data)
#see the describe statistic of the data
summary(data)

# Count number of missing values in each column
Num_of_Null<- sapply(data, function(x) sum(is.na(x)))
print(Num_of_Null)

#numeric data
#correlation matrix
#load library
library(ggplot2)
library(corrplot)
#select numeric features
numeric_data <- data[sapply(data, is.numeric)]
#plot correlation matrix
cor_matrix <- cor(numeric_data)
# Plot the correlation matrix
corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, tl.cex = 1, 
         addCoef.col = "darkgray")

#histogram(frequency)
numeric_features <- names(data)[sapply(data, is.numeric) & names(data) != "charges"]

# Loop through each numeric feature to plot its histogram
for (feature in numeric_features) {
  p <- ggplot(data, aes_string(x = feature)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +  # Adjust binwidth as necessary
    xlab(feature) +
    ylab("Count") +
    ggtitle(paste("Histogram of", feature))
  
  print(p)
}

#scatter plot 
#select numeric features and except charges
numeric_features <- names(data)[sapply(data, is.numeric) & names(data) != "charges"]

# Loop through each numeric feature to plot it against 'charges'
for (feature in numeric_features) {
  p <- ggplot(data, aes_string(x = feature, y = "charges")) +
    geom_point() +
    xlab(feature) +
    ylab("Charges") +
    ggtitle(paste("Scatter Plot of", feature, "vs Charges"))
  
  print(p)
}

#Nominal Data
#select nominal data
nominal_features <- data[sapply(data, is.character)]
#print(nominal_data)

#select nominal data
nominal_columns <- names(nominal_features)

# Loop through each nominal column to create a pie chart with percentages
for (column in nominal_columns) {
  # Calculate the count of each category
  data_for_plot <- as.data.frame(table(nominal_features[[column]]))
  
  # Calculate percentages
  data_for_plot$Percentage <- round(data_for_plot$Freq / sum(data_for_plot$Freq) * 100, 1)
  
  # Create the pie chart
  p <- ggplot(data_for_plot, aes(x = "", y = Freq, fill = Var1)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y") +
    geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5)) +
    labs(title = paste("Pie Chart of", column), x = NULL, y = NULL, fill = column) +
    theme_void()
  
  print(p)
}

#box plot
nominal_features <- names(data[sapply(data, function(x) is.character(x))])

# Loop through each nominal column to create a box plot for 'charges' by each category
for (column in nominal_features) {
  p <- ggplot(data, aes_string(x = column, y = "charges")) +
    geom_boxplot() +
    labs(x = column, y = "Charges", title = paste("Box Plot of Charges by", column)) +
    theme_light() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x labels for readability
  
  print(p)
}
