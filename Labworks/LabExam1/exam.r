 

getwd()
setwd("C:\\Users\\aravi\\Desktop\\StatisticsUsingR\\Labworks\\LabExam1")
getwd()

data <- read.csv("UScereal1.csv")
nrow(data)


# find the maximum protien value of each manufacturer
max_protein_by_manufacturer <- aggregate(protein ~ mfr, data = data, FUN = max)
print(max_protein_by_manufacturer)

# Investigate the data set for missing na values look at teh distribution of the data set to replace the missing values 
# if the distribution is normal distribution replace missing value with mean value ,
# if the distribution is left skewed then replace with minimum and if is right skewed replace with maximum value

missing_values <- sum(is.na(data))
print("Number of missing values:")
missing_values

print(summary(data))

for (col in names(data)) {
  if (sum(is.na(data[[col]])) > 0) {  
    if (is.numeric(data[[col]])) {
      data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
    } else {
      data[[col]][is.na(data[[col]])] <- names(sort(table(data[[col]]), decreasing = TRUE)[1])
    }
  }
}

missing_values_after_replace <- sum(is.na(data))

cat("Number of missing values after replacement:", missing_values_after_replace, "\n")

#get the summary statistics after handling the missing data 
# (mean median min max 1st quartile 3rd quartile and standard deviation
print(summary(data))
sd1<-sd(data$calories)
sd2<-sd(data$fat)
sd3<-sd(data$sugars)
sd4<-sd(data$sodium)
sd5<-sd(data$carbo)
sd6<-sd(data$potassium)
sd7<-sd(data$protein)
sd8<-sd(data$fibre)

sddf<-data.frame("Standard Deviation" = c(sd1,sd2,sd3,sd4,sd5,sd6,sd7,sd8) ,
"Parameter"= c("Calorie","fat","Sugars","Sodium","CarboHydrates","Potassium","Protein","Fibre")
)
sddf
hist(data$calories,main = "Calories")
hist(data$fat,main = "Fat")
hist(data$sugar,main = "Sugar")
hist(data$carbo,main = "Calories")
hist(data$potassium,main = "Calories")
hist(data$protein,main = "Calories")



# draw appropriate graph using ggplot 2 for the following
# analyse the spread of the data set for the manufacturer to check how each one has given preference on Fiber

library(ggplot2)


# Plot spread of Fiber for each manufacturer
ggplot(data, aes(x = mfr, y = fibre, fill = mfr)) +
  geom_boxplot() +
  labs(title = "Spread of Fiber for Each Manufacturer",
       x = "Manufacturer Name",
       y = "Fiber Content Vale") +
  theme_minimal()

# create a plot to find the outlier for each shelf
ggplot(data, aes(x = as.factor(shelf), y = fibre)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Outliers In Each Shelf",
       x = "ShelfNumber",
       y = "Fiber Content") +
  theme_minimal()


#create a plot to explore all numeric variable
ggplot(data, aes(x = factor(1), y = fibre)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  facet_wrap(~mfr, scales = "free_y") +
  labs(title = "Numeric Variables",
       x = "",
       y = "Fiber Content") +
  theme_minimal()




#   Identify the top four mean variables and create a dataframe called GreaterMeanFour
str(data)
means<-c(
mean(data$calories),
mean(data$protein),
mean(data$fat),
mean(data$sodium),
mean(data$fibre),
mean(data$carbo),
mean(data$sugars),
mean(data$potassium))
means
names(means)<-c("Cal","Protein","Fat","Sodium","Fibre","Carbs","Sugar","Pottasium")
means<-sort(means,decreasing = TRUE)
means
GreaterMeanFour<-data.frame(
    data$sodium,
    data$potassium,
    data$calories,
    data$carbo
)
names(GreaterMeanFour)<-c("sodium","potassium","calories","carbo")
print(GreaterMeanFour)


# Find the strength of the relationship of GreaterMeanFour and plot the relationship
corr <- cor(GreaterMeanFour) 
corr
strength <- round(corr^2, 0)
print(paste("The correlation strength is",strength))
# Assuming 'GreaterMeanFour' is your data frame
# Plot the relationships using ordinary scatter plots

plot(GreaterMeanFour$sodium, GreaterMeanFour$potassium, xlab = "Sodium", ylab = "Potassium", main = "Scatter Plot between Sodium and Pottasium")
plot(GreaterMeanFour$sodium, GreaterMeanFour$calories, xlab = "Sodium", ylab = "Calories", main = "Scatter Plot between Sodium and Calories")
plot(GreaterMeanFour$sodium, GreaterMeanFour$carbo, xlab = "Sodium", ylab = "Carbo", main = "Scatter Plot between Sodium and Carbohydrates ")
plot(GreaterMeanFour$potassium, GreaterMeanFour$calories, xlab = "Potassium", ylab = "Calories", main = "Scatter Plot between Pottasium and Calories")
plot(GreaterMeanFour$potassium, GreaterMeanFour$carbo, xlab = "Potassium", ylab = "Carbohydrates", main = "Scatter Plot between Pottasium and Carbohydrates")
plot(GreaterMeanFour$carbo, GreaterMeanFour$calories, xlab = "Carbohydrates", ylab = "Calories", main = "Scatter Plot between Carbodydrates and Calories")


# create a simple linear regression model using strongly positively correlated variables and plot it 

StrongCorrFields <- GreaterMeanFour[c("sodium", "potassium")]
model <- lm(potassium ~ sodium, data = StrongCorrFields)
plot(StrongCorrFields$sodium, StrongCorrFields$potassium, xlab = "Sodium", ylab = "Potassium", main = "Scatter Plot and Regression Line")
abline(model, col = "red")
# regression equation 
text(200, 800, labels = paste("y =", round(coef(model)[1], 2), "+", round(coef(model)[2], 2), "x"), col = "blue")








# show the prediction for the value before and after removal of outlier

# Create a simple linear regression model
model <- lm(potassium ~ sodium, data = GreaterMeanFour)
# Predict the values
beforeOutlier <- predict(model, newdata = GreaterMeanFour)

# Identify and remove outliers
outliers <- which(abs(residuals(model)) > 2 * sd(residuals(model)))
GreaterMeanFourNoOutliers <- GreaterMeanFour[-outliers, ]

# Fit a new model after removing outliers
modelNoOutliers <- lm(potassium ~ sodium, data = GreaterMeanFourNoOutliers)

afterOutlierRemoval <- predict(modelNoOutliers, newdata = GreaterMeanFourNoOutliers)

print("Predictions Before Outlier Removal:")
print(beforeOutlier)

print("Predictions After Outlier Removal:")
print(afterOutlierRemoval)


# Only Seven Null were missing values were found in the data and was removed based on the type of distribution
# potassium has the highest standard deviation
# Manufacture N has sthe most spread of the calories followed by manufacture P
# Shelf 3 has the most number of outliers on the basis of fibre count 
# There exist 8 numeric values in the dataset
# There exist a very strong positive correlation between sodium and potassium 





