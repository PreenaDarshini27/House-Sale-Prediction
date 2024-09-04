# Installing and loading necessary packages
install.packages("Amelia")
install.packages("leaflet")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("car")
install.packages("lmtest")

library(lmtest)
library(car)
library(Amelia)
library(leaflet)
library(ggplot2)
library(corrplot)

# Loading the housing.csv dataset
data <- read.csv("housing.csv")

# Basic data exploration
print(dim(data))  # Dimensions of the dataset
print(nrow(data)) # Number of rows
print(ncol(data)) # Number of columns
print(colnames(data)) # Display column names
head(data, 20)    # View the first 20 rows of the dataset
str(data)         # Check the structure of the dataset
summary(data)     # Summary statistics of the dataset

# Checking for missing values
# Figure 1
print(sapply(data, function(x) sum(is.na(x)))) # Percentage of missing values
missmap(data, main = "Missing Map") # Missingness map

# Histograms for distribution analysis
hist(data$Lot_Area)#Figure 3
hist(data$Lot_Frontage) #Figure 2
hist(data$Year_Built)
hist(data$Total_Bsmt_SF)
hist(data$First_Flr_SF)#Figure 4
hist(data$Second_Flr_SF)#Figure 5
hist(data$Full_Bath)
hist(data$Half_Bath)
hist(data$Bedroom_AbvGr)
hist(data$Kitchen_AbvGr)
hist(data$Fireplaces)
hist(data$Longitude)
hist(data$Latitude)
hist(data$Sale_Price)#Figure 6
# Bar plots for categorical variables
barplot(table(data$Bldg_Type))
barplot(table(data$House_Style))
barplot(table(data$Overall_Cond))
barplot(table(data$Exter_Cond))
# Categorizing Sale Price and mapping it
data$Price_Category <- ifelse(data$Sale_Price > median(data$Sale_Price), "High", "Low")
map <- leaflet(data) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~Longitude, lat = ~Latitude, radius = 5, color = "white",
                   fillColor = ~ifelse(Price_Category == "High", "red", "green"),
                   fillOpacity = 0.8, popup = ~paste("Sale Price: $", Sale_Price)) %>%
  addLegend(colors = c("green", "red"), labels = c("Low Sale Price", "High Sale Price"),
            title = "Price Category", opacity = 0.8)
print(map)#Figure 12

# Data type conversion for categorical variables
data$Bldg_Type <- as.factor(data$Bldg_Type)
data$House_Style <- as.factor(data$House_Style)
data$Overall_Cond <- as.factor(data$Overall_Cond)
data$Exter_Cond <- as.factor(data$Exter_Cond)


# Creating dummy variables for categorical predictors
data$Bldg_Type_Duplex <- ifelse(data$Bldg_Type == "Duplex", 1, 0)
str(data) # Check updated data structure


# Outlier detection using boxplot
boxplot(data$Lot_Area)
boxplot(data$Lot_Frontage)
boxplot(data$Year_Built)
boxplot(data$Total_Bsmt_SF)
boxplot(data$First_Flr_SF)
boxplot(data$Second_Flr_SF)
boxplot(data$Full_Bath)#no outliers
boxplot(data$Half_Bath)#no outliers
boxplot(data$Bedroom_AbvGr)
boxplot(data$Kitchen_AbvGr)
boxplot(data$Fireplaces)
boxplot(data$Longitude)#no outliers
boxplot(data$Latitude)#no outliers
boxplot(data$Sale_Price)
# Handling outliers
# Calculating the first quartile
q1 <- quantile(data$Sale_Price, 0.25)

# Calculating the third quartile
q3 <- quantile(data$Sale_Price, 0.75)

# Calculating the interquartile range
iqr <- q3 - q1

# Calculating upper bound and lower bound for outliers
upper_bound <- q3 + 3 * iqr
lower_bound <- q1 - 3 * iqr
outliers <- data$Sale_Price < lower_bound | data$Sale_Price > upper_bound

# Extracting outliers
outlier_data <- data[outliers, ]

# Displaying details of outliers 
print(outlier_data)

#Similarly for independent variables
q11 <- quantile(data$First_Flr_SF, 0.25)
q33 <- quantile(data$First_Flr_SF, 0.75)
iqr1 <- q33 - q11
upper_bound1 <- q33 + 1.5 * iqr1
lower_bound1 <- q11 - 1.5 * iqr1
outliers1 <- data$First_Flr_SF < lower_bound1 | data$First_Flr_SF > upper_bound1
outlier_data1 <- data[outliers1, ]
print(outlier_data1)
#Transformation of variables to remove outliers and normalize it 
# We take log or square root transformations to handle skewness and outliers and the outliers are meaningful.
# Log transformation of Sale_Price as the outliers in Sale_Price are not incorrect data entries. Hence, taking Log transformation is a better choice than omitting / deleting the outliers.
data$Log_Sale_Price <- log(data$Sale_Price)
data$Log_First_Flr_SF<- log(data$First_Flr_SF)
# Log Transformation for variables with left skew or right skew
data$Log_Lot_Area<- log(data$Lot_Area)
data$Sq_Lot_Frontage<- sqrt(data$Lot_Frontage)
data$Sq_Second_Flr_SF<- sqrt(data$Second_Flr_SF)


# see if variable is normally distributed
hist(data$Log_Lot_Area)#Figure 8
hist(data$Sq_Second_Flr_SF)#Figure 10
hist(data$Log_First_Flr_SF)#Figure 9
hist(data$Log_Sale_Price)#Figure 11
hist(data$Sq_Lot_Frontage)#Figure 7

# Calculate the correlation matrix
selected_columns <- c("Log_Sale_Price",  "Sq_Lot_Frontage","Log_Lot_Area", "Total_Bsmt_SF", "Log_First_Flr_SF", "Sq_Second_Flr_SF", "Full_Bath", "Half_Bath", "Bedroom_AbvGr", "Kitchen_AbvGr", "Fireplaces", "Longitude", "Latitude")
data_subset <- data[, selected_columns]
# Compute the correlation matrix
correlation_matrix <- cor(data_subset)
print(correlation_matrix) #Figure 14
# Plot the correlation matrix to understand relationships between the variables
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust")#Figure 13

# Set the random seed
set.seed(22238590)

# Split the data into train and test sets
train_index <- sample(1:2413, size = 1930)
train <- data[train_index, ]
test <- data[-train_index, ]

# Print the number of rows in each set
print(nrow(train))
print(nrow(test))

# Check levels in the training and test datasets
levels_train <- levels(train$Overall_Cond)
levels_test <- levels(test$Overall_Cond)

# Check if there are any new levels in the test dataset
new_levels <- levels_test[!(levels_test %in% levels_train)]

# If there are new levels, add them to the training dataset levels
if (length(new_levels) > 0) {
  train$Overall_Cond <- factor(train$Overall_Cond, levels = c(levels_train, new_levels))
}


# Model Building

# Train the model using linear regression
model <- lm(Log_Sale_Price ~ Sq_Lot_Frontage + Log_Lot_Area + Bldg_Type_Duplex + House_Style + Overall_Cond + Year_Built + Exter_Cond + Total_Bsmt_SF + Log_First_Flr_SF + Sq_Second_Flr_SF + Full_Bath + Half_Bath + Bedroom_AbvGr + Kitchen_AbvGr + Fireplaces + Longitude + Latitude, data = train)

# Calculate the fitted values for the training set
train$fitted.values <- predict(model, newdata = train)

# Print the first few rows of the fitted values
head(train$fitted.values)
str(train$fitted.values)
coef(model)
# Evaluate the model performance on the training set
summary(model)$r.squared

# Durbin-Watson Test for Autocorrelation

dw_test <- dwtest(model, data = train)
print(dw_test)

# P-P Plot for Normality of Residuals #Figure 15
residuals_model <- residuals(model)
qqnorm(residuals_model, main = "P-P Plot for Model Residuals")
qqline(residuals_model, col = "red")

cooks_dist <- cooks.distance(model)#Figure 16
plot(cooks_dist, main="Cook's Distance", type="h")  
# type="h" is used to create a stem-and-leaf plot
# Identifying influential observations
abline(h = 4/length(cooks_dist), col = "red")

# Using a scatterplot to see the relationship between actual Log_Sale_Price and predicted Log_Sale_Price on the training data
plot(train$Log_Sale_Price ~ train$fitted.values)
abline(a = 0, b = 1, col = "blue")#Figure 17



# Residual plots to check the model

# Scatter plot of residuals and fitted values
plot(model, which = 1)#Figure 18

# Histogram of residuals to check the distribution
residuals <- resid(model)#Figure 19
hist(residuals, breaks = 30, main = "Histogram of Residuals", xlab = "Residuals")

# Calculating the fitted values on the test dataset
test$fitted.values <- predict(model, newdata = test)

# Evaluating the model performance on the test dataset
test_r_squared <- summary(lm(Log_Sale_Price ~ fitted.values, data = test))$r.squared

# Calculating MAE, MSE, and RMSE
mae <- mean(abs(test$Log_Sale_Price - test$fitted.values))
mse <- mean((test$Log_Sale_Price - test$fitted.values)^2)
rmse <- sqrt(mse)

# Printing the evaluation metrics
cat("Test R-squared:", test_r_squared, "\n")
cat("MAE:", mae, "\n")
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")

# Viewing the model coefficients
coef(model)

# Checking the significance of coefficients
summary(model)

# Creating scatterplot of actual vs. predicted values on the test dataset
plot(test$Log_Sale_Price ~ test$fitted.values, main = "Actual vs. Predicted Log_Sale_Price on Test Set")
abline(a = 0, b = 1, col = "blue")#Figure 20



