# DA301:  Advanced Analytics for Organisational Impact
# Ryan Brenner

###############################################################################

## Overview:
## Turtle Games is a game manufacturer and retailer. They manufacture and sell 
## their own products, along with sourcing and selling products manufactured by 
## other companies. Their product range includes books, board games, video games
## and toys. They have a global customer base and have a business objective of 
## improving overall sales performance by utilising customer trends.

## To improve overall sales performance, Turtle Games wants to understand:
## - Part 1: How customers accumulate loyalty points (analysis conducted using 
##    Python)
## - Part 2: How groups within the customer base can be used to target specific 
##    market segments (analysis conducted using Python)
## - Part 3: How social data (e.g. customer reviews) can be used to inform  
##    marketing campaigns (analysis conducted using Python)
## - Part 4: What is the impact that each product has on sales (analysis  
##    conducted using R)
## - Part 5: The reliability of the data (e.g. normal distribution, Skewness, 
##    Kurtosis) (analysis conducted using R)
## - Part 6: If there is any possible relationship(s) between North 
##    America, Europe, and global sales (analysis conducted using R)

###############################################################################
###############################################################################
###############################################################################

# Part 4: Exploratory data analysis using R.

# Determine what initial insights can be gathered from the sales data using  
# statistics and plots.

# 1. Load and explore the data.

# Install the tidyverse library.
# install.packages('tidyverse')
# Import Tidyverse.
library('tidyverse')

# Set working directory.
setwd("/Users/ryanbrenner/Documents/LSE Data Analytics Career Accelerator/Course 3/Course 3 Final Project/Files and Code")

# Import the data set.
sales <- read.csv('turtle_sales.csv', header = TRUE)

# Print the data frame.
head(sales)
View(sales)

# View the structure of the data frame to view the data types for each column, 
# as well as the total number of rows and columns.
str(sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns (Ranking, Year, Genre, Publisher). 
sales2 <- select(sales, -Ranking, -Year, -Genre, -Publisher)

# View the new data frame.
head(sales2)
View(sales2)
### The unnecessary columns have been successfully removed.

# Check for missing (NA) values.
sum(is.na(sales2))
### There are no missing values.

# View the structure and descriptive statistics for the new data frame.
str(sales2)
summary(sales2)

################################################################################

# 2. Review plots to conduct exploratory data analysis and determine initial 
#    insights into the data set.

# The warning message regarding qplot() being deprecated can be ignored.

# 2a) Scatterplots
# Create scatterplots.
# Compare NA and EU sales.
qplot(NA_Sales, EU_Sales,
      data=sales2,
      geom=c('point'),
      xlab='North American Sales (£M)', ylab='European Sales (£M)',
      main='North American vs European Sales')
### There appear to be some outliers in the data. One product sold significantly
### more than others in both NA and EU, and three products sold much more in NA
### than they did in EU.
### North American and European sales seem to have a positive linear
### relationship, although the relationship is moderate.

# Compare NA and Global sales.
qplot(NA_Sales, Global_Sales,
      data=sales2,
      geom=c('point'),
      xlab='North American Sales (£M)', ylab='Global Sales (£M)',
      main='North American vs Global Sales')
### There appear to be a few outliers in the data. One of the products sold
### significantly more than all others in both NA and globally.
### North American and global sales seems to have a very strong positive linear
### relationship.

# Compare EU and Global sales.
qplot(EU_Sales, Global_Sales,
      data=sales2,
      geom=c('point'),
      xlab='European Sales (£M)', ylab='Global Sales (£M)',
      main='European vs Global Sales')
### There appears to be one major outlier in the data. One of the products sold
### significantly more than others in both EU and globally. Also, there appear
### to be a few products that sold better in other parts of the world than they
### did in EU.
### European and global sales seem to have a moderately strong positive linear
### relationship.


# 2b) Histograms
# Create histograms.
# Visualise the distribution of NA sales.
qplot(NA_Sales, data=sales2, bins=20,
      xlab='North American Sales (£M)',
      ylab='Frequency',
      main='Distribution of North American Sales')
### There appear to be some outliers in the data.
### North American sales are skewed to the right.

# Visualise the distribution of EU sales.
qplot(EU_Sales, data=sales2, bins=20,
      xlab='European Sales (£M)',
      ylab='Frequency',
      main='Distribution of European Sales')
### There appears to be a major outlier in the data.
### European sales are skewed to the right.

# Visualise the distribution of global sales.
qplot(Global_Sales, data=sales2, bins=20,
      xlab='Global Sales (£M)',
      ylab='Frequency',
      main='Distribution of Global Sales')
### There appears to be a major outlier in the data.
### Global sales are skewed to the right.


# 2c) Boxplots
# Create boxplots.
# Visualise the distribution of NA sales.
qplot(NA_Sales,
      data=sales2,
      geom='boxplot',
      xlab='North American Sales (£M)',
      main='Distribution of North American Sales')
### There appear to be a number of outliers for NA sales.
### Most products have less than 15 million in sales in NA.

# Visualise the distribution of EU sales.
qplot(EU_Sales,
      data=sales2,
      geom='boxplot',
      xlab='European Sales (£M)',
      main='Distribution of European Sales')
### There appear to be a number of outliers for EU sales.
### Most products have less than 10 million in sales in EU.

# Visualise the distribution of global sales.
qplot(Global_Sales,
      data=sales2,
      geom='boxplot',
      xlab='Global Sales (£M)',
      main='Distribution of Global Sales')
### There appear to be a number of outliers for global sales.
### Most products have less than 30 million in sales globally.

###############################################################################

# 3. Identify which product represents the most extreme outlier in the data
#    set/visualisations.
filter(sales2, Global_Sales > 60)
### Product 107 has significantly more sales in NA, EU and globally than all
### other products.

###############################################################################
###############################################################################
###############################################################################


# Part 5: Cleaning and maniulating data using R.

# Explore, prepare and explain the normality of the data set based on plots,
# Skewness, Kurtosis, and a Shapiro-Wilk test.


# 1. Load and explore the data.

# View the sales2 data frame.
head(sales2)

# Check output: Determine the min, max, and mean values.
# Look at minimum values of all three sales columns.
min(sales2$NA_Sales) 
min(sales2$EU_Sales) 
min(sales2$Global_Sales)
### Some products didn't sell in North America and Europe.

# Look at maximum values of all three sales columns.
max(sales2$NA_Sales) 
max(sales2$EU_Sales) 
max(sales2$Global_Sales) 

# Look at mean values of all three sales columns.
mean(sales2$NA_Sales) 
mean(sales2$EU_Sales) 
mean(sales2$Global_Sales) 
### Average sales in North America are higher than in Europe.

# View the descriptive statistics.
summary(sales2)

###############################################################################

# 2. Determine the impact on sales per product.

# 2a) Use the group_by function.
# Group data based on Product and determine the sum of sales per Product.
product_sales <- sales2 %>% group_by(Product) %>%
  summarise(sum_NA_sales=sum(NA_Sales),
            sum_EU_sales=sum(EU_Sales),
            sum_global_sales=sum(Global_Sales),
            .groups='drop')

# View the new data frame and number of unique values.
View(product_sales)
dim(product_sales)
length(unique(product_sales$Product))
### There are 175 unique products in the data set.

# Explore the descriptive statistics of the new data frame.
summary(product_sales)


# 2b) Create plots to review the grouped data and determine insights.
# Import plotly to create interactive plots.
library(plotly)

# Create scatterplots.
# Compare North American vs European product sales. 
scatterplot1 <- ggplot(product_sales, aes(x = sum_NA_sales,
                                          y = sum_EU_sales)) +
  geom_point() +
  # Add a trend line.
  geom_smooth(method = 'lm', se = FALSE) +
  scale_x_continuous(breaks = seq(0, 35, 5)) +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  # Add labels.
  labs(title = "North American vs European Product Sales",
       x = "Total Sales per Product in North America (£M)",
       y = "Total Sales per Product in Europe (£M)") 
# Make the plot interactive.
ggplotly(scatterplot1)
### There appear to be more total sales per product in NA than in EU.
### There are a number of outliers in the scatterplot, with one major outlier
### accounting for a product with significantly more sales than others in both 
### NA and EU.
### North American and European product sales seem to have a positive linear
### relationship, although the relationship is moderate.

# Compare North American vs Global product sales. 
scatterplot2 <- ggplot(product_sales, aes(x = sum_NA_sales,
                                          y = sum_global_sales)) +
  geom_point() +
  # Add a trend line.
  geom_smooth(method = 'lm', se = FALSE) +
  scale_x_continuous(breaks = seq(0, 35, 5)) +
  scale_y_continuous(breaks = seq(0, 70, 10)) +
  # Add labels.
  labs(title = "North American vs Global Product Sales",
       x = "Total Sales per Product in North America (£M)",
       y = "Total Sales per Product Globally (£M)") 
# Make the plot interactive.
ggplotly(scatterplot2)
### There are a number of outliers in the scatterplot, with one major outlier
### accounting for a product with significantly more sales than others in both 
### NA and globally.
### North American and global product sales seems to have a very strong positive
### linear relationship.

# Compare European vs Global product sales. 
scatterplot3 <- ggplot(product_sales, aes(x = sum_EU_sales,
                                          y = sum_global_sales)) +
  geom_point() +
  # Add a trend line.
  geom_smooth(method = 'lm', se = FALSE) +
  scale_x_continuous(breaks = seq(0, 25, 5)) +
  scale_y_continuous(breaks = seq(0, 70, 10)) +
  # Add labels.
  labs(title = "European vs Global Product Sales",
       x = "Total Sales per Product in Europe (£M)",
       y = "Total Sales per Product Globally (£M)") 
# Make the plot interactive.
ggplotly(scatterplot3)
### There are a number of outliers in the scatterplot, with one major outlier
### accounting for a product with significantly more sales than others in both 
### EU and globally.
### European and global product sales seem to have a moderately strong positive
### linear relationship.


# Create histograms.
# View the distribution of product sales in North America.
hist_NA <- ggplot(product_sales, aes(x=sum_NA_sales)) + 
  geom_histogram(bins=20) +
  # Add labels.
  labs(title = "Distribution of Product Sales in North America",
       x = "Total Sales per Product in North America (£M)",
       y = "Frequency") 
# Make the plot interactive.
ggplotly(hist_NA)
### The distribution of total sales per product in NA is skewed to the right 
### with some outliers.
 
# View the distribution of product sales in Europe.
hist_EU <- ggplot(product_sales, aes(x=sum_EU_sales)) + 
  geom_histogram(bins=20) +
  # Add labels.
  labs(title = "Distribution of Product Sales in Europe",
       x = "Total Sales per Product in Europe (£M)",
       y = "Frequency") 
# Make the plot interactive.
ggplotly(hist_EU)
### The distribution of total sales per product in EU is skewed to the right 
### with some outliers.

# View the distribution of product sales globally.
hist_global <- ggplot(product_sales, aes(x=sum_global_sales)) + 
  geom_histogram(bins=20) +
  # Add labels.
  labs(title = "Distribution of Product Sales Globally",
       x = "Total Sales per Product Globally (£M)",
       y = "Frequency") 
# Make the plot interactive.
ggplotly(hist_global)
### The distribution of total sales per product globally is skewed to the right 
### with some outliers.


# Create boxplots.
# View the distribution of product sales in North America.
ggplot(product_sales, aes(x = sum_NA_sales)) +
  geom_boxplot() +
  coord_flip() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  # Add label.
  labs(title = "Distribution of Product Sales in North America",
       x = "Total Sales per Product in North America (£M)")
### The distribution of total sales per product in NA is skewed to the right 
### with some outliers.
### Most products have less than 15 million in sales in NA.

# View the distribution of product sales in Europe.
ggplot(product_sales, aes(x = sum_EU_sales)) +
  geom_boxplot() +
  coord_flip() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  # Add label.
  labs(title = "Distribution of Product Sales in Europe",
       x = "Total Sales per Product in Europe (£M)")
### The distribution of total sales per product in EU is skewed to the right 
### with some outliers.
### Most products have less than 10 million in sales in EU.

# View the distribution of product sales globally.
ggplot(product_sales, aes(x = sum_global_sales)) +
  geom_boxplot() +
  coord_flip() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  # Add label.
  labs(title = "Distribution of Product Sales Globally",
       x = "Total Sales per Product Globally (£M)")
### The distribution of total sales per product globally is skewed to the right 
### with some outliers.
### Most products have less than 30 million in sales globally.

###############################################################################


# 3. Determine the normality of the data set.

# 3a) Create Q-Q Plots
# Measure normality in NA product sales.
qqnorm(product_sales$sum_NA_sales)
qqline(product_sales$sum_NA_sales, col='red')
### The output doesn't demonstrate normality.

# Measure normality in EU product sales.
qqnorm(product_sales$sum_EU_sales)
qqline(product_sales$sum_EU_sales, col='blue')
### The output doesn't demonstrate normality.

# Measure normality in global product sales.
qqnorm(product_sales$sum_global_sales)
qqline(product_sales$sum_global_sales, col='purple')
### The output doesn't demonstrate normality.


# 3b) Perform Shapiro-Wilk test
# Import Moments.
library(moments)

# Perform Shapiro-Wilk test on North American product sales.
shapiro.test((product_sales$sum_NA_sales))
### The p-value is less than 0.05, so the data is not normally distributed.

# Perform Shapiro-Wilk test on European product sales.
shapiro.test((product_sales$sum_EU_sales))
### The p-value is less than 0.05, so the data is not normally distributed.

# Perform Shapiro-Wilk test on global product sales.
shapiro.test((product_sales$sum_global_sales))
### The p-value is less than 0.05, so the data is not normally distributed.


# 3c) Determine Skewness and Kurtosis
# Evaluate the skewness of product sales in NA.
skewness(product_sales$sum_NA_sales)
### The output suggests a positive (right) skewness.

# Evaluate the skewness of product sales in EU.
skewness(product_sales$sum_EU_sales)
### The output suggests a positive (right) skewness.

# Evaluate the skewness of product sales globally.
skewness(product_sales$sum_global_sales)
### The output suggests a positive (right) skewness.

# Evaluate the kurtosis of product sales in NA.
kurtosis(product_sales$sum_NA_sales)
### The kurtosis value is much higher than 3, suggesting the data is
### leptokurtic.

# Evaluate the kurtosis of product sales in EU.
kurtosis(product_sales$sum_EU_sales)
### The kurtosis value is much higher than 3, suggesting the data is 
### leptokurtic.

# Evaluate the kurtosis of product sales globally.
kurtosis(product_sales$sum_global_sales)
### The kurtosis value is much higher than 3, suggesting the data is 
### leptokurtic.

### The Shapiro-Wilk tests, as well as the skewness and kurtosis for all the
### sales data indicates that none of the sales data in the data set is normally
### distributed.

# 3d) Determine correlation

# Check correlation between NA product sales and EU product sales.
cor(product_sales$sum_NA_sales, product_sales$sum_EU_sales)
### There is a moderate positive correlation between NA and EU product sales.

# Check correlation between NA product sales and global product sales.
cor(product_sales$sum_NA_sales, product_sales$sum_global_sales)
### There is a very strong positive correlation between NA and global product
### sales.

# Check correlation between EU product sales and global product sales.
cor(product_sales$sum_EU_sales, product_sales$sum_global_sales)
### There is a strong positive correlation between EU and global product sales.


###############################################################################
###############################################################################
###############################################################################


# Part 6: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales.
## Investigate any possible relationship(s) in the sales data by creating  
## simple and multiple linear regression models. 


# 1. Load and explore the data.
# View the product_sales data frame.
head(product_sales)

# Determine a summary of the data frame.
summary(product_sales)

###############################################################################

# 2. Create simple linear regression models.
# Determine the correlation between North American and global sales.
# Create a linear regression model using sum_NA_sales as the x variable to 
# predict global sales.
model_NA <- lm(sum_global_sales~sum_NA_sales,
             data=product_sales)
# View the model.
model_NA

# View the full regression table for the model.
summary(model_NA)
### In the regression table, the very small p-value for the x variable shows
### it is a significant variable. The Multiple R-squared value of 0.8395 tells
### us that sales in North America alone explain 83.95% of the variability in 
### global sales.

# Plot the residuals to identify any patterns.
plot(model_NA$residuals)
### There don't appear to be any patters in the residuals. A linear model seems
### appropriate for modeling the data.

# Plot the linear regression model.
plot(product_sales$sum_NA_sales, product_sales$sum_global_sales)
# Add the line-of-best-fit.
abline(coefficients(model_NA))
### The model appears to be a good fit because the relationship between North
### American sales and global sales is a linear relationship.



# Determine the correlation between European and global sales.
# Create a linear regression model using sum_EU_sales as the x variable to 
# predict global sales.
model_EU <- lm(sum_global_sales~sum_EU_sales,
               data=product_sales)
# View the model.
model_EU

# View the full regression table for the model.
summary(model_EU)
### In the regression table, the very small p-value for the x variable shows
### it is a significant variable. The Multiple R-squared value of 0.7201 tells
### us that sales in Europe alone explain 72.01% of the variability in global 
### sales.

# Plot the residuals to identify any patterns.
plot(model_EU$residuals)
### Although a small minority of residuals are larger than the rest, there 
### don't appear to be any strong patters in the residuals. A linear model 
### seems appropriate for modeling the data.

# Plot the linear regression model.
plot(product_sales$sum_EU_sales, product_sales$sum_global_sales)
# Add the line-of-best-fit.
abline(coefficients(model_EU))
### The model appears to be a good fit because the relationship between
### European sales and global sales is a linear relationship.


###############################################################################

# 3. Create a multiple linear regression model
# Create a multiple linear regression model using sum_NA_sales and sum_EU_sales
# as the x variables to predict global sales.
model_multi <- lm(sum_global_sales~sum_NA_sales+sum_EU_sales,
               data=product_sales)

# Plot the residuals to identify any patterns.
plot(model_multi$residuals)
### There don't appear to be any patterns in the residuals. A multiple linear
### model seems appropriate for modeling the data.

# View the full regression table for the multiple linear regression model.
summary(model_multi)
### In the regression table, the very small p-values for the x variables show
### that both are significant variables. 
### Both the multiple and the adjusted R-squared values in this model are
### significantly higher than those in the two simple linear regression models.
### The multiple and adjusted R-squared values in the multiple linear 
### regression model show that sales in North America and Europe together 
### explain 96.6% of the variability in global sales. This means the multiple 
### linear regression model is stronger and should be the model that is used to 
### predict global sales. 

# Test for multicollinearity in the multiple linear regression model.
# Install the car library in order to use the vif function.
# install.packages('car')
# Import car.
library(car)

# Calculate the variance inflation factor (VIF) to identify correlation
# between the independent variables and the strength of the correlation.
vif(model_multi)
### The x variables have a VIF of approximately 1.627, suggesting there is
### little correlation between the x variables and therefore no evidence
### of multicollinearity.

###############################################################################

# 4. Use the multiple linear regression model to make predictions.
# Compare predicted values with observed values for a number of records.

# Create a data frame of values that will be used to make predictions. 
sum_NA_sales <- c(34.02, 3.93, 2.73, 2.26, 22.08)
sum_EU_sales <- c(23.80, 1.56, 0.65, 0.97, 0.52)
# Create the data frame.
prediction_values <- data.frame(sum_NA_sales, sum_EU_sales)
# View the data frame.
prediction_values

# Create a new object and specify the predict function. Use the newly created
# data frame.
predictTest = predict(model_multi, newdata=prediction_values)

# Print the object to view the predicted values.
predictTest 

# Subset the original data set (sales2) to compare the predicted values with the
# observed values and test for accuracy.
subset(sales2, NA_Sales==34.02 & EU_Sales==23.80) 
subset(sales2, NA_Sales==3.93 & EU_Sales==1.56)
subset(sales2, NA_Sales==2.73 & EU_Sales==0.65) 
subset(sales2, NA_Sales==2.26 & EU_Sales==0.97) 
subset(sales2, NA_Sales==22.08 & EU_Sales==0.52) 
### The predicted values are relatively close to the observed values. For
### example, the multiple linear regression model predicts that with sales of
### 34.02 in North America and sales of 23.80 in Europe, global sales would be 
### 68.056. The observed value for global sales based on these sales numbers 
### in North America and Europe is actually 67.85, which is close to the 
### predicted value.

### Overall, it can be concluded that the multiple linear regression model
### provides accurate predictions of global sales. 

###############################################################################
###############################################################################
###############################################################################