# Install necessary package.
install.packages('tidyverse') 
install.packages('skimr')  
install.packages('DataExplorer')   

# Import the necessary libraries.
library(tidyverse)
library(skimr)
library(DataExplorer)

# Read the CSV file (turtle_reviews.csv).
reviews <- read.csv("turtle_reviews.csv", header=T)

# Explore the data set
# View the first six lines of the data frame.
head(reviews)

# View the last six lines of the data frame.
tail(reviews)

# Determine the structure of the data set.
str(reviews)

# Look if there are any missing values in the data set.
sum(is.na(reviews))

#Summary statistics of the data set.
summary(reviews)

# Create a downloadable HTML file containing summary of the data set.
DataExplorer::create_report(reviews)

# Understand the shopper persona of Turtle Games.
# Remove unnecessary columns.
reviews_1 <- select(reviews, -language, -platform, -product, -review, -summary)

# View new data frame.
head(reviews_1)

# Rename columns.
colnames(reviews_1)<- c("gender","age","remuneration", "spending_score", "loyalty_points", "education")

# View new data frame.
head(reviews_1)

# Determine the unique values in each column.
unique(reviews_1$gender)
unique(reviews_1$education)

# Determine the number of players per gender.
table(reviews_1$gender) 

# Visualise players by gender.
barplot(table(reviews_1$gender),
        main='Players by gender',
        xlab='Gender',
        ylab='Count',
        col='blue')

# Visualise players by age.
barplot(table(reviews_1$age),
        main='Age of players',
        xlab='Age',
        ylab='Count',
        col='red')

# Visualise players by age.
barplot(table(reviews_1$age),
        main='Age of players',
        xlab='Age',
        ylab='Count',
        col='red')

# Comparing gender and education.
ggplot(reviews_1, aes(x=gender, fill=education)) + 
  geom_bar(position='dodge')

# Comparing age, remuneration and gender.
ggplot(reviews_1, aes(x=age, y=remuneration, col=gender)) + 
  geom_smooth(se=FALSE)

# Comparing gender, education and remuneration.
qplot(gender, education, colour=remuneration, 
      data=reviews_1, geom=c('point', 'jitter'))

# Comparing gender, education and spending score.
qplot(gender, education, colour=spending_score, 
      data=reviews_1, geom=c('point', 'jitter'))

# Comparing gender, education and loyalty points.
qplot(gender, education, colour=loyalty_points, 
      data=reviews_1, geom=c('point', 'jitter'))

# Comparing gender, age and remuneration.
qplot(gender, age, colour=remuneration, 
      data=reviews_1, geom=c('point', 'jitter'))

# Comparing gender, age and spending score.
qplot(gender, age, colour=spending_score, 
      data=reviews_1, geom=c('point', 'jitter'))

# Comparing gender, age and loyalty points.
qplot(gender, age, colour=loyalty_points, 
      data=reviews_1, geom=c('point', 'jitter'))

# Moving on to understanding sales of products.
# Read the CSV file (turtle_sales.csv).
sales <- read.csv("turtle_sales.csv", header=T)

# View the first six lines of the data frame.
head(sales)

# View the statistical summary.
summary(sales)

# Create a downloadable HTML file containing summary.
DataExplorer::create_report(sales)

# Look for missing values.
sum(is.na(sales))

# There are two missing values in the ranking row, but that is not
# data that I will use, so I will ignore it for now.

# Remove unnecessary columns to calculate platforms, genre and publishers sell the most.
sales1 <- select(sales, -Ranking, - Product, -Year)

# Check the new data frame.
head(sales1)

# Determine the unique values in each column.
unique(sales1$Genre)
unique(sales1$Publisher)
unique(sales1$Platform)

# Group by Genre.
sales_genre <- sales1 %>% group_by(Genre) %>%
  summarise(NA_Sales_sum=sum(NA_Sales),
            EU_Sales_sum=sum(EU_Sales),
            Global_Sales_sum=sum(Global_Sales),
            .groups='drop')

# Check the new data frame.
sales_genre

# Arrange from highest to lowest seller.
arrange(sales_genre, desc(Global_Sales_sum))

# Group by Publisher.
sales_publisher <- sales1 %>% group_by(Publisher) %>%
  summarise(NA_Sales_sum=sum(NA_Sales),
            EU_Sales_sum=sum(EU_Sales),
            Global_Sales_sum=sum(Global_Sales),
            .groups='drop')

# Arrange from highest to lowest seller.
arrange(sales_publisher, desc(Global_Sales_sum))

# Group by Platform.
sales_platform <- sales1 %>% group_by(Platform) %>%
  summarise(NA_Sales_sum=sum(NA_Sales),
            EU_Sales_sum=sum(EU_Sales),
            Global_Sales_sum=sum(Global_Sales),
            .groups='drop')

# Arrange from highest to lowest seller.
arrange(sales_platform, desc(Global_Sales_sum))

# Looking at sales per product data in more detail.
# Drop unnecessary columns.
sales_df <- select(sales, -Ranking, -Platform, -Year, -Genre, - Publisher)

#Check the file.
head(sales_df)

# Group by Product.
sales_df1 <- sales_df %>% group_by(Product) %>%
  summarise(NA_Sales_sum=sum(NA_Sales),
            EU_Sales_sum=sum(EU_Sales),
            Global_Sales_sum=sum(Global_Sales),
            .groups='drop')

#Check the file.
head(sales_df1)

# Arrange from highest to lowest seller.
arrange(sales_df1, desc(Global_Sales_sum))

# Save the data set as a csv file.
write.csv(sales_df1, 'sales_sum.csv')

# Create scatterplots.
qplot(NA_Sales_sum, EU_Sales_sum, data=sales_df1)
qplot(NA_Sales_sum, Global_Sales_sum, data=sales_df1)
qplot(EU_Sales_sum, Global_Sales_sum, data=sales_df1)

# Create histograms.
qplot(NA_Sales_sum, bins=20, data=sales_df1)
qplot(EU_Sales_sum, bins=20, data=sales_df1)
qplot(Global_Sales_sum, bins=20, data=sales_df1)

# Create boxplots.
qplot(NA_Sales_sum, data = sales_df1, colour = I("red"), geom = "boxplot")
qplot(EU_Sales_sum, data = sales_df1, colour = I("red"), geom = "boxplot")
qplot(Global_Sales_sum, data = sales_df1, colour = I("red"), geom = "boxplot")

# Determine min, max and mean.
summary(sales_df1)

# Specify qqnorm function.
qqnorm(sales_df1$NA_Sales_sum)
qqnorm(sales_df1$EU_Sales_sum)
qqnorm(sales_df1$Global_Sales_sum)

# Specify qqline function.
qqline(sales_df1$NA_Sales_sum)
qqline(sales_df1$EU_Sales_sum)
qqline(sales_df1$Global_Sales_sum)

# Perform Shapiro-Wilk test.
shapiro.test(sales_df1$NA_Sales_sum)
shapiro.test(sales_df1$EU_Sales_sum)
shapiro.test(sales_df1$Global_Sales_sum)

# With p value above 0.05 we can conclude that the data 
# is normally distributed.

# Determine log for the values
sales_df1$log_NA_Sales_sum = log(sales_df1$NA_Sales_sum)
sales_df1$log_EU_Sales_sum = log(sales_df1$EU_Sales_sum)
sales_df1$log_Global_Sales_sum = log(sales_df1$Global_Sales_sum)

# View the data frame
head(sales_df1)

# Perform Shapiro-Wilk test on log values
shapiro.test(sales_df1$log_NA_Sales_sum)
shapiro.test(sales_df1$log_EU_Sales_sum)
shapiro.test(sales_df1$log_Global_Sales_sum)

# Install the moments package and load the library.
install.packages('moments') 
library(moments)

# Specify the skewness and kurtosis functions.
skewness(sales_df1$NA_Sales_sum)
kurtosis(sales_df1$NA_Sales_sum)

skewness(sales_df1$EU_Sales_sum)
kurtosis(sales_df1$EU_Sales_sum)

skewness(sales_df1$Global_Sales_sum)
kurtosis(sales_df1$Global_Sales_sum)

# Specify the skewness and kurtosis functions for log values
skewness(sales_df1$log_NA_Sales_sum)
kurtosis(sales_df1$log_NA_Sales_sum)

skewness(sales_df1$log_EU_Sales_sum)
kurtosis(sales_df1$log_EU_Sales_sum)

skewness(sales_df1$log_Global_Sales_sum)
kurtosis(sales_df1$log_Global_Sales_sum)

# Determine correlation (1 strong positive, - 1 strong negative)
cor(sales_df1$EU_Sales_sum, sales_df1$NA_Sales_sum)
cor(sales_df1$EU_Sales_sum, sales_df1$Global_Sales_sum)
cor(sales_df1$NA_Sales_sum, sales_df1$Global_Sales_sum)

# There is a strong correlation of 91% between Global and North American sales.

# Determine correlation based on log values
cor(sales_df1$log_EU_Sales_sum, sales_df1$log_NA_Sales_sum)
cor(sales_df1$log_EU_Sales_sum, sales_df1$log_Global_Sales_sum)
cor(sales_df1$log_NA_Sales_sum, sales_df1$log_Global_Sales_sum)

library(tidyverse)

#Plots
ggplot(data=sales_df1,
       mapping=aes(x=Global_Sales_sum, y=NA_Sales_sum))+
  geom_point(colour = "red",
             alpha = .5,
             size = 3) + geom_smooth(method = lm, se = FALSE)

ggplot(data=sales_df1,
       mapping=aes(x=Global_Sales_sum, y=EU_Sales_sum))+
  geom_point(colour = "red",
             alpha = .5,
             size = 3) + geom_smooth(method = lm, se = FALSE)

ggplot(data=sales_df1,
       mapping=aes(x=NA_Sales_sum, y=EU_Sales_sum))+
  geom_point(colour = "red",
             alpha = .5,
             size = 3) + geom_smooth(method = lm, se = FALSE)

# Create linear regression.
sales_lnreg_1 <- select(sales_df1, NA_Sales_sum, Global_Sales_sum)
str(sales_lnreg_1)
sales_lnreg_1 %>% cor()

sales_lnreg_2 <- select(sales_df1, EU_Sales_sum, Global_Sales_sum)
str(sales_lnreg_2)
sales_lnreg_2 %>% cor()

sales_lnreg_3 <- select(sales_df1, NA_Sales_sum, EU_Sales_sum)
str(sales_lnreg_3)
sales_lnreg_3 %>% cor()

# Visualise the regression.
ggplot(sales_lnreg_1, aes(NA_Sales_sum, Global_Sales_sum)) +
  geom_point()

ggplot(sales_lnreg_2, aes(EU_Sales_sum, Global_Sales_sum)) +
  geom_point()

ggplot(sales_lnreg_3, aes(NA_Sales_sum, EU_Sales_sum)) +
  geom_point()

# Multiple linear regression, remove log values from the data frame.
sales_mlr <- subset(sales_df1, select=-c(log_NA_Sales_sum, log_EU_Sales_sum, log_Global_Sales_sum))

colnames(sales_mlr)
str(sales_mlr)
sales_mlr %>% cor()

# Train multiple linear regression model
model_mr <- lm(Global_Sales_sum~.,data = sales_mlr)
model_mr 
summary(model_mr)

# Adjusted R-squared is a fantastic 0.9709 

# Example
Product <- 107
NA_Sales_sum <- c(34.02)
EU_Sales_sum <- c(23.80)
predict_data <- data.frame(Product, NA_Sales_sum, EU_Sales_sum)

# Predict
predict(model_mr, newdata = predict_data)

# Good fit with an observed value of 67.85

