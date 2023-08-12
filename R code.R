library(lubridate)
library(ggplot2)
library(dplyr)

library(tidyr)
library(caret)

# Read the data
data<-read.csv('superstore_data.csv')
head(data,5)

data$Dt_Customer <- mdy(data$Dt_Customer)
sapply(data,class)
str(data)
summary(data)

#Extracting the age column from year_birth and Dt_Customer
# Convert the "Dt_Customer" column to datetime format
data$Dt_Customer <- as.Date(data$Dt_Customer)

# Calculate age in years by subtracting the year from "Dt_Customer" with "Year_Birth"
data$Age <- as.integer(format(data$Dt_Customer, "%Y")) - data$Year_Birth

# Print the new "Age" column
print(data$Age)

# check for missing values
sum(is.na(data))

#Omitting NA values
df <- na.omit(data)


# Calculate IQR
Q1 <- quantile(df$Income, 0.25)
Q3 <- quantile(df$Income, 0.75)
IQR <- Q3 - Q1

# Define upper and lower limits
upper_limit <- Q3 + 1.5 * IQR
lower_limit <- Q1 - 1.5 * IQR

# Remove outliers
df_cleaned <- subset(df, Income >= lower_limit & Income <= upper_limit)


# Histogram of Age
ggplot(data = df_cleaned, aes(x = Age)) + 
  geom_histogram(bins = 25, color = "black", fill = "lightblue") +
  labs(title = "Histogram of Age", x = "Age", y = "Count")

ggplot(data = data, aes(y = Income)) + 
  geom_boxplot(color = "black", fill = "lightblue") +
  labs(title = "Boxplot of Income", y = "Income ($)")

# Bar chart of Education
ggplot(data = data, aes(x = Education)) + 
  geom_bar(color = "black", fill = "lightgreen") +
  labs(title = "Bar Chart of Education of customers", x = "Education", y = "Count")

# Create the line chart
ggplot(data = data, aes(x = Dt_Customer, y = Income)) +
  geom_line() +
  labs(title = "Income Over Time", x = "Date", y = "Income")

# Calculate mean income by response
income_by_response <- aggregate(Income ~ Response, data = df_cleaned, mean)

# Plot mean income by response
library(ggplot2)
ggplot(income_by_response, aes(x = factor(Response), y = Income)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Mean Income by Response", x = "Response", y = "Mean Income")

df_cleaned$Marital_Status

# Calculate mean income by response
#Age_by_response <- aggregate(Marital_Status ~ Response, data = df_cleaned, mean)
# Plot mean income by response
library(ggplot2)
#ggplot(Age_by_response, aes(x = factor(Response), y = Age)) +
#  geom_bar(stat = "identity", fill = "lightgreen") +
#  labs(title = "Mean Age by Response", x = "Response", y = "Mean Age")


# Create the line chart
ggplot(data = df_cleaned, aes(x = Dt_Customer, y = Income)) +
  geom_line() +
  labs(title = "Income Over Time", x = "Date", y = "Income")

# Percentage of customers in target group and controlled group
response_pct <- data %>% 
  group_by(Response) %>% 
  summarise(count = n()) %>% 
  mutate(percentage = count / sum(count) * 100)

# Average income for target group and controlled group
response_income <- data %>% 
  group_by(Response) %>% 
  summarise(avg_income = mean(Income))

# Average number of purchases for target group and controlled group
response_purchases <- data %>% 
  group_by(Response) %>% 
  summarise(avg_purchases = mean(NumDealsPurchases + NumWebPurchases + 
                                   NumCatalogPurchases + NumStorePurchases))
cat("Percentage of customers in target group and controlled group:\n")
print(response_pct)
cat("\nAverage income for target group and controlled group:\n")
print(response_income)
cat("\nAverage number of purchases for target group and controlled group:\n")
print(response_purchases)


# calculate mean of each purchase variable separately for target and control group
target_means <- data %>% group_by(Response) %>% summarise(mean_wines = mean(MntWines),
                                                          mean_fruits = mean(MntFruits),
                                                          mean_meat = mean(MntMeatProducts),
                                                          mean_fish = mean(MntFishProducts),
                                                          mean_sweet = mean(MntSweetProducts),
                                                          mean_gold = mean(MntGoldProds)) %>% filter(Response == 1)

control_means <- data %>% group_by(Response) %>% summarise(mean_wines = mean(MntWines),
                                                           mean_fruits = mean(MntFruits),
                                                           mean_meat = mean(MntMeatProducts),
                                                           mean_fish = mean(MntFishProducts),
                                                           mean_sweet = mean(MntSweetProducts),
                                                           mean_gold = mean(MntGoldProds)) %>% filter(Response == 0)

# combine the means into a single dataframe
means_df <- rbind(target_means, control_means)
means_df <- pivot_longer(means_df, cols = c(mean_wines, mean_fruits, mean_meat, mean_fish, mean_sweet, mean_gold), names_to = "Purchase", values_to = "Mean")

# plot bar chart of mean purchases for target and control group
ggplot(means_df, aes(x = Purchase, y = Mean, fill = factor(Response))) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Mean purchases for target and control group", x = "Purchase", y = "Mean") +
  scale_fill_manual(values = c("red", "blue"), labels = c("Target", "Control"))


data_purchases <- df_cleaned %>%
  select(Response, NumDealsPurchases, NumWebPurchases, NumCatalogPurchases, NumStorePurchases) %>%
  pivot_longer(cols = -Response, names_to = "Purchase_Type", values_to = "Num_Purchases") %>%
  group_by(Response, Purchase_Type) %>%
  summarize(total_purchases = mean(Num_Purchases)) %>%
  ungroup()

ggplot(data = data_purchases, aes(x = Response, y = total_purchases, fill = Purchase_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Purchases by Response Group", x = "Response Group", y = "Total Purchases", fill = "Purchase Type")

ggplot(df_cleaned, aes(x=Marital_Status, fill=as.factor(Response))) +
  geom_bar(position="dodge", alpha=0.8) +
  labs(title="Distribution of Response based on Marital Status",
       x="Marital Status", y="Count") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data = df_cleaned, aes(x = Marital_Status, y = Income, fill = Response)) +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 4, color = "black", position = position_dodge(width = 0.9)) +
  labs(title = "Response by Marital Status", x = "Marital Status", y = "Income", fill = "Response") +
  scale_fill_manual(values = c("#F8766D", "#00BA38"), labels = c("Control", "Target"))

# group the data by marital status and response, and calculate the mean income
marital_response_income <- df_cleaned %>%
  group_by(Education, Response) %>%
  summarize(mean_income = mean(Income))

# create a bar chart of mean income by marital status and response
ggplot(marital_response_income, aes(x = Education, y = mean_income, fill = factor(Response))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Income by Education and Response", x = "Education", y = "Mean Income", fill = "Response")

#dummy_df<-predict(dummyVars(~ Education,data=df_cleaned),newdata=df_cleaned) 

#dummy_Martial_status<-predict(dummyVars(~ Marital_Status,data=df_cleaned),newdata=df_cleaned) 

#df_1<-cbind(df_cleaned,dummy_df)
#df_2<-cbind(df_1,dummy_Martial_status)

df_final<-df_cleaned[,-c(1,2,3,4,8,22)]

table(df_final$Response)

df_balanced <- df_final %>%
  group_by(Response)%>%
  slice_sample(n=min(table(df_final$Response)),replace=FALSE)

table(df_balanced$Response)

# Separate the data by response class
response_sample <- subset(df_balanced, Response == 0)$Income
response_population <- subset(df, Response == 0)$Income

# Set the null hypothesis and alternative hypothesis for each response class
null_hypothesis_0 <- mean(response_population)  # the population mean for response class 0
alternative_hypothesis <- "two.sided" # Can also be "less" or "greater" for one-tailed tests

mean(response_population)
mean(response_sample)
# Conduct the two-sample t-test for each response class
t.test(response_sample, mu = null_hypothesis_0, alternative = alternative_hypothesis, conf.level = 0.95)


set.seed(123) # Set a seed for reproducibility
train_index <- sample(1:nrow(df_balanced), size = round(0.7 * nrow(df_balanced)))
train_data <- df_balanced[train_index, ]
valid_data <- df_balanced[-train_index, ]

#Logistic Regression
logit.reg <- glm(Response ~ ., data = train_data, family = "binomial")
options(scipen=999)
summary(logit.reg)


logit.reg.pred_train <- predict(logit.reg, train_data, type = "response")
confusionMatrix(as.factor(ifelse(logit.reg.pred_train > 0.5, 1, 0)), as.factor(train_data$Response))


logit.reg.pred <- predict(logit.reg, valid_data, type = "response")
# first 5 actual and predicted records
data.frame(actual = valid_data$Response[1:5],
           predicted = logit.reg.pred[1:5])

confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.5, 1, 0)), as.factor(valid_data$Response))


KNN<-class::knn(train=train_data,
                test=valid_data,
                cl=train_data$Response,k=1)

#accuracy<-data.frame(k=seq(1,15,1 ,overallaccuracy=rep(0,15)))
#for (i in 1:15){
#  KNN<-class::knn(train=train_data,
#                  test=valid_data,
#                  cl=train_data$Response,k=1)
#  accuracy[i,2]<-confusionMatrix(KNN,
#                                 as.factor(valid_data$Response))$overall[1]
#}

Knn.Class.pred <- predict(KNN, valid_data, type = "response")
# first 5 actual and predicted records
data.frame(actual = valid_data$Response[1:5],
           predicted = Knn.Class.pred[1:5])

confusionMatrix(as.factor(ifelse(Knn.Class.pred > 0.5, 1, 0)), as.factor(valid_data$Response))
