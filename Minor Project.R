# Load necessary libraries
library(plyr)
library(corrplot)
library(ggplot2)
library(ggthemes)
library(caret)
library(MASS)
library(party)
library(dplyr)

#Load the Dataset
churn <- read.csv("E:/other downloads/OTT Customer Churn Dataset.csv")

#Dataset Description
str(churn)

#Identifying the missing values
sapply(churn, function(x) sum(is.na(x)))

#remove 28 - max days inactive and 35 - churn missing records
churn <- churn[complete.cases(churn), ]

#Remove the columns that we don't need
churn$year <- NULL
churn$customer_id <- NULL
churn$phone_no <- NULL

#Turning the values into 0s and 1s
churn$multi_screen <- as.factor(mapvalues(churn$multi_screen,
                                          from=c("no", "yes"),
                                          to=c("0","1")))
churn$mail_subscribed <- as.factor(mapvalues(churn$mail_subscribed,
                                             from=c("no", "yes"),
                                             to=c("0","1")))
churn$gender <- as.factor(mapvalues(churn$gender,
                                    from=c("Male", "Female"),
                                    to=c("0","1")))

# Convert the 'churn' variable from a factor to a numeric, replacing non-numeric values with NA
churn$multi_screen <- as.numeric(churn$multi_screen, na.rm = TRUE)
churn$mail_subscribed <- as.numeric(churn$mail_subscribed, na.rm = TRUE)
churn$gender <- as.numeric(churn$gender, na.rm = TRUE)

# Replace extra value in gender, '1' with NA (as it is blank)
churn <- churn %>%
  mutate(gender = ifelse(gender == "1", NA, gender)) # Replace '1' with NA

#Identify the missing values again
sapply(churn, function(x) sum(is.na(x)))

#remove 21 missing records
churn <- churn[complete.cases(churn), ]

churn$gender <- as.factor(mapvalues(churn$gender,
                                    from=c("2", "3"),
                                    to=c("0","1")))

# Convert the 'churn' variable from a factor to a numeric, replacing non-numeric values with NA again
churn$gender <- as.numeric(churn$gender, na.rm = TRUE)

#Exploratory data analysis and feature selection

#Distribution of Churn and No Churn
ggplot(data = churn, aes(x = factor(churn), fill = factor(churn))) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = paste("n = ", formatC(..count..))), vjust = -0.5) +
  scale_fill_manual(values = c("0" = "orange", "1" = "orange")) +  # Set fill color for '0' and '1' labels
  theme_minimal() +
  ggtitle("Number of customer churn") +
  theme(plot.title = element_text(hjust = 0.5))


#Correlating the numeric values converted
numeric.var <- sapply(churn, is.numeric)
corr.matrix <- cor(churn[,numeric.var])

#Making a correlation matrix
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method = 'shade', order = 'AOE', diag = FALSE)

#They are correlated so we will remove them (Dimensionality Reduction)
churn$minimum_daily_mins <- NULL
churn$maximum_daily_mins <- NULL



#Logistic Regression
#First, we split the data into training and testing sets:
intrain<- createDataPartition(churn$churn,p=0.7,list=FALSE)
set.seed(2017)
training<- churn[intrain,]
testing<- churn[-intrain,]

#Fitting the Logistic Regression Model:
LogModel <- glm(churn ~ .,family=binomial(link="logit"),data=training)
print(summary(LogModel))

#Feature Analysis
anova(LogModel, test="Chisq")

#Assessing the predictive ability of the Logistic Regression model
#testing$Churn <- as.character(testing$Churn)
#testing$Churn[testing$Churn=="No"] <- "0"
#testing$Churn[testing$Churn=="Yes"] <- "1"
fitted.results <- predict(LogModel,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))

# Confusion Matrix
conf_matrix <- table(testing$churn, fitted.results > 0.5)
print("Confusion Matrix for Logistic Regression:")
print(conf_matrix)

# Performance Metrics
# Accuracy
accuracy <- (conf_matrix[1,1] + conf_matrix[2,2]) / sum(conf_matrix)
cat("Accuracy:", accuracy, "\n")

# Precision
precision <- conf_matrix[2,2] / sum(conf_matrix[,2])
cat("Precision:", precision, "\n")

# Recall (Sensitivity)
recall <- conf_matrix[2,2] / sum(conf_matrix[2,])
cat("Recall (Sensitivity):", recall, "\n")

# F1-score
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("F1-score:", f1_score, "\n")
# The model correctly predicted that 264 customers didn't churn (0) and 9 customers did churn (1).
# However, the model made the following mistakes:
# It incorrectly predicted that 27 customers would churn (1) when they actually didn't (0).
#Additionally, the model erroneously predicted that 12 customers wouldn't churn (0) when they actually did churn (1).

# Save the model object
saveRDS(LogModel, file = "C:/Users/Admin/OneDrive/Documents/LogisticRegressionModel.rds")







