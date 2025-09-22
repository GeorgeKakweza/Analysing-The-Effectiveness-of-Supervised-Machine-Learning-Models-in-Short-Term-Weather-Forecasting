#Importing xlsx data into R

setwd("C:/Users/Admin/Desktop/Project SORS4010/Project Data Set")
getwd()
library(readxl)
bulawayo_2011_01_01_to_2024_04_14 <- read_excel("bulawayo 2011-01-01 to 2024-04-14.xlsx")
View(bulawayo_2011_01_01_to_2024_04_14)

###Data Preprocessing

##Feature/Varibale Selection 
Weather_Data <- subset(bulawayo_2011_01_01_to_2024_04_14,
                       select = -c(datetime, name, precipprob, preciptype, snow, snowdepth,
                                   windgust, severerisk, sunrise, sunset, conditions,
                                   description, icon, stations))

##Dealing with missing data
#Identify columns with missing data
missing_data <- colSums(is.na(Weather_Data)) > 0
#Display the columns with missing data
print(names(Weather_Data)[missing_data])

#Choosing the appropriate measure to replace missing values
range(Weather_Data$sealevelpressure)
mean(Weather_Data$sealevelpressure)
sd(Weather_Data$sealevelpressure)
mode(Weather_Data$sealevelpressure)
median(Weather_Data$sealevelpressure)

range(Weather_Data$visibility)
mean(Weather_Data$visibility)
sd(Weather_Data$visibility)
mode(Weather_Data$visibility)
median(Weather_Data$visibility)

# Replace missing values with the mean of the column
Weather_Data$sealevelpressure <- ifelse(is.na(Weather_Data$sealevelpressure), 
                                        mean(Weather_Data$sealevelpressure, na.rm = TRUE), 
                                        Weather_Data$sealevelpressure)

Weather_Data$visibility <- ifelse(is.na(Weather_Data$visibility), 
                                        mean(Weather_Data$visibility, na.rm = TRUE), 
                                        Weather_Data$visibility)

#Correlation Matrix
Correlation_Matrix <- cor(Weather_Data)
print(Correlation_Matrix)

install.packages("ggplot2")
library(ggplot2)
install.packages("reshape2")
library(reshape2)
#Convert correlation matrix to long format
Correlation_Df <- melt(Correlation_Matrix)
Correlation_Df
#Create a heatmap
heatmap <- ggplot(Correlation_Df, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black") +  # Add text labels with rounded correlation values
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       breaks = c(seq(-1, 0, by = 0.2), seq(0.2, 1, by = 0.2)),
                       labels = c(seq(-1, 0, by = 0.2), seq(0.2, 1, by = 0.2))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap",
       x = "Variables",
       y = "Variables")

print(heatmap)

data <- Weather_Data

#Correlation between each predictor variable and the independent variable (precip/rainfall)
#Loop through predictor variables and calculate correlation with dependent variable
data <- Weather_Data
correlations <- lapply(names(data)[-1], function(var) {
  cor.test(data[[var]], data$precip)
})

# Extract correlation coefficients and p-values
results <- lapply(correlations, function(correlation) {
  c(correlation$estimate, correlation$p.value)
})

#Combine results into a data frame
results_df <- data.frame(variable = names(data)[-1], do.call(rbind, results))
# Reorder the levels of the Weather Parameter variable based on the value(correlation)
results_df$variable <- factor(results_df$variable, levels = results_df$variable[order(results_df$cor, decreasing = TRUE)])

#Create the bar chart
ggplot(results_df, aes(x = variable, y = cor)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Vertical Bar Chart",
       x = "Variable",  # Replace "Variable" with the actual variable name
       y = "Correlation Coefficient") +  # Replace "Correlation Coefficient" with the appropriate label
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Now we romove the varibales with 0 correlation with our dependent variable
#However we shall keep the varibales that are correlated to the key predictors
Weather_Data <- subset(Weather_Data,select = -c(winddir,moonphase))

#Changing the data type
AveragePrecip <- sum(Weather_Data$precip[Weather_Data$precip > 0])/
                    length(Weather_Data$precip[Weather_Data$precip > 0])
AveragePrecip

#Define the threshold for classifying rain (in mm)
threshold <- 2.5

#Classify precipitation as rain or no rain. 1 = rain, 0 = no rain
rain_classification <- ifelse(Weather_Data$precip > threshold, 1, 0)

#Print the classified precipitation
print(rain_classification)

#add new column rain_today
Weather_Data$rain <- rain_classification

#I also added a rain_tomorrow column to prepare for model building
#Create a new column rain_tomorrow with the same values as column rain but shifted upward
Weather_Data$rain_tomorrow <- c(Weather_Data$rain[-1], NA)

#Drop the last row using negative indexing
Weather_Data <- Weather_Data[-nrow(Weather_Data), ]

#Changing the data type
Weather_Data$rain <- as.factor(Weather_Data$rain)
Weather_Data$rain_tomorrow <- as.factor(Weather_Data$rain_tomorrow)


##Data Split: Training and Testing Data sets
#install and load the caret package
install.packages("caret")
library(caret)
#Set a random number generator see so that the partition is always made at the same
#point, choosing 1503.
set.seed(1503)
#Now we create the partition vector using the CreateDataPartition function. In this
#specific case we will use 80% of the data to train the model and 20% to test it.
#the list = FALSE avoids returning the data as a list
partition <- createDataPartition(y = Weather_Data$rain_tomorrow, p = 0.8, list = FALSE)
#Now let's split the Weather_Data into training and testing datasets using the
#partition vector
training <- Weather_Data[partition,]
testing <- Weather_Data[-partition,]


###Decision trees with ctree
#install packages that contain ctree
install.packages("partykit")
library(partykit)
#Now we make a tree with rain_tomorrow as the dependent variable and all other variables 
#as predictors 
tree.ctree <- ctree(training$rain_tomorrow ~ ., data = training)
plot(tree.ctree, gp = gpar(fontsize = 7))


library(partykit)
library(caret)

# Define the training control
train_control <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

# Define the parameter grid
param_grid <- expand.grid(
  mincriterion = c(0.8, 0.9, 0.95),  # Minimum criterion for splitting
  minsplit = c(10, 20, 30),            # Minimum number of observations for a split
  minbucket = c(5, 10, 15)             # Minimum number of observations in a terminal node
)

# Train the ctree model with hyperparameter tuning
ctree_model <- train(
  rain_tomorrow ~ .,              # Formula
  data = training,                # Training data
  method = "ctree",               # Model: conditional inference tree
  trControl = train_control,      # Training control
  tuneGrid = param_grid           # Parameter grid
)

# Print the best hyperparameters
print(ctree_model$bestTune)

# Plot the tuned ctree model
plot(ctree_model$finalModel, gp = gpar(fontsize = 7))



##Prediction with ctree model
#Once we have created our conditional inferencing model with ctree, it is time
#to use such model to predict the dependent variable on the testing dataset. For this 
#we will use the predict function. First, lets estimate the rain tomorrow prob
#on the testing dataset.
prob.ctree <- predict(tree.ctree, newdata = testing, type = "prob")
#prob.ctree is a table with 969 rows of the testing dataset and two columns for classes 0 and 1.
#class 0 contains the prob of no rain and class 1 contains the prob of rain, that is, the prob that
#it will rain tomorrow

#Based on the rain prob, lets classify the conditions for that, we need a cutoff value. We shall
#estimate the cutoff value as the average prob of rain 
mean(as.integer(Weather_Data$rain_tomorrow))
#We can observe that the average is 1.143858 which should call our attention because it is greater than 1.
#when we take a look at the dependant we notice R saves with values 1 and 2 instead of 0 and 1. So all we 
#have to do is subtract 1 from the mean, and we obtain that the mean prob of rain_tomorrow is 
mean(as.integer(Weather_Data$rain_tomorrow))-1
#0.143852, which we will use as the cut-off value
#cut off value to be confirmed 

#First lets create a vector of 969 obs, the size of the testing base
classification.ctree <- rep("1",969)
#And now we assign zero to those obs with a rain prob less than 0.143852
classification.ctree[prob.ctree[,2]<0.143852] = "0"
classification.ctree

##Confusion Matrix
#Now let's convert it to a factor, since the function that we will use in creating the confusion matrix
#requires the data to be a factor with the same levels, in our case two levels, 0 and 1
classification.ctree <- as.factor(classification.ctree)

#Now let's build the confusion matrix for our prediction, for this we will use the confusion matrix 
#function of the package e1071
install.packages("e1071")
library("e1071")
confusionMatrix(classification.ctree,testing$rain_tomorrow, positive = "1")
TP_ctree = 119
FP_ctree = 122
FN_ctree = 20
# Calculate precision
precision_ctree <- TP_ctree / (TP_ctree + FP_ctree)
# Calculate recall
recall_ctree <- TP_ctree / (TP_ctree + FN_ctree)
# Calculate F1 score
F1_score_ctree <- 2 * precision_ctree * recall_ctree / (precision_ctree + recall_ctree)
# Print F1 score
print(F1_score_ctree)

##ROC Curve: Reciever Operating Characteristic
#This is a graphical representation commonly used to evaluate the performance of binary
#classification models. It illustrates the trade-off between the True Positive Rate(TPR)
#and the False Positive Rate across different threshhold values.
install.packages("ROCR")
library(ROCR)
library(caret)
#Let's calculate the errors
prediction.ctree.ROC <- prediction(prob.ctree[,2], testing$rain_tomorrow)
#Let's generate the data for for the ROC curve with the performance function and the parameters
#which are prediction.ctree.ROC, that is the models errors that we computed above, TPR in the 
#y-axis, and FPR on the x-axis.
ROC.ctree <- performance(prediction.ctree.ROC, "tpr", "fpr")
#let's plot the ROC curve
plot(ROC.ctree)
##How is the curve interpreted?
#The ROC curve show the trade-off between sensitivity (or TPR) and specificity(1-FPR).
#Classifiers that give curves closet to the upper left corner indicate better performance.
#As a baseline, a random classifier is expected to score points along the diagonal (FPR = TPR).
#The closer the ROC Curve is to the 45 degree diagonal the less accurate the model will be.
#Very useful as a comparison tool.

##Area Under the ROC Curve (AUC)
#Used the check the performance of any classification model
#Let's learn to estimate the area under the curve with R. Lets create the data to calculate the AUC
AUC.temp <- performance(prediction.ctree.ROC, "auc")
#Let's extract and convert the values to a numerical variable
AUC.ctree <- as.numeric(AUC.temp@y.values)
AUC.ctree


##Decision Tree with RPART
#Recursive Partitioning Decision Trees [RPART]
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
#let us define the complexity parameter
rpart.cp <- rpart.control(cp =0.0005)
#Let us build our first tree with RPART
tree.rpart <- rpart(rain_tomorrow ~., data = training, method = "class", control = rpart.cp)
rpart.plot(tree.rpart)

##Choosing Complexity Parameter
#First, let's print the error for different values of complexity parameter
printcp(tree.rpart)
plotcp(tree.rpart)
#Now, let it is time to prune i.e simplify the tree using the complexity parameter we established 
pruned.tree <- prune(tree.rpart, cp = 0.0088)
rpart.plot(pruned.tree)

##Probabilities, Classification and confusion matrix
#Let us claculate the probabilities 
prob.rpart <- predict(pruned.tree, newdata = testing, type = "prob")
#Now let us make the classification tree. First let's create a vector of 969 ones, the number 
#of observations in the testing dataset
classification.rpart <- rep("1",969)
#Then we assign 0 to those observation less than the cutoff value defined as 0.143852 as for the
#ctree classification 
classification.rpart[prob.rpart[,2]<0.143852] = "0"
#Finally, we convert such vector to a factor to build the confusion matrix. We must be consistent 
#and compare factors with factors 
classification.rpart<- as.factor(classification.rpart)
#Now, let us elaborate the confusion matrix
library(caret)
confusionMatrix(classification.rpart,testing$rain_tomorrow, positive = "1")
TP_rpart = 93
FP_rpart = 63
FN_rpart = 46

# Calculate precision
precision_rpart <- TP_rpart / (TP_rpart + FP_rpart)
# Calculate recall
recall_rpart <- TP_rpart / (TP_rpart + FN_rpart)
# Calculate F1 score
F1_score_rpart <- 2 * precision_rpart * recall_rpart / (precision_rpart + recall_rpart)
# Print F1 score
print(F1_score_rpart)

##ROC and AUC 
#First let's calculate the errors of the rpart model before building the ROC curve
library(ROCR)
prediction.rpart.ROC <- prediction(prob.rpart[,2], testing$rain_tomorrow)
#let us estimate the values of the ROC curve
ROC.rpart <- performance(prediction.rpart.ROC, "tpr", "fpr")
#let us plot the ROC curve
plot(ROC.rpart)
#Now let's compare rpart's curve with that of ctree
install.packages("pROC")
library(pROC)
plot(ROC.ctree, add = TRUE, col = "red")
#Now, lets estimate the area under the ROC curve
#lets create "AUC" data
AUC.temp.rpart <- performance(prediction.rpart.ROC, "auc")
AUC.rpart <- as.numeric(AUC.temp.rpart@y.values)
AUC.rpart

##Random Forest
install.packages("randomForest")
library(randomForest)
RF.model <- randomForest(rain_tomorrow ~., data = training, importance = TRUE,
                         proximity = TRUE, cutoff = c(0.5,0.5), type = "classification")
#Now lets print and plot the model
print(RF.model)
plot(RF.model)

#Now lets observe the importance of each of the variable in the model, both due 
#accuracy and the impurity associated with them
importance(RF.model)
#Now lets plot it
varImpPlot(RF.model)

##Probability, classification and confusion matrix
#lets compute the probabilities 
prob.rf <- predict(RF.model, newdata = testing, type ="prob")
#Vector of ones
classification.RF <- rep("1",969)
#Now lets classify based on the prob, and set the cutoff value at 0.143852 to be 
#consistent with the one we used with decision trees
classification.RF[prob.rf[,2]<0.143852] = "0"
#change to factor for use in confusion matrix
classification.RF <- as.factor(classification.RF)
#lets create the confusion matrix
confusionMatrix(classification.RF,testing$rain_tomorrow, positive = "1")
TP_RF = 125
FP_RF = 154
FN_RF = 14

# Calculate precision
precision_RF <- TP_RF / (TP_RF + FP_RF)
# Calculate recall
recall_RF <- TP_RF / (TP_RF + FN_RF)
# Calculate F1 score
F1_score_RF <- 2 * precision_RF * recall_RF / (precision_RF + recall_RF)
# Print F1 score
print(F1_score_RF)

#ROC curve
#lets generate the data for the ROC curve
library(ROCR)
prediction.RF.ROC <- prediction(prob.rf[,2], testing$rain_tomorrow)
#Now lets build the ROC curve
ROC.RF <- performance(prediction.RF.ROC, "tpr","fpr")
#plot the curve
plot(ROC.RF)
#lets compare this curve, with that of ctree and rpart
plot(ROC.ctree, add =TRUE, col = "red")
plot(ROC.rpart, add =TRUE, col = "blue")
#Now lets estimate the area under the ROC curve
AUC.RF.temp <- performance(prediction.RF.ROC, "auc")
AUC.RF <- as.numeric(AUC.RF.temp@y.values)


###Gradient boosting with xgboost
install.packages("xgboost")
library(xgboost)
#Divide data into training and testing for xgboost
training.X <- model.matrix(rain_tomorrow ~., data = training)
testing.X <- model.matrix(rain_tomorrow ~.,data = testing)
#creating design matrices using the formula interface ensures that the 
#predictors are correctly represented in a format suitable for modeling,
#and applying this process consistently to both training and testing datasets 
#ensures consistency and accuracy in model building and evaluation.

#Now we can build the model
model.XGB <- xgboost(data = data.matrix(training.X[,-1]), #remove 1st column (intercept)
                     label = as.numeric(as.character(training$rain_tomorrow)),
                     eta = 0.1,
                     max_depth = 20,
                     nround = 50,
                     objective = "binary:logistic")

#label: This parameter specifies the response variable (i.e., the target variable to be predicted). 
#It is typically a numeric vector. In this case, as.numeric(as.character(training$rain_tomorrow))
#is used to convert the response variable rain_tomorrow from the training dataset to a numeric vector.
#The as.character() function is used to ensure that the response variable is treated as a character
#vector before conversion to numeric.

#eta: [default = 0.3][range:(0,1)]. It controls the learning rate, i.e the rate at which our model
#learns patterns in data. After every round, it shinks the feature weights t reach the nest optimum.
#lower eta leads to slower computations.

#max_depth[default = 6][range"(0,inf)]. It controls the depth of the tree. Larger the depth, more 
#complex the model; higher chances of overfitting. There is no standard value for max_depth. Larger 
#data stes require deep trees to learn the rules from the data.

#nround[default = 100], It controls he max number of iterations. For classification, it is similar
#to the number of trees to grow.

#Objective[default = reg:linear]
#reg:linear for linear regression 
#binary:logistic- logistic regression for binary classification 
#multi:softmax-multiclassification using softmax objective. It returns predicted class probabilities

##Prediction and confusion matrix
#Prediction 
prediction.XGB <- predict(model.XGB, newdata = testing.X[,-1], type = "prob")
#Confusion Matrix
library(caret)
confusionMatrix(as.factor(ifelse(prediction.XGB > 0.143852, 1,0)),
                testing$rain_tomorrow, positive = "1")
TP_XGB = 103
FP_XGB = 102
FN_XGB = 36

# Calculate precision
precision_XGB <- TP_XGB / (TP_XGB + FP_XGB)
# Calculate recall
recall_XGB <- TP_XGB / (TP_XGB + FN_XGB)
# Calculate F1 score
F1_score_XGB <- 2 * precision_XGB * recall_XGB / (precision_XGB + recall_XGB)
# Print F1 score
print(F1_score_XGB)

##ROC and AUC
#Lets compute the model's errors
errors.XGB <-prediction(prediction.XGB, testing$rain_tomorrow)
#Now lets build the ROC Curve of the XGB model 
ROC.XGB <- performance(errors.XGB, "tpr", "fpr")
#lets plot the ROC curve
plot(ROC.XGB)
#Now compare
plot(ROC.ctree, add = TRUE, col = "red")
plot(ROC.rpart, add = TRUE, col = "blue")
plot(ROC.RF, add =TRUE, col = "yellow")
#AUC
AUC.XGB.temp <- performance(errors.XGB, "auc")
#Exract AUC
AUC.XGB <- as.numeric(AUC.XGB.temp@y.values)
