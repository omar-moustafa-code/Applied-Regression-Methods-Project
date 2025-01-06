# Installing & Loading the Necessary Packages:
if(!require("DescTools")) install.packages("DescTools")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("olsrr")) install.packages("olsrr")
if(!require("MASS")) install.packages("MASS")
if(!require("stats")) install.packages("stats")
if(!require("lmtest")) install.packages("lmtest")
if(!require("e1071")) install.packages("e1071")

# install.packages("DescTools")
# install.packages("ggplot2")
# install.packages("olsrr")
# install.packages("MASS")
# install.packages("stats")
# install.packages("lmtest")
# install.packages("e1071")

library(DescTools)
library(ggplot2)
library(olsrr)
library(MASS)
library(stats)
library(lmtest)
library(e1071)

# Loading & Viewing the Dataset:

# Load the data set
df = read.csv("StudentPerformanceFactors.csv", header = TRUE)

# Display the first 5 rows of the dataset
head(df, 5)

# Display the internal structure of the dataset
str(df)

# Display a summary of the dataset
summary(df)

# Computing & printing the standard deviation, sd, of each quantitative variable
print(round(sapply(df[, sapply(df, is.numeric)], sd, na.rm = TRUE), 2))

# Are there any NA data values?
any(is.na(df))

# Fitting a standard regression model; Exam_Score is the response variable
reg = lm(Exam_Score ~ ., data = df)
summary(reg)

# Checking the Standard 10 Regression Assumptions:

# 1. Checking Linearity
# Scatterplots of quantitative predictors vs response variable (Exam_Score) to check linearity
pairs(~ Exam_Score + Hours_Studied + Attendance + Sleep_Hours + Previous_Scores + Tutoring_Sessions + Physical_Activity, data = df, main = "Scatterplots of Predictors vs Response", pch = 19)

# Sleep_Hours and Exam_Score, as well as Physical_Activity and Exam_Score, do not exhibit clear linearity as seen in the previous plot
# Applying the square root transformation to Sleep_Hours and Physical_Activity
df$Sqrt_Sleep_Hours = sqrt(df$Sleep_Hours)
df$Sqrt_Physical_Activity = sqrt(df$Physical_Activity)
pairs(~ Exam_Score + Hours_Studied + Attendance + Sleep_Hours + Previous_Scores + Tutoring_Sessions + Physical_Activity, data = df, main = "Scatterplots of Predictors vs Response #2", pch = 19)

# 2. Checking Independence of Observations
dwtest(lm(Exam_Score ~ 1, data = df))

# 3. Checking Homoscedasticity
# Plot Fitted Values vs Residuals
plot(reg$fitted.values, residuals(reg), pch = 19, xlab = "Fitted Values", ylab = "Residuals", main = "Fitted Values vs Residuals (Check for Homoscedasticity)")
abline(h = 0, col = "red")

# 4. Checking Normality of Response (Dependent) Variable
# Normality of the response variable (Exam Score)

op = par(mfrow = c(1, 2))

hist(df$Exam_Score, main = "Histogram of Exam Score", xlab = "Exam Score", breaks = 10, col = "light blue", border = "black")

qqnorm(df$Exam_Score, main = "QQ Plot of Exam Score", pch = 19)
qqline(df$Exam_Score, col = "red")

par(op)

# Applying the log-transformation to the response variable, Exam_Score
df$log_Exam_Score = log(df$Exam_Score)

# output is the same even if we were to say Sqrt_Sleep_Hours and Sqrt_Physical_Activity
model_log = lm(log_Exam_Score ~ Hours_Studied + Attendance + Previous_Scores + Tutoring_Sessions + Sleep_Hours + Physical_Activity, data = df)

hist(df$log_Exam_Score, main = "Histogram of Log-Transformed Exam Score", xlab = "Log-Transformed Exam Score", breaks = 10, col = "light blue", border = "black")

qqnorm(df$log_Exam_Score, main = "QQ Plot of Log-Transformed Exam Score", pch = 19)
qqline(df$log_Exam_Score, col = "red")

# 5. Checking No Multicollinearity
# Correlation matrix for quantitative/numeric predictors
correlation_matrix = cor(df[, c("Hours_Studied", "Attendance", "Sleep_Hours", "Previous_Scores", "Tutoring_Sessions", "Physical_Activity")])
print(correlation_matrix)

# 6. Checking No Autocorrelation of Residuals
plot(reg$fitted.values, reg$residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Fitted Values vs Residuals")

abline(h = 0, col = "red")

# 7. Checking Outliers & Leverage
# Boxplots of all numeric variables (for outlier detection)

par(mfrow = c(3, 3))

boxplot(df$Hours_Studied, main = "Hours Studied", ylab = "Hours_Studied")

boxplot(df$Attendance, main = "Attendance", ylab = "Attendance")

boxplot(df$Sleep_Hours, main = "Sleep Hours", ylab = "Sleep_Hours")

boxplot(df$Previous_Scores, main = "Previous Scores", ylab = "Previous_Scores")

boxplot(df$Tutoring_Sessions, main = "Tutoring Sessions", ylab = "Tutoring_Sessions")

boxplot(df$Physical_Activity, main = "Physical Activity", ylab = "Physical_Activity")

boxplot(df$Exam_Score, main = "Exam Score", ylab = "Exam_Score")

par(op)

# Boxplots of categorical variables against Exam_Score (continuing the outlier detection)

par(mfrow = c(3, 2))

boxplot(Exam_Score ~ Parental_Involvement, data = df, main = "Exam Score by Parental Involvement", xlab = "Parental Involvement", ylab = "Exam Score")

boxplot(Exam_Score ~ Access_to_Resources, data = df, main = "Exam Score by Access to Resources", xlab = "Access to Resources", ylab = "Exam Score")

boxplot(Exam_Score ~ Extracurricular_Activities, data = df, main = "Exam Score by Extracurricular_Activities", xlab = "Extracurricular_Activities", ylab = "Exam Score")

boxplot(Exam_Score ~ Motivation_Level, data = df, main = "Exam Score by Motivation Level", xlab = "Motivation Level", ylab = "Exam Score")

boxplot(Exam_Score ~ Internet_Access, data = df, main = "Exam Score by Internet Access", xlab = "Internet Access", ylab = "Exam Score")

boxplot(Exam_Score ~ Family_Income, data = df, main = "Exam Score by Family Income", xlab = "Family Income", ylab = "Exam Score")

boxplot(Exam_Score ~ Teacher_Quality, data = df, main = "Exam Score by Teacher_Quality", xlab = "Teacher_Quality", ylab = "Exam Score")

boxplot(Exam_Score ~ School_Type, data = df, main = "Exam Score by School_Type", xlab = "School_Type", ylab = "Exam Score")

boxplot(Exam_Score ~ Peer_Influence, data = df, main = "Exam Score by Peer_Influence", xlab = "Peer_Influence", ylab = "Exam Score")

boxplot(Exam_Score ~ Learning_Disabilities, data = df, main = "Exam Score by Learning_Disabilities", xlab = "Learning_Disabilities", ylab = "Exam Score")

boxplot(Exam_Score ~ Parental_Education_Level, data = df, main = "Exam Score by Parental_Education_Level", xlab = "Parental_Education_Level", ylab = "Exam Score")

boxplot(Exam_Score ~ Distance_from_Home, data = df, main = "Exam Score by Distance_from_Home", xlab = "Distance_from_Home", ylab = "Exam Score")

boxplot(Exam_Score ~ Gender, data = df, main = "Exam Score by Gender", xlab = "Gender", ylab = "Exam Score")

par(op)

# 8. Checking No-Autocorrelation (Alternative Check to Homoscedasticity)
# Scatterplots of each numeric predictor as the x and the response variable as y to check for variance patterns

par(mfrow = c(3, 2))

plot(df$Hours_Studied, df$Exam_Score, pch = 19, xlab = "Hours Studied", ylab = "Exam Score", main = "Hours Studied vs Exam Score")

plot(df$Attendance, df$Exam_Score, pch = 19, xlab = "Attendance", ylab = "Exam Score", main = "Attendance vs Exam Score")

plot(df$Sleep_Hours, df$Exam_Score, pch = 19, xlab = "Sleep Hours", ylab = "Exam Score", main = "Sleep Hours vs Exam Score")

plot(df$Previous_Scores, df$Exam_Score, pch = 19, xlab = "Previous Scores", ylab = "Exam Score", main = "Previous Scores vs Exam Score")

plot(df$Tutoring_Sessions, df$Exam_Score, pch = 19, xlab = "Tutoring Sessions", ylab = "Exam Score", main = "Tutoring Sessions vs Exam Score")

plot(df$Physical_Activity, df$Exam_Score, pch = 19, xlab = "Physical Activity", ylab = "Exam Score", main = "Physical Activity vs Exam Score")

par(op)

#9. Checking Transformation of Skewness
# Histograms for all numeric predictors to check skewness

par(mfrow = c(3, 2))

hist(df$Hours_Studied, main = "Hours Studied", xlab = "Hours_Studied", breaks = 10)

hist(df$Attendance, main = "Attendance", xlab = "Attendance", breaks = 10)

hist(df$Sleep_Hours, main = "Sleep Hours", xlab = "Sleep_Hours", breaks = 10)

hist(df$Previous_Scores, main = "Previous Scores", xlab = "Previous_Scores", breaks = 10)

hist(df$Tutoring_Sessions, main = "Tutoring Sessions", xlab = "Tutoring_Sessions", breaks = 10)

hist(df$Physical_Activity, main = "Physical Activity", xlab = "Physical_Activity", breaks = 10)

par(op)

# 10. Checking Model Fit
# Plot residuals vs fitted values for model diagnostics
plot(reg$fitted.values, residuals(reg), pch = 19, xlab = "Fitted Values", ylab = "Residuals", main = "Fitted Values vs Residuals")

# Normal Q-Q Plot for Residuals
qqnorm(residuals(reg), pch = 19, main = "Normal Q-Q Plot")
qqline(residuals(reg), col = "red")

# Cook's Distance Plot
plot(cooks.distance(reg), pch = 19, xlab = "Index", ylab = "Cook's Distance", main = "Cook's Distance Plot")

# Graphs Before Fitting a Model to the Data:

# Adjusted Code for the Categorical Variables:

par(mfrow = c(3, 2))

barplot(table(df$Parental_Involvement), main = "Parental Involvement", xlab = "Parental Involvement", ylab = "Frequency")

barplot(table(df$Access_to_Resources), main = "Access to Resources", xlab = "Access to Resources", ylab = "Frequency")

barplot(table(df$Extracurricular_Activities), main = "Extracurricular Activities", xlab = "Extracurricular Activities", ylab = "Frequency")

barplot(table(df$Motivation_Level), main = "Motivation Level", xlab = "Motivation Level", ylab = "Frequency")

barplot(table(df$Internet_Access), main = "Internet Access", xlab = "Internet Access", ylab = "Frequency")

barplot(table(df$Family_Income), main = "Family Income", xlab = "Family Income", ylab = "Frequency")

barplot(table(df$Teacher_Quality), main = "Teacher_Quality", xlab = "Teacher_Quality", ylab = "Frequency")

barplot(table(df$School_Type), main = "School_Type", xlab = "School_Type", ylab = "Frequency")

barplot(table(df$Peer_Influence), main = "Peer_Influence", xlab = "Peer_Influence", ylab = "Frequency")

barplot(table(df$Learning_Disabilities), main = "Learning_Disabilities", xlab = "Learning_Disabilities", ylab = "Frequency")

barplot(table(df$Parental_Education_Level), main = "Parental_Education_Level", xlab = "Parental_Education_Level", ylab = "Frequency")

barplot(table(df$Distance_from_Home), main = "Distance_from_Home", xlab = "Distance_from_Home", ylab = "Frequency")

barplot(table(df$Gender), main = "Gender", xlab = "Gender", ylab = "Frequency")

par(op)

# Distribution of the Response Variable, which is Exam_Score
# Histogram is a satisfactory choice since df has well over 150 observations

hist(df$Exam_Score, main = "Distribution of (Final) Exam Scores", xlab = "Exam_Score", col = "light blue", border = "black")

# Initial Model Fit to the Data (Before Any Transformation If Needed):
# Converting the categorical variables to factors using the as.factor function

df$Parental_Involvement = as.factor(df$Parental_Involvement)

df$Access_to_Resources = as.factor(df$Access_to_Resources)

df$Extracirricular_Activities = as.factor(df$Extracurricular_Activities)

df$Motivation_Level = as.factor(df$Motivation_Level)

df$Internet_Access = as.factor(df$Internet_Access)

df$Family_Income = as.factor(df$Family_Income)

df$Teacher_Quality = as.factor(df$Teacher_Quality)

df$School_Type = as.factor(df$School_Type)

df$Peer_Influence = as.factor(df$Peer_Influence)

df$Learning_Disabilities = as.factor(df$Learning_Disabilities)

df$Parental_Education_Level = as.factor(df$Parental_Education_Level)

df$Distance_from_Home = as.factor(df$Distance_from_Home)

df$Gender = as.factor(df$Gender)

# Initial model fit to the data (full list of predictors)
initial_reg_model = lm(Exam_Score ~ Hours_Studied + Attendance + Sleep_Hours +
                       Previous_Scores + Tutoring_Sessions + Physical_Activity +
                       Parental_Involvement + Access_to_Resources + Extracurricular_Activities +
                       Motivation_Level + Internet_Access + Family_Income + Teacher_Quality +
                       School_Type + Peer_Influence + Learning_Disabilities +
                       Parental_Education_Level + Distance_from_Home + Gender,
                       data = df)

summary(initial_reg_model)

# Residual plots as they can help with checking linearity and homoscedasticity

par(mfrow = c(2, 2))
plot(initial_reg_model)

# More Transformation of Variables (If Needed):
quantitative_variables = c("Hours_Studied", "Attendance", "Sleep_Hours", "Previous_Scores", "Tutoring_Sessions", "Physical_Activity")

# Calculating the skewness for each of the quantitative variables
list_of_skewness_values = sapply(df[quantitative_variables], skewness)

print(list_of_skewness_values)

cat("\n")

highly_skewed_greater_than_1 = names(list_of_skewness_values[abs(list_of_skewness_values) > 1])
print(paste("Quantitative Variables With Skewness > 1: ", paste(highly_skewed_greater_than_1, collapse = ", ")))

highly_skewed_less_than_negative_1 = names(list_of_skewness_values[abs(list_of_skewness_values) < -1])
print(paste("Quantitative Variables With Skewness < -1: ", paste(highly_skewed_less_than_negative_1, collapse = ", ")))

# Identification of Outliers, Leverage Points, & Influential Observations
# 1. Identifying Outliers:

standardized_residuals = rstandard(reg)
list_of_outliers = which(abs(standardized_residuals) > 3)

print("Outliers (Those With |Standardized Residual| > 3):")
print(list_of_outliers)

# 2. Leverage Points:

leverage_value = hatvalues(initial_reg_model)
list_of_high_leverage = which(leverage_value > (2 * mean(leverage_value)))

print("High Leverage Points: ")
print(list_of_high_leverage)

# 3. Influential Observations:

cooksDistance_value = cooks.distance(reg)
list_of_influential_observations = which(cooksDistance_value > (4 / nrow(df)))

print("Influential Points (Those With A Cook's Distance > 4/n)")

print(list_of_influential_observations)

plot(cooksDistance_value, type = "h", main = "Cook's Distance Plot", ylab = "Cook's Distance")
abline(h = 4 / nrow(df), col = "red")

# Graphs After Fitting the Model to the Data & Removing Outliers:
# Removing the highly influential points from df
df_cleaned = df[-list_of_influential_observations, ]

reg_updated = lm(Exam_Score ~ ., data = df_cleaned)

par(mfrow = c(2, 2))
plot(reg_updated)

# Updating the Cook's Distance and Leverage Values
cooksDistance_updated = cooks.distance(reg_updated)
leverage_updated = hatvalues(reg_updated)

plot(cooksDistance_updated, type = "h", main = "Cook's Distance (Refitted/Updated)", ylab = "Cook's Distance")
abline(h = 4 / nrow(df_cleaned), col = "red")

plot(leverage_updated, type = "h", main = "Leverage Values (Refitted/Updated)", ylab = "Leverage")
abline(h = 2 * mean(leverage_updated), col = "red")

# Model Criticism & Reformulation:
# Checking the VIF values of all variables as those with a value > 10 must be removed
ols_vif_tol(initial_reg_model)

# Dropping variables with VIF > 10 and refitting the model
reg_model_vif = lm(Exam_Score ~ . - Teacher_Quality - Parental_Education_Level - Distance_from_Home - Extracurricular_Activities, data = df)

ols_vif_tol(reg_model_vif)

# If Condition Indices greater than 30, severe collinearity could be an issue which would require PC Regression
df_numeric_vars = df[, sapply(df, is.numeric)]

X = as.matrix(df_numeric_vars[,-1])

R = cor(X)

e = eigen(R)

V = e$vec

lambdaj = e$val

kappaj = sqrt(lambdaj[1] / lambdaj)

cbind(lambdaj, kappaj)

C = X %*% V # Principal components

colnames(C) = paste("C", (1:ncol(C)), sep = "")  # Gives names to the columns of W

df1 = data.frame(Exam_Score = df$Exam_Score, C)

reg.pc = lm(Exam_Score ~ C, data = df1)

summary(reg.pc)

alpha = reg.pc$coef[-1]

beta.pc = V %*% alpha

y.hat.pc = C %*% beta.pc

# Plotting the fitted vs. observed values
plot(y.hat.pc, df$Exam_Score, pch = 20, main = "Scatter Plot of Fitted vs Observed Values", xlab = "Fitted Values", ylab = "Observed Final Exam Scores")

# Predicted values after PC Regression and multi-collinearity issues were dealt with
predicted_values = predict(reg.pc, newdata = df1)

# Plot predicted values vs. actual values (final model)
plot(predicted_values, df1$Exam_Score, pch = 20, main = "Predicted Values vs Actual Values [Final Model]", xlab = "Predicted Values", ylab = "Actual Values")

# Check the correlation between the fitted values and the actual response variable (original)
cor(y.hat.pc, df$Exam_Score)

# Check the correlation between the predicted values and the actual response variable (final model)
cor(predicted_values, df1$Exam_Score)



