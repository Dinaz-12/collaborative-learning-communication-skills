# Load Data set
setwd("D://Y3S1//TPSM")
getwd()

df <- read.csv("D://Y3S1//TPSM//PISA_Cleaned_Data.csv")

#Check columns
names(df)
str(df)
summary(df)

#Correlation result (Connect Member 2 part)
cor.test(df$Collaboration_Score, df$Communication_Score)


#Build the predictive model
model1 <- lm(Communication_Score ~ Collaboration_Score, data = df)

#View the model summary
summary(model1)

# Create a Regression line graph 
plot(df$Collaboration_Score, df$Communication_Score,
     main = "Collaboration vs Communication",
     xlab = "Collaboration Score",
     ylab = "Communication Score",
     col = "lightblue",
     pch = 19,
     cex = 0.6)

abline(model1,
       col = "red",
       lwd = 3)


#View Predicted values 
df$Predicted_Communication <- predict(model1)
head(df)

#View Residuals
head(residuals(model1))

#Assumption checking

#Check the linear relationship using a scatter plot. 
plot(df$Collaboration_Score, df$Communication_Score,
     main = "Scatter Plot for Linearity Check",
     xlab = "Collaboration Score",
     ylab = "Communication Score")

abline(model1, lwd = 2)

#Residual plot
plot(model1$fitted.values, residuals(model1),
main = "Residual Plot",
xlab = "Fitted Values",
ylab = "Residuals")
abline(h = 0, lwd = 2)

#check Normality 
hist(residuals(model1),
     main = "Histogram of Residuals",
     xlab = "Residuals",
     col = "lightblue")

qqnorm(residuals(model1))
qqline(residuals(model1), lwd = 2)
#ANOVA on regression model
anova(model1)




