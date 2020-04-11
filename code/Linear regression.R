library(pacman)
library(datasets)
library(e1071) 
library(DMwR)
library(DAAG)
library(caret)
library(gridExtra)
library(dplyr)
library(reshape2)

theme_set(theme_bw())
pacman::p_load(pacman, party, rio, tidyverse)


setwd("C:\\Users\\85036758\\Documents\\LinkedInLearning")

?cars
df <- cars %>%
      print()


gg <- ggplot(df, aes(x=speed, y=dist)) +
      geom_point(aes( size=dist)) +
      geom_smooth(method="lm", se=F) + 
      labs(
        #subtitle="scatterplot (Distance by speed)", 
       y="Distance", 
       x="Speed", 
       title="Distance by speed", 
       caption = "Source: cars")
plot(gg)



# Box Plot (Show Speed Outliers)

mboxplot <- ggplot(df, aes(" ", speed)) 
mboxplot <- mboxplot + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Speed", 
       caption="Source: CARS",
       x="Outlier rows",
       y="Speed")


# Box Plot (Show Distance Outliers)
m1boxplot <- ggplot(df, aes(" ", dist))
m1boxplot<- m1boxplot + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Distance", 
       caption="Source: CARS",
       x="Outlier rows",
       y="Distance")

grid.arrange(mboxplot,m1boxplot, ncol=2, nrow=1)


msdensityplot <- ggplot(cars, aes(speed))
msdensityplot <- msdensityplot + geom_density(fill="red", alpha=0.8) + 
  labs(title="Density plot", 
       subtitle="Frequency by Speed",
       caption="Source: cars",
       x="Speed")


mddensityplot <- ggplot(cars, aes(dist))
mddensityplot <- mddensityplot + geom_density(fill="red", alpha=0.8) + 
  labs(title="Density plot", 
       subtitle="Frequency by Distance",
       caption="Source: cars",
       x="Distance")

grid.arrange(msdensityplot, mddensityplot, ncol=2, nrow=1)

# check correlation
cor(cars$speed, cars$dist)

# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))  # row indices for training data
trainingData <- cars[trainingRowIndex, ]  # model training data
testData  <- cars[-trainingRowIndex, ]   # test data

linModel <- lm(dist~speed, data=trainingData)
par(mfrow = c(2,2))
plot(linModel)

distPred <- predict(linModel, testData)
distPred

print(linModel)
summary(linModel)


# make actuals_predicteds dataframe.
actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))%>% 
  rename(actual_distance = actuals, predicted_distance = predicteds)

testData <- testData %>%
            mutate(predicted_distance = actuals_preds$predicted_distance)

dev.off()
#testData <- melt(testData, id="speed")

mlineplot <- ggplot(testData, aes(x=speed)) +
             geom_line(aes(y=dist, col="plum")) +
             geom_line(aes(y=predicted_distance, col="blue")) +
             labs(
                title = "Actual Distance vs Predicted Distance",
                x ="Speed",
                y = "Distance"
                
                
              ) + theme(legend.position = "none")

plot(mlineplot)

correlation_accuracy <- cor(actuals_preds)  # 82.7%

regr.eval(actuals_preds$actual_distance, actuals_preds$predicted_distance)

#k-fold cross validation
# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)
# Train the model
model <- train(dist ~., data = cars, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)



