# Trevor Stephens - 10 Jan 2014
# Titanic: Getting Started With R - Part 3: Decision Trees
# Full guide available at http://trevorstephens.com/

# Install and load required packages for fancy decision tree plotting
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# DECISIONTREE1: Recreate the gender model
fit <- rpart(Survived ~ Sex, data=train, method="class")
fancyRpartPlot(fit)

# DECISIONTREE2: Build a deeper tree (using all NON-unique predictors)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
# Plot it with base-R
plot(fit)
text(fit)
# And then make it look better with fancyRpartPlot!
fancyRpartPlot(fit)

# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, type = "class") # can use method=”anova” for regression/continuous variables
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = file.path(outPath, "T3a_myfirstdtree.csv"), row.names = FALSE)

# Let's unleash the decision tree and let it grow to the max
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)

# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = file.path(outPath, "T3b_myfullgrowntree.csv"), row.names = FALSE)

# Manually trim a decision tree (overriding the default complexity that the tree grows to)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control(minsplit=2, cp=0.005)) # cp:=metric that stops splits that aren’t deemed important enough.
                                                                          # minsplit:=how many passengers must sit in a bucket before even looking for a split.
# CAUTION: Don't run DT too extreme such that the training data fits perfectly:
# "Perhaps that 34 year old female in third class who paid $20.17 for a ticket from Southampton with a sister and
#  mother aboard may have been a bit of a rare case after all."
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)