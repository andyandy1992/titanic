# Trevor Stephens - 9 Jan 2014
# Titanic: Getting Started With R - Part 2: The gender-class model
# Full guide available at http://trevorstephens.com/

# PREDICTOR2=Sex: Look at gender patterns
summary(train$Sex)
prop.table(table(train$Sex, train$Survived))
prop.table(table(train$Sex, train$Survived), 1)

# Create new column in test set with our prediction that everyone dies
test$Survived <- 0
# Update the prediction to say that all females will survive
test$Survived[test$Sex == 'female'] <- 1

# Create submission dataframe and output to file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = file.path(outPath, "T2a_gendermodel.csv"), row.names = FALSE)

# PREDICTOR3a=Age: Look at age patterns:
# "Our last few tables were on categorical variables, ie. they only had a few values. Now we
# have a continuous variable which makes drawing proportion tables almost useless, as there may
# only be one or two passengers for each age! So, let’s create a new variable, Child, to indicate
# whether the passenger is below the age of 18."
summary(train$Age)
train$Child <- 0
train$Child[train$Age < 18] <- 1 # we could assume the 177 missing values are the average age
                                 # of the rest of the passengers=29.70 (i.e. an adult).
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
# "Well, it still appears that if a passenger is female most survive, and if they were male most
# don’t, regardless of whether they were a child or not. So we haven’t got anything to change our
# predictions on here."

#PREDICTORS 3b and 4=Pclass and Fare:Look at class and fare patterns
"Create a new predictor by binning the fares into <$10, $10-$20, $20-$30 and $30+."
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# Create new column in test set with our prediction that everyone dies
test$Survived <- 0
# Update the prediction to say that all females will survive
test$Survived[test$Sex == 'female'] <- 1
# Update once more to say that females who pay more for a third class fare also perish
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

# Create submission dataframe and output to file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = file.path(outPath, "T2b_genderclassmodel.csv"), row.names = FALSE)