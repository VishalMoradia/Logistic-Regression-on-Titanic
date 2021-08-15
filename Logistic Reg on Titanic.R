# package installation------------------

install.packages("Amelia")

# importing libraries-----------

library(dplyr)
library(tidyr)
library(Amelia)
library(ggplot2)
library(caTools)

# importing data -----------------------

df_train <- read.csv(file.choose())

head(df_train)


# lets plot the missing map using Amelia package----------

missmap(df_train, main = 'missing map', col = 
             c('yellow', 'black'), legend = FALSE)
# thorugh this map we can see that there are lot of missing values in the column of age-------

# Exploring data through visualizations..................

gplot(df_train, aes(Survived)) + geom_bar()


# most people are in thrid class.
# interestingly enough, there care more people in first class than in second class...

ggplot(df_train, aes(Pclass)) + geom_bar(aes(fill = factor(Pclass)))


# Males are almost double the number compared to females onboard.

ggplot(df_train, aes(Sex)) + geom_bar(aes(fill = factor(Sex)))     


# looks like there are more young people than older people. 
# Number of children is also considerably good.

ggplot(df_train, aes(Age)) + geom_histogram(bins = 20, alpha = 0.7)

# lets look at the fare column using histogram.......
# we can easily see that most people bought low fare tickets.
# which also explains why there are more people in third class than in any other class....
ggplot(df_train, aes(Fare)) + geom_histogram(bin = 20, alpha = 0.7)



# Before we run logistics regression on the data, we have to clean the data........

# There are many missing values in the age column which we see in missmap()....

# One way is to delete all the values but there are over 150 rows.......

# other method is to impute missing age with the average age of passengers.

# But here we will impute the average age by class of the passenger.......

# before we impute let's plot a boxplot to check the median values for age against class....



plot1 <- ggplot(df_train, aes(Pclass, Age)) +
  geom_boxplot(aes(group = Pclass, fill = factor(Pclass), alpha = 0.5))

plot1 + scale_y_continuous(breaks = seq(min(0), max(80), by = 2)) + theme_bw()


# to impute missing age values with the average age of passengers with respect to their class.
# we will create a function.

age_imputation <- function(age, class){ # taking two areguments, age and class.
  outcome <- age
  for (i in 1:length(age)){ # this will iterate all values in age column. 
    if (is.na(age[i])){ # conditional statement to see if the age in the column is missing then...
      if (class[i] == 1){ # nested if-else statement to check the class of the passenger
        outcome[i] <- 37 # if class is 1 then it return avergae age as 37 and so on
      } else if (class[i] == 2) {
        outcome[i] <- 29
      } else{
        outcome[i] <- 24
      }
    }else{
      outcome[i] <- age[i] # here if the age is not missing then outcome will be equal to that age
    }
  }
  return(outcome)
}

# creating new age column to run the created function..............

new_age <- age_imputation(df_train$Age, df_train$Pclass)

any(is.na(new_age)) # return false. All missing age values are successfully imputed.

#replacing original age column with the new derived age column.........

df_train$Age <- new_age

# lets run missmap() again to see if there any more missing values........


missmap(df_train, main = 'Missing Value Check', col = c('yellow', 'black'), legend = FALSE)


# as we can see map is completely black and hence there is no missing value.





# Removing unwanted columns from the fruther analysis...............
df_train <- select(df_train, -PassengerId, -Name, -Ticket, -Cabin)


head(df_train)

str(df_train)

# converting few columns into factors from integers.....................

df_train$Survived <- factor(df_train$Survived)
df_train$Pclass <- factor(df_train$Pclass)
df_train$SibSp <- factor(df_train$SibSp)
df_train$Parch <- factor(df_train$Parch)


# checking the structure of the data ........

str(df_train)




# Training the MODEL>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


log_model <- glm(Survived ~., family = binomial(link = 'logit'), data = df_train)


summary(log_model)


#lets split the data for training and testing the model for prediction of values...........


split_df <- sample.split(df_train$Survived, SplitRatio = 0.7)

split_train <-  subset(df_train, split_df == TRUE)
split_test <- subset(df_train, split_df == FALSE)

model_split <- glm(Survived~., family = binomial(link = 'logit'), data = split_train)
summary(model_split)



# lets predict now--------------------------

fitted_probability <- predict(model_split, split_test, type = 'response')
fitted_result <- ifelse(fitted_probability>0.5,1,0) # if my probability is greater than 0.5 than set it equal to 1 else 0.

# we will now get our missclassification error results..............

missclasserror <- mean(fitted_result != split_test$Survived)

# rough accuracy of our results......................

accuracy <- 1 - missclasserror
accuracy


# lets create CONFUSION MATRIX ...................................................

table(split_test$Survived, fitted_probability>0.5)

