getwd()
titanic_dataset = read.csv('titanic3.csv')
titanic_dataset
summary(titanic_dataset)
str(titanic_dataset)
View(titanic_dataset)

#Preprocess the passenger names to come up with a list of titles that represent families
titanic_dataset$tittle <-substring(titanic_dataset$name,regexpr(",",titanic_dataset$name)+2,regexpr("\\.",titanic_dataset$name)-1)
titanic_dataset$tittle
table(titanic_dataset$tittle)

#Processing titles 
titanic_dataset[titanic_dataset$tittle %in% c('Ms','Mlle'),'tittle'] ='Miss'
titanic_dataset[titanic_dataset$tittle %in% c('Dona','Lady','the Countess','Mme'),'tittle'] ='Mrs'
titanic_dataset[titanic_dataset$tittle %in% c('Jonkheer','Don', 'Sir'),'tittle'] ='Mr'
titanic_dataset[titanic_dataset$tittle %in% c('Col','Capt', 'Major'),'tittle'] ='Others'
View(titanic_dataset)

#represent using appropriate visualization graph.
library(ggplot2)
table(titanic_dataset$tittle)
ggplot(titanic_dataset,aes(x= tittle)) + 
  geom_bar(stat = 'count')  +   labs(x = 'tittle')  + labs(y ='title counts')  


#Represent the proportion of people survived from the family size using a graph
titanic_dataset$familysize <-titanic_dataset$sibsp + titanic_dataset$parch + 1
ggplot(titanic_dataset,aes(x= titanic_dataset$familysize,  fill = factor(titanic_dataset$survived ))) + 
  geom_bar(stat = 'count')  +   labs(x = 'family size')  + labs(y ='survived') 
titanic_dataset$familysize

#Impute the missing values in Age variable using Mice Library, create two different graphs showing Age distribution before and after imputation.
#install.packages("mice")
library(mice)
set.seed(8)
computed_df = titanic_dataset[, names(titanic_dataset) %in% c('age','sibsp','parch','fare','embarked')] 
ageimputed = mice(computed_df, method = "rf", m=5)
imputedage = complete(ageimputed)
par(mfrow=c(1,2))
hist(titanic_dataset$age, main = "Before Imputation", col = "navy blue")
hist(imputedage$age, main = "After Imputation", col = "dark red")

