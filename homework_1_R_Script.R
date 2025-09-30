#Turn the file into a data frame 
su <- read.delim("Su_raw_matrix.txt")

#1.a
#checking to make sure I didnt mess anything up
head(su)

#1.b
#Calculating the mean and sd of the Liver_2 column.
mean(su$Liver_2.CEL)
sd(su$Liver_2.CEL)

#1.c
# Calculating mean and sum for the column
colMeans(su[, sapply(su, is.numeric)])
colSums(su[, sapply(su, is.numeric)])

#Step 2
#Generate 10k numbers for step 2. first one has a SD of .2 the second has an SD of .5
data1 <- rnorm(10000, mean=0, sd=0.2)
hist(data1,main = "Histogram where mean = 0, and sigma = .2)", xlab = "Value", xlim = c(-5,5), col = "skyblue", border = "black")

data2 <- rnorm(10000, mean=0, sd=0.5)
hist(data2,main = "Histogram where mean = 0, and sigma = .5)", xlab = "Value", xlim = c(-5,5), col = "salmon", border = "black")


#Step 3 
#Creating the dat dataframe for step 3
#demonstrates how the different plotting functions work
dat <- data.frame(cond = factor(rep(c("A","B"), each=200)),
                                      rating = c(rnorm(200),rnorm(200, mean=.8)))
library(ggplot2)
#Overlaid histogram
ggplot(dat, aes(x=rating, fill=cond)) +
    geom_histogram(binwidth=.5, alpha=.5, position="identity")

#Interleaved histogram
ggplot(dat, aes(x=rating, fill=cond)) + geom_histogram(binwidth=.5, position="dodge")

#Density Plot
ggplot(dat, aes(x=rating, colour=cond)) + geom_density()

#Transparent Density Plot
ggplot(dat, aes(x=rating, fill=cond)) + geom_density(alpha=.3)

# Load ggplot
library(ggplot2)

#Step 3.F
# Read in the data set
diabetes <- read.csv("diabetes_train.csv")

#Overlaid histogram
ggplot(diabetes, aes(x = mass, fill = class)) +
       geom_histogram(binwidth = 0.5, alpha = 0.5, position = "identity") +
       labs(title = "Overlaid Histograms")

#Interleaved histogram
ggplot(diabetes, aes(x = mass, fill = class)) +
       geom_histogram(binwidth = 0.5, position = "dodge") +
       labs(title = "Interleaved Histograms")

#Density Plot
ggplot(diabetes, aes(x = mass, colour = class)) +
       geom_density() +
       labs(title = "Density Plots")

#Transparent Density Plot
ggplot(diabetes, aes(x = mass, fill = class)) +
       geom_density(alpha = 0.3) +
       labs(title = "Density Plots with Transparency")


# Reading the titanic file into passengers
passengers <- read.csv("C:/computer science/Data_Mining_Course/titanic.csv")

#install tidyverse
install.packages("tidyverse")
library(tidyverse)

#filter out the missing data then summarize whats cleaned 
passengers %>% drop_na() %>% summary()

#filter the rows so its just data on men will say, it does look weird im not sure if i did it right.
passengers %>% filter(Sex == "male")

#arrange the data so its sorted by Fare in descending order
#this works well when it outputs i can see it is in fact in descending order
passengers %>% arrange(desc(Fare))

#combining the sum of Parch and SibSp into FamSize (I think??)
#not sure how this is able to calculate the FamSize but I trust the directions
passengers %>% mutate(FamSize = Parch + SibSp)

#calculating the number of women and man that survived 
passengers %>% group_by(Sex) %>% summarise(meanFare = mean(Fare), numSurv = sum(Survived))

#re load the data because it was giving me troubles 
diabetes <- read.csv("diabetes_train.csv")

#Step 5, calculating the quantiles for the skin attribute 
quantile(diabetes$skin, probs = c(0.10, 0.30, 0.50, 0.60))

