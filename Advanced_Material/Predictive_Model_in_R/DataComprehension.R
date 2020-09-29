# To be shared with Audience before the session

# The objective is to get you thinking.  Not to worry about technique or math.
# 
# You as Data Scientist
# 
# You are asked to build model to predict impact of drug on individuals, can you think of approach?
# The data is provided to you by pharama scientists. Which of these makes sense?
# 1. build a holistic model
# 2. Build two models for each Gender
# 3. Build 4 different models for distinct Age groups
# 4. build model for each Gender and each of Age groups
# 
# if you were asked to model income of individuals across world, would you
# 1. build one holistic model?
# 2. build one model for each country?
# 3. build one model for each city?
# 4. what other variables will help you decide number of models?
# where do you draw the line on number of models?
# 
# 
# you are given a dataset that is heterogenous.  how would you go about deciding on number of models that will be required to built?
# 
# Lets discuss these use cases at the end of the session




# Data Scientists skills

# 1. Learn a technique(clustering/classification/Regression/Trees/DL)
# 2. Learn a Language/Package(R/SPSS/SAS/Python)
# 3. Learn a Database(SQL/MongoDB/etc)
# 4. Models and intepretations(Linear/Logistic Regression)
# 5. Visualization tools and packages(Tableau etc)
# 6. How to look at data? what to look in data?  what to infer?

# Objective:
# 1. We will estimate(predict) missing value in a column/predict a value in a variable given other variables
# 2. mean/meidan/mode are ineffective
# 3. We will try and impute via regression, especially, APPROPRIATE regression


# install.packages('ggplot2')
# install.packages('moments')
# install.packages('fGarch')
# install.packages('lme4')
library(lme4)
library(fGarch)
library(ggplot2)
library(moments)


data("iris")

fix(iris)
# The first step is to check out Data structure 
str(iris)
# Continuous Data/Categorical Data

# Questions
# 1. Summary statistics?  what summary statistics reveal/does not reveal?
# 2. There are 4 continuous variables. are they releated/redundant? which one to chuck off?
# 3. Does the categorical variable add any value beyond identifying rows


summary(iris)

cat("Sepal.Length:",var(iris$Sepal.Length),'\n',
    "Sepal.Width:",var(iris$Sepal.Width),'\n',
    "Petal.Length:",var(iris$Petal.Length),'\n',
    "Petal.Width:",var(iris$Petal.Width))

cat("Sepal.Length:",sd(iris$Sepal.Length)/mean(iris$Sepal.Length),'\n',
    "Sepal.Length:",sd(iris$Sepal.Width)/mean(iris$Sepal.Width),'\n',
    "Sepal.Length:",sd(iris$Petal.Length)/mean(iris$Petal.Length),'\n',
    "Sepal.Length:",sd(iris$Petal.Width)/mean(iris$Petal.Width))

# Google(images.google.com) anscombe's quartet

#Lets play with Bell curve
sigma=4
mu=2
X=seq(-10,10,length=500)
Y=1/(sigma*sqrt(2*pi))*exp(-(X-mu)^2/(2*sigma))
plot(X,Y,type="l",lwd=2,col="red",ylim=c(0,0.5))
# can you get an idea of mean/median/mode from chart?


#Lets play with Bell curve and kurtosis
k=3
sigma=1
mu=0
X=seq(-10,10,length=500)
Y=1/(sigma*sqrt(2*pi))*exp(-(X-mu)^2/(2*sigma))*k/3
plot(X,Y,type="l",lwd=2,col="red", ylim=c(0,0.5))

#Lets play with Bell curve and skewness
sk=-0.5
sigma=1
mu=0
X=seq(-10,10,length=500)
Y=dsnorm(X, mean = mu, sd = sigma, xi = exp(sk), log = FALSE)
plot(X,Y,type="l",lwd=2,col="red",ylim=c(0,0.5))


bucket_size=5
hist(iris$Sepal.Length, breaks=bucket_size)
hist(iris$Sepal.Width, breaks=bucket_size)
hist(iris$Petal.Length,breaks=bucket_size)
hist(iris$Petal.Width, breaks=bucket_size)

hc <- hclust(dist(iris$Sepal.Length), "ave");plot(hc)
hc <- hclust(dist(iris$Sepal.Width), "ave");plot(hc)
hc <- hclust(dist(iris$Petal.Length), "ave");plot(hc)
hc <- hclust(dist(iris$Petal.Width), "ave");plot(hc)


cat("Sepal.Length:",skewness(iris$Sepal.Length),'\n',
    "Sepal.Width:",skewness(iris$Sepal.Width),'\n',
    "Petal.Length:",skewness(iris$Petal.Length),'\n',
    "Petal.Width:",skewness(iris$Petal.Width))


cat("Sepal.Length:",kurtosis(iris$Sepal.Length),'\n',
    "Sepal.Width:",kurtosis(iris$Sepal.Width),'\n',
    "Petal.Length:",kurtosis(iris$Petal.Length),'\n',
    "Petal.Width:",kurtosis(iris$Petal.Width))


# we extend the summary statistics for more than two variables
pairs(iris)
# please apply all that we have learnt so far in two dimensions




# Lets talk about variance
# Variance is key to modelling.  
# It is important we interpret in right manner


# The following Y1,Y2,Y3 all have same mean
X=c(1,2,3,4,5,6)
Y1=c(2,2,2,2,2,2)
Y2=c(1.7,2.3,1.9,2.1,1.8,2.2)
Y3=c(1.7,1.8,1.9,2.1,2.2,2.3)
Y4=c(2,4,6,8,10,12)
Y5=c(3,5,7,9,11,13)
#Can you recall(google) formula for variance and explain yourself the meaning variance formulae trying to convey?
df=data.frame(X,Y1,Y2,Y3,Y4,Y5)
fix(df)

ggplot(df, aes(x=X, y=Y1)) + 
  geom_point(color='red', size=3) +  
  geom_line(linetype='dashed', size=1, colour='blue')
; sum((Y1-mean(Y1))^2)/(length(X)-1)
paste("Variance calculated from Dotted line:",var(Y1),sum((Y1-mean(Y1))^2)/(length(X)-1))


ggplot(df, aes(x=X, y=Y2)) + 
  geom_point(color='red', size=3) +  
  geom_line(aes(y=Y1),linetype='dashed', size=1, colour='blue')
paste("Variance calculated from Dotted line:",var(Y2),sum((Y2-mean(Y2))^2)/(length(X)-1))

#Base Model
ggplot(df, aes(x=X, y=Y3)) + 
  geom_point(color='red', size=3) +  
  geom_line(aes(y=Y1),linetype='dashed', size=1, colour='blue')
paste("Variance calculated from Dotted line:",var(Y3),sum((Y3-mean(Y3))^2)/(length(X)-1))

#Improved Model
constant=1.65
slope=0.1
ggplot(df, aes(x=X, y=Y3)) + 
  geom_point(color='red', size=3) +  
  geom_line(aes(y=constant+X*slope),linetype='dashed', size=1, colour='blue')
paste("Variance calculated from Dotted line:",sum((Y3-(constant+X*slope))^2)/(length(X)-1))


Base_variance=sum((Y3-mean(Y2))^2)/(length(X)-1)
Improved_variance=sum((Y3-(constant+X*slope))^2)/(length(X)-1)
paste(Base_variance,Improved_variance)

# R Squared cat out of the magical hat
# R Squared is metric calculated out of variance
Rsquare=1-Improved_variance/Base_variance
# Can R Square be negative?

paste("R-Square:",Rsquare*100)

# Task 1
# Consider Improved Variance as base vairance 
# Change constant and slope to any number
# Calculate new improved variance
# Report R Square

# Task 2
# calculate base vairance with Y1
# assume constant and slope to any number for Y4 and Y5
# Calculate new improved variance
# Report R Square




# Lets check given Sepal.Length, can we predict Petal.Length
# Lets investigate scatter chart first
ggplot(iris, aes(x=Sepal.Length, y=Petal.Length)) + geom_point(shape=21)

# Run the linear model and glance through summary
BaseModel = lm(Petal.Length ~ Sepal.Length, data = iris)
summary(BaseModel)

constant=summary(BaseModel)$coefficients[1,1]
slope=summary(BaseModel)$coefficients[2,1]
rsquare=summary(BaseModel)$r.squared
rse=sigma(BaseModel)

#run through a basic metrics for inference
cat("Constant:",constant,'\n',
    "Slope:",slope,'\n',
    "R-Squared:",rsquare,'\n',
    "Residual Error:",rse,'\n')

ggplot(iris_clusters, aes(x=Sepal.Length, y=Petal.Length)) + 
  geom_point(shape=21) +
  geom_line(aes(y=constant+Sepal.Length*slope),linetype='dashed', size=1, colour='blue')
paste("Variance calculated from Dotted line:",sum((iris_clusters$Petal.Length-(constant+iris_clusters$Sepal.Length*slope))^2)/(nrow(iris_clusters)-1))  
# Do you see scope for improvement?



#Hierarchical Clustering.  Clustering is grouping based on similarities/proximities
sepal_data=data.frame(Sepal.Length=iris$Sepal.Length,Petal.Length=iris$Petal.Length,Species=iris$Species)
hc <- hclust(dist(sepal_data[-c(3)]),method="centroid");plot(hc)


number_of_groups=2
# try different number_of_groups
plot(hc);rect.hclust(hc, k=number_of_groups, border="red")             


groups <-cutree(hc,k=number_of_groups)
iris_clusters=data.frame(Species=sepal_data$Species,Sepal.Length=sepal_data$Sepal.Length,Petal.Length=sepal_data$Petal.Length,groups)
iris_clusters$groups=factor(iris_clusters$groups)
fix(iris_clusters)

# check if species in-line with groups
table(iris_clusters$Species,iris_clusters$groups)


ggplot(iris_clusters, aes(x=Sepal.Length, y=Petal.Length, color=groups, shape=groups)) + geom_point(shape=21) 
# Loop till here playing with different number_of_groups

BaseModel = lm(Petal.Length ~ Sepal.Length, data = iris_clusters)
summary(BaseModel)
constant=summary(BaseModel)$coefficients[1,1]
slope=summary(BaseModel)$coefficients[2,1]
rsquare=summary(BaseModel)$r.squared
# rse=sqrt(deviance(BaseModel)/df.residual(BaseModel))
rse=sigma(BaseModel)
cat("Constant:",constant,'\n',
    "Slope:",slope,'\n',
    "R-Squared:",rsquare,'\n',
    "Residual Error:",rse,'\n')

ggplot(iris_clusters, aes(x=Sepal.Length, y=Petal.Length, color=groups, shape=groups)) + 
  geom_point(shape=21) +
  geom_line(aes(y=constant+Sepal.Length*slope),linetype='dashed', size=1, colour='blue')
BaseVariance=sum((iris_clusters$Petal.Length-(constant+iris_clusters$Sepal.Length*slope))^2)/(nrow(iris_clusters)-1)
paste("Variance calculated from Dotted line:",BaseVariance)  



ImprovedModel = lmList(Petal.Length ~ Sepal.Length | groups, data = iris_clusters)
summary(ImprovedModel)
summary(BaseModel)
fix(iris_clusters)

constant1=coef(ImprovedModel)[1,1]
constant2=coef(ImprovedModel)[2,1]
slope1=coef(ImprovedModel)[1,2]
slope2=coef(ImprovedModel)[2,2]
ggplot(iris_clusters, aes(x=Sepal.Length, y=Petal.Length, color=groups, shape=groups)) + 
  geom_point(shape=21) +
  geom_line(aes(y=constant+Sepal.Length*slope),linetype='dashed', size=1, colour='blue') +
  geom_line(aes(y=constant1+Sepal.Length*slope1),linetype='dashed', size=1, colour='red') +
  geom_line(aes(y=constant2+Sepal.Length*slope2),linetype='dashed', size=1, colour='red')

group1=iris_clusters[iris_clusters$groups==1,]
group2=iris_clusters[iris_clusters$groups==2,]
group1variance=sum((group1$Petal.Length-(constant1+group1$Sepal.Length*slope1))^2)/(nrow(group1)-1)
group2variance=sum((group2$Petal.Length-(constant2+group2$Sepal.Length*slope2))^2)/(nrow(group2)-1)
paste("Variance calculated from Dotted line in group1:",group1variance)  
paste("Variance calculated from Dotted line in group2:",group2variance)  
ImprovedVariance=(group1variance*(nrow(group1)-1)+group2variance*(nrow(group2)-1))/((nrow(group1)-1)+(nrow(group2)-1))

paste("Improvement over base model:",100*(1-ImprovedVariance/BaseVariance))


