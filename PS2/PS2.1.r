rm(list=ls())
#install.packages('MASS')
set.seed(123)
library(mvtnorm)
library(ggplot2)
library(MASS)

#Exercise 1a
# initial parameters and data generation

mu1 <- c(-3,3)
mu2<- c(5,5)
cov.ma<- matrix(c(16,-2,-2,9),2,2, byrow = T)
n1<- 300
n2<- 500
N<- n1+n2

# function to generate data
sample.func<- function(mean1, mean2, sigma1, sigma2, n1,n2){
  Class<- c(rep(1,n1), rep(2,n2))
  x1<- rmvnorm(mean = mu1, n= n1, sigma = cov.ma)
  x2<- rmvnorm(n=n2, mean = mu2, sigma = cov.ma)
  X<- rbind(x1,x2)
  df<- data.frame(Class, X)
  
  return(df)
}
#### generated data set and plot####
datasetx<- sample.func(mu1,mu2,sigma, sigma, n1,n2)
# plot of initial data set
#palette(c(rgb(0,0,1,0.5),rgb(1,0,0,0.5)))
colorss<- c('1'= 'Red', '2'= 'Blue')
ggplot(data= datasetx) + 
  geom_point(aes(x= datasetx$X1, y= datasetx$X2, color= factor(Class))) +
  #scale_color_manual(values=colorss) +
  labs(x= 'X1', y='X2', col= 'Colors')


datasetx$Class
# exercise 1b
# fitting lda and getting corrsponding outputs and predictions
fit.lda<- lda(Class~X1+X2, data =datasetx )
fit.lda
ldaresults<- predict(fit.lda, data= datasetx) 
ldaresults
# ploting the fitted lda data
ggplot(data= datasetx) + 
  geom_point(aes(x= datasetx$X1, y= datasetx$X2, color= factor(ldaresults$class))) 
  #scale_color_manual(values=colorss) +
  #labs(x= 'X1', y='X2', col= 'Colors')


# what actually predict function does?
ldaresults
#### fitting qda and getting corrsponding outputs and predictions####
qda.fit<- qda(Class~ X1+X2, data = datasetx)
qda.fit
qdaresults<- predict(qda.fit, data= datasetx)
# ploting qda
ggplot(data= datasetx) + 
geom_point(aes(x= datasetx$X1, y= datasetx$X2, color= factor(qdaresults$class))) 
#scale_color_manual(values=colorss) +
#labs(x= 'X1', y='X2', col= 'Colors')
#### results for Mean training error ####
true.class<- datasetx$Class
ldapred.class<- ldaresults$class
qdapred.class<- qdaresults$class
total.results<- data.frame(true.class, ldapred.class, qdapred.class, datasetx$X1, datasetx$X2)
colnames(total.results)<- c('True.class', 'LDA.Pred', 'QDA.Pred', 'X1', 'X2')
#function for training MSE
faslyclasified.in.percent<- function(object, prediction){
  train.MSE<- mean(object[,prediction]!= object[,'True.class'])
  return(train.MSE)
}
mean(total.results[,'LDA.Pred']!= total.results[,'True.class'])

faslyclasified.in.percent(total.results, 'LDA.Pred')

faslyclasified.in.percent(total.results, 'QDA.Pred')

#### Exercise 2a####
num.sim<- 100
lda.res100<- c()
qda.res100<- c()
for (i in 1:num.sim){
  samape<- sample.func(mean1, mean2, sigm1, sigma2, n1, n2)
  # for LDA
  ldafit<- lda(Class~X1+X2, data = samape)
  ldapre<- predict(ldafit, samape)
  ldamse<- mean(ldapre$class!=samape$Class)
  lda.res100<- c(lda.res100, ldamse)
  #For QDA
  qdafit<- qda(Class~X1+X2, data = samape)
  qdapre<- predict(qdafit, samape)
  qdamse<- mean(qdapre$class!=samape$Class)
  qda.res100<- c(qda.res100, qdamse)

  }
mean(lda.res100)
mean(qda.res100)
#### Excercise 2b####
#we can change covariance matrix so that QDA will perform better than lda
covar.mat2<- matrix()

