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
sigma1<- matrix(c(16,-2,-2,9),2,2, byrow = T)
n1<- 300
n2<- 500
N<- n1+n2
#### 1a, Data generating fucntion####
data.sim<- function(n1,n2,mu1,mu2,sigma1, sigma2){
  Class<- c(rep(1, n1), rep(2, n2))
  X1<- rmvnorm(n1, mean = mu1, sigma = sigma1)
  X2<- rmvnorm(n2, mean = mu2, sigma = sigma2)
  X<- rbind(X1,X2)
  Output.dataset<- data.frame(Class, X)
  return(Output.dataset)
}
thedataset<- data.sim(n1,n2,mu1,mu2,sigma1 , sigma1)
#####1b, lda, qda results generating function####
LDA.QDA.results<- function(y, x, data){
  LDA.results<- lda(y~x, data = data)
  QDA.results<- qda(y~x, data = data)
  
  LDA.pred<- predict(LDA.results, data=thedataset)
  QDA.pred<- predict(QDA.results, data= thedataset)
  
  LDA.MSE<- mean(LDA.pred$class!=y)
  QDA.MSE<- mean(QDA.pred$class!= y)
  return(c(LDA.MSE, QDA.MSE))
}

LDA.QDA.results(thedataset$Class, (thedataset$X1+thedataset$X2), data = thedataset)
#### 2a, 100 simulations####
set.seed(456)
num.sim<- 100
MSE.simulation.func<- function(num.sim,n1, n2, mu1, m2, sigma1, sigma2) {
  LDA.100MSE<- c()
  QDA.100MSE<- c()
  for (i in 1:num.sim) {
    data1<- data.sim(n1, n2, mu1, mu2, sigma1, sigma1)
    ldaqdamses<- LDA.QDA.results(data1$Class, (data1$X1+data1$X2), data=data1)
    LDA.100MSE<- c(LDA.100MSE, ldaqdamses[1])
    QDA.100MSE<- c(QDA.100MSE, ldaqdamses[2])
  }
  return(cbind(LDA.100MSE, QDA.100MSE))
}
set.seed(100)
colMeans(MSE.simulation.func(num.sim, n1, n2, mu1, mu2, sigma1, sigma1))
#### 2b, manipulation of initial simulation for increasing difference between 
        # LDA and QDA MSEs
# setting sigma.difference 1 and 2
#sigma.diff<- matrix(c(1,0,0,3), 2,2, byrow = T)
sigma.diff<- matrix(c(24,2,2,-20), 2,2, byrow = T)
# now we will assume that sigma1!= sigma2 and we expect that QDA will outperform LDA
# sigma2= sigma1+ alpha*sigma.difference
sigma2<- sigma1
#we will use alpha 30
alpha=30
LDA.QDA.res.matrix<- matrix(NA, 30,2)
for (s in 1:alpha) {
  sigma2<- sigma1+ s*sigma.diff
  data2<- data.sim(n1,n2,mu1,mu2, sigma1, sigma2)
  ress<- LDA.QDA.results(data2$Class, (data2$X1+data2$X2), data = data2)
  LDA.QDA.res.matrix[s,1]<- ress[1]
  LDA.QDA.res.matrix[s,2]<- ress[2]
}
MSE.diff<- LDA.QDA.res.matrix[,1]-LDA.QDA.res.matrix[,2]
plot(1:length(LDA.QDA.res.matrix[,1]),LDA.QDA.res.matrix[,1] , type = 'l', col='red', xlab = 'Alpha', ylab = 'Mean squared error')
lines(1:length(LDA.QDA.res.matrix[,2]), LDA.QDA.res.matrix[,2], type = 'l', col='blue')
lines(1:length(MSE.diff), MSE.diff, col='Black', type = 'l')

