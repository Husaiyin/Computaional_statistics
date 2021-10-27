#PS3
set.seed(1)
#Excersice 1----
rm(list = ls())# some house keeping
# names vector for the data, bcz names are not assigned in the data
names<- c('mpg', 'cylinders', 'displacement', 'horsepower', 'weight', 'acceleration', 'modelyear', 'origin', 'carname')

Auto_mpg<- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data', col.names = names)
summary(Auto_mpg)
#View(Auto_mpg) 
# Ex1. a ----
mpg1<- rep(0, length(Auto_mpg$mpg))
mpg1[Auto_mpg$mpg>=median(Auto_mpg$mpg)]<- 1# NOTE: I used mpg=1 if value is >= median
mean(mpg1)
Auto_mpg<- cbind(Auto_mpg, mpg1)
#ex1 b----
#since some of the values of horsepower are missing we will exclude them from our analysis

Auto_mpg$horsepower<- as.numeric(Auto_mpg$horsepower)# missing values('?') in horsepower will be converted to NA
horsepow<- complete.cases(Auto_mpg$horsepower)# generating binary vector to find which values are NA
Auto_mpg<- Auto_mpg[horsepow,]# #removing the rows in which the horsepwer data was missing, NA values
cor(Auto_mpg[,1:8]) # finding corrlation among variables except carname.
# weight is a good predeictor for mpg
# ex1,  c----
# one way of slpiting data
Auto_mpg$Group <- sample(factor(rep(1:2, length.out=nrow(Auto_mpg)), 
                                labels=paste0("Group", 1:2)))
#View(datatrain)
datatrain<- Auto_mpg[which(Auto_mpg$Group=='Group1'),]
datatest<- Auto_mpg[which(Auto_mpg$Group=='Group2'),]

## another way of splitting the data


nrow(datatrain)
nrow(datatest)

head(datatrain)
head(datatest)

# ex1, d----
log.fit<- glm(mpg1~weight, family = 'binomial', data= datatrain)
log.fit$coefficients
#prob1.mpg1<- exp(predict(log.fit, datatrain))
prob1.mpg1<- exp(log.fit$coefficients[1]+ log.fit$coefficients[2]*datatrain$weight)
yprob<- prob1.mpg1/(1+prob1.mpg1)
ytrain.pred<- rep(0, nrow(datatrain))
ytrain.pred[yprob>= .5]<- 1 

mean(datatrain$mpg1!=ytrain.pred)
probtest1<- exp(log.fit$coefficients[1]+log.fit$coefficients[2]*datatest$weight)
testprob<- probtest1/(1+probtest1)
testpred<- rep(0, nrow(datatest))   
testpred[testprob>=.5]<- 1  

mean(testpred!=datatest$mpg1)
P1<- c(1,1.3,1.7,2.0,2.3,2.7,3.0,3.3,3.7,4,5)
P1y<- c(9,20,31,40,48,54,60,65,68,71,100)
P2y<- c(12,27,40,46,63,72,81,90,90,92,100)
plot(P1,P1y,type = 'l')
lines(P1,P2y,type = 'l', col='red')
L1y<- c(7,11,18,29,37,48,51,63,66,79,100)
L2y<- c(13,20,20,20,26,46,60,73,80,86,100)
plot(P1,L1y,type = 'l')
lines(P1,L2y,type = 'l', col='red')
