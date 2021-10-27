#### some housekeeping####
rm(list = ls())
dev.off()
set.seed(1)
#### exercise 1####
#Initial parameters
N<- 1000
betas<- c(5, -.5)
x1<- rep(1, N)
x2<- rnorm(N, mean=0, sd=(sqrt(1.5)))
x22<- rnorm(N, mean=0, sd=(sqrt(1.5)))
eps<- rnorm(N, mean = 0, sd=(sqrt(10)))
#a
y1<- betas[1]+ betas[2]*x2+eps
train_sam<- data.frame(y1, x1, x2)# putting data into data frame
#b
y2<- betas[1]+ betas[2]*x22+eps
test_sam<- data.frame(y2,x1,x22)# putting data into data frame
#c
model_fit<- lm(y1~ x2)# filling regression model
model_fit
#d: finding MSE

yhat_train<- model_fit$coefficients[1]+ model_fit$coefficients[2]*x2
e1<-y1-yhat_train
MSE_training<- sum(e1)^2/N

yhat_test<-  model_fit$coefficients[1]+ model_fit$coefficients[2]*x22
e2<- y2- yhat_test
MSE_training
APA_test<- sum(yhat_test-y2)^2/N
APA_test
#d 
x.2<- x2^2 
x.3<- x2^3
x.4<- x2^4
x.5<- x2^5
N_datatrain<- data.frame(y1, x1,x2,x.2, x.3,x.4,x.5)
N_train_model<- lm(y1~ x2+x.2+ x.3 +x.4+ x.5)
N_train_model
Nytrainhat<- N_train_model$coefficients[1]+N_train_model$coefficients[2]*x2 +N_train_model$coefficients[3]*x.2+ N_train_model$coefficients[4]*x.3 +N_train_model$coefficients[5]*x.4 +N_train_model$coefficients[6]*x.5
e_N_train<- sum(Nytrainhat-y1)
N_TRAIN_MSE<- sum(e_N_train^2)/N
N_TRAIN_MSE
N_datatest<- data.frame(y2, x1,x2,x.2, x.3,x.4,x.5)

#polynomial data for test variables
x22.2<- x22^2 
x22.3<- x22^3
x22.4<- x22^4
x22.5<- x22^5

Nytesthat<- N_train_model$coefficients[1] + N_train_model$coefficients[2]*x22 +N_train_model$coefficients[3]*x22.2+ N_train_model$coefficients[4]*x22.3 +N_train_model$coefficients[5]*x22.4 +N_train_model$coefficients[6]*x22.5
e_test_New<- y2- Nytesthat
N_TEST_APA<- sum(e_test_New^2)/N
N_TRAIN_MSE
N_TEST_APA
##### exercise 2####
rm(list = ls())
#a     looping the seed
times<- 1000
MSEvec<- rep(NA, times)
APAvec<- rep(NA, times)
MSEvec
for (i in 1:times){
  
  set.seed(100+i)
  N<- 1000
  betas<- c(5, -.5)
  x1<- rep(1, N)
  x2<- rnorm(N, mean=0, sd=(1.5^-1))
  x22<- rnorm(N, mean=0, sd=(1.5^-1))
  eps<- rnorm(N, mean = 0, sd=(10^-1))
  y1<- betas[1]+ betas[2]*x2+eps
  y2<- betas[1]+ betas[2]*x22+eps
  # polynomials
  x.2<- x2^2 
  x.3<- x2^3
  x.4<- x2^4
  
  x.5<- x2^5
  
  x22.2<- x22^2 
  x22.3<- x22^3
  x22.4<- x22^4
  x22.5<- x22^5
  datatrain<- data.frame(y1, x1,x2,x.2, x.3,x.4,x.5)
  model_train<- lm(y1~ x2+x.2+ x.3 +x.4+ x.5)
  model_train
  Nytrainhat<- model_train$coefficients[1]+model_train$coefficients[2]*x2 +model_train$coefficients[3]*x.2+ model_train$coefficients[4]*x.3 +model_train$coefficients[5]*x.4 +model_train$coefficients[6]*x.5
  e_train<- sum(Nytrainhat-y1)
  e_train
  MSEvec[i]<- sum(e_train^2)/N
  ##########
  Nytrainhat2<- model_train$coefficients[1]+model_train$coefficients[2]*x22 +model_train$coefficients[3]*x22.2+ model_train$coefficients[4]*x22.3 +model_train$coefficients[5]*x22.4 +model_train$coefficients[6]*x22.5
  e_test<- sum(Nytrainhat2-y2)
  e_test
  APAvec[i]<- sum(e_test^2)/N
  
  
  i=i+1
  
  }

APAvec
MSEvec
plot(1:N, MSEvec, col='blue', xlab = 'N', ylab = 'MSE and APA')
par(new=T)
plot(1:N, APAvec, col='green', xlab = '', ylab = '')
plot(density(MSEvec), main = 'Density of MSE')
