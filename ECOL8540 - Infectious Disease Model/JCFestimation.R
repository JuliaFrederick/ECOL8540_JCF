#Julia Frederick
#16MAY2019
#ECOL 8540 - Day 4 Part 2
#Dr. Drake and Dr. Rohani
#Estimation

setwd("~/CompWorkshop/Estimation")
load('data.RData')     #load the data and plot flu cases
plot(flu,type='b',log='y',main='Epidemic in a British boarding school', cex.main=0.85,
     xlab='Day', ylab='Active influenza cases')

####Exercise 1 - Plot the relationship between the total epi size and Ro for the complete
####range of values between 0 and 1
episize <- seq(0.001,0.999, 0.001)
Ro <- c()
for (ZN in episize){
  Ro <- c(Ro, log10(1-ZN)/-ZN)
  }
plot(episize,Ro)


###################################################
### code chunk number 2: estimation.rnw:101-105
###################################################
model<-lm(log(flu[1:4])~day[1:4],data=flu);  #fit a linear model
summary(model)         #summary statistics for fit model
slope<-coef(model)[2]  #extract slope parameter
slope                 #print to screen

####Exercise 2 - Supposing admission happened with 24hrs of the onset of symptoms
####How does this affect our estimate of Ro? 12hrs?
gamma=1
gamma12=1/0.5
Ro24 <- (slope/gamma)+1 ##Ro is 2.09
Ro12 <- (slope/gamma12)+1 ##Ro is 1.55
#As the time of admission decreased the R0 decreases with it

####Exercise 3 - dataframe niamey. Obtain estimates of Ro for measles from the first community
####assuming that the infectious period is approx 2 weeks or 14/365 = 0.0384
plot(niamey$V1,type='b',log='y',main='Epidemic in a Niamey', cex.main=0.85,
     xlab='Biweek', ylab='Active measles cases')

weeks <- seq(1,2*nrow(niamey),by=2)
niamey$weeks <- weeks

byweek <- seq(1,17,2)

for (t in byweek){
model<-lm(log(niamey$V1[1:t])~niamey$weeks[1:t],data=niamey);  #fit a linear model
print(summary(model))         #summary statistics for fit model
}
#going out to row 6 has the best r-squared (0.951)

model<-lm(log(niamey$V1[1:6])~niamey$weeks[1:6],data=niamey);  #fit a linear model
summary(model)
slopeV1<-coef(model)[2]  #extract slope parameter
slopeV1 
#slope = 0.213
RoV1 <- slopeV1/(1/2) + 1
#RoV1 = 1.426


####Exercise 4 - Plot the estimate of Ro obtained from n=3,4,5... data points against the
####standard error of the slope from the regression analysis to show this trade off
summary(model)$coefficients[2,2]

byweek <- seq(1,31,2)

alues <- seq(3,9,1)
sterr<-c()
rvals <- c()
for (x in alues){
  niamodel<-lm(log(V1[1:x])~weeks[1:x],data=niamey);  #fit a linear model
  SE <- coef(summary(niamodel))[2,2]
  slope<-coef(summary(niamodel))[1,2]
  RN <- slope/0.5 + 1
  sterr <- c(sterr,slope)         #summary statistics for fit model
  rvals <- c(rvals, RN)
  }
plot(rvals, sterr)



###################################################################################
####Exercise 5 - We have assumed the infectious period is 14 days. In terms of years
####gamma = (365/14)^-1 = 0.0384. Modify the code to estimate gamma and beta simultaneously

load('data.RData')
niamey[5,3]<-0  #replace a "NA"
niamey<-data.frame(biweek=rep(seq(1,16),3),site=c(rep(1,16),rep(2,16),rep(3,16)),
                   cases=c(niamey[,1],niamey[,2],niamey[,3])) #define "biweeks"


plot(niamey$biweek,niamey$cases,type='p',col=niamey$site,xlab='Biweek',ylab='Cases')
lines(niamey$biweek[niamey$site==1],niamey$cases[niamey$site==1])
lines(niamey$biweek[niamey$site==2],niamey$cases[niamey$site==2],col=2)
lines(niamey$biweek[niamey$site==3],niamey$cases[niamey$site==3],col=3)


closed.sir.model <- function (t, x, params) {  #SIR model equations
  S <- x[1]
  I <- x[2]
  beta <- params[1]
  gamma<-params[2]
  dS <- -beta*S*I
  dI <- beta*S*I-(gamma)*I
  list(c(dS,dI))
}


sse.sir <- function(params0,data,site){  #function to calculate squared errors
  data<-data[data$site==site,]    #working dataset, based on site
  t <- data[,1]*gamma            #time in biweeks
  cases <- data[,3]               #number of cases
  beta <- exp(params0[1])            #parameter beta
  S0 <- exp(params0[2])          #initial susceptibles
  I0 <- exp(params0[3])           #initial infected     
  gamma <- exp(params0[4])
  out <- as.data.frame(ode(c(S=S0,I=I0),times=t,closed.sir.model,params=c(beta=beta, gamma=gamma),hmax=1/120))
  sse<-sum((out$I-cases)^2)       #sum of squared errors
}


library(deSolve)   #differential equation library
params0<-c(-3.2,7.3,-2.6,log(13/365))  #initial guess

fit1 <- optim(params0,sse.sir,data=niamey,site=1) #fit
exp(fit1$par)  #back-transform parameters
fit2 <- optim(params0,sse.sir,data=niamey,site=2) #fit
exp(fit2$par)  #back-transform parameters
fit3 <- optim(params0,sse.sir,data=niamey,site=3) #fit
exp(fit3$par)  #back-transform parameters



par(mfrow=c(2,2))   #set up plotting area for multiple panels
plot(cases~biweek,data=subset(niamey,site==1),type='p',col='blue', pch=21) #plot site 1
t <- subset(niamey,site==1)[,1]*14/365
mod.pred<-as.data.frame(ode(c(S=exp(fit1$par[2]),I=exp(fit1$par[3])),times=t,
                            closed.sir.model,exp(fit1$par[1]),hmax=1/120))
#obtain model predictions
lines(mod.pred$I~subset(niamey,site==1)[,1]) #and plot as a line

plot(cases~biweek,data=subset(niamey,site==2),type='b',col=site) #site 2
t <- subset(niamey,site==2)[,1]*14/365
mod.pred<-as.data.frame(ode(c(S=exp(fit2$par[2]),I=exp(fit2$par[3])),times=t,
                            closed.sir.model,exp(fit2$par[1]),hmax=1/120))
lines(mod.pred$I~subset(niamey,site==2)[,1])


plot(cases~biweek,data=subset(niamey,site==3),type='b',col=site) #site 3
t <- subset(niamey,site==3)[,1]*14/365
mod.pred<-as.data.frame(ode(c(S=exp(fit3$par[2]),I=exp(fit3$par[3])),times=t,
                            closed.sir.model,exp(fit3$par[1]),hmax=1/120))
lines(mod.pred$I~subset(niamey,site==3)[,1])


