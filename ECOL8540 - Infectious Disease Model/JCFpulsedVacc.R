#Julia Frederick
#16MAY2019
#ECOL 8540 - Day 4 Part 1
#Dr. Drake and Dr. Rohani
#Pulsed Vaccination

#setwd("~/CompWorkshop/pulsedVacc")
#library("deSolve", lib.loc="~/R/win-library/3.5")

###################################################
### code chunk number 1: pulsed-vaccination.rnw:76-93
###################################################
#require(deSolve)                         

sir.model.open <- function (t, x, params) {    
  S <- x[1]                               
  I <- x[2]                              
  R <- x[3]                               
  with(                                   
    as.list(params),                   
    {                                 
      dS <- mu*(S+I+R) - beta*S*I - mu*S
      dI <- beta*S*I - gamma*I - mu*I
      dR <- gamma*I - mu*R
      dx <- c(dS,dI,dR)                
      list(dx)                        
    }
  )
}
###################################################
### code chunk number 2: parameters
###################################################
R0 <- 10
N <-  1					                       
mu <- 0.02                          
gamma <- 365/10  			                 
beta <- R0*(gamma+mu)/N 	           
xstart <- c(S=0.2, I=0.001, R=1-0.2-0.001)	
Tmax <- 120                            #integrate for 200 years after transients
params <- c(beta=beta, gamma=gamma, mu=mu)        
tau <- 0.1                             #size of time step
times <- seq(0, Tmax, by=tau)          #function seq returns a sequence


###################################################
### code chunk number 3: pulsed-vaccination.rnw:113-114
###################################################
out <- ode(xstart,times,sir.model.open,params, method='ode45', rtol=1e-7)  
#method is ode45 because it calls a higher order solver because the system gets very close to the boundary I=0
#rtol = relative error tolerance is 1E-7

###################################################
### code chunk number 4: pulsed-vaccination.rnw:119-124
###################################################
op <- par(fig=c(0,0.5,0,1),mar=c(4,4,1,1))                     
plot(I~time,data=out, type='l', lwd=2)                         
par(fig=c(0.5,1,0,1),mar=c(4,1,1,1),new=T)                     
plot(I~S,data=out,log='xy',yaxt='n',xlab='S', type='l', lwd=2)  
par(op)                                                         
#around t=50 the system settles to endemic eq, used in the next chunk to initilize the pulsed vaccination

###################################################
### code chunk number 5: reset-parameters
###################################################
xstart <- out[which(out[,1]==50),2:4]


###################################################
### code chunk number 6: new-parameters
###################################################
pv <- 0.1                     # fraction of susceptibles vaccinated
Tv <- 4                       # number of years between pulses
vacc.events <- floor(Tmax/Tv) # number of pulses in Tmax years


###################################################
### code chunk number 7: dataframe
###################################################
data <- data.frame(S=out[which(out[,1]==50),2],
                   I=out[which(out[,1]==50),3],
                   R=out[which(out[,1]==50),4])
#creating data frame at initial condition

###################################################
### code chunk number 8: pulsed-vaccination.rnw:155-162
###################################################
#use terminal conditions of transient period to run the model forward, vaccinate, and repreat

for(i in 1:vacc.events){
  out <- ode(xstart, seq(tau, Tv, by=tau), sir.model.open, params, method='ode45', rtol=1e-7)
  xstart <- out[dim(out)[1],2:4]        # reset initial condition
  xstart[1] <- (1-pv)*(tail(out,1)[2])  # vaccinate susceptibles
  xstart[3] <- xstart[3]+(pv)*(tail(out,1)[2])  # move to recovered class
  data <- rbind(data,out[,2:4])         # store result
}


###################################################
### code chunk number 9: plot-vax
###################################################
data$time <- seq(50, Tmax+50, by=tau)
par(mar=c(5,4,4,4)+0.1)
plot(data$time[1:500], data$I[1:500], type='l', xlab='Time', ylab='', col='red', axes=FALSE)
axis(2, col.axis='red')
mtext(side=2, line=2.5, 'Infected', col='red')
box()
axis(1)
par(new=TRUE)
plot(data$time[1:500], data$S[1:500], type='l', xlab='', ylab='', axes=FALSE, col='black')
axis(4)
mtext(side=4, line=2.5, 'Susceptibles')

####Exercise 1 - Why might this periodicity be of interest?  
####What might be the consequences of "peaks" and"troughs" for public health?
#The periodicity is of interest because the top of the peaks is where the births replenish the susceptible 
#populations and this is where a new pulse could occur
#The peaks and throughs are important to public health to know because if you are following a plused 
#vaccination plan then you will want to vacciate at your peaks to be most effective, and track the troughs
#to know when to be prepared for an upcoming pulse event.

####Exercise 2 - By modifying the value ofpv, see if you can locate the vaccination threshold.  
####Does this agree with the analytically predicted threshold?
#Analytically predicted threshold 1/R0 = 0.1
#having a pv value of at least 0.54 makes the value equation above less than 1/Ro

###################################################
### code chunk number 1: pulsed-vaccination.rnw:76-93
###################################################
#require(deSolve)                         

ex2 <- function(pv) {
  sir.model.open <- function (t, x, params) {    
    S <- x[1]                               
    I <- x[2]                              
    R <- x[3]                               
    with(                                   
      as.list(params),                   
      {                                 
        dS <- mu*(S+I+R) - beta*S*I - mu*S
        dI <- beta*S*I - gamma*I - mu*I
        dR <- gamma*I - mu*R
        dx <- c(dS,dI,dR)                
        list(dx)                        
      }
    )
  }
  ###################################################
  ### code chunk number 2: parameters
  ###################################################
  R0 <- 10
  N <-  1					                       
  mu <- 0.02                          
  gamma <- 365/10  			                 
  beta <- R0*(gamma+mu)/N 	           
  xstart <- c(S=0.2, I=0.001, R=1-0.2-0.001)	
  Tmax <- 120                            #integrate for 200 years after transients
  params <- c(beta=beta, gamma=gamma, mu=mu)        
  tau <- 0.1                             #size of time step
  times <- seq(0, Tmax, by=tau)          #function seq returns a sequence
  
  
  ###################################################
  ### code chunk number 3: pulsed-vaccination.rnw:113-114
  ###################################################
  out <- ode(xstart,times,sir.model.open,params, method='ode45', rtol=1e-7)  
  #method is ode45 because it calls a higher order solver because the system gets very close to the boundary I=0
  #rtol = relative error tolerance is 1E-7
  
  ###################################################
  ### code chunk number 4: pulsed-vaccination.rnw:119-124
  ###################################################
  #op <- par(fig=c(0,0.5,0,1),mar=c(4,4,1,1))                     
  #lot(I~time,data=out, type='l', lwd=2)                         
  #par(fig=c(0.5,1,0,1),mar=c(4,1,1,1),new=T)                     
  #plot(I~S,data=out,log='xy',yaxt='n',xlab='S', type='l', lwd=2)  
  #par(op)                                                         
  #around t=50 the system settles to endemic eq, used in the next chunk to initilize the pulsed vaccination
  
  ###################################################
  ### code chunk number 5: reset-parameters
  ###################################################
  xstart <- out[which(out[,1]==50),2:4]
  
  
  ###################################################
  ### code chunk number 6: new-parameters
  ###################################################
  #pv <- 0.1                     # fraction of susceptibles vaccinated
  Tv <- 4                       # number of years between pulses
  vacc.events <- floor(Tmax/Tv) # number of pulses in Tmax years
  
  
  ###################################################
  ### code chunk number 7: dataframe
  ###################################################
  data <- data.frame(S=out[which(out[,1]==50),2],
                     I=out[which(out[,1]==50),3],
                     R=out[which(out[,1]==50),4])
  #creating data frame at initial condition
  
  ###################################################
  ### code chunk number 8: pulsed-vaccination.rnw:155-162
  ###################################################
  #use terminal conditions of transient period to run the model forward, vaccinate, and repreat
  
  for(i in 1:vacc.events){
    out <- ode(xstart, seq(tau, Tv, by=tau), sir.model.open, params, method='ode45', rtol=1e-7)
    xstart <- out[dim(out)[1],2:4]        # reset initial condition
    xstart[1] <- (1-pv)*(tail(out,1)[2])  # vaccinate susceptibles
    xstart[3] <- xstart[3]+(pv)*(tail(out,1)[2])  # move to recovered class
    data <- rbind(data,out[,2:4])         # store result
  }
  
  
  ###################################################
  ### code chunk number 9: plot-vax
  ###################################################
  data$time <- seq(50, Tmax+50, by=tau)
  par(mar=c(5,4,4,4)+0.1)
  plot(data$time[1:500], data$I[1:500], type='l', xlab='Time', ylab='', col='red', axes=FALSE)
  axis(2, col.axis='red')
  mtext(side=2, line=2.5, 'Infected', col='red')
  box()
  axis(1)
  par(new=TRUE)
  plot(data$time[1:500], data$S[1:500], type='l', xlab='', ylab='', axes=FALSE, col='black')
  axis(4)
  mtext(side=4, line=2.5, 'Susceptibles') }

for (i in seq(0.1,0.95,0.05)){ 
  ex2(i) }


####Exercise 3 - One thing we might be interested in knowing is how our vaccination strategy affects mean
####disease prevalance. Devise a strategy to study this question and produce a plot illustrating the result.
#Want to vary our pv values, and get the mean of the I over time, and then plot those values.
sir.model.open <- function (t, x, params) {    
  S <- x[1]                               
  I <- x[2]                              
  R <- x[3]                               
  with(                                   
    as.list(params),                   
    {                                 
      dS <- mu*(S+I+R) - beta*S*I - mu*S
      dI <- beta*S*I - gamma*I - mu*I
      dR <- gamma*I - mu*R
      dx <- c(dS,dI,dR)                
      list(dx)                        
    }
  )
}

R0 <- 10
N <-  1					                       
mu <- 0.02                          
gamma <- 365/10  			                 
beta <- R0*(gamma+mu)/N 	           
xstart <- c(S=0.2, I=0.001, R=1-0.2-0.001)	
Tmax <- 120                            #integrate for 200 years after transients
params <- c(beta=beta, gamma=gamma, mu=mu)        
tau <- 0.01                             #size of time step
times <- seq(0, Tmax, by=tau)          #function seq returns a sequence

out <- ode(xstart,times,sir.model.open,params, method='ode45', rtol=1e-7)  
#method is ode45 because it calls a higher order solver because the system gets very close to the boundary I=0
#rtol = relative error tolerance is 1E-7

#use terminal conditions of transient period to run the model forward, vaccinate, and repreat
mdpL <- data.frame(pv=numeric(0), I=numeric(0))


for (pv in seq(0,0.6,by=0.01)){
  xstart <- out[which(out[,1]==50),2:4]
  Tv <- 4
  vacc.events <- floor(Tmax/Tv) 
  
  data <- data.frame(S=out[which(out[,1]==50),2],
                     I=out[which(out[,1]==50),3],
                     R=out[which(out[,1]==50),4])
  
  for(i in 1:vacc.events){
    out2 <- ode(xstart, seq(tau, Tv, by=tau), sir.model.open, params, method='ode45', rtol=1e-7)
    xstart <- out2[dim(out2)[1],2:4]        # reset initial condition
    xstart[1] <- (1-pv)*(tail(out2,1)[2])  # vaccinate susceptibles
    xstart[3] <- xstart[3]+(pv)*(tail(out2,1)[2])  # move to recovered class
    data <- rbind(data,out2[,2:4])         # store result
    
  }
  mdpL <- rbind(mdpL, data.frame(pv=pv, I=mean(tail(data$I,1000))))
  
}
plot(mdpL$pv, mdpL$I, type='l', xlab='p_v', ylab='') 

####Exercise 4a - The crossing of the red and green lines
  #This cross point is where the Ro meets the threshold value needed for the infection
  #to persist in the population
####Exercise 4b - Does the blue line confirm this?
  #The mean prevalence of the infection meets Ro and the threshold value at this point
  #meaning that the prevalence isn't high enough for it to presist

####Exercise 5 - 
  #Mean Prevalence Line from Exercise 3
sir.model.open <- function (t, x, params) {    
  S <- x[1]                               
  I <- x[2]                              
  R <- x[3]                               
  with(                                   
    as.list(params),                   
    {                                 
      dS <- mu*(S+I+R) - beta*S*I - mu*S
      dI <- beta*S*I - gamma*I - mu*I
      dR <- gamma*I - mu*R
      dx <- c(dS,dI,dR)                
      list(dx)                        
    }
  )
}

R0 <- 10
N <-  1					                       
mu <- 0.02                          
gamma <- 365/10  			                 
beta <- R0*(gamma+mu)/N 	           
xstart <- c(S=0.2, I=0.001, R=1-0.2-0.001)	
Tmax <- 120                            #integrate for 200 years after transients
params <- c(beta=beta, gamma=gamma, mu=mu)        
tau <- 0.01                             #size of time step
times <- seq(0, Tmax, by=tau)          #function seq returns a sequence

out <- ode(xstart,times,sir.model.open,params, method='ode45', rtol=1e-7)  
#method is ode45 because it calls a higher order solver because the system gets very close to the boundary I=0
#rtol = relative error tolerance is 1E-7

#use terminal conditions of transient period to run the model forward, vaccinate, and repreat
mdpL <- data.frame(pv=numeric(0), I=numeric(0))


for (pv in seq(0,0.6,by=0.01)){
  xstart <- out[which(out[,1]==50),2:4]
  Tv <- 4
  vacc.events <- floor(Tmax/Tv) 
  
  data <- data.frame(S=out[which(out[,1]==50),2],
                     I=out[which(out[,1]==50),3],
                     R=out[which(out[,1]==50),4])
  
  for(i in 1:vacc.events){
    out2 <- ode(xstart, seq(tau, Tv, by=tau), sir.model.open, params, method='ode45', rtol=1e-7)
    xstart <- out2[dim(out2)[1],2:4]        # reset initial condition
    xstart[1] <- (1-pv)*(tail(out2,1)[2])  # vaccinate susceptibles
    xstart[3] <- xstart[3]+(pv)*(tail(out2,1)[2])  # move to recovered class
    data <- rbind(data,out2[,2:4])         # store result
    
  }
  mdpL <- rbind(mdpL, data.frame(pv=pv, I=mean(tail(data$I,1000))))
  
}
plot(mdpL$pv, mdpL$I, type='l', xlab='p_v', ylab='')
abline(h=10)
library(stats)

vax <- uniroot(crit, lower=0, upper=0.6, mu=mu, Tv=Tv, R0=R0)
abline(v=vax$root, lty=2)



