#Julia Frederick
#15MAY2019
#ECOL 8540 - Day 3 Part 2
#Modeling Infectious Diseases

### R code from vignette source 'deterministic-models.rnw'
### Encoding: UTF-8
install.packages("deSolve")
require(deSolve)
library(deSolve)

sir.model.closed <- function(t,x,params){
  S <- x[1]
  I <- x[2]
  R <- x[3]
  with(
    as.list(params), 
    {
      dS <- -beta*S*I
      dI <- beta*S*I-gamma*I
      dR <- gamma*I
      dx <- c(dS,dI,dR)
      list(dx)})}

times <- seq(0,120,by=5)
params <- c(beta=0.3, gamma=1/7)
xstart <- c(S=9999/10000,I=1/10000,R=0)


out <- as.data.frame(ode(xstart, times, sir.model.closed, params))

op <- par(fig=c(0,0.5,0,1),mar=c(4,4,1,1))
plot(I~time, data=out, type='b')
par(fig=c(0.5,1,0,1),mar=c(4,1,1,1), new=T)
plot(I~S, data=out, type='b',yaxt='n', xlab='S')
par(op)

####Exercise 1 - Dynamics of the system for diff beta and gamma
#with increasing beta the infection over time occurs quicker and dies out faster
params <- c(beta=0.8, gamma=1/7)
out <- as.data.frame(ode(xstart, times, sir.model.closed, params))
op <- par(fig=c(0,0.5,0,1),mar=c(4,4,1,1))
plot(I~time, data=out, type='b')
par(fig=c(0.5,1,0,1),mar=c(4,1,1,1), new=T)
plot(I~S, data=out, type='b',yaxt='n', xlab='S')
par(op)
#with decreasing beta the infection over time is slower to establish, if it does
params <- c(beta=0.1, gamma=1/7)
out <- as.data.frame(ode(xstart, times, sir.model.closed, params))
op <- par(fig=c(0,0.5,0,1),mar=c(4,4,1,1))
plot(I~time, data=out, type='b')
par(fig=c(0.5,1,0,1),mar=c(4,1,1,1), new=T)
plot(I~S, data=out, type='b',yaxt='n', xlab='S')
par(op)
#with decreasing gamma the infection has a slower recovery phase
params <- c(beta=0.3, gamma=1/100)
out <- as.data.frame(ode(xstart, times, sir.model.closed, params))
op <- par(fig=c(0,0.5,0,1),mar=c(4,4,1,1))
plot(I~time, data=out, type='b')
par(fig=c(0.5,1,0,1),mar=c(4,1,1,1), new=T)
plot(I~S, data=out, type='b',yaxt='n', xlab='S')
par(op)
#with increasing gamma the infection has a quicker recovery phase
params <- c(beta=0.3, gamma=1/1)
out <- as.data.frame(ode(xstart, times, sir.model.closed, params))
op <- par(fig=c(0,0.5,0,1),mar=c(4,4,1,1))
plot(I~time, data=out, type='b')
par(fig=c(0.5,1,0,1),mar=c(4,1,1,1), new=T)
plot(I~S, data=out, type='b',yaxt='n', xlab='S')
par(op)

####Exercise 2- dynamics of the system for different initial conditions? Pre-existing immunity?
#Half of the population is immune creates a slow in the system for infection
times <- seq(0,120,by=5)
params <- c(beta=0.3, gamma=1/7)
xstart <- c(S=5000/10000,I=1/10000,R=4999/10000)

out <- as.data.frame(ode(xstart, times, sir.model.closed, params))

op <- par(fig=c(0,0.5,0,1),mar=c(4,4,1,1))
plot(I~time, data=out, type='b')
par(fig=c(0.5,1,0,1),mar=c(4,1,1,1), new=T)
plot(I~S, data=out, type='b',yaxt='n', xlab='S')
par(op)

#If a quarter of the population is infected, another quarter is immune than the infection doesn't
#invade the population
xstart <- c(S=5000/10000,I=2500/10000,R=2500/10000)

out <- as.data.frame(ode(xstart, times, sir.model.closed, params))

op <- par(fig=c(0,0.5,0,1),mar=c(4,4,1,1))
plot(I~time, data=out, type='b')
par(fig=c(0.5,1,0,1),mar=c(4,1,1,1), new=T)
plot(I~S, data=out, type='b',yaxt='n', xlab='S')
par(op)

####Exercise 3 - Modify the codes given to study the dynamics of a demographically open SIR model
#In an open model the infection doesn't die out but goes to epidemic equilibrium 
sir.model.open <- function(t,x,params){
  S <- x[1]
  I <- x[2]
  R <- x[3]
  with(
    as.list(params), 
    {
      dS <- b-beta*S*I-u*S
      dI <- beta*S*I-gamma*I-u*I
      dR <- gamma*I-u*R
      dx <- c(dS,dI,dR)
      list(dx)})}

times <- seq(0,120,by=5)
params <- c(beta=0.3, gamma=1/7, b=1/(75*365), u=1/(75*365))
xstart <- c(S=9999/10000,I=1/10000,R=0)


out <- as.data.frame(ode(xstart, times, sir.model.open, params))

op <- par(fig=c(0,0.5,0,1),mar=c(4,4,1,1))
plot(I~time, data=out, type='b')
par(fig=c(0.5,1,0,1),mar=c(4,1,1,1), new=T)
plot(I~S, data=out, type='b',yaxt='n', xlab='S')
par(op)

####Exercise 4 - Modify the code for SEIR model
sir.model.seir <- function(t,x,params){
  S <- x[1]
  E <- x[2]
  I <- x[3]
  R <- x[4]
  with(
    as.list(params), 
    {
      dS <- -beta*S*I
      dE <- beta*S*I-rho*E
      dI <- rho*E-gamma*I
      dR <- gamma*I
      dx <- c(dS,dE,dI,dR)
      list(dx)})}

times <- seq(0,120,by=5)
params <- c(beta=0.3, gamma=1/7, rho=1/2)
xstart <- c(S=9899/10000,E=100/10000,I=1/10000,R=0)


out <- as.data.frame(ode(xstart, times, sir.model.seir, params))

op <- par(fig=c(0,0.5,0,1),mar=c(4,4,1,1))
plot(I~time, data=out, type='b')
par(fig=c(0.5,1,0,1),mar=c(4,1,1,1), new=T)
plot(I~S, data=out, type='b',yaxt='n', xlab='S')
par(op)

#Open SEIR Model
sir.model.seir.open <- function(t,x,params){
  S <- x[1]
  E <- x[2]
  I <- x[3]
  R <- x[4]
  with(
    as.list(params), 
    {
      dS <- b-beta*S*I-u*S
      dE <- beta*S*I-rho*E-u*E
      dI <- rho*E-gamma*I-u*I
      dR <- gamma*I-u*R
      dx <- c(dS,dE,dI,dR)
      list(dx)})}

times <- seq(0,120,by=5)
params <- c(beta=0.3, gamma=1/7, rho=1/2,b=1/(75*365),u=1/(75*365))
xstart <- c(S=9899/10000,E=100/10000,I=1/10000,R=0)


out <- as.data.frame(ode(xstart, times, sir.model.seir.open, params))

op <- par(fig=c(0,0.5,0,1),mar=c(4,4,1,1))
plot(I~time, data=out, type='b')
par(fig=c(0.5,1,0,1),mar=c(4,1,1,1), new=T)
plot(I~S, data=out, type='b',yaxt='n', xlab='S')
par(op)

############################################################################################
#########################################Notes##############################################
############################################################################################

#Boundary value problems
  ##want to start with a condition on the state variables(the initial conditions) and inquire
  #about the future values of the state variables as the system evolves over its trajectory
    ##What have changed from initial to "end" point
  ##Phase portrait - time is complicit, and the variable moves in order?
#Numerical integration
  ##typically not analytically tractable (can't use pencil and paper to calculate this)
  ##approximation to obtain an approximate solution by solving a sequence of (tractable) linear
  #approximations at smalled and smaller step sizes until a specified tolerance is achieved
#Numerical solution of ODEs in R - Lawrence Livermore National Laboratory
  #ordinary differential equations (including delay differential eqs) - #deSolve# package
    #Function #ode# automatically selects the optimal solving algorithm based on numerical performance

  