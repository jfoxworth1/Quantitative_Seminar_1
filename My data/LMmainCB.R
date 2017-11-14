############# LMmainCB #############

thetaVALUE <- thetaActual

###Starting values###
UStart <- 30 * rep(1, N)
CStart <- 15 * rep(1, N)
WEStart <- 16 * rep(1, N)
WKStart <- 17 * rep(1, N)

###Solving###
outputLMTH <- NULL
deltaStart <- 0
gammaStart <- 0.55

unks <- c(UStart, CStart, WEStart, WKStart, deltaStart, gammaStart)
source('C:/Users/jfoxworth/Desktop/Slides/High Discounts, High Unemployment/My data/LMfuncNoZ.R')
check <- fsolve(LMfuncNoZ, unks, maxiter = 5000, tol = 0.000000001)
unksNew <- check$x
fvals <- check$fval
Fit <- sd(fvals)
outputNoZ <- c(delta, gamma, Fit)

### Discount, r ###
rStart <- c(0.3, 0.2, 0.15, 0.05, 0)
source('C:/Users/jfoxworth/Desktop/Slides/High Discounts, High Unemployment/My data/IRR.R')
r <- fsolve(IRR, rStart, maxiter = 5000, tol = 0.000000001)$x

### Expected Productivity Growth ###
exg <- rep(0, N)
for(s in 1:N){
  exg[s] <- PiMatrix[s,] %*% t(growthMatrix)[,s]
}

#Not included from original paper
#resultsNoZ <-  c(U, C, m, WE, WK, W, X, thetaActual, 100*r, exg, X-W, c(beta, delta, gamma, z, 0), omega, r, X-W-kappa*sqrt(thetaVALUE)/mu)

###Use solution as a starting value for theta, delta and gamma###
unks <- c(unksNew[1:(2*N)], thetaActual, unksNew[(2*N+1):(4*N+2)])

source('C:/Users/jfoxworth/Desktop/Slides/High Discounts, High Unemployment/My data/LMfuncZ.R')

checkZ <- LMfuncZ(unks)
check <- fsolve(LMfuncZ, unks, maxiter = 5000, tol = 0.000000001)
fvals <- check$fval
unksNew <- check$x
Fit <- sd(fvals)
outputZ <- c(delta, gamma, Fit)
#resultsCB <-  c(U, C, m, WE, WK, W, X, thetaActual, 100*r, exg, X-W, c(beta, delta, gamma, z, kappa), omega, r)
UR <- unksNew[1:5]
C <- unksNew[6:10]
thetaVALUE <- unksNew[11:15]
WE <- unksNew[16:20]
WK <- unksNew[21:25]
delta <- unksNew[26]
gamma <- unksNew[27]

