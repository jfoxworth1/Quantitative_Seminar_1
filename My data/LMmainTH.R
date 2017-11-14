############# LMmainTH #############
#  Calculates variables given theta, beta, m, and g
#  Uses beta and m already in memory, from SM.R

UStart <- 30 * rep(1, N)
CStart <- 15 * rep(1, N)
WStart <- 16 * rep(1, N)


###Solves for U, C, and W
outputLMTH <- NULL
thetaVALUE <- thetaActual
unks <- c(UStart, CStart, WStart)
source('C:/Users/jfoxworth/Desktop/Slides/High Discounts, High Unemployment/My data/LMfuncTH.R')
check <- fsolve(LMfuncTH, unks, maxiter = 5000, tol = 0.000000001)
fvals <- check$fval
unksNew <- check$x
U <- unksNew[1:5]
C <- unksNew[6:10]
W <- unksNew[11:15]

Fit <- sd(fvals)
outputLMTH <- c(outputLMTH, c(wBar, gamma, z, kappa, thetaActual[3], thetaActual[5] - thetaActual[1], Fit))

### Discount, r ###
rStart <- c(0.3, 0.2, 0.15, 0.05, 0)
source('C:/Users/jfoxworth/Desktop/Slides/High Discounts, High Unemployment/My data/IRR.R')
r <- fsolve(IRR, rStart, maxiter = 5000, tol = 0.000000001)$x

###Expected Productivity growth###
exg <- rep(0, N)
for(s in 1:N){
  exg[s] <- PiMatrix[s,] %*% t(growthMatrix)[,s]
}

#Not included from original paper
#resultsTH <- c(X, U, C, m, X, Q, thetaActual, 100*r, exg, X-W, c(beta, rep(0, N-1)), omega, r)