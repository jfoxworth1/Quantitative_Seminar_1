############# LMmainCBCF #############


#possibly set g to a constant equal to average
{if (FALSE){
  gpi <- growthMatrix * PiMatrix
  gMean <- mean(rowSums(gpi))
  g <- gMean * matrix(1, nrow = 5, ncol = 5)
}
  else{
    g <- growthMatrix
  }
}

#Possibly use variable seperation Rate
{if(FALSE){
  psis <- psiState
}
  else{
    psis <- rep(Psi, N)
  }
}

thetaVALUE <- thetaActual

###Starting values###
UStart <- 30 * rep(1, N)
CStart <- 15 * rep(1, N)
WEStart <- 16 * rep(1, N)
WKStart <- 17 * rep(1, N)


###Solving###
outputLMTH <- NULL
kappa <- 0.213
z <- 0.4

unks <- unks <- c(UStart, CStart, WEStart, WKStart)
source('C:/Users/jfoxworth/Desktop/Slides/High Discounts, High Unemployment/My data/LMfuncNoZCF.R')
unksNew <- fvals <- LMfuncNoZCF(unks)
Fit <- sd(fval)
outputNoZCF <- c(delta, gamma, Fit)

### Discount, r ###
rStart <- c(0.3, 0.2, 0.15, 0.05, 0)
source('C:/Users/jfoxworth/Desktop/Slides/High Discounts, High Unemployment/My data/IRR.R')
r <- IRR(rStart)

### Expected Productivity Growth ###
exg <- rep(0, N)
for(s in 1:N){
  exg[s] <- PiMatrix[s,] %*% t(growthMatrix)[,s]
}

#Not included from original paper
#resultsNoZ <-  c(U, C, m, WE, WK, W, X, thetaActual, 100*r, exg, X-W, c(beta, delta, gamma, z, 0), omega, r, X-W-kappa*sqrt(thetaVALUE)/mu)

###Use solution as a starting value for theta, delta and gamma###
unks <- c(unksNew[1:2*N], thetaActual, unksNew[2*N+1:4*N+2])
source('C:/Users/jfoxworth/Desktop/Slides/High Discounts, High Unemployment/My data/LMfuncZCF.R')
checkZ <- LMfuncZCF(unks)
unks <- fval <- checkz
Fit <- sd(fval)
outputZ <- c(delta, gamma, Fit)
#resultsCB <-  c(U, C, m, WE, WK, W, X, thetaActual, 100*r, exg, X-W, c(beta, delta, gamma, z, kappa), omega, r)
