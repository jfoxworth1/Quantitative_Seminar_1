############# LMfuncTH #############

### Finds sticky wage given theta and SDF ###

LMfuncTH <- function(unks){

  uLoc <- 1
  UR <- unks[uLoc:(uLoc + N - 1)]
  uLoc <- uLoc + N
  C <- unks[uLoc:(uLoc + N - 1)]
  uLoc <- uLoc + N
  W <- unks[uLoc:(uLoc + N -1 )]
  
  ### Omega ###
  source("C:/Users/jfoxworth/Desktop/Slides/High Discounts, High Unemployment/My data/omegaM.r")
  omega <- omegaM(m)
  
  ### X ###
  Mat <- diag(N) - (1 - Psi)* omega
  X <- solve(Mat) %*% rep(1, N)
  
  ### Form Discrepencies ###
  dLoc <- 1
  discs <- rep(1, 3*N)
  for(s in 1:N){
    discs[dLoc] <- z - UR[s] #U Equation
    f <- mu * thetaVALUE[s]^eta #job finding rate
    
    for(sprime in 1:N){
      discs[dLoc] <- discs[dLoc] + omega[s, sprime] * (f*(W[sprime] + C[sprime]) + (1-f)*UR[sprime])  
    }
    dLoc <- dLoc + 1
    discs[dLoc] <- -C[s] #C Equation
    
    for (sprime in 1:N){
      discs[dLoc] <- discs[dLoc] + omega[s, sprime] * (Psi * UR[sprime] + (1-Psi) * C[sprime])
    }
    
    dLoc <- dLoc + 1
    
    Q[s] <- f/thetaVALUE[s] #Vacancy filling rate
    discs[dLoc] <- -Q[s] * (X[s] - W[s]) + kappa #zero profit condition
    dLoc <- dLoc + 1
  }
   
  ###Should be 3N Discrepencies###
  
  if(ifTrack){
    Track <- c(Track, thetaVALUE, X, WE, WK, discs)
  }
  return(discs)
}