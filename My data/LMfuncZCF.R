#############LMfuncZCF#############
###Calculates discrepencies in Credible Bargaining Model###
###SDF is driving force; solves for theta, delta, gamma###


LMfuncZCF <- function(unks){
  
  #uns = realunks
  
  uLoc <- 1
  UR <- unks[uLoc:(uLoc + N - 1)]
    uLoc <- uLoc + N
  C <- unks[uLoc:(uLoc + N - 1)]
    uLoc <- uLoc + N
  thetaVALUE <- unks[uLoc:(uLoc + N - 1)]
    uLoc <- uLoc + N
  WE <- unks[uLoc:(uLoc + N -1 )]
    uLoc <- uLoc + N
  WK <- unks[uLoc:(uLoc + N - 1)]
    uLoc <- uLoc + N
 
  ###5N + 2 Unknowns###
  
  ###Form omega###
  M <- matrix(0, nrow = N, ncol = N)
  for(s in 1:N){
    for (sprime in 1:N){
      M[s, sprime] <- m[sprime]/m[s]
    }
  }
  omega <- beta * PiMatrix * M * growthMatrix
  
  
  ###Calculate X and R###
  M <- diag(N)
  for(s in 1:N){
    for(sprime in 1:N){
      M[s, sprime] <- Mat[s, sprime] - (1-psis[s]) * omega[s, sprime]
    }
  }
  Inv <- solve(Mat)
  X <- Inv * rep(1, N)
  
  ###Form Discrepencies###
  dLoc <- 1
  discs <- rep(0, 4*N)
  W <- 0.5 * (WE + WK)
  for(s in 1:N){
    discs[dLoc] <- z - UR[s] #U Equation
    f <- mu * thetaVALUE[s]^eta #job finding rate
    
    for(sprime in 1:N){
      discs[dLoc] <- discs[dLoc] + omega[s, sprime] * (f*(W[sprime] + C[sprime]) + (1-f)*UR[sprime])  
    }
    dLoc <- dLoc + 1
    
    discs[dLoc] <- -C[s] #C Equation
    
    for (sprime in 1:N){
      discs[dLoc] <- discs[dLoc] + omega[s, sprime] * (psis[s] * UR[sprime] + (1-psis[s]) * C[sprime])
    }
    dLoc <- dLoc + 1
    
    Q[s] <- f/thetaVALUE[s] #vacancy filling rate
    discs[dLoc] <- -Q[s] * (X[s] - W[s]) + kappa #zero-profit condition (theta)
    dLoc <- dLoc + 1
    
    discs[dLoc] <- delta * UR[s] + (1-delta) * z - WE[s] - C[s] #WE equation
    for(sprime in 1:N){
      discs[dLoc] <- discs[dLoc] + (1-delta) * omega[s, sprime] * (WK[sprime] + C[sprime])
    }
    dLoc <- dLoc + 1
    
    discs[dLoc] <- -(1-delta) * gamma - X[s] + WK[s] #WK equation
    for(sprime in 1:N){
      discs[dLoc] <- discs[dLoc] + (1-delta) * omega[s, sprime] * (X[sprime] - WE[sprime])
    }
    dLoc <- dLoc + 1
  }
  
  discs[dLoc] <- thetaVALUE[3] -thetaActual[3] #Match the central tendency
  dLoc <- dLoc + 1
  discs[dLoc] <- thetaVALUE[5] - thetaVALUE[1]-(thetaActual[5]-thetaActual[1]) #match overall slope
  
  ###5N discrepencies###
  #discs <- real(discs)
  
  if(ifTrack){
    Track <- c(Track, thetaVALUE, X, WE, WK, discs)
  }
  return(discs)
}

