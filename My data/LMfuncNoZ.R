############# LMfunNoZ #############
###Calculates discrepencies in original model###
###SDF is driving force; solves for theta###

LMfuncNoZ <- function(unks){
#uns = realunks

  uLoc <- 1
  UR <- unks[uLoc:(uLoc + N - 1)]
    uLoc <- uLoc + N
  C <- unks[uLoc:(uLoc + N - 1)]
    uLoc <- uLoc + N
  WE <- unks[uLoc:(uLoc + N -1 )]
    uLoc <- uLoc + N
  WK <- unks[uLoc:(uLoc + N - 1)]
    uLoc <- uLoc + N
  delta <- unks[uLoc]
    uLoc <- uLoc + 1
  gamma <- unks[uLoc]  
  
  
  ###4N +1 Unknowns###
  
  ###Form omega###
  source("C:/Users/jfoxworth/Desktop/Slides/High Discounts, High Unemployment/My data/omegaM.r")
  omega <- omegaM(m)
  
  
  ###Calculate X and R###
  M <- diag(N)
  for(s in 1:N){
    for(sprime in 1:N){
      M[s, sprime] <- Mat[s, sprime] - (1-psis[s]) * omega[s, sprime]
    }
  }
  Inv <- solve(Mat)
  X <- Inv %*% rep(1, N)
  
  ###Form Discrepencies###
  dLoc <- 1
  discs <- rep(1, 4*N)
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
    
    discs[dLoc] <- delta * UR[s] + (1-delta) * z - WE[s] - C[s] #WE equation
    for(sprime in 1:N){
      discs[dLoc] <- discs[dLoc] + (1-delta) * omega[s, sprime] * (WK[sprime] + C[sprime])
    }
    dLoc <- dLoc + 1
    
    discs[dLoc] <- (-1)*(1-delta) * gamma - X[s] + WK[s] #WK equation
    for(sprime in 1:N){
      discs[dLoc] <- discs[dLoc] + (1-delta) * omega[s, sprime] * (X[sprime] - WE[sprime])
    }
    dLoc <- dLoc + 1
  }
  
  J <- X - W
  Jdiscs <- J - kappa * sqrt(thetaVALUE)/mu
  discs[dLoc] <- Jdiscs[3]
  dLoc <- dLoc + 1
  discs[dLoc] <- Jdiscs[5] - Jdiscs[1]
  
###4N+2 discrepencies###
#discs <- real(discs)
  
  if(ifTrack){
    Track <- c(Track, thetaVALUE, X, WE, WK, discs)
  }
  return(discs)
}