############# U fit #############

ufit <- function(AIstate){
  fittedu <- NULL
  for(i in 1:length(AIstate)){
   if(AIstate[i] > 5 || AIstate[i] < 1 || (AIstate[1] %% 2 != 1 || 0)){
      warning("ALL VALUES OF AIstate MUST BE INTEGERS BETWEEN 1 AND 5")
    }
   
    if(i == 1){
      fittedu[i] <- urate[1]
    }
    else{
      fittedu[i] <- (1 - (mu * thetaVALUE[AIstate[i-1]]^eta)) * fittedu[i-1] + Psi * (100 - fittedu[i - 1])
    }
  }
  return(fittedu)
}