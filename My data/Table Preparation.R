############# Table Preparation #############
for(i in 0:6){
  if (i==0){
    SUMthetaDETRENDED <- NULL
    SUMgrowth <- NULL
    SUMPsi <- NULL
    SUMR <- NULL
  }
  if(i !=0 && i != 6){
    for(j in 1:5){
      for(k in 0:length(SS$AIstate)){
        if(k == 0){
          SUMthetaDETRENDEDCOUNTER <- NULL
          SUMgrowthCOUNTER <- NULL
          SUMPsiCOUNTER <- NULL
          SUMRCOUNTER <- NULL
        }
        else{
          if(SS$AIstate[k] == i && i != 6){
            if (is.na(SS$PsiValues[k]) == FALSE && j==5){
              SUMPsiCOUNTER <- c(SUMPsiCOUNTER, round(SS$PsiValues[k], 1))
            }
            if(j==5){
              SUMthetaDETRENDEDCOUNTER <- c(SUMthetaDETRENDEDCOUNTER, 
                                            SS$thetaDETRENDED[k])
            }
            if(is.na(SS$AIstate[k+1]) == FALSE){
              if(SS$AIstate[k+1] == j){
                SUMgrowthCOUNTER <- c(SUMgrowthCOUNTER, SS$growth[k])
              }}
            if(is.na(SS$AIstate[k+1]) == FALSE){
              if(SS$AIstate[k+1] == j){
                SUMRCOUNTER <- c(SUMRCOUNTER, SS$R[k])
              }}
          }}}
      ifelse(length(SUMgrowthCOUNTER) == 0,
             SUMgrowth <- c(SUMgrowth, 0),
             SUMgrowth <- c(SUMgrowth, mean(SUMgrowthCOUNTER)))
      ifelse(length(SUMRCOUNTER) == 0,
             SUMR <- c(SUMR, 0),
             SUMR <- c(SUMR, mean(SUMRCOUNTER)))
    }
    SUMPsi <- c(SUMPsi, mean(SUMPsiCOUNTER))
    SUMthetaDETRENDED <- c(SUMthetaDETRENDED, mean(SUMthetaDETRENDEDCOUNTER))
  }
  if(i == 6){
    PsiMEAN <- mean(na.omit(SS$PsiValues))
    for(l in 1:length(SUMPsi)){SUMPsi[l] <- (SUMPsi[l]*Psi/PsiMEAN)}
    thetaActual <- SUMthetaDETRENDED
    psiState <- SUMPsi
    RMatrix <- matrix(SUMR, nrow = 5, ncol=5, byrow=TRUE)
    growthMatrix <- matrix(SUMgrowth, nrow = 5, ncol=5, byrow=TRUE)
  }}
