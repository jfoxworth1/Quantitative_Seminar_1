

############# Cases #############

N <- 5
stateCal <- 3
eta <- 0.5
UatCalibrationPoint <- 0.055
thetaSTAR <- 0.59
thetaCal <- thetaActual[stateCal]
f <- ((1 - UatCalibrationPoint)*Psi)/UatCalibrationPoint
mu <- (thetaCal^(-1*eta))*f
qSTAR <- f/thetaSTAR
z <- 0.4
gamma <- 0.57
delta <- 0.013
ifTrack <- TRUE

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
}}

z <- 0.4
kappa <- 0.213

source("C:/Users/jfoxworth/Desktop/Slides/High Discounts, High Unemployment/My data/SM.r")
source('C:/Users/jfoxworth/Desktop/Slides/High Discounts, High Unemployment/My data/LMmainTH.r')
source('C:/Users/jfoxworth/Desktop/Slides/High Discounts, High Unemployment/My data/LMmainCB.r')
