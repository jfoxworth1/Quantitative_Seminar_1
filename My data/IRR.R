############# IRR #############

IRR <- function(r){
  
  rMthly=r/12
  omegabar <- PiMatrix * growthMatrix
  N <- dim(PiMatrix)[2]
  discIRR <- NULL
   for(s in 1:N){
    MatBar <- diag(N)-(1-psis[s])*omegabar/(1+rMthly[s])
    XBar <- solve(MatBar) %*% rep(1, N)
    discIRR[s] <- X[s] - XBar[s]
  }
  return(discIRR)
}