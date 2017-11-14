############# SMfunc #############
#returns discrepancies between the fitted and actual valuations of the
#  return matrix R for a trial value of m, which contains beta and
#  m(1)...m(N)
#  Returns are in productivity units and the discounter is omega in
#  productivity units

SMfunc <- function(m){
  beta <- m[1]
  mH <- m
  mH[1] <- 1
  N <- length(mH)
  discs <- rep(0, N)

  for(s in 1:N){
   for(sprime in 1:N){
    discs[s] <- discs[s] + beta * PiMatrix[s, sprime] * growthMatrix[s, sprime] * mH[sprime] * RMatrix[s, sprime] / mH[s]
    }
  discs[s] <- discs[s] - 1
  }
return(discs)
}