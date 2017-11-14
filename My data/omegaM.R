### Omega ###
omegaM <- function(m){

M <- matrix(0, nrow = N, ncol = N)
for(s in 1:N){
  for (sprime in 1:N){
    M[s, sprime] <- m[sprime]/m[s]
  }
}
omega <- beta * PiMatrix * M * growthMatrix
return(omega)
}