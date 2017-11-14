############# SM #############

m <- rep(1, N)
source("C:/Users/jfoxworth/Desktop/Slides/High Discounts, High Unemployment/My data/SMfunc.r")

mNew <- fsolve(SMfunc, m)$x
beta <- mNew[1]
m <- c(1, mNew[2:N])
OutputSM <- c(beta, m)


###Form omega###
source("C:/Users/jfoxworth/Desktop/Slides/High Discounts, High Unemployment/My data/omegaM.r")
omega <- omegaM(m)


###Calculate X and R###
Mat <- diag(N)
for(s in 1:N){
  for(sprime in 1:N){
    Mat[s,sprime] <- Mat[s, sprime] - (1-psis[s])*omega[s, sprime]
  }
}
X <- solve(Mat) %*% rep(1, N)


rStart <- c(0.3, 0.2, 0.15, 0.05, 0)

source("C:/Users/jfoxworth/Desktop/Slides/High Discounts, High Unemployment/My data/IRR.r")
r <- fsolve(IRR, rStart, maxiter = 5000, tol = 0.000000001)$x

