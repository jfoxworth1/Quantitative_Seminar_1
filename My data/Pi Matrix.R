###### Create Pi ######
for(i in 0:5){ #row
  if(i==0){x <- NULL} #counting variable
  else {
    for(j in 0:5){ #column
      if(j==0){y <- NULL} #counting variable
      else{
        for(k in 0:length(AIstate)){
          if(k==0){z <- NULL} #counting variable
          else{
            ifelse(AIstate[k]==i && AIstate[(k+1)]==j, z <- c(z,1), z <- c(z, 0)) #state space
            ifelse(AIstate[k]==i && j==1 && !is.na(AIstate[k+1]), y <- c(y,1), y <- c(y,0)) #total number in state
          }}
        x <- c(x, (sum(z)/sum(y)))
      }}}}

PiMatrix <- matrix(x, nrow=5, ncol = 5, byrow = TRUE)