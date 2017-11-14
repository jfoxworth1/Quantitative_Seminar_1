############# Make AI States ################

###Average index creation ###
Averageindex <- (thetaDETRENDED[1:length(thetaDETRENDED)-1]/
                   sd(thetaDETRENDED[1:length(thetaDETRENDED)-1]) 
                 + PDDETRENDED/sd(PDDETRENDED))


### AI states Creation ###
AIstate <- {
  ifelse(Averageindex>quantile(Averageindex, 0.8), 5, 
         ifelse(Averageindex>quantile(Averageindex, 0.6), 4, 
                ifelse(Averageindex>quantile(Averageindex, 0.4), 3, 
                       ifelse(Averageindex>quantile(Averageindex, 0.2), 2, 1
                       ))))}