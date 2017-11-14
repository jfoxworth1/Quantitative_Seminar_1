############# Calculates theta and P/D Detrended #############

###Standardize Values###
JOLTSvacs <- c(vector(length = 635), JOLTSvacs, 0)
PNZvacs <- c(PNZvacs, vector(length = 25))

###Make Year###
for(i in 1947:2015){
  if (i == 1947){year <- NULL}
  else{
    for(j in 1:12){
      if(i == 2015 && j > 7){break}
      else{
        year <- c(year, paste(i, j, 01, sep = "-"))
      }}}}
year <- as.Date(as.character(year))

###Combine###
th_calcs <- data.frame(year = year, JOLTSvacs = JOLTSvacs, PNZvacs = PNZvacs, U = U, urate = urate)


### Make thetaJOTS calc ###
for(i in 0:(length(th_calcs$year)+1)){
  if(i == 0){thetaJOLTS <- NULL}
  else{
    ifelse(th_calcs$JOLTSvacs[i] != 0, 
         thetaJOLTS <- c(thetaJOLTS, (JOLTSvacs[i]/U[i])),
          thetaJOLTS <- c(thetaJOLTS, NA))
  }}

### Make thetaPNZ ###
for(i in 0:length(th_calcs$year)){
  if(i == 0){thetaPNZ <- NULL}
  else{
    ifelse(PNZvacs[i] == 0, 
           thetaPNZ <- c(thetaPNZ, NA),
           thetaPNZ <- c(thetaPNZ, (1000*PNZvacs[i]/urate[i])))
  }}


###Make theta###
for(i in 0:length(th_calcs$year)){
  if(i == 0){theta <- NULL}
  else{
   ifelse(is.na(thetaJOLTS[i]), 
          theta <- c(theta, mean(na.omit(thetaJOLTS/thetaPNZ))*thetaPNZ[i]),
          theta <- c(theta, thetaJOLTS[i]))
}}

###Detrend theta###
trend <- c(-420:389)
trendADJ <- cov(sapply(na.omit(theta), FUN= log), trend)/(var(trend)*((length(trend)-1)/length(trend)))
thetaDETRENDED <- exp(sapply(na.omit(theta), FUN=log)-trendADJ*trend)


###Detrend P/D###
PoverD <- P[1:(length(P)-1)]/D[1:(length(D)-1)]
PDtrend <- c(-400:(length(PoverD)-401))
PDtrendADJ <- cov(PoverD, PDtrend)/var(PDtrend)
PDDETRENDED <- PoverD-PDtrendADJ*PDtrend