############# Create SS table #############

###growth###
for(i in 0:(length(WP)-4)){
  if(i == 0){growth <- NULL}
  else{
    for(j in 1:3){
      growth <- c(growth, (WP[i+4]/WP[i+3])^(1/3))
    }}}


###WP Extended###
for(i in 0:(length(growth))){
  if(i == 0){WPEXTENDED <- 1}
  else{
    WPEXTENDED <- c(WPEXTENDED, (growth[i]*WPEXTENDED[i]))
  }}


### R ###
for(i in 0:(length(P)-1)){
  if(i == 0){R <- NULL}
  else{
    R <- c(R, (((D[i])/P[i])+
                 (P[i+1]/(CPI[i+1]*WPEXTENDED[i+1])/
                  (P[i]/(CPI[i]*WPEXTENDED[i])))))
  }}


SStrend <- c(-400:408)

for(i in 0:length(R)){
  if(i == 0){
    RDETRENDED <- NULL
  }
  else{
    RDETRENDED <- c(RDETRENDED, (R[i]- (cov(R, SStrend)/var(SStrend))*SStrend[i]))
  }}


Psitrend <- seq(from = (length(PsiValues[!is.na(PsiValues)]) 
                        * (PsiValues[!is.na(PsiValues)])[length(PsiValues[!is.na(PsiValues)])]
                        -PsiValues[!is.na(PsiValues)][1]
                        /length(PsiValues[!is.na(PsiValues)])), 
                to = 0, length.out = length(na.omit(PsiValues)))

PsiDETRENDED <- (PsiValues[!is.na(PsiValues)] + Psitrend)

for (i in 1:length(PsiValues)){
  if(is.na(PsiValues[i]) == FALSE){
   PsiValues[i] <- PsiDETRENDED[i-(length(PsiValues)-length(na.omit(PsiValues)))] 
  }}

SS <- data.frame(year=year[1:length(AIstate)], 
                 AIstate=AIstate[1:length(AIstate)], 
                 thetaDETRENDED=thetaDETRENDED[1:length(AIstate)], 
                 growth=growth[1:length(AIstate)], 
                 PsiValues=PsiValues[1:length(AIstate)], 
                 R=RDETRENDED[1:length(AIstate)])
