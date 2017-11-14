############# Change from Date to Year Fraction ##############

Date2Year <- function(Date){
  library(stringr)
  sequence <- seq(from = 0, to = 99, length.out = 12)
  Date <- as.character(Date)
  for (i in 1:length(Date)){
    Date[i] <- paste(str_sub(Date[i], 1, 4), 
                     ifelse(nchar(sequence[as.numeric(str_sub(Date[i], 6, 7))]) == 1,
                            paste(0 , as.character(sequence[as.numeric(str_sub(Date[i] , 6))]), sep = ""),
                            as.character(sequence[as.numeric(str_sub(Date[i], 6, 7))])
                     )
                    , sep = "")
  }
  Date <- as.numeric(Date)
  return(Date)
}