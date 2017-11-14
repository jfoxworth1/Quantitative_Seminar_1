###Read files from HDHU excel document and Define Base Variables###

library(readxl)
library(pracma)
library(tidyverse)

N <- delta <- Psi <- psis <- pi <- kappa <- z <-  gamma <- wBar <- mu <- eta <- stateCal <- NULL
g <- R <- X <- W <- U <- C <- m <- WE <- WK <- ifTrack <- Track <- thetaSTAR <- beta <- omega <- NULL
M <- Q <- r <- thetaActual <- NULL


Schiller_data <- read_excel("C:/Users/jfoxworth/Desktop/Slides/High Discounts, High Unemployment/Schiller data.xls", 
                            sheet = "Data")
P <- as.numeric(Schiller_data$X__1[931:(length(Schiller_data$X__1)-29)])
D <- as.numeric(Schiller_data$X__2[931:(length(Schiller_data$X__2)-29)])/12


HDHU_results <- read_excel("C:/Users/jfoxworth/Desktop/Slides/High Discounts, High Unemployment/HDHU results.xlsx", 
                           sheet = "u rate")
urate <- as.numeric(HDHU_results$X__1[27:length(HDHU_results$X__1)])

HDHU_results <- read_excel("C:/Users/jfoxworth/Desktop/Slides/High Discounts, High Unemployment/HDHU results.xlsx", 
                           sheet = "PNZ vacs")
PNZvacs <- as.numeric(HDHU_results$X__1[228:length(HDHU_results$X__1)])


HDHU_results <- read_excel("C:/Users/jfoxworth/Desktop/Slides/High Discounts, High Unemployment/HDHU results.xlsx", 
                           sheet = "U")
U <- as.numeric(HDHU_results$X__1[17:length(HDHU_results$X__1)])


HDHU_results <- read_excel("C:/Users/jfoxworth/Desktop/Slides/High Discounts, High Unemployment/HDHU results.xlsx", 
                           sheet = "JOLTS vacs")
JOLTSvacs <- as.numeric(HDHU_results$X__1[17:length(HDHU_results$X__1)])


HDHU_results <- read_excel("C:/Users/jfoxworth/Desktop/Slides/High Discounts, High Unemployment/HDHU results.xlsx", 
                           sheet = "x")
WP <- as.numeric(HDHU_results$`Business Sector: Real Output Per Person`[12:length(HDHU_results$`Business Sector: Real Output Per Person`)])
              

HDHU_results <- read_excel("C:/Users/jfoxworth/Desktop/Slides/High Discounts, High Unemployment/HDHU results.xlsx", 
                           sheet = "Sep")
PsiValues <- HDHU_results$`Total Separations: Total Nonfarm`
PsiValues <- as.numeric(PsiValues[12:length(PsiValues)])
PsiValues <- c(vector(length = 635), PsiValues[1:(length(PsiValues)-2)])
PsiValues[1:635] <- NA
Psi <- 1-0.9^(1/3)

CPI <- as.numeric(Schiller_data$X__4[931:1740])
