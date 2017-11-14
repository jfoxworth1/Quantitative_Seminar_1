############# Make Output #############
source('C:/Users/jfoxworth/Desktop/Slides/High Discounts, High Unemployment/My data/ufit.R')
source('C:/Users/jfoxworth/Desktop/Slides/High Discounts, High Unemployment/My data/Date2Year.R')

ufit <- ufit(SS$AIstate)


ProjectedVSActualUValues <- data.frame("Year" = SS$year[1:length(ufit)], 
                                    PredictedU = ufit, 
                                    ActualU = urate[1:length(ufit)])
tidyUValues <- gather(ProjectedVSActualUValues, ProjVsAct, Amount, 
                      PredictedU : ActualU, factor_key = TRUE)

levels(tidyUValues$ProjVsAct)[levels(tidyUValues$ProjVsAct) == "PredictedU"] <- "Unemployment Rate Predictions"
levels(tidyUValues$ProjVsAct)[levels(tidyUValues$ProjVsAct) == "ActualU"] <- "Actual Unemployment Rate"

p1 <- ggplot(data = tidyUValues, 
              aes(x = Year, y = Amount, 
              stat = "identity", color = ProjVsAct)) +
                geom_line(size = 1) +
                ggtitle("Fitted Vs Actual Unemployment Rates") +
                labs(y = "Unemployment Rate (%)", x = ("Year")) +
                scale_x_date(date_breaks= "5 years", date_labels = "%Y")+
                theme(legend.position = "bottom")





