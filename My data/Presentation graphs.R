HDHU_results <- read_excel("C:/Users/jfoxworth/Desktop/Slides/High Discounts, High Unemployment/HDHU results.xlsx",
                           sheet = "u fit")

predicted.from.paper <- HDHU_results$X__4[4:length(HDHU_results$X__4)]
predicted.from.paper <- as.numeric(predicted.from.paper)

poster.plot.data <- data.frame("Year" = SS$year[1:length(ufit)],
                               PredictedU = ufit,
                               PaperU = predicted.from.paper[1:length(ufit)],
                               ActualU = urate[1:length(ufit)])

poster.plot.data <- gather(poster.plot.data, ProjVsAct, Amount,
                           PredictedU : ActualU, factor_key = TRUE)

levels(poster.plot.data$ProjVsAct)[levels(poster.plot.data$ProjVsAct) == "PredictedU"] <- "Unemployment Rate Predictions"
levels(poster.plot.data$ProjVsAct)[levels(poster.plot.data$ProjVsAct) == "PaperU"] <- "Paper's Unemployment Rate Predictions"
levels(poster.plot.data$ProjVsAct)[levels(poster.plot.data$ProjVsAct) == "ActualU"] <- "Actual Unemployment Rate"

p2 <- ggplot(data = poster.plot.data,
             aes(x = Year, y = Amount,
                 stat = "identity", color = ProjVsAct)) +
  geom_line(size = 1) +
  ggtitle("Fitted Vs Actual Unemployment Rates") +
  theme(plot.title = element_text(face = "bold", size = "24"), legend.position = "bottom")+
  labs(y = "Unemployment Rate (%)", x = ("Month (2011)")) +
  scale_x_date(breaks= seq(as.Date("1945-01-01"), as.Date("2015-01-01"), by = "5 years"), date_labels = "%Y")

#  scale_color_manual(values = c('#F8766D', '#00BA38',))+
#  coord_cartesian(xlim = c(as.Date("2011-05-01"), as.Date("2011-08-01")), ylim = c(6.3379,6.344))

plot(p2)
