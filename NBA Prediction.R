##NBAPlayoff standings

NBA <- read.csv("D:\\MSBA\\Summer 2018\\MSBX5410\\NBAPlayoff standings.csv", stringsAsFactors=FALSE)
NBA <- as.data.frame( NBA )
NBA

##Subseting our DATA points

lowrating <- NBA[NBA$Rating < 77.5,]
lowrating

Upperrating <- NBA[NBA$Rating > 77.5,]
Upperrating

##Linear Regression Analysis

lmMod <- glm( Playoffchance ~ Rating, data= NBA, family=binomial )
summary( lmMod )

##Each team prediction - Spurs playoffchance Prob

NBA.1 <- data.frame(Rating=81)
predict(lmMod, NBA.1, type="response")

##Each team prediction - Lakers playoffchance Prob

NBA.1 <- data.frame(Rating=77.65)
predict(lmMod, NBA.1, type="response")

## Plotting the Logistic regression model to show the teams play-off chances

library(ggplot2)

ggplot( data=NBA, aes(x=Rating, y=Playoffchance)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE)+
  theme_bw() 