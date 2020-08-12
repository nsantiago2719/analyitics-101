library(tidyr)
library(dplyr)

universityStat <- read.csv("./data/univstat.csv")

plotData <-universityStat %>% 
            gather( term, n, 2:25) %>%
            separate(term, c("xyear", "sem"), sep = "([.])") %>%
            mutate(year = substr(xyear, 2, 5)) %>%
            spread(sem, n) %>%
            group_by(courses)

bsitData = plotData %>%
              filter(courses == "BSIT")

maxRange = max(c(bsitData$'1', bsitData$'2', bsitData$'3'))
xRange    <- bsitData$year
yRange    <- c('0', '100', '200', '250', '300', '350', '400', '500')

firstSem  <- bsitData$'1'
secondSem <- bsitData$'2'
summer    <- bsitData$'3'

plot(xRange, yRange,
     xlab = "Year",
     ylab = "Student Population",
     main = "BSIT",
     type = "n"
     )

lines(xRange,
      firstSem,
      type = "b",
      col = "red",
      lwd = 2)

lines(xRange,
      secondSem,
      type = "b",
      col = "green",
      lwd = 2)


lines(xRange,
      summer,
      type = "b",
      col = "blue",
      lwd = 2)

legend(2017, 500, 
       legend = c("1-Sem", "2-Sem", "Summer"),
       col    = c("red", "green", "blue"),
       lty    = 1,
       cex    = 0.8)
