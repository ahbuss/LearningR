library(dplyr)
library(ggplot2)
library(gridExtra)

dat <- tibble(time = 1:20,
              demand = rep(0,20))
dat$demand[2] <- 1
dat$demand[4] <- 1
dat$demand[5] <- 2
dat$demand[12] <- 1
dat$demand[15] <- 2

histData <- dat %>% 
  mutate(period = ifelse(demand > 0, 1, 0),
         period = cumsum(period)) %>%
  group_by(period) %>%
  summarize(ArrivalTime = n())

demandProb <- 
  tibble(Demand = c(TRUE, FALSE),
         Prob = c(length(which(dat$demand > 0)) / length(dat$demand),
                  1 - length(which(dat$demand > 0)) / length(dat$demand)))

newSamples2 <- tibble(ArrivalTime = 
                        rnbinom(100, 1, prob = filter(demandProb, Demand)$Prob[1]))

dat <- dplyr::bind_rows(mutate(newSamples2, Type = "Generated"), mutate(histData, Type = "Historical"))
p1 <- ggplot(dat, aes(x = ArrivalTime, y = ..density.., fill = Type)) +
  geom_histogram(color = 'black', alpha = .5) 

p2 <- ggplot(dat, aes(x = ArrivalTime, y = ..density.., fill = Type)) +
  geom_density(color = 'black', alpha = .5) 

gridExtra::grid.arrange(p1, p2, ncol = 2)