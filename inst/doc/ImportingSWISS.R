## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.width=7, fig.height=6, fig.align = "center")

## ----read----------------------------------------------------------------
library(tidyverse)
library(juggerdata)

# note: the data-file can be named arbitrarily, since the program does not enforce any file endings
HanseaticJuggerCup2013 <- readSwissTournament("HanseaticJuggerCupR6") # R6 stands for round 6

summary(HanseaticJuggerCup2013)


ggplot(HanseaticJuggerCup2013$rounds %>%
         union_all(HanseaticJuggerCup2013$rounds %>%
                     rename(entryId1 = entryId2, entryId2 = entryId1, points1 = points2, points2 = points1)) %>%
         mutate(status = factor(x = sign(points2 - points1), labels = c("Team1", "Draw", "Team2"))),
       aes(x = entryId1, y = entryId2, fill = status)) +
  geom_raster() +
  scale_x_continuous("Team1", breaks = HanseaticJuggerCup2013$teams$entryId, minor_breaks = NULL, labels = HanseaticJuggerCup2013$teams$teamName) + #, limits = c(1,nrow(HanseaticJuggerCup2013$teams))
  scale_y_continuous("Team2", breaks = HanseaticJuggerCup2013$teams$entryId, minor_breaks = NULL, labels = HanseaticJuggerCup2013$teams$teamName) + #, limits = c(1,nrow(HanseaticJuggerCup2013$teams))
  scale_fill_discrete("Winner", h = c(40,200), l = 50) +
  ggtitle("1. Hanseatic Jugger Cup, 2013", "games played and won") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.3),
                          plot.subtitle = element_text(hjust = 0.3),
                          axis.text.x = element_text(angle = -90, vjust = 0.5))

## ----draw rankings-------------------------------------------------------
library(directlabels)

HanseaticJuggerCup2013Summary <- summary(HanseaticJuggerCup2013)

ggplot(HanseaticJuggerCup2013Summary$rankings %>%
         left_join(HanseaticJuggerCup2013Summary$teams, by = "entryId"),
       aes(x = round, y = rank, color = teamName, group = teamName)) +
  geom_line() +
  geom_dl(aes(label = teamName), method = list(dl.combine("first.points", "last.points"), cex = 0.8)) +
  scale_x_continuous("Swiss tournament rounds", breaks = 1:6, minor_breaks = NULL, labels = paste('R', 1:6, sep = ''), limits = c(0,7)) +
  scale_y_continuous("Swiss rank", breaks = 1:14, minor_breaks = NULL, trans = "reverse", position = "right") +
  scale_color_discrete(h = c(0,240), l = 50, guide = FALSE) +
  ggtitle("1. Hanseatic Jugger Cup, 2013", "rankings over all swiss system rounds") +
  theme_minimal() + theme(panel.grid.major.y = element_blank(),
                          plot.title = element_text(hjust = 0.3),
                          plot.subtitle = element_text(hjust = 0.3))


## ----draw ranking changes------------------------------------------------
changeData <- HanseaticJuggerCup2013Summary$rankings %>% 
  group_by(entryId) %>%
  arrange(round) %>%
  mutate(round = as.factor(round),
        'total rank changes' = lag(rank, 1) - rank,
        'absolute rank changes' = abs(lag(rank, 1) - rank)) %>%
  filter(round != 1) %>% select(-rank, -opp, -scoreCum, -pointsCum, -pointsDiffCum, -BHZ) %>%
  gather("rankChangeTyp", "Value", -entryId, -round)

ggplot(changeData,
       aes(x = round, y = Value, color = as.factor(1), fill = as.factor(1))) +
  facet_wrap(~rankChangeTyp, scales = "free", ncol = 1) +
  geom_boxplot() +
  scale_x_discrete("Swiss tournament rounds transition", labels = paste('R', 1:5, '-R', 2:6, sep = '')) +
  scale_y_continuous("Swiss rank change", breaks = -5:5, minor_breaks = NULL, position = "left") +
  scale_color_discrete(h = c(100,140), l = 50, guide = FALSE) +
  scale_fill_discrete(h = c(100,140), l = 75, c = 100, guide = FALSE) +
  ggtitle("1. Hanseatic Jugger Cup, 2013", "rank changes swiss system rounds") +
  theme_minimal() + theme(panel.grid.major.y = element_blank(),
                          plot.title = element_text(hjust = 0.3),
                          plot.subtitle = element_text(hjust = 0.3))


## ----testing swiss A, warning=FALSE--------------------------------------
testData <- changeData %>% ungroup() %>% filter(rankChangeTyp == "absolute rank changes") %>% select(entryId, round, Value) %>%
  spread(round, Value) %>% select(-entryId)

indices <- expand.grid(round1 = 1:ncol(testData), round2 = 1:ncol(testData))
testResultsA <- cbind(indices,
                      t(mapply(function(i1,i2) { broom::tidy(wilcox.test(x = as.numeric(testData[[i1]]),
                                                                         y = as.numeric(testData[[i2]]),
                                                                         alternative = "less")) }, indices[[1]], indices[[2]])[1:2,]))
testResultsA <- testResultsA %>% mutate(statistic = unlist(statistic), p.value = unlist(p.value))
# knitr::kable(testResultsA)
ggplot(testResultsA %>% mutate(sigLabel = if_else(p.value < 0.05, '*', '')),
       aes(x = as.factor(round1), y = as.factor(round2), fill = p.value, label = sigLabel)) +
  geom_raster() + geom_text() +
  scale_x_discrete("round transition", labels = paste('R', 1:5, '-R', 2:6, sep = '')) +
  scale_y_discrete("round transition", labels = paste('R', 1:5, '-R', 2:6, sep = '')) +
  scale_fill_continuous(name = "p") +
  ggtitle("1. Hanseastic Jugger Cup, 2013", "wilcoxon test results (one-sided)") +
  theme_minimal() + theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(),
                          plot.title = element_text(hjust = 0.3),
                          plot.subtitle = element_text(hjust = 0.3))

## ----swiss test B--------------------------------------------------------
testData <- changeData %>% ungroup() %>% filter(rankChangeTyp == "total rank changes") %>% select(round, Value)

indices <- expand.grid(round1 = unique(testData$round), round2 = unique(testData$round))
testResultsB <- cbind(indices,
                      t(mapply(function(i1,i2) { if (i1 == i2) return(data_frame(statistic = NA, p.value = NA, variance = var(testData %>% filter(round == i1))[2,2])) else cbind(broom::tidy(car::leveneTest(Value ~ round, testData %>% filter(round %in% c(i1,i2))))[1,3:4], variance = NA) }, indices[[1]], indices[[2]])))
testResultsB <- testResultsB %>% mutate(statistic = unlist(statistic), p.value = unlist(p.value), variance = unlist(variance))

ggplot(testResultsB %>% mutate(varianceLabel = if_else(is.na(variance), if_else(p.value < 0.05, '*', ''), paste("var = ", format(variance, digits = 3)) )),
       aes(x = as.factor(round1), y = as.factor(round2), fill = p.value, label = varianceLabel)) +
  geom_raster() + geom_text() +
  scale_x_discrete("round transition", labels = paste('R', 1:5, '-R', 2:6, sep = '')) +
  scale_y_discrete("round transition", labels = paste('R', 1:5, '-R', 2:6, sep = '')) +
  scale_fill_continuous(name = "p", na.value = "white") +
  ggtitle("1. Hanseastic Jugger Cup, 2013", "Levene test results") +
  theme_minimal() + theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(),
                          plot.title = element_text(hjust = 0.3),
                          plot.subtitle = element_text(hjust = 0.3))


## ----ranking stability---------------------------------------------------
rankTaus <- cor(HanseaticJuggerCup2013Summary$rankings %>% select(round, entryId, rank) %>%
                  spread(round, rank) %>% select(-entryId), method = "kendall")

# ggplot(rankTaus %>% as.data.frame() %>% rownames_to_column("round1") %>% gather("round2", "tau", -round1),
#       aes(x = round1, y = round2, fill = tau)) +
#   geom_raster() +
#   scale_x_discrete("round", labels = paste('R', 1:6, sep = '')) +
#   scale_y_discrete("round", labels = paste('R', 1:6, sep = '')) +
#   scale_fill_continuous(name = "tau", na.value = "white") +
#   ggtitle("1. Hanseastic Jugger Cup, 2013", "Kendall's tau between each round") +
#   theme_minimal() + theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(),
#                           plot.title = element_text(hjust = 0.3),
#                           plot.subtitle = element_text(hjust = 0.3))

ggplot(data_frame(tau = rankTaus[row(rankTaus) - col(rankTaus) == 1], round = 1:5),
      aes(x = round, y = tau, color = tau)) +
  geom_step() +
  scale_x_continuous("round transition", labels = paste('R', 1:5, '-R', 2:6, sep = '')) +
  scale_y_continuous("tau") +
  scale_fill_continuous(name = "tau", na.value = "white") +
  ggtitle("1. Hanseastic Jugger Cup, 2013", "Kendall's tau between consecutive rounds") +
  theme_minimal() + theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(),
                          plot.title = element_text(hjust = 0.3),
                          plot.subtitle = element_text(hjust = 0.3))
  


## ----prepare package data------------------------------------------------


