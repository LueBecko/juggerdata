## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----read----------------------------------------------------------------
library(juggerdata)

# note: the data-file can be named arbitrarily, since the program does not enforce any file endings
HanseaticJuggerCup2013 <- readSwissTournament("Hanseatic Jugger Cup R6") # R6 stands for round 6

summary(HanseaticJuggerCup2013)


## ----draw rankings-------------------------------------------------------
library(tidyverse)
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
ggplot(HanseaticJuggerCup2013Summary$rankings %>%
         group_by(entryId) %>%
         arrange(round) %>%
         mutate(round = as.factor(round),
                'total rank changes' = lag(rank, 1) - rank,
                'absolut rank changes' = abs(lag(rank, 1) - rank)) %>%
         filter(round != 1) %>% select(-rank, -opp, -scoreCum, -pointsCum, -pointsDiffCum, -BHZ) %>%
         gather("rankChangeTyp", "Value", -entryId, -round),
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




