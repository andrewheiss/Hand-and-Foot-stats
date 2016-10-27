#' ---
#' title: "Hand and Foot Analysis"
#' author: "Andrew Heiss"
#' date: "`r format(Sys.time(), '%B %e, %Y')`"
#' output: 
#'   html_document: 
#'     code_folding: hide
#'     toc: yes
#'     toc_float: 
#'       collapsed: true
#'     toc_depth: 4
#'     highlight: tango
#'     theme: cosmo
#'     self_contained: yes
#' ...

#+ message=FALSE
knitr::opts_chunk$set(cache=FALSE, fig.retina=2,
                      tidy.opts=list(width.cutoff=120),  # For code
                      options(width=120))  # For output

# Load libraries and data
library(tidyverse)
library(pander)
library(scales)
library(broom)
library(forcats)

panderOptions('table.split.table', Inf)
panderOptions('table.split.cells', Inf)

theme_set(theme_light(base_family="Source Sans Pro"))

hf.df <- read_csv("hand_and_foot.csv")

round.details <- hf.df %>%
  mutate(score_round = score_books + score_cards) %>%
  group_by(game, round) %>%
  mutate(win_round = score_round == max(score_round)) %>%
  mutate(player_next = lead(player, default=player[1]),
         player_previous = lag(player, default=player[n()]))

game.details <- round.details %>%
  group_by(game, player) %>%
  summarise(score_total = sum(score_round)) %>%
  mutate(win_game = score_total == max(score_total))

hf.full <- round.details %>%
  left_join(game.details, by=c("game", "player"))


#' ## Does going down first matter?
#' 
#' ### What's the probability of winning the round if you go down first?
#' 
prob.win.first <- round.details %>%
  filter(!is.na(down_first)) %>%
  group_by(down_first) %>%
  summarise(win_round = sum(win_round)) %>%
  mutate(prob_win_round = percent(win_round / sum(win_round)))

#' There is a `r filter(prob.win.first, down_first)$prob_win_round` chance of
#' winning the round if you go down first.
#' 
#+ results="asis"
pandoc.table(prob.win.first)

#' ### Is there a difference in the total number of points you get if you go down first?
#' 
points.first <- round.details %>%
  filter(!is.na(down_first)) %>%
  group_by(down_first) %>%
  summarise(score_round_mean = mean(score_round),
            score_round_median = median(score_round))

points.first.test <- t.test(score_round ~ down_first, data=round.details) %>%
  tidy()

#' On average, going down first gives you 
#' `r round(diff(points.first$score_round_mean), 2)` additional points 
#' (`r round(diff(points.first$score_round_median), 2)` if you consider the 
#' median). This difference is not statistically significant, though, 
#' (t = `r round(points.first.test$statistic, 2)`, 
#' p = `r round(points.first.test$p.value, 2)`) and we cannot rule out the 
#' possibility that this difference is due to chance.
#' 
#+ results="asis"
pandoc.table(points.first)


#' ## Does getting to your foot first matter?
#' 
#' ### What's the probability of winning the round if you get to your foot first?
#' 
prob.win.foot <- round.details %>%
  filter(!is.na(foot_first)) %>%
  group_by(foot_first) %>%
  summarise(win_round = sum(win_round)) %>%
  mutate(prob_win_round = percent(win_round / sum(win_round)))

#' People who get their foot first win the round 
#' `r filter(prob.win.foot, foot_first)$prob_win_round` of the time.
#' 
#+ results="asis"
pandoc.table(prob.win.foot)

#' ### Is there a difference in the total number of points you get if you get your foot first?
#' 
points.foot <- round.details %>%
  filter(!is.na(foot_first)) %>%
  group_by(foot_first) %>%
  summarise(score_round_mean = mean(score_round),
            score_round_median = median(score_round))

points.foot.test <- t.test(score_round ~ foot_first, data=round.details) %>%
  tidy()

#' On average, getting to your foot first gives you 
#' `r round(diff(points.foot$score_round_mean), 2)` additional points 
#' (`r round(diff(points.foot$score_round_median), 2)` if you consider the 
#' median). This difference is not statistically significant, though, 
#' (t = `r round(points.foot.test$statistic, 2)`, p = 
#' `r round(points.foot.test$p.value, 2)`) and we cannot rule out the 
#' possibility that this difference is due to chance.
#' 
#+ results="asis"
pandoc.table(points.foot)


#' ## Does seating order matter?
typical.round <- mean(round.details$score_round)

sitting.before <- round.details %>%
  group_by(player_next) %>%
  summarise(score_round_mean = mean(score_round),
            score_round_sd = sd(score_round)) %>%
  ungroup() %>%
  mutate(position = "Before") %>%
  rename(player = player_next)

sitting.after <- round.details %>%
  group_by(player_previous) %>%
  summarise(score_round_mean = mean(score_round),
            score_round_sd = sd(score_round)) %>%
  ungroup() %>%
  mutate(position = "After") %>%
  rename(player = player_previous) %>%
  mutate(diff_from_mean = score_round_mean - typical.round)

sitting <- bind_rows(sitting.before, sitting.after) %>%
  mutate(position = fct_relevel(position, "Before"))

#' Assuming an average of `r round(typical.round, 2)` points per round, at 
#' first glance, it appears that sitting after Andrew gives you an average
#' *increase* of 
#' `r round(filter(sitting.after, player=="Andrew")$diff_from_mean, 2)` points, 
#' while sitting after Nancy *decreases* your score by 
#' `r round(abs(filter(sitting.after, player=="Nancy")$diff_from_mean), 2)` 
#' points. However, these differences are not statistically significant (phew) 
#' and could be due to chance. 
#' 
#' There is no substantial difference in score when sitting before someone else.
#' 
ggplot(sitting, aes(x=player, y=score_round_mean, colour=position)) + 
  geom_hline(yintercept=typical.round, linetype="dotted") +
  geom_pointrange(aes(ymin = score_round_mean - score_round_sd,
                      ymax = score_round_mean + score_round_sd),
                  size=1, position=position_dodge(width=0.5)) +
  guides(colour=guide_legend(title="If you are sitting")) +
  labs(x=NULL, y="Average score",
       caption="Dotted line shows average score for all rounds") +
  theme(legend.position="bottom")


#' ## Do people score more in different rounds?
round.totals <- round.details %>%
  group_by(round) %>%
  summarise(score_round_mean = mean(score_round),
            score_round_sd = sd(score_round),
            score_round_median = median(score_round)) %>%
  mutate(round = factor(round))

#' On average, it appears that people score more points in later rounds, though
#' the difference between rounds is not statistically significant.
ggplot(round.totals, aes(x=round, score_round_mean)) + 
  geom_hline(yintercept=typical.round, linetype="dotted") +
  geom_pointrange(aes(ymin = score_round_mean - score_round_sd,
                      ymax = score_round_mean + score_round_sd),
                  size=1, position=position_dodge(width=0.5)) +
  labs(x="Round", y="Average score",
       caption="Dotted line shows average score for all rounds")


#' ## How are book points, card points, and card piles related?
#' 
#' ### Book points and card points
#'
#' Unsurprisingly, the number of points you score in cards is closely related
#' to the number of points you score with books.
#' 
ggplot(round.details, aes(x=score_cards, y=score_books)) + 
  geom_point() +
  geom_smooth(method="lm") +
  labs(x="Points scored with cards", y="Points scored with books")

#' ### Book points and piles
#' 
books.piles <- lm(score_books ~ piles, data=round.details) %>% tidy()

#' Also unsurprisingly, the number of piles you play is correlated with the
#' number of book points you score. Playing an additional set of cards will
#' give you an expected increase of 
#' `r round(filter(books.piles, term=="piles")$estimate, 0)` points, which is 
#' close to a black book.
#' 
ggplot(filter(round.details, !is.na(piles)), aes(x=piles, y=score_books)) + 
  geom_point() +
  geom_smooth(method="lm") +
  labs(x="Piles of cards", y="Points scored with books")

#' ### Card points and piles
#' 
cards.piles <- lm(score_cards ~ piles, data=round.details) %>% tidy()

#' The number of piles you play is also (obviously) correlated with the number 
#' of card points you score. Playing an additional set of cards will give you 
#' an expected increase of 
#' `r round(filter(cards.piles, term=="piles")$estimate, 0)` points, on 
#' average.
#' 
ggplot(filter(round.details, !is.na(piles)), aes(x=piles, y=score_cards)) + 
  geom_point() +
  geom_smooth(method="lm") +
  labs(x="Piles of cards", y="Points scored with cards")
