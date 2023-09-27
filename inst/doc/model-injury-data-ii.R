## ---- include = FALSE---------------------------------------------------------
library(knitr)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE) # to supress R-CMD check

## to fold/hook the code
hook_output <- knit_hooks$get("output")
knit_hooks$set(output = function(x, options) {
  lines <- options$output.lines
  if (is.null(lines)) {
    return(hook_output(x, options))  # pass to default hook
  }
  x <- unlist(strsplit(x, "\n"))
  more <- "..."
  if (length(lines) == 1) {
    if (length(x) > lines) {
      # truncate the output, but add ....
      x <- c(head(x, lines), more)
    }
  } else {
    x <- c(if (abs(lines[1]) > 1) more else NULL,
           x[lines],
           if (length(x) > lines[abs(length(lines))]) more else NULL
    )
  }
  # paste these lines together
  x <- paste(c(x, ""), collapse = "\n")
  hook_output(x, options)
})

modern_r <- getRversion() >= "4.1.0"

## ----setup, message = F-------------------------------------------------------
library(injurytools)
library(dplyr)
library(stringr)
library(survival)
library(survminer)
library(coxme)
library(ggplot2)
library(gridExtra)

## ----setup2, echo = F---------------------------------------------------------
# set the global theme of all plots
theme_set(theme_bw())

## ---- warning = F-------------------------------------------------------------
## 17/18
df_exposures1718 <- prepare_exp(df_exposures0 = 
                                  raw_df_exposures |> filter(season == "17/18"),
                                player        = "player_name",
                                date          = "year",
                                time_expo     = "minutes_played") |> 
  mutate(seasonb = date2season(date))
df_injuries1718 <- prepare_inj(df_injuries0   =
                                 raw_df_injuries |> filter(season == "17/18"),
                               player         = "player_name",
                               date_injured   = "from",
                               date_recovered = "until")
injd1718 <- prepare_all(data_exposures = df_exposures1718,
                        data_injuries  = df_injuries1718,
                        exp_unit = "matches_minutes")

injd1718 <- injd1718 |> 
  mutate(seasonb = date2season(tstart)) |> 
  ## join to have info such as position, age, citizenship etc.
  left_join(df_exposures1718, by = c("player" = "player", 
                                     "seasonb" = "seasonb")) 

## create injd1718_sub:
##  - time to first injury
##  - equivalent tstart and tstop in calendar days
injd1718_sub <- injd1718 |> 
  mutate(tstop_day = as.numeric(difftime(tstop, tstart, units = "days"))) %>%
  group_by(player) |>  ## important
  mutate(tstop_day = cumsum(tstop_day),
         tstart_day = lag(tstop_day, default = 0)) |> 
  ungroup() |> 
  dplyr::select(player:tstop_minPlay, tstart_day, tstop_day, everything()) |> 
  filter(enum == 1) ## time to first injury

## ---- warning = F-------------------------------------------------------------
## 18/19
df_exposures1819 <- prepare_exp(df_exposures0 = 
                                  raw_df_exposures |> filter(season == "18/19"),
                                player        = "player_name",
                                date          = "year",
                                time_expo     = "minutes_played") |> 
  mutate(seasonb = date2season(date))
df_injuries1819 <- prepare_inj(df_injuries0   = 
                                 raw_df_injuries |> filter(season == "18/19"),
                               player         = "player_name",
                               date_injured   = "from",
                               date_recovered = "until")
injd1819 <- prepare_all(data_exposures = df_exposures1819,
                        data_injuries  = df_injuries1819,
                        exp_unit = "matches_minutes")

injd1819 <- injd1819 |> 
  mutate(seasonb = date2season(tstart)) |> 
  ## join to have info such as position, age, citizenship etc.
  left_join(df_exposures1819, by = c("player" = "player", 
                                     "seasonb" = "seasonb")) 

## create injd1819_sub:
##  - time to first injury
##  - equivalent tstart and tstop in calendar days
injd1819_sub <- injd1819 |> 
  mutate(tstop_day = as.numeric(difftime(tstop, tstart, units = "days"))) %>%
  group_by(player) |>  ## important
  mutate(tstop_day = cumsum(tstop_day),
         tstart_day = lag(tstop_day, default = 0)) |> 
  ungroup() |> 
  dplyr::select(player:tstop_minPlay, tstart_day, tstop_day, everything()) |> 
  filter(enum == 1) ## time to first injury

## -----------------------------------------------------------------------------
## CHECK
any(injd1718_sub$tstop_day == injd1718_sub$tstart_day)
any(injd1718_sub$tstop_minPlay == injd1718_sub$tstart_minPlay)
any(injd1819_sub$tstop_day == injd1819_sub$tstart_day)
any(injd1819_sub$tstop_minPlay == injd1819_sub$tstart_minPlay)

## -----------------------------------------------------------------------------
injd_sub <- bind_rows("17-18" = injd1718_sub,
                      "18-19" = injd1819_sub,
                      .id = "season")

## -----------------------------------------------------------------------------
fit <- survfit(Surv(tstart_day, tstop_day, status) ~ seasonb,
               data = injd_sub)

## -----------------------------------------------------------------------------
fit

## ---- fig.width = 10, fig.height = 4.4----------------------------------------
ggsurvplot(fit, data = injd_sub,
           palette = c("#E7B800", "#2E9FDF")) + ## colors for the curves
  xlab("Time (calendar days)") + 
  ylab(expression("Survival probability  ("*hat(S)[KM](t)*")")) +
  ggtitle("Kaplan-Meier curves", 
          subtitle = "in each season (time to first injury)") 

## ---- results = "hide"--------------------------------------------------------
## since tstop_day == (tstop_day - tstart_day)
all(injd_sub$tstop_day == (injd_sub$tstop_day - injd_sub$tstart_day))
# [1] TRUE

## equivalent fits:
fit <- survfit(Surv(tstart_day, tstop_day, status) ~ seasonb, data = injd_sub)
fit <- survfit(Surv(tstop_day, status) ~ seasonb, data = injd_sub)

## ---- warning = F, eval = F---------------------------------------------------
#  ggsurv <- ggsurvplot(fit, data = injd_sub,
#             palette = c("#E7B800", "#2E9FDF"),
#             surv.median.line = "hv",
#             ggtheme = theme_bw(),
#             break.time.by = 60,
#             xlim = c(0, 370),
#             legend.labs = c("Season 17/18", "Season 18/19")) +
#    xlab("Time (calendar days)") +
#    ylab(expression("Survival probability  ("*hat(S)[KM](t)*")")) +
#    ggtitle("Kaplan-Meier curves",
#            subtitle = "in each season (time to first injury)")
#  
#  # add median survival estimates
#  ggsurv$plot <- ggsurv$plot +
#    annotate("text",
#             x = 70, y = 0.4,
#             label = expression(hat(S)[18/19]*"(106)=0.5"),
#             col = "#2E9FDF") +
#      annotate("text",
#             x = 230, y = 0.4,
#             label = expression(hat(S)[17/18]*"(265)=0.5"),
#             col = "#E7B800")
#  
#  ggsurv$plot <- ggsurv$plot +
#    theme(plot.title = element_text(size = rel(1.5)),
#          plot.subtitle = element_text(size = rel(1.5)),
#          axis.title = element_text(size = rel(1.3)),
#          axis.text = element_text(size = rel(1.3)),
#          legend.title = element_blank(),
#          legend.text = element_text(size = rel(1.2)))
#  
#  ggsurv

## ---- echo = F, warning = F, fig.width = 10, fig.height = 4.8-----------------
ggsurv <- ggsurvplot(fit, data = injd_sub,
           palette = c("#E7B800", "#2E9FDF"),
           surv.median.line = "hv",
           ggtheme = theme_bw(),
           break.time.by = 60,
           xlim = c(0, 370),
           legend.labs = c("Season 17/18", "Season 18/19")) +
  xlab("Time (calendar days)") +
  ylab(expression("Survival probability  ("*hat(S)[KM](t)*")")) +
  ggtitle("Kaplan-Meier curves", 
          subtitle = "in each season (time to first injury)") 

# add median survival estimates
ggsurv$plot <- ggsurv$plot +
  annotate("text", 
           x = 70, y = 0.4,
           label = expression(hat(S)[18/19]*"(106)=0.5"),
           col = "#2E9FDF") +
    annotate("text", 
           x = 230, y = 0.4,
           label = expression(hat(S)[17/18]*"(265)=0.5"),
           col = "#E7B800")

ggsurv$plot <- ggsurv$plot + 
  theme(plot.title = element_text(size = rel(1.5)),
        plot.subtitle = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text = element_text(size = rel(1.3)),
        legend.title = element_blank(),
        legend.text = element_text(size = rel(1.2)))

ggsurv

## ---- warning = F, eval = F---------------------------------------------------
#  ggsurv <- ggsurvplot(fit, data = injd_sub,
#             palette = c("#E7B800", "#2E9FDF"),
#             risk.table = T,
#             conf.int = T,
#             pval = T,
#             surv.median.line = "hv",
#             risk.table.col = "strata",
#             ggtheme = theme_bw(),
#             break.time.by = 60,
#             xlim = c(0, 370),
#             legend.labs = c("Season 17/18", "Season 18/19"),
#             legend.title = "") +
#    xlab("Time (calendar days)") +
#    ylab(expression("Survival probability  ("*hat(S)[KM](t)*")")) +
#    ggtitle("Kaplan-Meier curves",
#            subtitle = "in each season (time to first injury)")
#  
#  # add median survival estimates
#  ggsurv$plot <- ggsurv$plot +
#    annotate("text",
#             x = 70, y = 0.4,
#             label = expression(hat(S)[18/19]*"(106)=0.5"),
#             col = "#2E9FDF") +
#      annotate("text",
#             x = 230, y = 0.4,
#             label = expression(hat(S)[17/18]*"(265)=0.5"),
#             col = "#E7B800")
#  # quit title and y text of the risk table
#  ggsurv$table <- ggsurv$table +
#    ggtitle("Number of players at risk") +
#    theme(plot.subtitle = element_blank(),
#          axis.title.y = element_blank(),
#          plot.title = element_text(size = rel(1.5)),
#          axis.title.x = element_text(size = rel(1.3)),
#          axis.text = element_text(size = rel(1.3)))
#  
#  ggsurv$plot <- ggsurv$plot +
#    theme(plot.title = element_text(size = rel(1.5)),
#          plot.subtitle = element_text(size = rel(1.5)),
#          axis.title = element_text(size = rel(1.3)),
#          axis.text = element_text(size = rel(1.3)),
#          legend.title = element_blank(),
#          legend.text = element_text(size = rel(1.2)))
#  
#  ggsurv

## ---- echo = F, warning = F, fig.width = 10, fig.height = 6-------------------
ggsurv <- ggsurvplot(fit, data = injd_sub, 
           palette = c("#E7B800", "#2E9FDF"),
           risk.table = T,
           conf.int = T,  
           pval = T,
           surv.median.line = "hv",
           risk.table.col = "strata", 
           ggtheme = theme_bw(),
           break.time.by = 60,
           xlim = c(0, 370),
           legend.labs = c("Season 17/18", "Season 18/19"),
           legend.title = "") +
  xlab("Time (calendar days)") +
  ylab(expression("Survival probability  ("*hat(S)[KM](t)*")")) +
  ggtitle("Kaplan-Meier curves", 
          subtitle = "in each season (time to first injury)") 

# add median survival estimates
ggsurv$plot <- ggsurv$plot +
  annotate("text", 
           x = 70, y = 0.4,
           label = expression(hat(S)[18/19]*"(106)=0.5"),
           col = "#2E9FDF") +
    annotate("text", 
           x = 230, y = 0.4,
           label = expression(hat(S)[17/18]*"(265)=0.5"),
           col = "#E7B800")
# quit title and y text of the risk table
ggsurv$table <- ggsurv$table + 
  ggtitle("Number of players at risk") + 
  theme(plot.subtitle = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.3)),
        axis.text = element_text(size = rel(1.3)))

ggsurv$plot <- ggsurv$plot + 
  theme(plot.title = element_text(size = rel(1.5)),
        plot.subtitle = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text = element_text(size = rel(1.3)),
        legend.title = element_blank(),
        legend.text = element_text(size = rel(1.2)))
  
ggsurv

## -----------------------------------------------------------------------------
## create positionb column 
## (so that the categories are: Attack, Defender, Goalkeeper and Midfield)
injd1819_sub <- mutate(injd1819_sub,
                   positionb = factor(str_split_i(position, "_", 1)))

## -----------------------------------------------------------------------------
cfit <- coxph(Surv(tstop_day, status) ~ positionb + age + yellows,
              data = injd1819_sub |> 
                filter(positionb != "Goalkeeper") |> droplevels())

## -----------------------------------------------------------------------------
summary(cfit)

## ---- fig.width = 10, fig.height = 5.4----------------------------------------
ggforest(model = cfit,
         data = injd1819_sub |> 
           filter(positionb != "Goalkeeper") |> as.data.frame(),
         fontsize = 1.2)

## -----------------------------------------------------------------------------
cox.zph(cfit)

## ---- fig.width = 10, fig.height = 8------------------------------------------
ggcoxzph(cox.zph(cfit))

## -----------------------------------------------------------------------------
sfit <- coxph(Surv(tstart_day, tstop_day, status) ~ age + strata(seasonb), 
              data = injd_sub)

## -----------------------------------------------------------------------------
summary(sfit)

## -----------------------------------------------------------------------------
## surv estimates of a player of 18 year-old based on sfit
player1 <- data.frame(age = 18)
sfitn1 <- survfit(sfit, newdata = player1)
sfitn1 <- data.frame(surv = sfitn1$surv, 
                     time = sfitn1$time,
                     strata = c(rep(names(sfitn1$strata)[[1]], sfitn1$strata[[1]]),
                                rep(names(sfitn1$strata)[[2]], sfitn1$strata[[2]])),
                     age = 18) 
## surv estimates of a player of 36 year-old based on sfit
player2 <- data.frame(age = 36) 
sfitn2 <- survfit(sfit, newdata = player2)
sfitn2 <- data.frame(surv = sfitn2$surv, 
                     time = sfitn2$time,
                     strata = c(rep(names(sfitn2$strata)[[1]], sfitn2$strata[[1]]),
                                rep(names(sfitn2$strata)[[2]], sfitn2$strata[[2]])),
                     age = 36) 

## bind both data frames
sfitn <- bind_rows(sfitn1, sfitn2) |> 
  mutate(strata = factor(strata),
         Age = factor(age))

## ---- fig.width = 10, fig.height = 4.8----------------------------------------
ggplot(sfitn, aes(x = time, y = surv, col = strata)) +
  geom_step(aes(linetype = Age)) +
  scale_color_manual(name = "Season", values = c("#E7B800", "#2E9FDF")) +
  xlab("t [calendar days]") + ylab(expression(hat(S)(t))) +
  theme(legend.title = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.3)),
        axis.text = element_text(size = rel(1.4)),
        axis.title = element_text(size = rel(1.4)))

## -----------------------------------------------------------------------------
cox.zph(sfit)

## -----------------------------------------------------------------------------
## prepare exposure data set and create seasonb column
df_exposures <- prepare_exp(df_exposures0 = raw_df_exposures,
                             player        = "player_name",
                             date          = "year",
                             time_expo     = "minutes_played") |> 
  mutate(seasonb = date2season(date))

## add more info to injd data frame (based on exposure data)
injd <- injd |> 
  mutate(seasonb = date2season(tstart)) |> 
  left_join(df_exposures, by = c("player" = "player",
                                  "seasonb" = "seasonb")) |> 
  mutate(positionb = factor(str_split_i(position, "_", 1)),
         injury_severity = addNA(injury_severity),
         days_lost = lag(days_lost, default = 0),
         games_lost = lag(games_lost, default = 0),
         prev_inj = lag(enum, default = 0))

## -----------------------------------------------------------------------------
injd <- injd |> 
  mutate(tstop_minPlay = ifelse(tstop_minPlay == tstart_minPlay,
                                tstop_minPlay + 1, tstop_minPlay)) |> 
  filter(positionb != "Goalkeeper") |> 
  droplevels()

## ---- warning = F-------------------------------------------------------------
sffit <- coxph(Surv(tstart_minPlay, tstop_minPlay, status) ~ 
                 age + days_lost + 
                 frailty(player, distribution = "gamma"), data = injd)

## -----------------------------------------------------------------------------
sffit2 <- coxme(Surv(tstart_minPlay, tstop_minPlay, status) ~ 
                  age + days_lost + (1 | player), data = injd)

## ---- warning = F-------------------------------------------------------------
summary(sffit)

## -----------------------------------------------------------------------------
summary(sffit2)

## ---- warning = F-------------------------------------------------------------
## plot p1, for covariate effects
## a trick to not to plot frailties as HRs
sffit_aux <- sffit
attr(sffit_aux$terms, "dataClasses") <- 
  attr(sffit_aux$terms, "dataClasses")[1:3] 
p1 <- ggforest(sffit_aux, data = injd,
               fontsize = 0.8)

## ---- eval = F----------------------------------------------------------------
#  ## plot p2, for frailty terms
#  df_frailties <- data.frame(player = levels(injd$player),
#                             frail = sffit$frail,
#                             expfrail = exp(sffit$frail),
#                             col = factor(ifelse(sffit$frail >= 0, 1, 0)))
#  p2 <- ggplot(df_frailties) +
#    geom_segment(aes(x = player, xend = player,
#                     y = 1, yend = expfrail, col = col)) +
#    geom_hline(yintercept = 1) +
#    geom_text(aes(x = player, y = expfrail + 0.12*sign(frail), label = player),
#              size = 3, angle = 30) +
#    scale_color_manual(name = "", values = c("darkred", "dodgerblue"))

## -----------------------------------------------------------------------------
df_frailties <- data.frame(player = levels(injd$player), 
                           frail = sffit$frail,
                           expfrail = exp(sffit$frail),
                           col = factor(ifelse(sffit$frail >= 0, 1, 0)))
p2 <- ggplot(df_frailties) +
  geom_segment(aes(x = player, xend = player,
                   y = 1, yend = expfrail, col = col)) +
  geom_hline(yintercept = 1) + 
  geom_text(aes(x = player, y = expfrail + 0.12*sign(frail), label = player),
            size = 3, angle = 15) +
  scale_color_manual(name = "", values = c("darkred", "dodgerblue")) + 
  scale_x_discrete(expand = c(0.08, 0)) +
  scale_y_continuous(expand = c(0.2, 0)) + 
  ylab(expression(exp*"(frail)")) + xlab("Player") +
  ggtitle("Frailties") + 
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_text(size = rel(1.2)),
        axis.text.y = element_text(size = rel(1.2)),
        plot.title = element_text(size = rel(1.4), hjust = 0.5))

## ---- fig.widht = 10, fig.height = 6.8----------------------------------------
grid.arrange(p1, p2, nrow = 2, heights = c(1, 1.3))

