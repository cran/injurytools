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

## ----setup, message = F, warning = F------------------------------------------
library(injurytools)
library(dplyr)
library(stringr)
library(tidyr)
library(lme4)
library(pscl)
# library(glmmTMB)
library(MASS)
library(ggplot2)
library(gridExtra)
library(knitr)

## ----setup2, echo = F---------------------------------------------------------
# set the global theme of all plots
theme_set(theme_bw())

## ----datasetup1, warning = F--------------------------------------------------
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

## ----datasetup2, warning = F--------------------------------------------------
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

## ---- warning = F-------------------------------------------------------------
## calculate injury summary statistics
injds1718 <- injsummary(injd1718, quiet = T)
injds1819 <- injsummary(injd1819, quiet = T)

injds1718p <- injds1718$playerwise
injds1819p <- injds1819$playerwise

injdsp <- bind_rows("Season 17-18" = injds1718p,
                    "Season 18-19" = injds1819p,
                    .id = "season")

## ---- eval = F----------------------------------------------------------------
#  ## plot
#  p1 <- ggplot(data = injdsp) +
#    geom_histogram(aes(x = injincidence, fill = season)) +
#    facet_wrap(~season) +
#    scale_fill_manual(name = "", values = c("#E7B800", "#2E9FDF")) +
#    ylab("Number of players") +
#    xlab("Incidence (number of injuries per 100 player-match)") +
#    ggtitle("Histogram of injury incidence in each season") +
#    theme(legend.position = "none")
#  
#  p2 <- ggplot(data = injdsp) +
#    geom_histogram(aes(x = injburden, fill = season)) +
#    facet_wrap(~season) +
#    scale_fill_manual(name = "", values = c("#E7B800", "#2E9FDF")) +
#    ylab("Number of players") +
#    xlab("Burden (number of days lost due to injury per 100 player-match)") +
#    ggtitle("Histogram of injury burden in each season") +
#    theme(legend.position = "none")
#  
#  grid.arrange(p1, p2, ncol = 1)

## ---- echo = F, warning = F---------------------------------------------------
p1 <- ggplot(data = injdsp) + 
  geom_histogram(aes(x = injincidence, fill = season)) + 
  facet_wrap(~season) +
  scale_fill_manual(name = "", values = c("#E7B800", "#2E9FDF")) +
  ylab("Number of players") + 
  xlab("Incidence (number of injuries per 100 player-match)") +
  ggtitle(bquote("Histogram of injury" ~ bold("incidence") ~ "in each season")) + 
  theme(legend.position = "none")

p2 <- ggplot(data = injdsp) + 
  geom_histogram(aes(x = injburden, fill = season)) + 
  facet_wrap(~season) +
  scale_fill_manual(name = "", values = c("#E7B800", "#2E9FDF")) +
  ylab("Number of players") + 
  xlab("Burden (number of days lost due to injury per 100 player-match)") +
  ggtitle(bquote("Histogram of injury" ~ bold("burden") ~ "in each season")) + 
  theme(legend.position = "none")

## ---- eval = F----------------------------------------------------------------
#  theme_counts <- theme(axis.text = element_text(size = rel(1.2)),
#                        axis.title = element_text(size = rel(1.3)),
#                        strip.text = element_text(size = rel(1.4)),
#                        plot.title = element_text(size = rel(1.4)),
#                        legend.text = element_text(size =  rel(1.3)),
#                        legend.title = element_text(size = rel(1.3)))
#  p1 <- p1 + theme_counts
#  p2 <- p2 + theme_counts

## ---- warning = F, message = F, echo = F, fig.width = 10, fig.height = 6.8----
theme_counts <- theme(axis.text = element_text(size = rel(1.2)),
                      axis.title = element_text(size = rel(1.3)),
                      strip.text = element_text(size = rel(1.4)),
                      plot.title = element_text(size = rel(1.4)),
                      legend.text = element_text(size =  rel(1.3)),
                      legend.title = element_text(size = rel(1.3)))
p1 <- p1 + theme_counts
p2 <- p2 + theme_counts

grid.arrange(p1, p2, ncol = 1)

## -----------------------------------------------------------------------------
## 17/18
df_exposures1718 <- prepare_exp(df_exposures0 = 
                                  raw_df_exposures |> filter(season == "17/18"),
                                player        = "player_name",
                                date          = "year",
                                time_expo     = "minutes_played") |> 
  mutate(seasonb = date2season(date))

injds1718p <- injds1718p |> 
  mutate(seasonb = "2017/2018") |> 
  ## join to have info, such as position, age, citizenship etc.
  left_join(df_exposures1718, by = c("player" = "player", 
                                     "seasonb" = "seasonb")) |> 
  ## create positionb column 
  ## (so that the categories are: Attack, Defender, Goalkeeper and Midfield)
  mutate(positionb = factor(str_split_i(position, "_", 1)))

## -----------------------------------------------------------------------------
## quit Goalkeepers
injds1718p <- dplyr::filter(injds1718p, positionb != "Goalkeeper") |> 
  droplevels()

## -----------------------------------------------------------------------------
incidence_glm_pois <- glm(ninjuries ~ positionb, # + offset(log(totalexpo))
                          offset = log(totalexpo),
                          data = injds1718p,
                          family = poisson)

## ---- eval = F----------------------------------------------------------------
#  # incidence_glm_pois2 <- glmmTMB(formula = ninjuries ~ foot,
#  #                                offset = log(totalexpo),
#  #                                family = poisson(), data = injds1718p)
#  # summary(incidence_glm_pois2)

## -----------------------------------------------------------------------------
df_exposures <- prepare_exp(df_exposures0 = raw_df_exposures,
                                player        = "player_name",
                                date          = "year",
                                time_expo     = "minutes_played") |> 
  mutate(seasonb = date2season(date))

injdsp <- injdsp |> 
  mutate(seasonb = if_else(season == "Season 17-18", "2017/2018", "2018/2019")) |> 
  ## join to have info, such as position, age, citizenship etc.
  left_join(df_exposures, by = c("player" = "player",
                                 "seasonb" = "seasonb")) |> 
  ## create positionb column 
  ## (so that the categories are: Attack, Defender, Goalkeeper and Midfield)
  mutate(positionb = factor(str_split_i(position, "_", 1))) |> 
  droplevels()

## ---- eval = F----------------------------------------------------------------
#  incidence_glmm_pois <- glmer(formula = ninjuries ~ positionb + (1 | player),
#                               offset = log(totalexpo),
#                               data = injdsp,
#                               family = poisson)
#  # incidence_glmm_pois2 <- glmmTMB::glmmTMB(formula = ninjuries ~ positionb + (1 | player),
#  #                                          offset = log(totalexpo),
#  #                                          data = injdsp,
#  #                                          family = poisson)

## -----------------------------------------------------------------------------
burden_glm_pois <- glm(ndayslost ~ positionb, offset = log(totalexpo), ## or ~ foot
                       data = injds1718p,
                       family = poisson)

## -----------------------------------------------------------------------------
summary(burden_glm_pois)

## -----------------------------------------------------------------------------
cbind(estimate = exp(coef(burden_glm_pois)) * c(90*100, 1, 1), 
      exp(confint(burden_glm_pois)) * c(90*100, 1, 1)) |> # (to report per 100 player-matches)
  kable()

## ---- warning = F-------------------------------------------------------------
burden_glm_nb <- glm.nb(ndayslost ~ positionb + offset(log(totalexpo)),
                           data = injds1718p)

## -----------------------------------------------------------------------------
summary(burden_glm_nb)

## -----------------------------------------------------------------------------
burden_zinfpois <- zeroinfl(ndayslost ~ positionb | positionb,
                             offset = log(totalexpo),
                             data = injds1718p,
                             link = "logit",
                             dist = "poisson",
                             trace = FALSE, EM = FALSE)
## Or:
# burden_zinfpois <- glmmTMB::glmmTMB(formula = ndayslost ~ 1 +  positionb,
#                                     offset = log(totalexpo),
#                                     ziformula = ~ 1 + positionb,
#                                     data = injds1718p,
#                                     family = poisson)

## -----------------------------------------------------------------------------
summary(burden_zinfpois)

## -----------------------------------------------------------------------------
burden_zinfnb <- zeroinfl(ndayslost ~ positionb | positionb,
                             offset = log(totalexpo),
                             data = injds1718p,
                             link = "logit",
                             dist = "negbin",
                             trace = FALSE, EM = FALSE)
## Or:
# burden_zinfnb <- glmmTMB::glmmTMB(ndayslost ~ 1 + positionb, offset = log(totalexpo),
#                                      ziformula = ~ 1 + positionb,
#                                      data = injds1718p,
#                                      family = nbinom2)

## -----------------------------------------------------------------------------
summary(burden_zinfnb)

## -----------------------------------------------------------------------------
## pois
## predprob: for each subj, prob of observing each value
phat_pois <- predprob(burden_glm_pois) 
phat_pois_mn <- apply(phat_pois, 2, mean) 
## nb
phat_nb <- predprob(burden_glm_nb)           
phat_nb_mn <- apply(phat_nb, 2, mean) 
## zinfpois
phat_zinfpois <- predprob(burden_zinfpois)            
phat_zinfpois_mn <- apply(phat_zinfpois, 2, mean) 
## zinfnb
phat_zinfnb <- predprob(burden_zinfnb)           
phat_zinfnb_mn <- apply(phat_zinfnb, 2, mean) 

## put in a data frame
idx <- seq(1, 62, length.out = 30)
df_probs <- data.frame(phat_pois_mn     = phat_pois_mn[idx],
                       phat_nb_mn       = phat_nb_mn[idx],
                       phat_zinfpois_mn = phat_zinfpois_mn[idx],
                       phat_zinfnb_mn   = phat_zinfnb_mn[idx], 
                       x= idx) |> 
  tidyr::gather(key = "prob_type", value = "value", -x) |> 
  mutate(prob_type = factor(prob_type))

## ---- fig.width = 9, fig.height = 4.8-----------------------------------------
ggplot(data = injds1718p) + 
  geom_histogram(aes(x = injburden/100, after_stat(density)), 
                 breaks = seq(-0.5, 62, length.out = 30),
                 col = "black", alpha = 0.5) +
  geom_point(data = df_probs, aes(x = x, y = value, 
                                  group = prob_type, col = prob_type)) + 
  geom_line(data = df_probs, aes(x = x, y = value, 
                                 group = prob_type, col = prob_type)) + 
  ylim(c(0, 0.3)) + xlab("Injury burden") + ylab("Density") +
  scale_color_manual(name = "Model:",
                     labels = c("Negative Binomial", "Poisson",
                                "Zero-Inflated Negative Binomial",
                                "Zero-Inflated Poisson"),
                     values = c("darkblue", "chocolate", "purple", "red")) +
  ggtitle("Histogram of injury burden in 2017/2018\nwith conditional Poisson, NB, ZIP and ZINB Densities") +
  theme_counts +
  theme(legend.position = c(0.7, 0.7))

## ---- echo = F, eval = F------------------------------------------------------
#  ## using base R
#  with(injds1718p, {
#    hist(ndayslost, prob = TRUE, breaks = seq(-0.5, 316.5, length.out = 30),
#         xlab = "Injury burden (number of injuries per player-season)",
#         main = "Histogram of overall injury burden\nwith conditional Poisson, NB, ZIP and ZINB Densities")
#    lines(x = idx, y = phat_pois_mn[idx], type = "b", lwd = 2, col = "black")
#    lines(x = idx, y = phat_nb_mn[idx], type = "b", lwd = 2, col = "purple")
#    lines(x = idx, y = phat_zinfpois_mn[idx], type = "b", lwd = 2, col = "red")
#    lines(x = idx, y = phat_zinfnb_mn[idx], type = "b", lwd = 2, col = "blue")
#  })
#  legend(250, 0.026, c("Poisson", "NB", "ZIP","ZINB"), lty = 1,
#         col = c("black", "purple", "red","blue"), lwd = 2)
#  

## -----------------------------------------------------------------------------
models <- list("Poisson model" = burden_glm_pois,
               "Negative binomial model" = burden_glm_nb,
               "Zero-inflated Poisson model" = burden_zinfpois,
               "Zero-inflated Negative Binomial model" = burden_zinfnb)

res_gof <- lapply(models, function(model) {
  aic      <- AIC(model)
  bic      <- BIC(model)
  if (class(model)[[1]] == "zeroinfl") {
    deviance <- -2*logLik(model)[[1]]
    null_model <- update(model, .~ -positionb)
    null_deviance <- -2*logLik(null_model)[[1]]
  } else {
    deviance <- model$deviance
    null_deviance <- model$null.deviance
  }
  dev_expl <- (null_deviance - deviance)/null_deviance * 100
  return(res = data.frame(aic = aic, bic = bic, dev_expl = dev_expl))
})

## -----------------------------------------------------------------------------
res_gof |>   
  bind_rows(.id = "model") |>  
  ## arrange them according to dev_expl.
  arrange(desc(dev_expl)) |> 
  knitr::kable(digits = 2,
               col.names = c("Model", "AIC", "BIC", "Deviance Explained"))

