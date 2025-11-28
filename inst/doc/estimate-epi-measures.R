## ----include = FALSE----------------------------------------------------------
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

## ----setup, warning=FALSE, message=FALSE--------------------------------------
library(injurytools)
library(dplyr)      
library(knitr)      
library(kableExtra) 

## ----eval = FALSE-------------------------------------------------------------
# df_exposures <- prepare_exp(raw_df_exposures, person_id = "player_name",
#                             date = "year", time_expo = "minutes_played")
# df_injuries  <- prepare_inj(raw_df_injuries, person_id = "player_name",
#                             date_injured = "from", date_recovered = "until")
# injd         <- prepare_all(data_exposures = df_exposures,
#                             data_injuries  = df_injuries,
#                             exp_unit = "matches_minutes")

## ----warnings = FALSE---------------------------------------------------------
df_summary   <- calc_summary(injd)
df_summary_p <- calc_summary(injd, overall = FALSE)

## ----eval = F-----------------------------------------------------------------
# # the 'playerwise' data frame
# df_summary_p

## ----eval = F-----------------------------------------------------------------
# # format the 'playerwise' data frame for output as a table
# df_summary_p |>
#   arrange(desc(incidence)) |> # sort by decreasing order of incidence
#   mutate(iqr_dayslost = paste0(qt25_dayslost, " - ", qt75_dayslost)) |>
#   select("person_id", "ncases", "ndayslost", "mean_dayslost",
#          "median_dayslost", "iqr_dayslost", "totalexpo",
#          "incidence", "burden") |>
#   head(10) |>
#   kable(digits = 2, col.names = c("Player", "N injuries", "N days lost",
#                                   "Mean days lost", "Median days lost",
#                                   "IQR days lost",
#                                   "Total exposure", "Incidence", "Burden"))

## ----echo = F, eval = modern_r------------------------------------------------
# format the 'playerwise' data frame for output as a table
df_summary_p |> 
  arrange(desc(incidence)) |> # sort by decreasing order of incidence
  mutate(iqr_dayslost = paste0(qt25_dayslost, " - ", qt75_dayslost)) |> 
  select("person_id", "ncases", "ndayslost", "mean_dayslost",
         "median_dayslost", "iqr_dayslost", "totalexpo",
         "incidence", "burden") |> 
  head(10) |>
  kable(digits = 2, col.names = c("Player", "N injuries", "N days lost", 
                                  "Mean days lost", "Median days lost", 
                                  "IQR days lost",
                                  "Total exposure", "Incidence", "Burden"))

## ----eval = F-----------------------------------------------------------------
# # the 'overall' data frame
# df_summary

## ----eval = F-----------------------------------------------------------------
# # format the table of total incidence and burden (main columns)
# df_summary |>
#   mutate(iqr_dayslost = paste0(qt25_dayslost, " - ", qt75_dayslost)) |>
#   select("ncases", "ndayslost", "mean_dayslost", "median_dayslost",
#          "iqr_dayslost", "totalexpo", "incidence", "burden") |>
#   data.frame(row.names = "TOTAL") |>
#   kable(digits = 2,
#         col.names = c("N injuries", "N days lost", "Mean days lost",
#                       "Median days lost", "IQR days lost",
#                       "Total exposure", "Incidence", "Burden"),
#         row.names = TRUE) |>
#   kable_styling(full_width = FALSE)

## ----echo = F, eval = modern_r------------------------------------------------
# format the table of total incidence and burden (main columns)
df_summary |> 
  mutate(iqr_dayslost = paste0(qt25_dayslost, " - ", qt75_dayslost)) |> 
  select("ncases", "ndayslost", "mean_dayslost", "median_dayslost",
         "iqr_dayslost", "totalexpo", "incidence", "burden") |> 
  data.frame(row.names = "TOTAL") |> 
  kable(digits = 2,
        col.names = c("N injuries", "N days lost", "Mean days lost",
                      "Median days lost", "IQR days lost",
                      "Total exposure", "Incidence", "Burden"),
        row.names = TRUE) |> 
  kable_styling(full_width = FALSE)

## ----eval = F-----------------------------------------------------------------
# # format the table of total incidence and burden (point + ci estimates)
# dfs_cis <- df_summary |>
#   select(starts_with("incid"), starts_with("burd")) |>
#   data.frame(row.names = "TOTAL")
# dfs_cis$ci_incidence <- paste0("[",  round(dfs_cis$incidence_lower, 1),
#                                         ", ", round(dfs_cis$incidence_upper, 1), "]")
# dfs_cis$ci_burden    <- paste0("[",  round(dfs_cis$burden_lower, 1),
#                                         ", ", round(dfs_cis$burden_upper, 1), "]")
# 
# conf_level <- 0.95 * 100
# 
# dfs_cis |>
#   select(1, 9, 5, 10) |>
#   kable(digits = 2,
#         col.names = c("Incidence", paste0(conf_level, "% CI for \\(I_r\\)"),
#                       "Burden",    paste0(conf_level, "% CI for \\(I_{br}\\)")))

## ----echo = F, eval = modern_r------------------------------------------------
# format the table of total incidence and burden (point + ci estimates)
dfs_cis <- df_summary |> 
  select(starts_with("incid"), starts_with("burd")) |> 
  data.frame(row.names = "TOTAL")
dfs_cis$ci_incidence <- paste0("[",  round(dfs_cis$incidence_lower, 1),
                                        ", ", round(dfs_cis$incidence_upper, 1), "]")
dfs_cis$ci_burden    <- paste0("[",  round(dfs_cis$burden_lower, 1),
                                        ", ", round(dfs_cis$burden_upper, 1), "]")

conf_level <- 0.95 * 100

dfs_cis |> 
  select(1, 9, 5, 10) |> 
  kable(digits = 2,
        col.names = c("Incidence", paste0(conf_level, "% CI for \\(I_r\\)"), 
                      "Burden",    paste0(conf_level, "% CI for \\(I_{br}\\)")))

## -----------------------------------------------------------------------------
dfs_pertype <- calc_summary(injd, by = "injury_type", quiet = T)

## ----eval = F-----------------------------------------------------------------
# dfs_pertype

## ----eval = F-----------------------------------------------------------------
# dfs_pertype |>
#   select(1:5, 9:15) |>
#   mutate(ncases2 = paste0(ncases, " (", percent_ncases, ")"),
#          ndayslost2 = paste0(ndayslost, " (", percent_ndayslost, ")"),
#          iqr_dayslost = paste0(qt25_dayslost, " - ", qt75_dayslost),
#          median_dayslost2 = paste0(median_dayslost, " (", iqr_dayslost, ")")) |>
#   select(1, 13:14, 16, 4:5, 12) |>
#   arrange(desc(burden)) |>
#   kable(digits = 2,
#         col.names = c("Type of injury", "N injuries (%)", "N days lost (%)",
#                       "Median days lost (IQR)",
#                       "Total exposure", "Incidence", "Burden"),
#         row.names = TRUE) |>
#   kable_styling(full_width = FALSE)

## ----echo = F, eval = modern_r------------------------------------------------
dfs_pertype |> 
  select(1:5, 9:15) |> 
  mutate(ncases2 = paste0(ncases, " (", percent_ncases, ")"),
         ndayslost2 = paste0(ndayslost, " (", percent_ndayslost, ")"),
         iqr_dayslost = paste0(qt25_dayslost, " - ", qt75_dayslost),
         median_dayslost2 = paste0(median_dayslost, " (", iqr_dayslost, ")")) |> 
  select(1, 13:14, 16, 4:5, 12) |> 
  arrange(desc(burden)) |> 
  kable(digits = 2,
        col.names = c("Type of injury", "N injuries (%)", "N days lost (%)",
                      "Median days lost (IQR)",
                      "Total exposure", "Incidence", "Burden"),
        row.names = TRUE) |> 
  kable_styling(full_width = FALSE)

## ----eval = FALSE-------------------------------------------------------------
# df_exposures <- prepare_exp(raw_df_exposures, person_id = "player_name",
#                             date = "year", time_expo = "minutes_played")
# df_injuries  <- prepare_inj(raw_df_injuries, person_id = "player_name",
#                             date_injured = "from", date_recovered = "until")
# injd         <- prepare_all(data_exposures = df_exposures,
#                             data_injuries  = df_injuries,
#                             exp_unit = "matches_minutes")

## -----------------------------------------------------------------------------
prev_table1 <- calc_prevalence(injd, time_period = "season")
prev_table1

## -----------------------------------------------------------------------------
kable(prev_table1,
      col.names = c("Season", "Status", "N", "Total", "%"))

## ----eval = modern_r----------------------------------------------------------
prev_table2 <- calc_prevalence(injd, time_period = "monthly")

## compare two seasons July and August
prev_table2 |>
  group_by(season) |> 
  slice(1:4)


## compare two seasons January and February
prev_table2 |>
  group_by(season) |> 
  slice(13:16)

## -----------------------------------------------------------------------------
prev_table3 <- calc_prevalence(injd, time_period = "monthly", by = "injury_type")

## ----eval = F-----------------------------------------------------------------
# ## season 1
# prev_table3 |>
#   filter(season == "season 2017/2018", month == "Jan") |>
#   kable(col.names = c("Season", "Month", "Status", "N", "Total", "%"),
#         caption = "Season 2017/2018") |>
#   kable_styling(full_width = FALSE, position = "float_left")
# ## season 2
# prev_table3 |>
#   filter(season == "season 2018/2019", month == "Jan") |>
#   kable(col.names = c("Season", "Month", "Status", "N", "Total", "%"),
#         caption = "Season 2018/2019") |>
#   kable_styling(full_width = FALSE, position = "left")

## ----echo = F, eval = modern_r------------------------------------------------
## season 1
prev_table3 |> 
  filter(season == "season 2017/2018", month == "Jan") |> 
  kable(col.names = c("Season", "Month", "Status", "N", "Total", "%"),
        caption = "Season 2017/2018") |> 
  kable_styling(full_width = FALSE, position = "left")
## season 2
prev_table3 |> 
  filter(season == "season 2018/2019", month == "Jan") |> 
  kable(col.names = c("Season", "Month", "Status", "N", "Total", "%"),
        caption = "Season 2018/2019") |> 
  kable_styling(full_width = FALSE, position = "left")

