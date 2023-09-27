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

## ----setup, warning=FALSE, message=FALSE--------------------------------------
library(injurytools)
library(dplyr)      
library(knitr)      
library(kableExtra) 

## ---- eval = FALSE------------------------------------------------------------
#  df_exposures <- prepare_exp(raw_df_exposures, player = "player_name",
#                              date = "year", time_expo = "minutes_played")
#  df_injuries  <- prepare_inj(raw_df_injuries, player = "player_name",
#                              date_injured = "from", date_recovered = "until")
#  injd         <- prepare_all(data_exposures = df_exposures,
#                              data_injuries  = df_injuries,
#                              exp_unit = "matches_minutes")

## -----------------------------------------------------------------------------
injds <- injsummary(injd)

## -----------------------------------------------------------------------------
str(injds, 1)

## ---- eval = F----------------------------------------------------------------
#  # the 'playerwise' data frame
#  injds[[1]]

## ---- eval = F----------------------------------------------------------------
#  # format the 'playerwise' data frame for output as a table
#  injds[[1]] |>
#    arrange(desc(injincidence)) |> # sort by decreasing order of injincidence
#    head(10) |>
#    kable(digits = 2, col.names = c("Player", "N injuries", "N days lost",
#                                    "Mean days lost", "Median days lost", "IQR days lost",
#                                    "Total exposure", "Incidence", "Burden"))

## ---- echo = F, eval = modern_r-----------------------------------------------
# format the 'playerwise' data frame for output as a table
injds[[1]] |> 
  arrange(desc(injincidence)) |> # sort by decreasing order of injincidence
  head(10) |>
  kable(digits = 2, col.names = c("Player", "N injuries", "N days lost", 
                                  "Mean days lost", "Median days lost", "IQR days lost",
                                  "Total exposure", "Incidence", "Burden"))

## ---- eval = F----------------------------------------------------------------
#  # the 'overall' data frame
#  injds[[2]]

## ---- eval = F----------------------------------------------------------------
#  # format the table of total incidence and burden (main columns)
#  injds[[2]] |>
#    select(1:8) |>
#    data.frame(row.names = "TOTAL") |>
#    kable(digits = 2,
#          col.names = c("N injuries", "N days lost", "Mean days lost",
#                        "Median days lost", "IQR days lost",
#                        "Total exposure", "Incidence", "Burden"),
#          row.names = TRUE) |>
#    kable_styling(full_width = FALSE)

## ---- echo = F, eval = modern_r-----------------------------------------------
# format the table of total incidence and burden (main columns)
injds[[2]] |> 
  select(1:8) |> 
  data.frame(row.names = "TOTAL") |> 
  kable(digits = 2,
        col.names = c("N injuries", "N days lost", "Mean days lost",
                      "Median days lost", "IQR days lost",
                      "Total exposure", "Incidence", "Burden"),
        row.names = TRUE) |> 
  kable_styling(full_width = FALSE)

## ---- eval = F----------------------------------------------------------------
#  # format the table of total incidence and burden (point + ci estimates)
#  injds_tot_cis <- injds[[2]] |>
#    select(7:last_col()) |>
#    data.frame(row.names = "TOTAL")
#  injds_tot_cis$ci_injincidence <- paste0("[",  round(injds_tot_cis$injincidence_lower, 1),
#                                          ", ", round(injds_tot_cis$injincidence_upper, 1), "]")
#  injds_tot_cis$ci_injburden    <- paste0("[",  round(injds_tot_cis$injburden_lower, 1),
#                                          ", ", round(injds_tot_cis$injburden_upper, 1), "]")
#  
#  conf_level <- attr(injds, "conf_level") * 100
#  
#  injds_tot_cis |>
#    select(1, 9, 2, 10) |>
#    kable(digits = 2,
#          col.names = c("Incidence",  paste0("CI ", conf_level, "% for \\(I_r\\)"),
#                        "Burden",     paste0("CI ", conf_level, "% for \\(I_{br}\\)")))

## ---- echo = F, eval = modern_r-----------------------------------------------
# format the table of total incidence and burden (point + ci estimates)
injds_tot_cis <- injds[[2]] |> 
  select(7:last_col()) |> 
  data.frame(row.names = "TOTAL")
injds_tot_cis$ci_injincidence <- paste0("[",  round(injds_tot_cis$injincidence_lower, 1),
                                        ", ", round(injds_tot_cis$injincidence_upper, 1), "]")
injds_tot_cis$ci_injburden    <- paste0("[",  round(injds_tot_cis$injburden_lower, 1),
                                        ", ", round(injds_tot_cis$injburden_upper, 1), "]")

conf_level <- attr(injds, "conf_level") * 100

injds_tot_cis |> 
  select(1, 9, 2, 10) |> 
  kable(digits = 2,
        col.names = c("Incidence",  paste0("CI ", conf_level, "% for \\(I_r\\)"), 
                      "Burden",     paste0("CI ", conf_level, "% for \\(I_{br}\\)")))

## -----------------------------------------------------------------------------
injstats_pertype <- injsummary(injd, var_type_injury = "injury_type", quiet = T)

## ---- eval = F----------------------------------------------------------------
#  injstats_pertype[["overall"]]

## ---- eval = F----------------------------------------------------------------
#  injstats_pertype[["overall"]] |>
#    select(1:5, 7:11) |>
#    mutate(ninjuries2 = paste0(ninjuries, " (", percent_ninjuries, ")"),
#           ndayslost2 = paste0(ndayslost, " (", percent_dayslost, ")"),
#           median_dayslost2 = paste0(median_dayslost, " (", iqr_dayslost, ")")) |>
#    select(1, 11:13, 8:10) |>
#    arrange(desc(injburden)) |>
#    kable(digits = 2,
#          col.names = c("Type of injury", "N injuries (%)", "N days lost (%)",
#                        "Median days lost (IQR)",
#                        "Total exposure", "Incidence", "Burden"),
#          row.names = TRUE) |>
#    kable_styling(full_width = FALSE)

## ---- echo = F, eval = modern_r-----------------------------------------------
injstats_pertype[["overall"]] |> 
  select(1:5, 7:11) |> 
  mutate(ninjuries2 = paste0(ninjuries, " (", percent_ninjuries, ")"),
         ndayslost2 = paste0(ndayslost, " (", percent_dayslost, ")"),
         median_dayslost2 = paste0(median_dayslost, " (", iqr_dayslost, ")")) |> 
  select(1, 11:13, 8:10) |> 
  arrange(desc(injburden)) |> 
  kable(digits = 2,
        col.names = c("Type of injury", "N injuries (%)", "N days lost (%)",
                      "Median days lost (IQR)",
                      "Total exposure", "Incidence", "Burden"),
        row.names = TRUE) |> 
  kable_styling(full_width = FALSE)

## ---- eval = FALSE------------------------------------------------------------
#  df_exposures <- prepare_exp(raw_df_exposures, player = "player_name",
#                              date = "year", time_expo = "minutes_played")
#  df_injuries  <- prepare_inj(raw_df_injuries, player = "player_name",
#                              date_injured = "from", date_recovered = "until")
#  injd         <- prepare_all(data_exposures = df_exposures,
#                              data_injuries  = df_injuries,
#                              exp_unit = "matches_minutes")

## -----------------------------------------------------------------------------
prev_table1 <- injprev(injd, by = "season")
prev_table1

## -----------------------------------------------------------------------------
kable(prev_table1,
      col.names = c("Season", "Status", "N", "Total", "%"))

## ---- eval = modern_r---------------------------------------------------------
prev_table2 <- injprev(injd, by = "monthly")

## compare two seasons July and August
prev_table2 |>
  group_by(season) |> 
  slice(1:4)


## compare two seasons January and February
prev_table2 |>
  group_by(season) |> 
  slice(13:16)

## -----------------------------------------------------------------------------
prev_table3 <- injprev(injd, by = "monthly", var_type_injury = "injury_type")

## ---- eval = F----------------------------------------------------------------
#  ## season 1
#  prev_table3 |>
#    filter(season == "season 2017/2018", month == "Jan") |>
#    kable(col.names = c("Season", "Month", "Status", "N", "Total", "%"),
#          caption = "Season 2017/2018") |>
#    kable_styling(full_width = FALSE, position = "float_left")
#  ## season 2
#  prev_table3 |>
#    filter(season == "season 2018/2019", month == "Jan") |>
#    kable(col.names = c("Season", "Month", "Status", "N", "Total", "%"),
#          caption = "Season 2018/2019") |>
#    kable_styling(full_width = FALSE, position = "left")

## ---- echo = F, eval = modern_r-----------------------------------------------
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

