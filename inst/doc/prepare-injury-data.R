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

## ----setup, warning = F, message = F------------------------------------------
library(injurytools)
library(dplyr)

## -----------------------------------------------------------------------------
head(raw_df_injuries)
head(raw_df_exposures)

## -----------------------------------------------------------------------------
df_injuries <- prepare_inj(df_injuries0   = raw_df_injuries,
                           player         = "player_name",
                           date_injured   = "from",
                           date_recovered = "until")

## -----------------------------------------------------------------------------
df_exposures <- prepare_exp(df_exposures0 = raw_df_exposures,
                            player        = "player_name",
                            date          = "year",
                            time_expo     = "minutes_played")

## -----------------------------------------------------------------------------
## a possible way for the case where each row in exposure data correspond to a
## season and there is no more information about time of exposure
raw_df_exposures$time_expo_aux <- 1 
df_exposures2 <- prepare_exp(df_exposures0 = raw_df_exposures,
                             player        = "player_name",
                             date          = "year",
                             time_expo     = "time_expo_aux")

## note 'tstart_s' and 'tstop_s' columns
injd <-  prepare_all(data_exposures = df_exposures2,
                     data_injuries  = df_injuries,
                     exp_unit = "seasons")
head(injd)

## -----------------------------------------------------------------------------
injd <-  prepare_all(data_exposures = df_exposures,
                    data_injuries  = df_injuries,
                    exp_unit = "matches_minutes")
head(injd)
# injd %>% 
#   group_by(player) %>% 
#   slice(1, n())

## -----------------------------------------------------------------------------
class(injd)

## -----------------------------------------------------------------------------
str(injd, 1)

## -----------------------------------------------------------------------------
attr(injd, "unit_exposure")

