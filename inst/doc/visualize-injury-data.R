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

## ----setup, message = F-------------------------------------------------------
library(injurytools)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(knitr)

## ---- fig.width = 13.7, fig.height = 7----------------------------------------
gg_injphoto(injd, 
            title = "Overview of injuries:\nLiverpool FC 1st male team during 2017-2018 and 2018-2019 seasons",
            by_date = "2 month", 
            fix = TRUE) +
  
  ## plus some lines of ggplot2 code..
  xlab("Follow-up date") + ylab("Players") + labs(caption = "source: transfermarkt.com") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 22),
        axis.text.x.bottom = element_text(size = 13, angle = 20, hjust = 1),
        axis.text.y.left = element_text(size = 12),
        axis.title.x = element_text(size = 20, face = "bold", vjust = -1),
        axis.title.y = element_text(size = 20, face = "bold", vjust = 1.8),
        legend.text = element_text(size = 20),
        plot.caption = element_text(face = "italic", size = 12, colour = "gray10"))

## ---- warning = FALSE---------------------------------------------------------
# warnings set to FALSE
injds <- injsummary(injd) 
injds_perinj <- injsummary(injd, var_type_injury = "injury_type") 
# injds

## ---- eval = F----------------------------------------------------------------
#  injds[["overall"]] %>%
#    mutate(incidence_new = paste0(round(injincidence, 2), " (", round(injincidence_lower, 2), ",", round(injincidence_upper, 2), ")"),
#           burden_new = paste0(round(injburden, 2), " (", round(injburden_lower, 2), ",", round(injburden_upper, 2), ")")) %>%
#    select(1:2, 6, incidence_new, burden_new) %>%
#    kable(col.names = c("N injuries", "N days lost", "Total expo", "Incidence (95% CI)", "Burden (95% CI)"),
#          caption = "Injury incidence and injury burden are reported as 100 player-matches",
#          align = "c")
#  
#  injds_perinj[["overall"]] %>%
#    mutate(incidence_new = paste0(round(injincidence, 2), " (", round(injincidence_lower, 2), ",", round(injincidence_upper, 2), ")"),
#           burden_new = paste0(round(injburden, 2), " (", round(injburden_lower, 2), ",", round(injburden_upper, 2), ")")) %>%
#    select(1:2, 4, 9, incidence_new, burden_new) %>%
#    kable(col.names = c("Type of injury", "N injuries", "N days lost", "Total expo", "Incidence (95% CI)", "Burden (95% CI)"),
#          caption = "Injury incidence and injury burden are reported as 100 player-matches",
#          align = "c")

## ---- echo = F----------------------------------------------------------------
injds[["overall"]] %>% 
  mutate(incidence_new = paste0(round(injincidence, 2), " (", round(injincidence_lower, 2), ",", round(injincidence_upper, 2), ")"),
         burden_new = paste0(round(injburden, 2), " (", round(injburden_lower, 2), ",", round(injburden_upper, 2), ")")) %>% 
  select(1:2, 6, incidence_new, burden_new) %>% 
  kable(col.names = c("N injuries", "N days lost", "Total expo", "Incidence (95% CI)", "Burden (95% CI)"),
        caption = "Injury incidence and injury burden are reported as 100 player-matches",
        align = "c")

## ---- echo = F----------------------------------------------------------------
injds_perinj[["overall"]] %>% 
  mutate(incidence_new = paste0(round(injincidence, 2), " (", round(injincidence_lower, 2), ",", round(injincidence_upper, 2), ")"),
         burden_new = paste0(round(injburden, 2), " (", round(injburden_lower, 2), ",", round(injburden_upper, 2), ")")) %>% 
  select(1:2, 4, 9, incidence_new, burden_new) %>% 
  kable(col.names = c("Type of injury", "N injuries", "N days lost", "Total expo", "Incidence (95% CI)", "Burden (95% CI)"),
        caption = "Injury incidence and injury burden are reported as 100 player-matches",
        align = "c")

## ---- eval = F----------------------------------------------------------------
#  # warnings set to FALSE
#  gg_injriskmatrix(injds_perinj,
#                   var_type_injury = "injury_type",
#                   title = "Risk matrix")

## ---- eval = F----------------------------------------------------------------
#  # warnings set to FALSE
#  palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
#               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#  # source of the palette: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
#  theme3 <- theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
#                  axis.text.x.bottom = element_text(size = 18),
#                  axis.text.y.left = element_text(size = 18),
#                  axis.title.x = element_text(size = 15),
#                  axis.title.y = element_text(size = 15),
#                  legend.title = element_text(size = 15),
#                  legend.text = element_text(size = 15))
#  
#  gg_injriskmatrix(injds_perinj,
#                   var_type_injury = "injury_type",
#                   title = "Risk matrix") +
#    scale_fill_manual(name = "Type of injury",
#                      values = palette[c(7, 8, 2:3, 5)]) +
#    theme3

## ---- echo = F, fig.width = 9, fig.height = 5.8, warning = F------------------
palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
             "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# source of the palette: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
theme3 <- theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
                axis.text.x.bottom = element_text(size = 18),
                axis.text.y.left = element_text(size = 18),
                axis.title.x = element_text(size = 15),
                axis.title.y = element_text(size = 15),
                legend.title = element_text(size = 15),
                legend.text = element_text(size = 15))

gg_injriskmatrix(injds_perinj, 
                 var_type_injury = "injury_type", 
                 title = "Risk matrix") +
  scale_fill_manual(name = "Type of injury",
                    values = palette[c(7:8, 2:3, 5)]) +
  theme3

## ---- warning=F---------------------------------------------------------------
# warnings set to FALSE
injd1 <- cut_injd(injd, datef = 2017)
injd2 <- cut_injd(injd, date0 = 2018)

## ---- eval = F----------------------------------------------------------------
#  ## Plot just for checking whether cut_injd() worked well
#  p1 <- gg_injphoto(injd1, fix = TRUE, by_date = "3 months")
#  p2 <- gg_injphoto(injd2, fix = TRUE, by_date = "3 months")
#  grid.arrange(p1, p2, ncol = 2)

## ---- echo = F, fig.width = 18, fig.height = 4--------------------------------
p1 <- gg_injphoto(injd1, fix = TRUE, by_date = "3 months")
p1$layers[[3]]$aes_params$size <- 2
p2 <- gg_injphoto(injd2, fix = TRUE, by_date = "3 months") 
p2$layers[[3]]$aes_params$size <- 2
grid.arrange(p1, p2, ncol = 2)

## ---- warning = FALSE---------------------------------------------------------
# warnings set to FALSE
injds1 <- injsummary(injd1)
injds2 <- injsummary(injd2)

## ---- eval = F----------------------------------------------------------------
#  ## **Season 2017/2018**
#  injds1[["overall"]] %>%
#    mutate(incidence_new = paste0(round(injincidence, 2), " (", round(injincidence_lower, 2), ",", round(injincidence_upper, 2), ")"),
#           burden_new = paste0(round(injburden, 2), " (", round(injburden_lower, 2), ",", round(injburden_upper, 2), ")")) %>%
#    select(1:2, 6, incidence_new, burden_new) %>%
#    kable(col.names = c("N injuries", "N days lost", "Total expo", "Incidence (95% CI)", "Burden (95% CI)"),
#          caption = "Injury incidence and injury burden are reported as 100 player-matches",
#          align = "c")
#  
#  ## **Season 2018/2019**
#  injds2[["overall"]] %>%
#    mutate(incidence_new = paste0(round(injincidence, 2), "  (", round(injincidence_lower, 2), ",", round(injincidence_upper, 2), ")"),
#           burden_new = paste0(round(injburden, 2), "  (", round(injburden_lower, 2), ",", round(injburden_upper, 2), ")")) %>%
#    select(1:2, 6, incidence_new, burden_new) %>%
#    kable(col.names = c("N injuries", "N days lost", "Total expo", "Incidence (95% CI)", "Burden (95% CI)"),
#          caption = "Injury incidence and injury burden are reported as 100 player-matches",
#          align = "c")

## ---- echo = F----------------------------------------------------------------
injds1[["overall"]] %>% 
  mutate(incidence_new = paste0(round(injincidence, 2), " (", round(injincidence_lower, 2), ",", round(injincidence_upper, 2), ")"),
         burden_new = paste0(round(injburden, 2), " (", round(injburden_lower, 2), ",", round(injburden_upper, 2), ")")) %>% 
  select(1:2, 6, incidence_new, burden_new) %>% 
  kable(col.names = c("N injuries", "N days lost", "Total expo", "Incidence (95% CI)", "Burden (95% CI)"),
        caption = "Injury incidence and injury burden are reported as 100 player-matches",
        align = "c")

## ---- echo = F----------------------------------------------------------------
injds2[["overall"]] %>% 
  mutate(incidence_new = paste0(round(injincidence, 2), "  (", round(injincidence_lower, 2), ",", round(injincidence_upper, 2), ")"),
         burden_new = paste0(round(injburden, 2), "  (", round(injburden_lower, 2), ",", round(injburden_upper, 2), ")")) %>% 
  select(1:2, 6, incidence_new, burden_new) %>% 
  kable(col.names = c("N injuries", "N days lost", "Total expo", "Incidence (95% CI)", "Burden (95% CI)"),
        caption = "Injury incidence and injury burden are reported as 100 player-matches",
        align = "c")

## -----------------------------------------------------------------------------
p11 <- gg_injbarplot(injds1)
p12 <- gg_injbarplot(injds1, type = "burden")
p21 <- gg_injbarplot(injds2)
p22 <- gg_injbarplot(injds2, type = "burden") 

# grid.arrange(p11, p21, p12, p22, nrow = 2)

## ---- eval = F----------------------------------------------------------------
#  theme2 <- theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 26),
#                  axis.text.x.bottom = element_text(size = 18),
#                  axis.text.y.left = element_text(size = 13),
#                  axis.title.x = element_text(size = 11, vjust = 1),
#                  axis.title.y = element_text(size = 22, face = "bold", vjust = 1))
#  
#  p11 <- p11 +
#    xlab("Injury incidence") +
#    ylab("Playerwise incidence (injuries per 100 player-match)") +
#    ggtitle("2017/2018 season") +
#    scale_y_continuous(limits = c(0, 80)) + ## same x axis
#    theme2 +
#    theme(plot.margin = margin(0.2, 0.2, 0.2, 0.5, "cm"))
#  p12 <- p12 +
#    xlab("Injury burden") +
#    ylab("Playerwise burden (days lost per 100 player-match)") +
#    scale_y_continuous(limits = c(0, 6110)) +
#    theme2 +
#    theme(plot.margin = margin(0.2, 0.2, 0.2, 0.65, "cm"))
#  
#  p21 <- p21 +
#    ylab("Playerwise incidence (injuries per 100 player-match)") +
#    ggtitle("2018/2019 season") +
#    scale_y_continuous(limits = c(0, 80)) +
#    theme2
#  p22 <-p22 +
#    ylab("Playerwise burden (days lost per 100 player-match)") +
#    scale_y_continuous(limits = c(0, 6110)) +
#    theme2
#  
#  grid.arrange(p11, p21, p12, p22, nrow = 2)

## ---- echo = F, fig.width = 14, fig.height = 11.8-----------------------------
theme2 <- theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 26),
                axis.text.x.bottom = element_text(size = 18),
                axis.text.y.left = element_text(size = 13),
                axis.title.x = element_text(size = 11, vjust = 1),
                axis.title.y = element_text(size = 22, face = "bold", vjust = 1))

p11 <- p11 + 
  xlab("Injury incidence") + 
  ylab("Playerwise incidence (injuries per 100 player-match)") +
  ggtitle("2017/2018 season") +
  scale_y_continuous(limits = c(0, 80)) + ## same x axis 
  theme2 +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.5, "cm"))
p12 <- p12 +
  xlab("Injury burden") + 
  ylab("Playerwise burden (days lost per 100 player-match)") +
  scale_y_continuous(limits = c(0, 6110)) + 
  theme2 +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.65, "cm"))

p21 <- p21 + 
  ylab("Playerwise incidence (injuries per 100 player-match)") +
  ggtitle("2018/2019 season") +
  scale_y_continuous(limits = c(0, 80)) + 
  theme2 
p22 <-p22 +
  ylab("Playerwise burden (days lost per 100 player-match)") +
  scale_y_continuous(limits = c(0, 6110)) + 
  theme2

grid.arrange(p11, p21, p12, p22, nrow = 2)

## ---- warning = F-------------------------------------------------------------
# warnings set to FALSE
## Calculate summary statistics
injds1_perinj <- injsummary(injd1, var_type_injury = "injury_type")
injds2_perinj <- injsummary(injd2, var_type_injury = "injury_type")

## Plot
p1 <- gg_injriskmatrix(injds1_perinj, var_type_injury = "injury_type", 
                       title = "Season 2017/2018", add_contour = FALSE)
p2 <- gg_injriskmatrix(injds2_perinj, var_type_injury = "injury_type",
                       title = "Season 2018/2019", add_contour = FALSE)

# Print both plots side by side
# grid.arrange(p1, p2, nrow = 1)

## ---- eval = F----------------------------------------------------------------
#  palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
#               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#  # source of the palette: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
#  theme3 <- theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
#                  axis.text.x.bottom = element_text(size = 18),
#                  axis.text.y.left = element_text(size = 18),
#                  axis.title.x = element_text(size = 15),
#                  axis.title.y = element_text(size = 15),
#                  legend.title = element_text(size = 15),
#                  legend.text = element_text(size = 15))
#  
#  
#  p1 <- p1 +
#    scale_x_continuous(limits = c(0, 5.9)) +
#    scale_y_continuous(limits = c(0, 165)) +
#    scale_fill_manual(name = "Type of injury",
#                      values = palette[c(1:3, 5)]) + # get rid off the green (pos: 4)
#    theme3
#  p2 <- p2 +
#    scale_x_continuous(limits = c(0, 5.9)) +
#    scale_y_continuous(limits = c(0, 165)) +
#    scale_fill_manual(name = "Type of injury",
#                      values = palette[c(7, 8, 2:3, 5)]) + # keep the same color coding
#    theme3
#  
#  grid.arrange(p1, p2, ncol = 2,
#               top = textGrob("Risk matrices", gp=gpar(fontsize = 26, font = 2))) ## for the main title

## ---- echo = F, fig.width = 13, fig.height = 5.8, warning = FALSE-------------
palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
             "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# source of the palette: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
theme3 <- theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
                axis.text.x.bottom = element_text(size = 18),
                axis.text.y.left = element_text(size = 18),
                axis.title.x = element_text(size = 15),
                axis.title.y = element_text(size = 15),
                legend.title = element_text(size = 15),
                legend.text = element_text(size = 15))


p1 <- p1 +
  scale_x_continuous(limits = c(0, 5.9)) +
  scale_y_continuous(limits = c(0, 165)) + 
  scale_fill_manual(name = "Type of injury",
                    values = palette[c(8, 2:3, 5)]) + # get rid off the green (pos: 4)
  theme3
p2 <- p2 +
  scale_x_continuous(limits = c(0, 5.9)) +
  scale_y_continuous(limits = c(0, 165)) + 
  scale_fill_manual(name = "Type of injury",
                    values = palette[c(7:8, 2:3, 5)]) + # keep the same color coding
  theme3

grid.arrange(p1, p2, ncol = 2, 
             top = textGrob("Risk matrices", gp=gpar(fontsize = 26, font = 2))) ## for the main title

## ---- eval = F----------------------------------------------------------------
#  gg_injprev_polar(injd, by = "monthly")

## ---- eval = F----------------------------------------------------------------
#  theme4 <- theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
#                  axis.text.x = element_text(size = 16),
#                  axis.text.y = element_text(size = 18),
#                  legend.title = element_text(size = 20),
#                  legend.text = element_text(size = 20),
#                  strip.text = element_text(size = 20))
#  
#  
#  gg_injprev_polar(injd, by = "monthly",
#              title = "Proportion of injured and available\n players in each month") +
#    scale_fill_manual(name = "Type of injury",
#                      values = c("seagreen3", "red3")) +
#    theme4

## ---- echo = F, fig.width = 10, fig.height = 7--------------------------------
theme4 <- theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
                axis.text.x = element_text(size = 16),
                axis.text.y = element_text(size = 18),
                legend.title = element_text(size = 20),
                legend.text = element_text(size = 20),
                strip.text = element_text(size = 20))


gg_injprev_polar(injd, by = "monthly",
            title = "Proportion of injured and available\n players in each month") +
  scale_fill_manual(name = "Type of injury", 
                    values = c("seagreen3", "red3")) + 
  theme4

## ---- eval = F----------------------------------------------------------------
#  gg_injprev_polar(injd, by = "monthly", var_type_injury = "injury_type")

## ---- eval = F----------------------------------------------------------------
#  palette2 <- c("seagreen3", "#000000", "#E69F00", "#56B4E9", "#009E73",
#               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#  # source of the palette: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
#  
#  
#  gg_injprev_polar(injd, by = "monthly",
#              var_type_injury = "injury_type",
#              title = "Proportion of injured and available\n players in each month according to the type of injury") +
#    scale_fill_manual(name = "Type of injury",
#                      values = palette2[c(1, 8:9, 3:4, 6)]) +
#    theme4

## ---- echo = F, fig.width = 10, fig.height = 7--------------------------------
palette2 <- c("seagreen3", "#000000", "#E69F00", "#56B4E9", "#009E73",
             "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# source of the palette: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/


gg_injprev_polar(injd, by = "monthly", 
                 var_type_injury = "injury_type",
                 title = "Proportion of injured and available\n players in each month according to the type of injury") +
  scale_fill_manual(name = "Type of injury", 
                    values = palette2[c(1, 8:9, 3:4, 6)]) + 
  theme4

