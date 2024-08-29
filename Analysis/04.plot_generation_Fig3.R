# PLOT GENERATION


# libraries
library(tidyverse)
library(tidybayes)
library(ggpubr)
library(ggthemes)
library(ggsci)
library(mgcv)
library(patchwork)

## Import corresponding datasets (data wrangling)


### Data for MODEL A ###

# read model file
predictions <- readRDS("pred_folds_rf_A.Rda")

# Median for each row in prediction matrix
newdata <- apply(predictions, 1, median_qi)

# final data for model A
data_plot_A <- do.call(rbind, newdata)

# import data test 
data_test_A <- read.csv(paste(getwd(), "/data_test_A.csv", sep = ""))


### Data for MODEL B ###

# read model file
predictions <- readRDS("pred_folds_rf_B.Rda")

# Median for each row in prediction matrix
newdata <- apply(predictions, 1, median_qi)

# final data for model A
data_plot_B <- do.call(rbind, newdata)

# import data test 
data_test_B <- read.csv(paste(getwd(), "/data_test_B.csv", sep = ""))


### Data for MODEL C ###

# read model file
predictions <- readRDS("pred_folds_rf.Rda")

# Median for each row in prediction matrix
newdata <- apply(predictions, 1, median_qi)

# final data for model A
data_plot_C <- do.call(rbind, newdata)

# import data test 
data_test_C <- read.csv(paste(getwd(), "/data_test_C.csv", sep = ""))



# PANELS A,B, and C

## Hours spent from general admission to ICU admission

# First, we have to calculate the predictions for the 95% CI lower and upper bounds
## 95% CI lower bound calculation
predictions.low <- data_test_A %>%
  mutate(pred = data_plot_A$y,
         pred.lower = data_plot_A$ymin,
         pred.upper = data_plot_A$ymax) %>%
  filter(prev_dept_los_hrs < 100) %>%
  gam(pred.lower ~ s(prev_dept_los_hrs, bs = "cs"), data = .) %>%
  predict()

## 95% CI upper bound calculation
predictions.up <- data_test_A %>%
  mutate(pred = data_plot_A$y,
         pred.lower = data_plot_A$ymin,
         pred.upper = data_plot_A$ymax) %>%
  filter(prev_dept_los_hrs < 100) %>%
  gam(pred.upper ~ s(prev_dept_los_hrs, bs = "cs"), data = .) %>%
  predict()

### plot!
plot.prev.A <- data_test_A %>%
  mutate(pred = data_plot_A$y) %>%
  filter(prev_dept_los_hrs < 100) %>%
  mutate(pred.lower = predictions.low,
         pred.upper = predictions.up) %>%
  ggplot(aes(x = prev_dept_los_hrs, y = pred)) +
  geom_ribbon(aes(ymin = pred.lower,
                  ymax = pred.upper),
              fill = "#2e86de",
              alpha = 0.3) + 
  geom_smooth(fill = "grey20",
              col = "black",
              se = FALSE) +
  labs(x = "Hours from general admission to ICU admission",
       y = "",
       col = "Actual status",
       shape = "Actual status") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, 0.25)) +
  ggsci::scale_color_lancet() +
  guides(col = guide_legend(override.aes = list(alpha = 1)))



# Age

# First, we have to calculate the predictions for the 95% CI lower and upper bounds
## 95% CI lower bound calculation
predictions.low <- data_test_A %>%
  mutate(pred = data_plot_A$y,
         pred.lower = data_plot_A$ymin,
         pred.upper = data_plot_A$ymax) %>%
  gam(pred.lower ~ s(age, bs = "cs"), data = .) %>%
  predict()

## 95% CI upper bound calculation
predictions.up <- data_test_A %>%
  mutate(pred = data_plot_A$y,
         pred.lower = data_plot_A$ymin,
         pred.upper = data_plot_A$ymax) %>%
  gam(pred.upper ~ s(age, bs = "cs"), data = .) %>%
  predict()

### plot!
plot.age.A <- data_test_A %>%
  mutate(pred = data_plot_A$y) %>%
  mutate(pred.lower = predictions.low,
         pred.upper = predictions.up) %>%
  ggplot(aes(x = age, y = pred)) +
  geom_ribbon(aes(ymin = pred.lower,
                  ymax = pred.upper),
              fill = "#2e86de",
              alpha = 0.3) + 
  geom_smooth(fill = "grey20",
              col = "black",
              se = FALSE) +
  labs(x = "Age",
     y = "") +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(breaks = seq(0, 1, 0.25),
                     limits = c(0, 1))


# Cardiac arrest 

## plot!
plot.CA_A <- data_test_A %>%
  mutate(pred = data_plot_A$y,
         pred.lower = data_plot_A$ymin,
         pred.upper = data_plot_A$ymax) %>%
  group_by(Cardiac_arrest) %>%
  summarise(pred.mean = mean(pred),
            pred.low = mean(pred.lower),
            pred.up = mean(pred.upper)) %>%
  ungroup() %>%
  ggplot(aes(x = factor(Cardiac_arrest), y = pred.mean)) +
  geom_errorbar(aes(ymin = pred.low,
                    ymax = pred.up,
                    y = pred.mean), width = 0.1) +
  geom_point(size = 4) +
  theme_bw() +
  labs(y = "",
       x = "Patient suffered a cardiac arrest",
       col = "Actual status",
       shape = "Actual status") +
  scale_x_discrete(labels = c("No",
                              "Yes")) +
  scale_y_continuous(breaks = seq(0, 1, 0.25),
                     limits = c(0, 1)) +
  ggsci::scale_color_lancet() +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(override.aes = list(alpha = 1)))



### plot list generation
plotlist <- ggarrange(plotlist = list(plot.prev.A, plot.age.A, plot.CA_A), common.legend = TRUE, nrow = 1,
                      legend = "bottom",
                      labels = c("A", "B", "C"))

# final panel creation
dw1 <- annotate_figure(plotlist,
                top = "Data window A (from 24 hours before ICU admission to ICU admission)")





# PANELS D, E, and F

# Norepinephrine administration
plot.nor.B <- data_test_B %>%
  mutate(norepinephrine = factor(norepinephrine)) %>%
  mutate(pred = data_plot_B$y,
         pred.lower = data_plot_B$ymin,
         pred.upper = data_plot_B$ymax) %>%
  group_by(norepinephrine) %>%
  summarise(pred.mean = mean(pred),
            pred.low = mean(pred.lower),
            pred.up = mean(pred.upper)) %>%
  ungroup() %>%
  ggplot(aes(x = norepinephrine, y = pred.mean)) +
  geom_errorbar(aes(ymin = pred.low,
                    ymax = pred.up,
                    y = pred.mean), width = 0.1) +
  geom_point(size = 4) +
  theme_bw() +
  labs(y = "",
       x = "Patient treated with norepinephrine",
       col = "") +
  scale_x_discrete(labels = c("No",
                              "Yes")) +
  scale_y_continuous(breaks = seq(0, 1, 0.25),
                     limits = c(0, 1)) +
  theme(legend.position = "none")


# Respiratory rate

# First, we have to calculate the predictions for the 95% CI lower and upper bounds
## 95% CI lower bound calculation
predictions.low <- data_test_B %>%
  mutate(pred = data_plot_B$y,
         pred.lower = data_plot_B$ymin,
         pred.upper = data_plot_B$ymax) %>%
  gam(pred.lower ~ s(respiratoryrate_clear, bs = "cs"), data = .) %>%
  predict()

## 95% CI upper bound calculation
predictions.up <- data_test_B %>%
  mutate(pred = data_plot_B$y,
         pred.lower = data_plot_B$ymin,
         pred.upper = data_plot_B$ymax) %>%
  gam(pred.upper ~ s(respiratoryrate_clear, bs = "cs"), data = .) %>%
  predict()

### plot!
plot.resp.B <- data_test_B %>%
  mutate(pred = data_plot_B$y,
         pred.lower = predictions.low,
         pred.upper = predictions.up) %>%
  ggplot(aes(x = respiratoryrate_clear, y = round(pred, 2))) +
  geom_ribbon(aes(ymin = pred.lower,
                  ymax = pred.upper),
              fill = "#2e86de",
              alpha = 0.3) + 
  geom_smooth(fill = "grey50",
              col = "black",
              se = FALSE) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = "Respiratory rate difference",
       y = "",
       col = "Mortality") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(-50, 100, 10)) + 
  scale_y_continuous(breaks = seq(0, 1, 0.25),
                     limits = c(0, 1)) +
  ggsci::scale_color_lancet() +
  guides(col = guide_legend(override.aes = list(alpha = 1)))


# Temperature rate

# First, we have to calculate the predictions for the 95% CI lower and upper bounds
## 95% CI lower bound calculation
predictions.low <- data_test_B %>%
  mutate(pred = data_plot_B$y,
         pred.lower = data_plot_B$ymin,
         pred.upper = data_plot_B$ymax) %>%
  gam(pred.lower ~ s(temperature_clear, bs = "cs"), data = .) %>%
  predict()

## 95% CI upper bound calculation
predictions.up <- data_test_B %>%
  mutate(pred = data_plot_B$y,
         pred.lower = data_plot_B$ymin,
         pred.upper = data_plot_B$ymax) %>%
  gam(pred.upper ~ s(temperature_clear, bs = "cs"), data = .) %>%
  predict()

### plot!
plot.temp.B <- data_test_B %>%
  mutate(pred = data_plot_B$y,
         pred.lower = predictions.low,
         pred.upper = predictions.up) %>%
  dplyr::filter(between(temperature_clear, -2, 2)) %>%
  ggplot(aes(x = temperature_clear, y = round(pred, 2))) +
  geom_ribbon(aes(ymin = pred.lower,
                  ymax = pred.upper),
              fill = "#2e86de",
              alpha = 0.3) + 
  geom_smooth(col = "black",
              se = FALSE) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = "Temperature rate difference",
       y = "",
       col = "Mortality") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(-2, 2, 0.4)) + 
  scale_y_continuous(breaks = seq(0, 1, 0.25),
                     limits = c(0, 1)) +
  ggsci::scale_color_lancet() +
  guides(col = guide_legend(override.aes = list(alpha = 1)))


# plot list generation
plotlist <- ggarrange(plotlist = list(plot.nor.B, plot.resp.B, plot.temp.B), 
                      common.legend = TRUE, 
                      nrow = 1,
                      legend = "bottom",
                      labels = c("D", "E", "F"))

# final panel creation
dw2 <- annotate_figure(plotlist,
                       top = "Data window B (from 24 hours before ICU admission to 24 hours after ICU admission)")




# PANELS G, H, and I

# Norepinephrine administration
plot.nor.C <- data_test_C %>%
  mutate(norepinephrine = factor(norepinephrine)) %>%
  mutate(pred = data_plot_C$y,
         pred.lower = data_plot_C$ymin,
         pred.upper = data_plot_C$ymax) %>%
  group_by(norepinephrine) %>%
  summarise(pred.mean = mean(pred),
            pred.low = mean(pred.lower),
            pred.up = mean(pred.upper)) %>%
  ungroup() %>%
  ggplot(aes(x = norepinephrine, y = pred.mean)) +
  geom_errorbar(aes(ymin = pred.low,
                    ymax = pred.up,
                    y = pred.mean), width = 0.1) +
  geom_point(size = 4) +
  theme_bw() +
  labs(y = "",
       x = "Patient treated with norepinephrine",
       col = "") +
  scale_x_discrete(labels = c("No",
                              "Yes")) +
  scale_y_continuous(breaks = seq(0, 1, 0.25),
                     limits = c(0, 1)) +
  theme(legend.position = "none")


# Respiratory rate

# First, we have to calculate the predictions for the 95% CI lower and upper bounds
## 95% CI lower bound calculation
predictions.low <- data_test_C %>%
  mutate(pred = data_plot_C$y,
         pred.lower = data_plot_C$ymin,
         pred.upper = data_plot_C$ymax) %>%
  gam(pred.lower ~ s(respiratoryrate_clear, bs = "cs"), data = .) %>%
  predict()

## 95% CI upper bound calculation
predictions.up <- data_test_C %>%
  mutate(pred = data_plot_C$y,
         pred.lower = data_plot_C$ymin,
         pred.upper = data_plot_C$ymax) %>%
  gam(pred.upper ~ s(respiratoryrate_clear, bs = "cs"), data = .) %>%
  predict()

### plot!
plot.resp.C <- data_test_C %>%
  mutate(pred = data_plot_C$y,
         pred.lower = predictions.low,
         pred.upper = predictions.up) %>%
  ggplot(aes(x = respiratoryrate_clear, y = round(pred, 2))) +
  geom_ribbon(aes(ymin = pred.lower,
                  ymax = pred.upper),
              fill = "#2e86de",
              alpha = 0.3) + 
  geom_smooth(fill = "grey50",
              col = "black",
              se = FALSE) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = "Respiratory rate difference",
       y = "",
       col = "Mortality") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(-50, 100, 10)) + 
  scale_y_continuous(breaks = seq(0, 1, 0.25),
                     limits = c(0, 1)) +
  ggsci::scale_color_lancet() +
  guides(col = guide_legend(override.aes = list(alpha = 1)))


# Temperature rate

# First, we have to calculate the predictions for the 95% CI lower and upper bounds
## 95% CI lower bound calculation
predictions.low <- data_test_C %>%
  mutate(pred = data_plot_C$y,
         pred.lower = data_plot_C$ymin,
         pred.upper = data_plot_C$ymax) %>%
  gam(pred.lower ~ s(temperature_clear, bs = "cs"), data = .) %>%
  predict()

## 95% CI upper bound calculation
predictions.up <- data_test_C %>%
  mutate(pred = data_plot_C$y,
         pred.lower = data_plot_C$ymin,
         pred.upper = data_plot_C$ymax) %>%
  gam(pred.upper ~ s(temperature_clear, bs = "cs"), data = .) %>%
  predict()

### plot!
plot.temp.C <- data_test_C %>%
  mutate(pred = data_plot_C$y,
         pred.lower = predictions.low,
         pred.upper = predictions.up) %>%
  dplyr::filter(between(temperature_clear, -2, 2)) %>%
  ggplot(aes(x = temperature_clear, y = round(pred, 2))) +
  geom_ribbon(aes(ymin = pred.lower,
                  ymax = pred.upper),
              fill = "#2e86de",
              alpha = 0.3) + 
  geom_smooth(col = "black",
              se = FALSE) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = "Temperature rate difference",
       y = "",
       col = "Mortality") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(-2, 2, 0.4)) + 
  scale_y_continuous(breaks = seq(0, 1, 0.25),
                     limits = c(0, 1)) +
  ggsci::scale_color_lancet() +
  guides(col = guide_legend(override.aes = list(alpha = 1)))


# plot list generation
plotlist <- ggarrange(plotlist = list(plot.nor.C, plot.resp.C, plot.temp.C), 
                      common.legend = TRUE, 
                      nrow = 1,
                      legend = "bottom",
                      labels = c("G", "H", "I"))

# final panel creation
dw3 <- annotate_figure(plotlist,
                       top = "Data window C (from 24 hours before ICU admission to 48 hours after ICU admission)")



### panels' assembling ### 
final_plot <- (dw1 + theme(plot.margin = unit(c(0, 0, 20, 0), "pt"))) /
  (dw2 + theme(plot.margin = unit(c(0, 0, 20, 0), "pt"))) /
  (dw3 + theme(plot.margin = unit(c(0, 0, 20, 0), "pt")))

# adding y axis label
wrap_elements(final_plot) +
  labs(tag = "Predicted probability of death after 72 hours from ICU admission") +
  theme(
    plot.tag = element_text(size = rel(1), angle = 90),
    plot.tag.position = "left")
