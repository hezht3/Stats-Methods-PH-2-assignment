####################################### Problem Set 2 #######################################

# I. Multiple linear regression to compare “growth” for male and female infants (Analysis of
# Covariance or ANCOVA)

# Set up
require(tidyverse)
require(tidymodels)

setwd("D:/OneDrive - Johns Hopkins/Course/140.622.81 - Statistical Methods in Public Health II/Problem Set/jhsphbiostat622-assignment/Problem Set 2")

nepalA <- read_csv("./data/nepal_anthro.csv")

nepalData <- nepalA %>%   ### store this dataset under a new name
    filter(age <= 12) %>%   ### keep only children with age <= 12
    drop_na("height", "weight", "armcirc")   ### remove NAs


nepalData <- nepalData %>% 
    mutate(gender = recode_factor(sex, `1`="Male", `2`="Female"))
    ### the first level you designate will be the reference!


# Scatter plot
qplot(x = jitter(age), y = weight, color = gender, shape = gender, 
      data = nepalData, xlab = "Age in months", ylab = "Weight in kg") +
    theme(panel.background = element_rect(fill='transparent', color='gray'),
          panel.border = element_rect(fill='transparent', color='transparent'),
          panel.grid = element_line(colour = "grey92"), 
          panel.grid.minor = element_line(size = rel(0.5)),
          axis.title.x = element_text(size = 16, face = "bold", color = "black"),
          axis.title.y = element_text(size = 16, face = "bold", color = "black"),
          axis.text = element_text(size = 12, color = "black"),
          legend.title = element_text(size = 12, face = "bold", color = "black"),
          legend.text = element_text(size = 12, color = "black"),
          legend.position = "bottom")


# Linear regression
model1 <- linear_reg() %>%
    set_engine("lm") %>%
    fit(weight ~ age + gender, data = nepalData) %>%
    extract_fit_engine()
summary(model1)
anova(model1)

model2 <- linear_reg() %>%
    set_engine("lm") %>%
    fit(weight ~ age*gender, data = nepalData) %>%
    extract_fit_engine()
summary(model2)
anova(model2)


# Scatter plot with fitted line
qplot(x = jitter(age), y = weight, color = gender, shape = gender,
      data = nepalData, xlab = "Age in months", ylab = "Weight in kg") +
    geom_line(aes(x = age, y=model2$fitted.values, color=gender)) +
    theme(panel.background = element_rect(fill='transparent', color='gray'),
          panel.border = element_rect(fill='transparent', color='transparent'),
          panel.grid = element_line(colour = "grey92"), 
          panel.grid.minor = element_line(size = rel(0.5)),
          axis.title.x = element_text(size = 16, face = "bold", color = "black"),
          axis.title.y = element_text(size = 16, face = "bold", color = "black"),
          axis.text = element_text(size = 12, color = "black"),
          legend.title = element_text(size = 12, face = "bold", color = "black"),
          legend.text = element_text(size = 12, color = "black"),
          legend.position = "bottom")


# Residual plot
qplot(y = model2$residuals, x = jitter(age), color = gender, shape = gender, 
      data = nepalData, ylab = "Residuals", xlab = "Age in months") +
    theme(panel.background = element_rect(fill='transparent', color='gray'),
          panel.border = element_rect(fill='transparent', color='transparent'),
          panel.grid = element_line(colour = "grey92"), 
          panel.grid.minor = element_line(size = rel(0.5)),
          axis.title.x = element_text(size = 16, face = "bold", color = "black"),
          axis.title.y = element_text(size = 16, face = "bold", color = "black"),
          axis.text = element_text(size = 12, color = "black"),
          legend.title = element_text(size = 12, face = "bold", color = "black"),
          legend.text = element_text(size = 12, color = "black"),
          legend.position = "bottom")


qplot(y = model2$residuals, x = jitter(age), color = gender, shape = gender, 
      data = nepalData, ylab = "Residuals", xlab = "Age in months") +
    geom_smooth(method = "loess", formula = y ~ x) +
    theme(panel.background = element_rect(fill='transparent', color='gray'),
          panel.border = element_rect(fill='transparent', color='transparent'),
          panel.grid = element_line(colour = "grey92"), 
          panel.grid.minor = element_line(size = rel(0.5)),
          axis.title.x = element_text(size = 16, face = "bold", color = "black"),
          axis.title.y = element_text(size = 16, face = "bold", color = "black"),
          axis.text = element_text(size = 12, color = "black"),
          legend.title = element_text(size = 12, face = "bold", color = "black"),
          legend.text = element_text(size = 12, color = "black"),
          legend.position = "bottom")


# Linear spline
nepalData <- nepalData %>%
    mutate(agesp = ifelse(age > 4, age-4, 0))

model3 <- linear_reg() %>%
    set_engine("lm") %>%
    fit(weight ~ age*gender + agesp*gender, data=nepalData) %>%
    extract_fit_engine()
summary(model3) %>% tidy()
confint(model3)


# Testing on spline term and residual plot
anova(model2, model3)
survey::regTermTest(model3, ~ agesp + agesp:gender)

qplot(y = model3$residuals, x = jitter(age), data = nepalData,
      ylab = "Residuals", xlab = "Age in months") +
    geom_hline(yintercept = 0, color = "red") +
    theme(panel.background = element_rect(fill='transparent', color='gray'),
          panel.border = element_rect(fill='transparent', color='transparent'),
          panel.grid = element_line(colour = "grey92"), 
          panel.grid.minor = element_line(size = rel(0.5)),
          axis.title.x = element_text(size = 16, face = "bold", color = "black"),
          axis.title.y = element_text(size = 16, face = "bold", color = "black"),
          axis.text = element_text(size = 12, color = "black"),
          legend.title = element_text(size = 12, face = "bold", color = "black"),
          legend.text = element_text(size = 12, color = "black"),
          legend.position = "bottom")


# II. Modelling Non-linear Relationships with MLR

# Scatter plot
qplot(x = jitter(age), y = weight, data = nepalData,
      xlab = "Age in months", ylab = "Weight in kg", ylim = c(0, 12)) +
    theme(panel.background = element_rect(fill='transparent', color='gray'),
          panel.border = element_rect(fill='transparent', color='transparent'),
          panel.grid = element_line(colour = "grey92"), 
          panel.grid.minor = element_line(size = rel(0.5)),
          axis.title.x = element_text(size = 16, face = "bold", color = "black"),
          axis.title.y = element_text(size = 16, face = "bold", color = "black"),
          axis.text = element_text(size = 12, color = "black"),
          legend.title = element_text(size = 12, face = "bold", color = "black"),
          legend.text = element_text(size = 12, color = "black"),
          legend.position = "bottom")

nepalData %>% group_by(age) %>% summarize(mean = mean(weight))

qplot(x = jitter(age), y = weight, data = nepalData,
      xlab = "Age in months", ylab = "Weight in kg", ylim = c(0, 12)) +
    stat_summary(aes(x = age, y = weight), fun.y = mean, geom = "line",
                 lwd = 2, color = "red") +
    theme(panel.background = element_rect(fill='transparent', color='gray'),
          panel.border = element_rect(fill='transparent', color='transparent'),
          panel.grid = element_line(colour = "grey92"), 
          panel.grid.minor = element_line(size = rel(0.5)),
          axis.title.x = element_text(size = 16, face = "bold", color = "black"),
          axis.title.y = element_text(size = 16, face = "bold", color = "black"),
          axis.text = element_text(size = 12, color = "black"),
          legend.title = element_text(size = 12, face = "bold", color = "black"),
          legend.text = element_text(size = 12, color = "black"),
          legend.position = "bottom")


# Simple linear regression weight ~ age
model4 <- linear_reg() %>%
    set_engine("lm") %>%
    fit(weight ~ age, data = nepalData) %>%
    extract_fit_engine()
summary(model4) %>% tidy() %>% kable()
confint(model4)

qplot(x = jitter(age), y = weight, data = nepalData, xlab = "Age in months",  
      ylab = "Weight in kg", ylim = c(0, 12)) +
    geom_smooth(method = "lm") +
    theme_622()

qplot(x = jitter(age), y = weight, data = nepalData, xlab = "Age in months", 
      ylab = "Weight in kg", ylim = c(0, 12)) +
    geom_line(aes(x = age, y = model4$fitted.values), color = "red", lwd = 2) +
    theme_622()

qplot(y = model4$residuals, x = jitter(age), data = nepalData, ylab = "Residuals",  
      xlab = "Age in months") + 
    geom_smooth(method = "loess", se = FALSE) +   # loess smoother
    geom_hline(yintercept = 0, color = "red") +   # horizontal y=0 line
    theme_622()


# Linear regression weight ~ monthly mean age
model5 <- linear_reg() %>%
    set_engine("lm") %>%
    fit(weight ~ as.factor(age), data = nepalData) %>%
    extract_fit_engine()
summary(model5) %>% tidy()
confint(model5)


# Linear splines
nepalData = nepalData %>%
    mutate(age_sp1 = ifelse(age > 2, age - 2, 0)) %>%
    mutate(age_sp2 = ifelse(age > 4, age - 4, 0)) %>%
    mutate(age_sp3 = ifelse(age > 6, age - 6, 0))

model6 <- linear_reg() %>%
    set_engine("lm") %>%
    fit(weight ~ age + age_sp1 + age_sp2 + age_sp3, data = nepalData) %>%
    extract_fit_engine()
summary(model6) %>% tidy()
confint(model6)

qplot(x = jitter(age), y = weight, data = nepalData, xlab = "Age in months",  
      ylab = "Weight in kg", ylim = c(0, 12)) +
    geom_line(aes(x = age, y = model6$fitted.values), color = "red", lwd = 2) +
    theme_622()


# Compare linearity
anova(model4, model6)
survey::regTermTest(model6, ~ age_sp1 + age_sp2 + age_sp3)


# Compare models
AIC(model4, model5, model6)

AIC.table <- tibble(
    `Model (# parameters)` = c("Linear (2)", "Monthly means (13)", "Linear spline (5)"),
    `parameters` = c(2, 13, 5),
    `Residual sum of squares (RSS)` = c(anova(model4)$`Sum Sq`[length(anova(model4)$`Sum Sq`)],
                                        anova(model5)$`Sum Sq`[length(anova(model5)$`Sum Sq`)],
                                        anova(model6)$`Sum Sq`[length(anova(model6)$`Sum Sq`)]),
    `Residual mean square (MSE)` = c(anova(model4)$`Mean Sq`[length(anova(model4)$`Mean Sq`)],
                                     anova(model5)$`Mean Sq`[length(anova(model5)$`Mean Sq`)],
                                     anova(model6)$`Mean Sq`[length(anova(model6)$`Mean Sq`)]),
    `AIC.lm` = nrow(nepalData)*(log(2*pi*`Residual sum of squares (RSS)`/nrow(nepalData))+1) +
        2*`parameters`,
    `log-likelihood` = sapply(c(logLik(model4), logLik(model5), logLik(model6)), as.numeric),
    `AIC.glm` = -2*(`log-likelihood`) + 2*`parameters`
) %>%
    mutate(`parameters` = NULL)

AIC.huxtable <- huxtable::as_hux(AIC.table) %>%
    huxtable::insert_row("", "Using the regress command", "", "", "Using the glm command", "") %>%
    huxtable::merge_cells(1, 2:4) %>%
    huxtable::merge_cells(1, 5:6) %>%
    huxtable::set_header_rows(1, TRUE) %>%
    huxtable::style_headers(bold = TRUE)
huxtable::number_format(AIC.huxtable) <- 2
AIC.huxtable