
library("tidyverse")
library("dplyr")
library("ggplot2")


# Load data
data <- read.csv("data/processed/panel_data_classification.csv")

library(plm)

# prepare panel data
data <- pdata.frame(data, index = c("Participant.id", "Round"))


# estimate a fixed effects model: Decision~constant+n_cooperators
model <- plm(Decision ~ prop_coop, data = data, model = "within", index = c("Participant.id", "Round"))

# extract participant coefficients
participant_coefs <- fixef(model)

# add participant coefficients to the data frame
data$participant_coef <- participant_coefs[data$Participant.id]

# reset data to a standard data frame
data <- data.frame(data)

# plot histogram of participant coefficients
ggplot(data, aes(x = participant_coef)) +
    geom_histogram(binwidth = 0.1) +
    labs(x = "Participant coefficient", y = "Count")

# convert Deception to factor
data$bot_condition <- as.factor(data$participant.in_deception)
data$lapprovals <- log(data$Total.approvals)

data$Negotiation.experience <- as.factor(data$Negotiation.experience)
data$pboxes <- data$bret.1.player.boxes_collected / 100
# run panel regression with fixed effects, but logit link function

model_logit <- plm(Decision ~ prop_coop * bot_condition + lapprovals + Negotiation.experience + pboxes, data = data, model = "random", index = c("Participant.id", "Round"), family = "binomial")
model_logit_3 <- plm(Decision ~ prop_coop * bot_condition + lapprovals + Negotiation.experience + pboxes, data = data, model = "pool", index = c("Participant.id", "Round"), family = "binomial")

summary(model_logit_3)

library(texreg)
# using texreg to create a table of the two models
texreg(list(model_logit, model_logit_3), digits = 3, booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, file = "reports/panel_regression.tex")

summary(model_logit)
colnames(data)

# save dataset to csv
write.csv(data, "data/processed/panel_data_fixed_effects.csv", row.names = FALSE)


# regression formula Decision ~ n_cooperators*bot_condition*classification

model_logit <- plm(Decision ~ n_cooperators + bot_condition + classification, data = data, model = "random", index = c("Participant.id", "Round"), family = "binomial")

data$after <- ifelse(data$n_cooperators > 10, 1, 0)

model_logit <- plm(Decision ~ bot_condition + crra * after, data = data, model = "pool", index = c("Participant.id", "Round"))

summary(model_logit)

model_logit

# what we want to know: are players choosing B more risk prone.
# regression: n_boxes ~ n_cooperators + classification

model_risk <- plm(bret.1.player.boxes_collected ~ classification, data = data, model = "random", index = c("Participant.id", "Round"))
summary(model_risk)

# what we want to know: are herding players more risk averse.

# regression:

# 1. create new dataset keeping only classification and n_boxes, bot_condition, participant.id
data_small <- data %>% select(classification, crra, bot_condition, Participant.id, Sex)
# drop duplicates
data_small <- data_small %>% distinct()

# 2. run regression
model_risk <- lm(crra ~ classification + bot_condition + Sex, data = data_small)
summary(model_risk)


texreg(list(model_risk), digits = 3, booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, file = "reports/risk_aversion.tex")





bla <- data %>%
    group_by(Participant.id) %>%
    mutate(mean_decision = mean(Decision)) %>%
    select(Participant.id, mean_decision, crra, rel_classification, Sex, Age, bret.1.player.boxes_collected) %>%
    unique()
bla$lage <- log(bla$Age)

mode_ccra <- lm(bla$mean_decision ~ bla$crra + bla$Sex + bla$lage)
mode_boxes <- lm(bla$mean_decision ~ bla$bret.1.player.boxes_collected + bla$Sex + bla$lage)
summary(mode_ccra)
summary(mode_boxes)

data$bret.1.player.boxes_collected

library(texreg)

texreg(list(mode_ccra, mode_boxes), digits = 3, booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, file = "reports/risk-regression.tex")


# filter for ""Participant.id" and "participant.in_deception" drop duplicates and count number by condition
data %>%
    select(Participant.id, participant.in_deception) %>%
    distinct() %>%
    group_by(participant.in_deception) %>%
    count()



#### CHI SQUARE TESTS
library("BayesFactor")


p_values <- c()
bfs <- c()
log_bf <- c()
differences <- c()
prop_levels <- c()


# Loop through unique levels of prop_coop
for (prop_level in unique(data$prop_coop)) {
    # Subset the data for the current level of prop_coop
    sub_data <- subset(data, prop_coop == prop_level)

    # Create contingency table
    contingency_table <- table(sub_data$participant.in_deception, sub_data$Decision)

    # Perform Chi-square test
    chi_test_result <- chisq.test(contingency_table)

    # Compute Bayes Factor
    bf_result <- contingencyTableBF(contingency_table, sampleType = "indepMulti", fixedMargin = "rows")

    # Extract the actual Bayes Factor value
    bf_value <- as.vector(bf_result)[1]

    # Calculate the difference in proportions
    prop_diff <- abs(diff(prop.table(contingency_table, 1)[, 1]))

    # Store the results
    p_values <- c(p_values, chi_test_result$p.value)
    bfs <- c(bfs, bf_value)
    log_bf <- c(log_bf, log(bf_value)) # if bf_value is very small, this may be negative, but bf_value itself should not be
    differences <- c(differences, prop_diff)
    prop_levels <- c(prop_levels, prop_level)
}

library(ggplot2)
library(latex2exp)

results_df <- data.frame(
  prop_levels = prop_levels,
  differences = differences,
  p_values = p_values,
  log_bf = log_bf,
  bf = bfs
)

# Determine the minimum y value to align text annotations along the x-axis
min_y <- -0.003

p <- ggplot(results_df, aes(x=prop_levels, y=differences)) +
  geom_point() +  # Add points
  geom_line() +  # Add lines
  geom_text(aes(y=min_y, label=sprintf("p=%.2f", p_values)), vjust=.8, size=3) +  # Add p-values
  geom_text(aes(y=min_y, label=sprintf("BF=%.2f", bf)), vjust=-1.2, size=3) +  # Add log-BF
  labs(title=TeX("Difference: $\\Delta = |$ P(C $\\wedge$ Informed) - P(C $\\wedge$ Not Informed) $|$"),
       x="Prop. Cooperating Bots (f)",
       y=TeX("Difference ($\\Delta$)")) +
  theme_minimal()
p

ggsave("reports/pvalues-bf-conditions.pdf", plot = p, width = 6, height = 3)


### Aggregate before and after

data$after <- ifelse(data$n_cooperators > 10, 1, 0)


p_values <- c()
bfs <- c()
differences <- c()
prop_levels <- c()

for (lvl in unique(data$after)) {
    # Subset the data for the current level of prop_coop
    sub_data <- subset(data, after == lvl)
    # print current level
    print(lvl)

    # Create contingency table
    contingency_table <- table(sub_data$participant.in_deception, sub_data$Decision)

    # Perform Chi-square test
    chi_test_result <- chisq.test(contingency_table)

    # Compute Bayes Factor
    bf_result <- contingencyTableBF(contingency_table, sampleType = "indepMulti", fixedMargin = "rows")

    # Extract the actual Bayes Factor value
    bf_value <- as.vector(bf_result)[1]

    # Calculate the difference in proportions
    prop_diff <- abs(diff(prop.table(contingency_table, 1)[, 1]))

    # Store the results
    p_values <- c(p_values, chi_test_result$p.value)
    bfs <- c(bfs, bf_value)
    differences <- c(differences, prop_diff)
    prop_levels <- c(prop_levels, lvl)
}
results_df <- data.frame(
  after = prop_levels,
  differences = differences,
  p_values = p_values,
  bf = bfs
)

library(xtable)
latex_table <- xtable(results_df)
print.xtable(latex_table, type = "latex", file = "reports/pvalues-bf-aggregate.tex")
