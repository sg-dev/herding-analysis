library(tidyverse)

decision_df <- read.csv("data/raw/table_game_decision_to_analyze.csv")
survey_df <- read.csv("data/raw/table_survey_and_risk.csv")
demographiscs_df <- read.csv("data/raw/table_demographics.csv")

df <- merge(merge(decision_df, survey_df, by='user_id'), demographiscs_df, by='user_id')

df$decision_num <- abs(as.numeric(decision_df$decision)-2)


# Overall effect

## Only deception
### In the round were they face exactly 50% defectors is the prob = 0.5 (theoretical prediction)?
df %>% filter(condition == 'deception' & fraction_plyaing_C == 0.5) %>% select(decision_num) %>% t.test(., mu=0.5)

### Do subjects in the last round, where majority plays D, play C more than p=0 (theoretical prediction)?
df %>% filter(condition == 'deception' & round == 8) %>% select(decision_num) %>% t.test(., mu=0)


## Pooled with dummy
df %>% filter(fraction_plyaing_C == 0.5) %>% lm(decision_num ~ condition, data=.) %>% summary



## Comparison with last round


## Do subjects in the bot condition play more C than their peers in the no-deception condition in the last round?
df %>% filter(round == 8) %>% lm(decision_num~condition, data=.) %>% summary

## Do subjects in the bot condition play more C than their peers in the no-deception condition in overall?
df  %>% lm(decision_num~condition, data=.) %>% summary

## Do subjects in the bot condition play more C than their peers in the no-deception condition in overall controlling for fraction?
df  %>% lm(decision_num~condition*fraction_plyaing_C, data=.) %>% summary


#------- Now with controls ------

## Pooled with dummy
df %>% filter(fraction_plyaing_C == 0.5) %>% lm(decision_num ~ condition + gender + age, data=.) %>% summary
