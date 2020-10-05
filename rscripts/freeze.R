library(tidyverse)
library(viridis)

theme_set(theme_minimal())

df <- read_csv(
  file.path("C:", "Users", "rory", "repos", "ah_vids", "output",
  "FP", "new", "conditioning.csv")
)


###### Freeze Analysis

freeze <- df %>%
  group_by(mouse_name) %>%
  summarise(obs=mean(was_freezing_obs),
            dem=mean(was_freezing_dem)) %>%
  left_join(distinct(df, mouse_name, group)) %>%
  gather(role, freeze, -mouse_name, -group)

freeze %>%
  ggplot(aes(x=group, y=freeze, color=group)) +
  geom_boxplot(width=0.3, lwd=1) +
    facet_grid(cols=vars(role)) +
    lims(y=c(0, 1)) +
    labs(x="", y="Proportion Freezing", color="") +
    ggsave("prop_freeze.svg")

m1 <-lm(freeze ~ role * group, data=freeze)
anova(m1)

# dems froze more than obs but there was no effect of group

################


freeze <- df %>%
  filter(trial_type %in% c("baseline", "CS", "ITI")) %>%
  group_by(mouse_name, trial_type) %>%
  summarise(obs=mean(was_freezing_obs),
            dem=mean(was_freezing_dem)) %>%
  left_join(distinct(df, mouse_name, group)) %>%
  gather(role, freeze, -mouse_name, -group, -trial_type) 

freeze %>%
  ggplot(aes(x=trial_type, y=freeze, color=group)) +
  geom_boxplot(width=0.5, lwd=1) +
  facet_grid(cols=vars(role)) +
  lims(y=c(0, 1)) +
  labs(x="", y="Proportion Freezing", color="") +
  ggsave("prop_freeze_trial.svg")

m1 <- lm(freeze ~ trial_type * role * group, data=freeze)
anova(m1)

# Mice froze different amount in the three blocks
# There was a significant interaction between role and trial type
# with obs freezing more during baseline than dems: potential effect of wires / surgery?

######################################

freeze <- df %>%
  remove_missing() %>%
  filter(
    trial_type %in% c("CS", "ITI"),
    trial_num >= max(trial_num) - 6
    | trial_num <= 5
    ) %>%
  mutate(trial_group = if_else(trial_num <= 5, 
                               "First Five", 
                               "Last Five")) %>%
  group_by(mouse_name, trial_type, trial_group, group) %>%
  summarise(obs=mean(was_freezing_obs),
            dem=mean(was_freezing_dem)) %>%
  gather(role, freeze, -mouse_name, -group, -trial_type, -trial_group)
  
freeze %>%
  ggplot(aes(x=trial_type, y=freeze, color=group)) +
  geom_boxplot(width=0.5, lwd=1) +
  facet_grid(cols=vars(trial_group),
             rows=vars(role)) +
  lims(y=c(0, 1)) +
  labs(y="Proportion Freezing", x="Block Type", color="") +
  ggsave("Freezing_x_block_firstlast.svg")

m1 <- lm(freeze ~ trial_type * role * group * trial_group, data=freeze)
anova(m1)

# Mice froze more in the last five trials compared with the first 5 trials
# This effect was stronger in YFP mice compared with Arch mice (significance interaction between trial 
# period and opto group)

################

fcor <- df %>%
  group_by(mouse_name, group) %>%
  summarise(freeze_cor= cor(was_freezing_dem, was_freezing_obs))

fcor %>%
  ggplot(aes(x=group, y=freeze_cor, color=group)) +
  geom_boxplot(width=0.2, lwd=1) +
  labs(y="Correlation of Freezing Activity", color="", x="") +
  lims(y=c(-1, 1)) +
  ggsave("CorrAmount.svg")

m1<- lm(freeze_cor ~ group, data=fcor)
anova(m1)

# Animals freezing behaviour was moderately correlated
# The degree of correlation was not associated with opto group

####

fcor <- df %>%
  drop_na() %>%
  group_by(mouse_name, group, trial_type) %>%
  summarise(freeze_cor= cor(was_freezing_dem, was_freezing_obs)) 
  
fcor %>% 
  ggplot(aes(x=trial_type, y=freeze_cor, color=group)) +
  geom_boxplot(width=0.5, lwd=1) +
  labs(y="Correlation of Freezing Activity", color="", x="Block Type") +
  lims(y=c(-1, 1)) +
  ggsave("CorrBlocks.svg")

m1<- lm(freeze_cor ~ group * trial_type, data=fcor)
anova(m1)

# There was no association between block type and freezing correlation,
# Similarly, no interaction between block type and opto group

fcor <- df %>%
  remove_missing() %>%
  filter(
    trial_type %in% c("CS", "ITI"),
    trial_num >= max(trial_num) - 6
    | trial_num <= 5
  ) %>%
  mutate(trial_group = if_else(trial_num <= 5, 
                               "First Five", 
                               "Last Five")) %>%
  group_by(mouse_name, trial_type, trial_group, group) %>%
  summarise(freeze_cor= cor(was_freezing_dem, was_freezing_obs)) 

fcor %>%
  ggplot(aes(x=trial_type, y=freeze_cor, color=group)) +
  geom_boxplot(width=0.5, lwd=1) +
  facet_grid(cols=vars(trial_group)) +
  lims(y=c(-1, 1)) +
  labs(y="Correlation of Freezing Activity", x="Block Type", color="") +
  ggsave("Corr_fflf.svg")

m1<- lm(freeze_cor ~ group * trial_type * trial_group, data=fcor)
anova(m1)

fcor
