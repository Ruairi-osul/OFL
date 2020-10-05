library(tidyverse)

theme_set(theme_minimal())
data_dir <- file.path("..", "notebooks")


###############
df <- read_csv(file.path(data_dir, "bouts_overall.csv"))

df %>%
  ggplot(aes(x=group, y=mean_bout, color=group)) +
  geom_boxplot(lwd=1, width=0.2) +
  labs(x="", y="Mean Freeze Bout", color="") +
  ggsave("bout_overall.svg")

m1 <- lm(mean_bout ~ group, data=df)
anova(m1)

# no main effect of group

#################

df <- read_csv(file.path(data_dir, "bouts_tt.csv"))

df %>%
  ggplot(aes(x=trial_type, y=mean_bout, color=group)) +
  geom_boxplot(lwd=1, width=0.5) +
  labs(x="", y="Mean Freeze Bout", color="") +
  ggsave("bout_tt.svg")
m1 <- lm(mean_bout ~ group * trial_type, data=df)
anova(m1)

# main effect of trial type

#################

df <- read_csv(file.path(data_dir, "bouts_fflf.csv"))

df %>%
  filter(trial_type != "baseline") %>%
  ggplot(aes(x=trial_type, y=mean_bout, color=group)) +
  geom_boxplot(lwd=1, width=0.5) +
  facet_grid(cols=vars(time_period)) +
  labs(x="", y="Mean Freeze Bout", color="") +
  ggsave("bout_fflf.svg")

m1 <- lm(mean_bout ~ group * trial_type * time_period, data=filter(df, trial_type != "baseline"))
anova(m1)
