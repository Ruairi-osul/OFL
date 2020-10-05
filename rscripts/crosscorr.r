library(tidyverse)

theme_set(theme_minimal())
data_dir <- file.path("..", "notebooks")


###############
df <- read_csv(file.path(data_dir, "CrossCorrOverall.csv"))


df %>%
  group_by(group) %>%
  summarise(is_left = mean(p < 0.05) * 100) %>%
  ggplot(aes(x=group, y=is_left, fill=group)) +
  geom_col(width=0.5) +
  labs(x="", fill="", y="% Mice") +
  ggtitle("Significant Left Dominance") +
  lims(y=c(0, 100)) +
  ggsave("OverallLeftDom.svg")

df %>%
  ggplot(aes(x=group, y=lag, color=group)) +
  geom_boxplot(lwd=1, width=0.5) +
  labs(x="", color="", y="Lag of Max Corss Correlation") +
  ggsave("CCOverallLag.svg")


df %>%
  ggplot(aes(x=group, y=observed, color=group)) +
  geom_boxplot(lwd=1, width=0.5) +
  labs(y="AUC Left - AUC Right", x="", color="") +
  ggsave("CCOverallAUC.svg")


#####################

df <- read_csv(file.path(data_dir, "CC_BLOCKS.csv"))

df %>%
  filter(trial_type=="baseline") %>%
  ggplot(aes(x=group, y=max_cc_lag, color=group)) +
  geom_boxplot(lwd=1, width=0.5) +
  labs(y="Lag of Max Corss Correlation [sec]", x="", color="") +
  lims(y=c(-9, 9)) +
  ggsave("CClag_base.svg")

df %>%
  filter(trial_type=="baseline") %>%
  ggplot(aes(x=group, y=left_right_auc, color=group)) +
  geom_boxplot(lwd=1, width=0.5) +
  labs(y="Left AUC - Right AUC", x="", color="") +
  lims(y=c(-15, 15)) +
  ggsave("CCAUC_base.svg")
  

####

df %>%
  filter(trial_type=="CS") %>%
  ggplot(aes(x=group, y=max_cc_lag, color=group)) +
  geom_boxplot(lwd=1, width=0.5) +
  labs(y="Lag of Max Corss Correlation [sec]", x="", color="") +
  lims(y=c(-9, 9)) +
  ggsave("CClag_CS.svg")

df %>%
  filter(trial_type=="CS") %>%
  ggplot(aes(x=group, y=left_right_auc, color=group)) +
  geom_boxplot(lwd=1, width=0.5) +
  labs(y="Left AUC - Right AUC", x="", color="") +
  lims(y=c(-15, 15)) +
  ggsave("CCAUC_CS.svg")

####

df %>%
  filter(trial_type=="ITI") %>%
  ggplot(aes(x=group, y=max_cc_lag, color=group)) +
  geom_boxplot(lwd=1, width=0.5) +
  labs(y="Lag of Max Corss Correlation [sec]", x="", color="") +
  lims(y=c(-9, 9)) +
  ggsave("CClag_ITI.svg")


df %>%
  filter(trial_type=="ITI") %>%
  ggplot(aes(x=group, y=left_right_auc, color=group)) +
  geom_boxplot(lwd=1, width=0.5) +
  labs(y="Left AUC - Right AUC", x="", color="") +
  lims(y=c(-15, 15)) +
  ggsave("CCAUC_ITI.svg")


######