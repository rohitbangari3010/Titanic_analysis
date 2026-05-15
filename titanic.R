############################################################
# TITANIC SURVIVAL ANALYSIS
# Author: Rohit Bangari
# Description: Data cleaning, feature engineering, and EDA
############################################################

# -----------------------------
# 1. Load Libraries
# -----------------------------
library(tidyverse)
library(stringr)
library(data.table)
library(ggplot2)
library(scales)
library(grid)
library(vcd)

# -----------------------------
# 2. Load Dataset
# -----------------------------
full <- read.csv("D:/MS Data science/Statistics/Projects/titanic/titanic.csv")

# Basic structure
str(full)
dim(full)

# -----------------------------
# 3. Missing Values & Uniques
# -----------------------------
lapply(full, function(x) length(unique(x)))
lapply(full, function(x) sum(is.na(x)))

# -----------------------------
# 4. Handle Missing Age & Create Age Groups
# -----------------------------
full <- full %>%
  mutate(
    Age = ifelse(is.na(Age), mean(full$Age, na.rm = TRUE), Age),
    Age_Group = case_when(
      Age < 13 ~ "Age.0012",
      Age >= 13 & Age < 18 ~ "Age.131",
      Age >= 18 & Age < 60 ~ "Age.185",
      Age > 60 ~ "Age.60ov"
    )
  )

# -----------------------------
# 5. Handle Missing Embarked
# -----------------------------
full$Embarked <- replace(full$Embarked, which(is.na(full$Embarked)), "S")

# -----------------------------
# 6. Family Size Feature
# -----------------------------
full$Familysize <- 1 + full$SibSp + full$Parch

full <- full %>%
  mutate(
    Familysized = case_when(
      Familysize == 1 ~ "single",
      Familysize < 5 & Familysize >= 2 ~ "small",
      Familysize >= 5 ~ "Big"
    )
  )

full$Familysized <- as.factor(full$Familysized)

# -----------------------------
# 7. Ticket Group Size Feature
# -----------------------------
ticket.unique <- rep(0, nrow(full))
tickets <- unique(full$Ticket)

for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.index <- which(full$Ticket == current.ticket)
  for (k in 1:length(party.index)) {
    ticket.unique[party.index[k]] <- length(party.index)
  }
}

full$ticket.unique <- ticket.unique

full <- full %>%
  mutate(
    ticketsize = case_when(
      ticket.unique == 1 ~ "single",
      ticket.unique < 5 & ticket.unique >= 2 ~ "small",
      ticket.unique >= 5 ~ "Big"
    )
  )

# -----------------------------
# 8. Convert Survived to Yes/No
# -----------------------------
full <- full %>%
  mutate(
    Survived = case_when(
      Survived == 1 ~ "Yes",
      Survived == 0 ~ "No"
    )
  )

# -----------------------------
# 9. Crude Survival Rate
# -----------------------------
crude_summary <- full %>%
  select(PassengerId, Survived) %>%
  group_by(Survived) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

crude_summary

crude_survrate <- crude_summary$freq[crude_summary$Survived == "Yes"]

# -----------------------------
# 10. Visualizations
# -----------------------------

# Survival by Class
p1 <- ggplot(full, aes(Pclass, fill = Survived)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  ylab("Survival Rate") +
  ggtitle("Survival Rate By Class") +
  geom_hline(yintercept = crude_survrate, col = "white", size = 2, lty = 2)

p1

# Survival by Sex
p2 <- ggplot(full, aes(Sex, fill = Survived)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  ylab("Survival Rate") +
  ggtitle("Survival Rate by Sex") +
  geom_hline(yintercept = crude_survrate, col = "white", size = 2, lty = 2)

p2

# Age Distribution
p3 <- ggplot(full, aes(Age, fill = Survived)) +
  geom_histogram(aes(y = ..density..), alpha = 0.5, bins = 30) +
  geom_density(alpha = 0.2, aes(color = Survived)) +
  ggtitle("Age Distribution by Survival")

p3

# -----------------------------
# 11. Save Plots
# -----------------------------
dir.create("plots", showWarnings = FALSE)

ggsave("plots/survival_by_class.png", p1, width = 6, height = 4)
ggsave("plots/survival_by_sex.png", p2, width = 6, height = 4)
ggsave("plots/age_distribution.png", p3, width = 6, height = 4)

############################################################
# END OF SCRIPT
############################################################
