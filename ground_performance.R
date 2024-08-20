# Loading necessary libraries

library(ggplot2)
library(Metrics)
library(stringr)

library(dplyr)
library(caret)
library(zoo)

##---------------------------GROUND PERFORMANCE ANALYSIS------------------------



# Read the CSV file
ground_data <- read.csv("Ground_Averages.csv", sep=";")

# Extract short ground names
ground_data <- ground_data %>%
  mutate(Ground_Short = str_trim(str_extract(Ground, "(?<=,)[^,]*$|(?<=,).*$")))


# If extraction fails (returns NA), use the original name
ground_data <- ground_data %>%
  mutate(Ground_Short = ifelse(is.na(Ground_Short), Ground, Ground_Short))

# 1. Calculate derived metrics
ground_analysis <- ground_data %>%
  mutate(
    Win_Rate = Won / Mat,
    Runs_per_Match = Runs / Mat,
    Wickets_per_Match = Wkts / Mat,
    Balls_per_Match = Balls / Mat,
    Overs_per_Match = Balls_per_Match / 6
  )

# 2. Rank grounds by different metrics
top_grounds_runs <- ground_analysis %>%
  arrange(desc(Runs_per_Match)) %>%
  select(Ground_Short, Runs_per_Match) %>%
  head(10)

top_grounds_wickets <- ground_analysis %>%
  arrange(desc(Wickets_per_Match)) %>%
  select(Ground_Short, Wickets_per_Match) %>%
  head(10)

top_grounds_rpo <- ground_analysis %>%
  arrange(desc(RPO)) %>%
  select(Ground_Short, RPO) %>%
  head(10)

# 3. Visualizations

# Sort grounds by RPO
sorted_grounds <- ground_analysis %>%
  arrange(desc(RPO))

# Calculate the midpoint
midpoint <- ceiling(nrow(sorted_grounds) / 2)

# Create two subsets
first_half <- sorted_grounds[1:midpoint, ]
second_half <- sorted_grounds[(midpoint+1):nrow(sorted_grounds), ]

# Create the first plot
p1 <- ggplot(first_half, aes(x = reorder(Ground_Short, RPO), y = RPO)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "Run Rate by Ground (Part 1)", x = "Ground", y = "Runs per Over")
plot(p1)

# Create the second plot
p2 <- ggplot(second_half, aes(x = reorder(Ground_Short, RPO), y = RPO)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "Run Rate by Ground (Part 2)", x = "Ground", y = "Runs per Over")
plot(p2)


# Runs per match by ground

p1 <- ggplot(first_half, aes(x = reorder(Ground_Short, Runs_per_Match), y = Runs_per_Match)) +
  geom_bar(stat = "identity", fill = "#6B8E23") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
  labs(title = "Runs per Match by Ground (Part 1)", x = "Ground", y = "Runs per Match")
plot(p1)

p2 <- ggplot(second_half, aes(x = reorder(Ground_Short, Runs_per_Match), y = Runs_per_Match)) +
  geom_bar(stat = "identity", fill = "#6B8E23") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
  labs(title = "Runs per Match by Ground (Part 2)", x = "Ground", y = "Runs per Match")
plot(p2)

# Wickets per match by ground

p1 <- ggplot(first_half, aes(x = reorder(Ground_Short, Wickets_per_Match), y = Wickets_per_Match)) +
  geom_bar(stat = "identity", fill = "#B22222") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
  labs(title = "Wickets per Match by Ground (Part 1)", x = "Ground", y = "Wickets per Match")
plot(p1)

p2 <- ggplot(second_half, aes(x = reorder(Ground_Short, Wickets_per_Match), y = Wickets_per_Match)) +
  geom_bar(stat = "identity", fill = "#B22222") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
  labs(title = "Wickets per Match by Ground (Part 2)", x = "Ground", y = "Wickets per Match")
plot(p2)


# 4. Print top grounds for different metrics
print("Top 10 Grounds by Runs per Match:")
print(top_grounds_runs)

print("Top 10 Grounds by Wickets per Match:")
print(top_grounds_wickets)

print("Top 10 Grounds by Run Rate:")
print(top_grounds_rpo)

# 5. Summary statistics
summary_stats <- ground_analysis %>%
  summarise(
    Avg_Runs_per_Match = mean(Runs_per_Match),
    Avg_Wickets_per_Match = mean(Wickets_per_Match),
    Avg_RPO = mean(RPO),
    Highest_RPO = max(RPO),
    Lowest_RPO = min(RPO)
  )

print("Summary Statistics:")
print(summary_stats)
