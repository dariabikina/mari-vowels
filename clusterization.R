library(tidyverse)
library(rstatix)
library(ggrepel)
library(mclust)
library(lme4)
library(emmeans)


df <- read.csv('report.csv')

df <- df %>% 
  separate(filename, into = c("vowel", "word", "occurrence", "recordingId", "placeholder"), sep = "_") %>%
  select(-placeholder)

#summary statistics by vowel
df %>% group_by(vowel) %>% get_summary_stats(F1, F2)

# vowel space viz
df %>%
  ggplot(aes(x = F2, y = F1, label = vowel, colour = vowel)) +
  geom_text_repel() +
  scale_x_reverse() +
  scale_y_reverse()

#subsetting to the two schwas
front_schwa <- df %>% filter(vowel == "front")
back_schwa <- df %>% filter(vowel == "back")

front_schwa %>% get_summary_stats(F1, F2)

# k-means clustering on schwas

schwa <- df %>% filter(vowel %in% c("front", "back"))
set.seed(123)
kmeans_result <- kmeans(schwa[, c("F1", "F2")], centers = 2)
schwa$cluster <- as.factor(kmeans_result$cluster)


ggplot(schwa, aes(x = F2, y = F1, color = cluster, label = vowel)) +
  geom_text_repel() +
  scale_x_reverse() +
  scale_y_reverse() +
  labs(title = "K-means Clustering Based on F1 & F2")


table(schwa$vowel, schwa$cluster)

adjustedRandIndex(schwa$vowel, schwa$cluster)

#normality checks; all pass
shapiro.test(front_schwa$F1)
shapiro.test(front_schwa$F2)
shapiro.test(back_schwa$F1)
shapiro.test(back_schwa$F2)

#t-tests of formants in the two schwas
t.test(front_schwa$F1, back_schwa$F1)
t.test(front_schwa$F2, back_schwa$F2)

# Elbow method to choose K for everything
wss <- function(k) {
  kmeans(df[, c("F1", "F2")], centers = k, nstart = 10)$tot.withinss
}

k.values <- 1:10
wss_values <- sapply(k.values, wss)

plot(k.values, wss_values, type = "b",
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares",
     main = "Elbow Method for Choosing K")

# gmm clustering on schwas
schwa <- schwa %>% mutate(F1_z = scale(F1), F2_z = scale(F2))
gmm <- Mclust(schwa[, c("F1_z", "F2_z")])
summary(gmm)
schwa$cluster_gmm <- as.factor(gmm$classification)

# GMM on all vowels
gmm_all <- Mclust(df[, c("F1", "F2")])
summary(gmm_all)



# Linear mixed model (F1)
model_f1 <- lmer(F1 ~ vowel + (1 | recordingId), data = df)
summary(model_f1)
emmeans(model_f1, pairwise ~ vowel)


# Linear mixed model (F2)
model_f2 <- lmer(F2 ~ vowel + (1 | recordingId), data = df)
summary(model_f2)
emmeans(model_f2, pairwise ~ vowel)
