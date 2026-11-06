
# ==========================================
# PROJECT: MUSIC LISTENING HABITS AND
# STUDY CONCENTRATION
# STEP 4: DATA ANALYSIS AND GRAPHS
# ==========================================

# Install ggplot2 if not yet installed
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(ggplot2)

# Load dataset
music_data <- read.csv("music_listening_concentration.csv")

# View dataset
View(music_data)

# ==========================================
# GRAPH 1: Average Concentration by Music Genre
# ==========================================

genre_average <- aggregate(
  Study_Concentration_Score ~ Preferred_Music_Genre,
  data = music_data,
  FUN = mean
)

graph1 <- ggplot(
  genre_average,
  aes(
    x = Preferred_Music_Genre,
    y = Study_Concentration_Score,
    fill = Preferred_Music_Genre
  )
) +
  geom_col() +
  labs(
    title = "Average Study Concentration by Music Genre",
    x = "Preferred Music Genre",
    y = "Average Concentration Score"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(graph1)

ggsave(
  "graph1_concentration_by_genre.png",
  graph1,
  width = 8,
  height = 5
)

# ==========================================
# GRAPH 2: Music Listening Hours vs
# Study Concentration
# ==========================================

graph2 <- ggplot(
  music_data,
  aes(
    x = Daily_Music_Listening_Hours,
    y = Study_Concentration_Score
  )
) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "red"
  ) +
  labs(
    title = "Music Listening Hours and Study Concentration",
    x = "Daily Music Listening Hours",
    y = "Study Concentration Score"
  ) +
  theme_minimal()

print(graph2)

ggsave(
  "graph2_music_hours_vs_concentration.png",
  graph2,
  width = 8,
  height = 5
)

# ==========================================
# GRAPH 3: Academic Performance by
# Music During Study
# ==========================================

graph3 <- ggplot(
  music_data,
  aes(
    x = Music_During_Study,
    y = Academic_Performance_Score,
    fill = Music_During_Study
  )
) +
  geom_boxplot() +
  labs(
    title = "Academic Performance by Music During Study",
    x = "Music Listening During Study",
    y = "Academic Performance Score"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(graph3)

ggsave(
  "graph3_performance_by_study_music.png",
  graph3,
  width = 8,
  height = 5
)

# ==========================================
# SUMMARY STATISTICS
# ==========================================

cat(
  "Average Daily Music Listening Hours:",
  mean(music_data$Daily_Music_Listening_Hours),
  "\n"
)

cat(
  "Average Study Hours:",
  mean(music_data$Study_Hours),
  "\n"
)

cat(
  "Average Study Concentration Score:",
  mean(music_data$Study_Concentration_Score),
  "\n"
)

cat(
  "Average Academic Performance Score:",
  mean(music_data$Academic_Performance_Score),
  "\n"
)

cat("ANALYSIS COMPLETED SUCCESSFULLY!\n")

