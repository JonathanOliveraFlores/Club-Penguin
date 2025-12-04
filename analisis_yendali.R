#ANALISIS STUDENT EXAM RESULTS
library(tidyverse)
library(ggcorrplot)
library(factoextra)

#1. Cargar datos
df <- read.csv("Student_Exam_Results.csv")

#2. Limpieza 
df1 <- df %>%
  select(-Total) %>%
  mutate(
    Results = as.factor(Results),
    Div = as.factor(Div)
  )

#3. Exploración básica
summary(df1)
colSums(is.na(df1))

#4. Histogramas por materia
materias <- c("Hindi", "English", "Science", "Maths", "History", "Geograpgy")

df1 %>%
  pivot_longer(cols = materias, names_to = "Materia", values_to = "Score") %>%
  ggplot(aes(x = Score)) +
  geom_histogram(bins = 20, color = "black", fill = "grey70") +
  facet_wrap(~ Materia, scales = "free") +
  labs(title = "Distribución de calificaciones por materia")

#5. Boxplots por materia 
df1 %>%
  pivot_longer(cols = materias, names_to = "Materia", values_to = "Score") %>%
  ggplot(aes(x = Materia, y = Score)) +
  geom_boxplot() +
  labs(title = "Boxplots por materia")

#6. Matriz de correlación
corr_mat <- cor(df1[materias], method = "spearman")
ggcorrplot(corr_mat, type = "lower", lab = TRUE)

#7. PCA
materias_scaled <- scale(df1[materias])
pca_res <- prcomp(materias_scaled, center = TRUE, scale. = TRUE)

summary(pca_res)

#Scree plot
fviz_eig(pca_res, addlabels = TRUE)

#Biplot limpio (solo variables)
fviz_pca_var(pca_res, repel = TRUE, col.var = "red")