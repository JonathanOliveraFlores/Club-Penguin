### Student_Exam_Results


###Descripción
###La base Student Exam Results.csv contiene las calificaciones de examenes de 1000 
###estudiantes en seis asignaturas, con tres columnas adicionales: 
###calificacion total (variable que debe ser eliminada), resultado y division. 
###El resultado puede ser aprobado (1) o reprobado (0). La division puede ser 0, 1, 2 y 3.
###Format
###A data frame with 1000 observations on the following variables:

#Hindi: hindi class score

#English: english class score

#Science: science class score

#Maths: maths class score

#History: history class score

#Geograpgy: geography class score

#Total: scores sum



###Librerias
library(learnr)
library(tidyverse)
library(tidymodels)
library(embed)
library(corrr)
library(tidytext)
library(gradethis)
library(sortable)
library(rstatix)
library(broom)
library(rgl)
library(plotly)
library(GGally)
library(tidyr)
library(FactoMineR)  
library(factoextra)
library(ggord)
theme_set(theme_bw(16))

df<-read.csv("C:\\Users\\luis2\\OneDrive\\Documents\\Diplomado\\Student_Exam_Results - Student_Exam_Results.csv")

df %>% dim()

df %>% glimpse()

df %>% head()

df1<-df[,1:9]

### Se eliminan las columnas 7 y 9
df1 <- df1[, -c(7, 9)]

df1 %>% head()


###Valores faltantes

colSums(is.na(df1))

df1 %>% summary()

###Variable de clasificacion: Results

df1 <- df1 %>% 
  mutate(results = relevel(as.factor(Results), "1", "0"))

df1  %>% count(results)

table(df1$results)

df1 %>% glimpse()

df1 %>%
  group_by(results) %>%
  summarise (n = n()) %>%
  mutate(prop = n / sum(n)) %>%
ggplot(aes(df1,x = results, y = n)) +
    geom_col(fill = c("#CC0033", "#e319dc")) +
    geom_text(aes(label = paste0(n, " | ", signif(n / nrow(df) * 100, digits = 4), '%')), nudge_y = 10) + ggtitle("Porcentajes de resultados de examenes")
    theme_gray()

df1 %>%
  select(where(is.numeric)) %>%
  colMeans()

###Histogramas

df1 |> pivot_longer((!results), 
 names_to = "Variable", values_to="Score") |>
   ggplot(aes(x=Score)) + geom_histogram(aes(y = ..density..),bins=20,colour = 3, fill = "darkmagenta") +
     facet_wrap("Variable",ncol = 4,scales = "free" ) + theme_minimal()

###box-plot

df1 |> pivot_longer((!results), 
  values_to="Score",names_to = "Variable") |>
    ggplot(aes(y=Score)) + geom_boxplot(aes(fill="darkred"),colour = 3,show.legend = FALSE) +
      facet_wrap("Variable",ncol = 4,scales = "free" ) + theme_minimal()

###Densidad

df1 |> pivot_longer((!results), 
   names_to = "Variable",values_to="Score") |>
     ggplot(aes(x=Score)) + geom_density(aes(fill="darkred"),colour = 3,show.legend = FALSE) +
       facet_wrap("Variable",ncol = 4,scales = "free" ) + theme_minimal() 

###Comparacion por la variable de clasificacion o respuesta

df1_long <- df1 %>% 
    pivot_longer(!results, names_to = "predictores", values_to = "values")

theme_set(theme_light())

df1_long %>% 
  ggplot(mapping = aes(x = results, y = values, fill = predictores)) +
  geom_boxplot() + 
  facet_wrap(~ predictores, scales = "free", ncol = 4) +
  scale_color_viridis_d(option = "plasma", end = .7) +
  theme(legend.position = "none") +
  labs(title = "Comparación vía box-plot")

df1_long |> ggplot(mapping = aes(values, fill = results)) +
  geom_histogram(color = "white") +
  facet_wrap(~predictores, scales = "free", ncol= 4) +
  scale_color_viridis_d(option = "plasma", end = .7) +
  labs(title = "Variables Distribution") +
  theme_light()+
  labs(title = "Comparación vía histograma")

df1_long |> ggplot(mapping = aes(values, fill = results)) +
  geom_density(color = "white") +
  facet_wrap(~predictores, scales = "free", ncol= 4) +
  scale_color_viridis_d(option = "plasma", end = .7) +
  labs(title = "Variables Distribution") +
  theme_light()+
  labs(title = "Comparación a través de densidad")

###

ggpairs(df1, mapping = aes(color = results),columns = seq(1,6))

### correlacion ###

wdbc_corr <- df1 %>%
  select(-results) %>%
  correlate(method = "spearman")

wdbc_corr 

cor_wdbc<-cor(df1[,-8],method="spearman")

cor_wdbc

ggcorrplot::ggcorrplot(corr = cor_wdbc,
                       type = "lower", 
                       show.diag = TRUE,
                       lab = TRUE, 
                       lab_size = 3)
det(cor_wdbc)

psych::KMO(cor_wdbc)


###Prueba de Bartlett

psych::cortest.bartlett(cor_wdbc,n=dim(df1)[1])

###Segun la prueba de bartlett se puede aplicar reduccion de dimensiones

cor.df1 <- df1 %>% select(-results) %>% cor_mat()
cor.df1

options(scipen=999)

cor.df1 %>% cor_get_pval()

cor.df1 %>%
  cor_reorder() %>%
  pull_lower_triangle() %>%
  cor_plot(label = TRUE)

cor.df1 %>% cor_gather() %>% print(n=Inf)

###PCA

wdbc_recipe <-
  recipe(~ ., data = df1) %>% 
  update_role(results, new_role = "id") %>% 
  step_dummy(all_nominal_predictors()) %>%  
  step_normalize(all_predictors()) %>%
  step_pca(all_numeric_predictors(), threshold = 0.8) 
  

wdbc_recipe

wdbc_pca <- prep(wdbc_recipe)

pca_loading <- tidy(wdbc_pca, id="pca_d3fvK")
pca_loading

pca_variances <- tidy(wdbc_pca, id = "pca_d3fvK", type = "variance")
pca_variances

pca_var_percent<- tidy(wdbc_pca, id = "pca_d3fvK", type = "variance")%>%
                filter(str_detect(terms, "percent variance"))
pca_var_percent

pca_var_cum_percent<-tidy(wdbc_pca, id = "pca_d3fvK", type = "variance")%>%
                filter(str_detect(terms, "cumulative percent variance"))
pca_var_cum_percent

wdbc_pca %>% 
  tidy(id = "pca_d3fvK", type = "variance") %>% 
  dplyr::filter(terms == "percent variance") %>% 
  ggplot(aes(x = component, y = value)) + 
  geom_col(fill = "#B53389") + 
  xlim(c(0, 10)) + 
  geom_point(size=3) +
  geom_line(color="darkblue", size=1.1)+
  labs(x="PC", y="% de varianza", title="Scree plot")

wdbc_pca %>% 
  tidy(id = "pca_d3fvK", type = "variance") %>% 
  dplyr::filter(terms == "cumulative percent variance") %>% 
  ggplot(aes(x = component, y = value)) + 
  geom_col(fill = "#F25E52") + 
  xlim(c(0, 10)) + 
  geom_point(size=3) +
  geom_line(color="darkblue", size=1.1)+
  labs(x="PC", y="% acumulado de varianza", title="Scree plot")

variance_exp <- tidy(wdbc_pca,id = "pca_d3fvK", type = "variance")%>%
                filter(str_detect(terms, "percent variance"))
variance_exp

###Grafica de los primeros dos componentes

VE <- paste("Varianza explicada por dos componentes:"
                       ,round(variance_exp$value[[1]]+variance_exp$value[[2]], digits = 2),"%")
VE

datos_pca<-bake(wdbc_pca, new_data = NULL)
datos_pca

datos_pca %>%
  ggplot(aes(x = PC1, y = PC2))+
  geom_point()+
  labs(x = paste0("PC1: ",round(variance_exp$value[[1]],2), "%"),
       y = paste0("PC2: ",round(variance_exp$value[[2]],2), "%"))+
  ggtitle(paste0("WDBC: Gráfica de componentes principales","\n",VE))

datos_pca %>%
  ggplot(aes(x = PC1, y = PC2,color = factor(results)))+
  geom_point()+
  labs(x = paste0("PC1: ",round(variance_exp$value[[1]],2), "%"),
       y = paste0("PC2: ",round(variance_exp$value[[2]],2), "%"))+
  ggtitle(paste0("WDBC: Gráfica de componentes principales","\n",VE))

juice(wdbc_pca) %>%
  ggplot(aes(PC1, PC2, label = results)) +
  geom_point(aes(color = results), alpha = 0.9, size = 2) +
  geom_text(check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +
  labs(color = NULL)

###Componentes

wdbc_pca %>%
  tidy(id = "pca_qyT3D") %>% 
  mutate(terms = tidytext::reorder_within(terms, 
                                          abs(value), 
                                          component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  tidytext::scale_y_reordered() +
  scale_fill_manual(values = c("#b6dfe2", "#0A537D")) +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "¿Positive?"
  ) 


wdbc_pca %>%
  tidy(id = "pca_d3fvK") %>%
  filter(component %in% paste0("PC", 1:3)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL)

###

###3D

plot3d( 
  x=datos_pca$PC1, y=datos_pca$PC2, z=datos_pca$PC3, 
  col = as.numeric(datos_pca$results), 
  type = 's', 
  radius = .1,
  xlab="PC1", ylab="PC2", zlab="PC3")

g.df1 <- plot_ly(datos_pca, x = ~PC1, y = ~PC2, z = ~PC3, color = ~results, colors =c("#0000FF", "#FF00FF") )
g.df1 <- g.df1 %>% add_markers()
g.df1 <- g.df1 %>% layout(scene = list(xaxis = list(title = 'PC1'),
                     yaxis = list(title = 'PC2'),
                     zaxis = list(title = 'PC3')))

g.df1


datos_pca$results[which(datos_pca$results == "1" )] <- "1"
datos_pca$results[which(datos_pca$results == "0" )] <- "0"
datos_pca$results <- as.factor(datos_pca$results)

fig <- plot_ly(datos_pca, x = ~PC1, y = ~PC2, z = ~PC3, color = ~results, colors = c('#BF382A', '#0C4B8E'))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'PC1'),
                     yaxis = list(title = 'PC2'),
                     zaxis = list(title = 'PC3')))

fig




















