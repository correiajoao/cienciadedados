library(dplyr)
library(ggplot2)

# Visão geral

dataset <- read.csv("ks-projects-201801.csv")
dataset <- as_tibble(dataset)

head(dataset)

summary(dataset)

# Análise exploratória  

### gráfico de barras das principais categorias

categorias <- dataset %>%
  group_by(main_category) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(10)

ggplot(categorias, aes(x=reorder(main_category, -n), y = n, fill=main_category)) + 
  xlab("Categoria") +
  ylab("Numero de projetos") +
  geom_bar(stat="identity") +
  guides(fill=guide_legend(title="Categorias"))

### gráfico de barras das principais subcategorias

subcategorias <- dataset %>%
  group_by(category) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(10)

ggplot(subcategorias, aes(x=reorder(category, -n), y = n, fill=category)) + 
  xlab("Subcategoria") + 
  ylab("Numero de projetos") + 
  geom_bar(stat="identity") +
  guides(fill=guide_legend(title="Subcategorias"))

### gráfico de barra dos principais países

paises <- dataset %>%
  group_by(country) %>% 
  count() %>%
  arrange(desc(n)) %>%
  head(10)

ggplot(paises, aes(x=reorder(country, -n), y = n, fill=country)) + 
  xlab("Pais") +
  ylab("Numero de projetos") +
  geom_bar(stat="identity") +
  guides(fill=guide_legend(title="Países"))

### estado dos projetos

estado <- dataset %>%
  group_by(state) %>% 
  count() %>% 
  arrange(desc(n)) 

ggplot(estado, aes(x=reorder(state, -n), y = n, fill=state)) + 
  xlab("Estado do projeto") + 
  ylab("Numero de projetos") +
  geom_bar(stat="identity")+ 
  guides(fill=guide_legend(title="Estado"))

### Meta vs obtido

dinheiro <- dataset %>%
  sample_n(10000) %>% 
  mutate(
    log_pledged = log(usd_pledged_real),
    log_goal = log(usd_goal_real)
    )

ggplot(dinheiro, aes(x=log_goal, y=log_pledged)) +
  geom_point(size=.5) +
  xlab("Meta (log)") + 
  ylab("Obtido (log)")

plot(log(data$backers), log(data$pledged), main="Apoiadores vs Obtido", xlab="Apoiadores", ylab="Obtido")

# Quais s??o as categorias e subcategorias que mais arrecadam/obtem sucesso(estado)?  
# Quais s??o as vari??veis que mais influenciam no sucesso de um projeto?
# Quais s??o as categorias que mais frequentemente obtem o que pedem?
# Qual a faixa de pre??o que mais frequentemente arrecada o que pede?
