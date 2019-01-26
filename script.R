dataset <- read.csv("ks-projects-201801.csv")
head(dataset)

library(dplyr)
library(ggplot2)


data <- dataset %>% group_by(category) %>% count() %>% arrange(desc(n))
ggplot(data[1:10,], aes(x=reorder(category, -n), y = n, fill=category)) + xlab("Categoria") + ylab("Numero de projetos") + geom_bar(stat="identity")

data <- dataset %>% group_by(country) %>% count() %>%  summarise(Count = n/100000) %>% arrange(desc(Count))
ggplot(data[1:10,], aes(x=reorder(country, -Count), y = Count, fill=country)) + xlab("Pais") + ylab("Numero de projetos/(100000)") + geom_bar(stat="identity")

data <- dataset %>% group_by(state) %>% count() %>% arrange(desc(n))
ggplot(data[1:10,], aes(x=reorder(state, -n), y = n, fill=state)) + xlab("Estado do projeto") + ylab("Numero de projetos") + geom_bar(stat="identity")

data <- sample_n(dataset, 10000)
plot(log(data$goal), log(data$pledged), main="Meta vs Obtido", xlab="Meta", ylab="Obtido")

plot(log(data$backers), log(data$pledged), main="Apoiadores vs Obtido", xlab="Apoiadores", ylab="Obtido")

# Quais s??o as categorias e subcategorias que mais arrecadam/obtem sucesso(estado)?  
# Quais s??o as vari??veis que mais influenciam no sucesso de um projeto?
# Quais s??o as categorias que mais frequentemente obtem o que pedem?
# Qual a faixa de pre??o que mais frequentemente arrecada o que pede?
