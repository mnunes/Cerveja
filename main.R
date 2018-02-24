# pacotes necessarios

library(rvest)
library(httr)
library(stringr)
library(tidyverse)
theme_set(theme_bw())
library(corrplot)
library(ggdendro)

# funcoes para obter os dados

source("cerveja.R")
source("scrap.R")

####################################
### baixar os dados para analise ###
####################################

# baixar os links das 10*k cervejas mais avaliadas

links <- scrap("http://www.brejas.com.br/cerveja/mais-avaliadas?page=1")

k <- 100

for (j in 2:k){
  url <- paste("http://www.brejas.com.br/cerveja/mais-avaliadas?page=", j, sep="")
  links <- c(links, scrap(url))
}

links <- sort(links)

dados <- cerveja(links[1])

for (j in links[c(-1)]){
  dados <- rbind(dados, cerveja(j))
}

# remove espacos ao final dos nomes

dados$Estilo <- trimws(dados$Estilo, "right") 

save(as_data_frame(dados), file="dados.RData")



##################################################
### colocar todas as variaveis na mesma escala ###
##################################################

load("dados.RData")

head(dados)

dados$Aroma    <- dados$Aroma/2
dados$Sabor    <- dados$Sabor/4
dados$Conjunto <- dados$Conjunto/2

dados <- as_data_frame(dados)

############################
### analise exploratoria ###
############################

# cervejas melhor avaliadas

dados %>% 
  select(Nome, Geral, Votos) %>%
  arrange(desc(Geral))

# estilos com maiores média e mediana

dados %>% 
  select(Estilo, Nome, Geral) %>%
  group_by(Estilo) %>%
  summarise(Media=mean(Geral, na.rm=T), Mediana=median(Geral, na.rm=T)) %>%
  arrange(desc(Media))

# boxplot comparando os melhores estilos

estilos <- dados %>% 
  select(Estilo, Nome, Geral) %>%
  group_by(Estilo) %>%
  summarise(Media=mean(Geral, na.rm=T), Mediana=median(Geral, na.rm=T)) %>%
  arrange(desc(Mediana)) %>%
  select(Estilo) %>%
  head(10)

estilos <- as_vector(estilos)

dados.estilos <- dados %>%
  filter(Estilo %in% estilos)

dados.estilos$Estilo <- na.omit(factor(dados$Estilo, levels=estilos, ordered=TRUE))

ggplot(dados.estilos, aes(x=Estilo, y=Geral)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))
  
# contagem por estilo

dados %>%
  select(Estilo) %>%
  group_by(Estilo) %>%
  count() %>%
  arrange(desc(n))

# correlacao
  
dados.plot <- dados %>%
  select(Aroma, Aparencia, Sabor, Sensacao, Conjunto)

dados.plot.corr <- cor(dados.plot, method="spearman", use="pairwise.complete.obs")

corrplot.mixed(dados.plot.corr, upper="ellipse")

# agrupamento

dados.cluster <- dados %>%
  select(Estilo, Aroma, Aparencia, Sabor, Sensacao, Conjunto) %>%
  group_by(Estilo) %>%
  summarise(Aroma=median(Aroma), 
            Aparencia=median(Aparencia), 
            Sabor=median(Sabor), 
            Sensacao=median(Sensacao), 
            Conjunto=median(Conjunto)) %>%
  na.omit()

dados.cluster.padronizado <- dados.cluster %>%
  select(-Estilo) %>%
  scale()

dados.cluster.padronizado <- as.data.frame(dados.cluster.padronizado)

rownames(dados.cluster.padronizado) <- dados.cluster$Estilo

dados.dist <- dist(dados.cluster.padronizado, method="euclidean")

dados.dist <- na.omit(dados.dist)

ggdendrogram(hclust(dados.dist))



