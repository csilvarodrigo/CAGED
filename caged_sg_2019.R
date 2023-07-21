library(tidyverse)
library(dplyr)

getwd()
caged_sg_2019 <- read.csv("caged_sg_2019.csv", header = TRUE, sep = ",")
head(caged_sg_2019)
glimpse(caged_sg_2019)

# Mudando as categorias
caged_sg_2019$mes <- as.numeric(caged_sg_2019$mes)
caged_sg_2019$admitidos_desligados <- as.factor(caged_sg_2019$admitidos_desligados)
caged_sg_2019$tipo_estabelecimento <- as.factor(caged_sg_2019$tipo_estabelecimento)
caged_sg_2019$salario_mensal <- as.numeric(caged_sg_2019$salario_mensal)
caged_sg_2019$cnae_2 <- as.factor(caged_sg_2019$cnae_2)
caged_sg_2019$grau_instrucao <- as.factor(caged_sg_2019$grau_instrucao)
caged_sg_2019$idade <- as.numeric(caged_sg_2019$idade)
caged_sg_2019$sexo <- as.factor(caged_sg_2019$sexo)
caged_sg_2019$raca_cor <- as.factor(caged_sg_2019$raca_cor)
caged_sg_2019$subsetor_ibge <- as.factor(caged_sg_2019$subsetor_ibge)
glimpse(caged_sg_2019)

# Agrupando por mes as admissoes == 1
jan <- group_by(caged_sg_2019, mes == 1)
ad_jan <- count(jan, caged_sg_2019$admitidos_desligados)

fev <- group_by(caged_sg_2019, mes == 2) 
ad_fev <- count(fev, caged_sg_2019$admitidos_desligados)

mar <- group_by(caged_sg_2019, mes == 3)
ad_mar <- count(mar, caged_sg_2019$admitidos_desligados)

abr <- group_by(caged_sg_2019, mes == 4) 
ad_abr <- count(abr, caged_sg_2019$admitidos_desligados)

mai <- group_by(caged_sg_2019, mes == 5) 
ad_mai <- count(mai, caged_sg_2019$admitidos_desligados)

jun <- group_by(caged_sg_2019, mes == 6) 
ad_jun <- count(jun, caged_sg_2019$admitidos_desligados)

jul <- group_by(caged_sg_2019, mes == 7) 
ad_jul <- count(jul, caged_sg_2019$admitidos_desligados)

ago <- group_by(caged_sg_2019, mes == 8) 
ad_ago <- count(ago, caged_sg_2019$admitidos_desligados)

set <- group_by(caged_sg_2019, mes == 9) 
ad_set <- count(set, caged_sg_2019$admitidos_desligados)

out <- group_by(caged_sg_2019, mes == 10) 
ad_out <- count(out, caged_sg_2019$admitidos_desligados)

nov <- group_by(caged_sg_2019, mes == 11) 
ad_nov <- count(nov, caged_sg_2019$admitidos_desligados)

dez <- group_by(caged_sg_2019, mes == 12) 
ad_dez <- count(dez, caged_sg_2019$admitidos_desligados)



meses <- c('jan', 'fev', 'mar', 'abr', 'mai', 'jun', 'jul', 'ago', 'set', 'out', 'nov', 'dez')
admissoes <- c(1967, 2461, 2313, 2280, 2365, 2659, 2552, 2748, 2628, 2938, 3024, 2238)
desligamentos <- c(2956, 2628, 2827, 2446, 2631, 2349, 2697, 2316, 2239, 2686, 2236, 2382)
admissao_desligamento <- data.frame(meses, admissoes, desligamentos)


# Perfil Homem - sexo == 1
homem <- subset(caged_sg_2019, sexo == 1)
summary(homem$salario_mensal)


# Perfil Mulher- sexo == 2
mulher <- subset(caged_sg_2019, sexo == 2)
summary(mulher$salario_mensal)

# Boxplot

boxplot(homem$salario_mensal, mulher$salario_mensal, main = "Salário Mensal",
        names = c("Homem", "Mulher"),
        col = c("light blue", "orange"),
        ylim = c(0, 5000))

# Raca: branca == 2, preta== 4, parda== 8
branca <- subset(caged_sg_2019, raca_cor == 2)
preta <- subset(caged_sg_2019, raca_cor == 4)
parda <- subset(caged_sg_2019, raca_cor == 8)

summary(branca$salario_mensal)
summary(preta$salario_mensal)
summary(parda$salario_mensal)

branca_mulher_dez <- filter(branca, sexo == 2 & mes == 12)
summary(branca_mulher_dez)

preta_mulher_dez <- filter(preta, sexo == 2 & mes == 12)
summary(preta_mulher_dez)

# admitidos em dezembro de 2019
dezembro_2019_ad <- filter(caged_sg_2019, admitidos_desligados == 1)
summary(dezembro_2019_ad)


# criando subset somente com brancos e negros
# branco = 1, negros = 0, homens=1, mulheres =0
#admitidos = 1, desligados = 0
caged_sg_2019_pb <- subset(caged_sg_2019, raca_cor %in% c(2, 4, 8))
summary(caged_sg_2019_pb)  

caged_sg_2019_pb$raca_cor <- ifelse(caged_sg_2019_pb$raca_cor == 2, 1,0)
caged_sg_2019_pb$sexo <- ifelse(caged_sg_2019_pb$sexo == 1, 1, 0)
caged_sg_2019_pb$admitidos_desligados <- ifelse(caged_sg_2019_pb$admitidos_desligados == 1, 1,0)

# probabilidade de um trabalhador negro ser admitido
library(stats)
unlist(caged_sg_2019_pb$admitdos_desligados)

logit_admissoes <- caged_sg_2019_pb %>%
glm(formula = admitidos_desligados~idade+sexo+raca_cor+subsetor_ibge+grau_instrucao,
         family = binomial(link = "logit"))
summary(object = logit_admissoes)
