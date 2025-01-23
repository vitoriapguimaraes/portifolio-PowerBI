# Detecção de Anomalias em Transações Financeiras com Linguagem R e Power BI

# Verifica a pasta de trabalho
getwd()

# Instala os pacotes
install.packages("tidyverse")
install.packages("dplyr")
install.packages("solitude")
install.packages("ggplot2")
install.packages("readr")

# Carrega os pacotes nesta sessão R
library(tidyverse)
library(dplyr)
library(solitude)
library(ggplot2)
library(readr)

# Carrega os dados históricos
dadosHistoricos <- read_csv("dadosHistoricos.csv")
View(dadosHistoricos)

# Cria o modelo de Machine Learning com algoritmo isolationForest
?isolationForest 
modelo_ml = isolationForest$new() 

# Treina o modelo
modelo_ml$fit(dadosHistoricos)

# Faz as previsões com o modelo usando os dados históricos
previsoesHistorico = dadosHistoricos %>%
  modelo_ml$predict() %>%
  arrange(desc(anomaly_score))

View(previsoesHistorico)

# Density Plot 
plot(density(previsoesHistorico$anomaly_score))

# Quanto maior o anomaly score maior a chance do registro ser uma anomalia
# Vamos definir como regra que anomaly score acima de 0.62 é uma anomalia
indicesHistorico = previsoesHistorico[which(previsoesHistorico$anomaly_score > 0.62)]

# Faz o filtro
anomaliasHistorico = dadosHistoricos[indicesHistorico$id, ]
normaisHistorico = dadosHistoricos[-indicesHistorico$id, ]

# Gráfico
colors()
ggplot() + 
  geom_point(data = normaisHistorico, 
             mapping = aes(transacao1,transacao2), 
             col = "skyblue3", 
             alpha = 0.5) + 
  geom_point(data = anomaliasHistorico,
             mapping = aes(transacao1,transacao2), 
             col = "red2", 
             alpha = 0.8)

# Agora carregamos novos dados
novosDados <- read.csv("novosDados.csv")
View(novosDados)

# Previsões com o modelo treinado
previsoesNovosDados = modelo_ml$predict(novosDados)

# Se o anomaly score é maior que 0.62 consideramos como anomalia
indicesNovosDados = previsoesNovosDados[which(previsoesNovosDados$anomaly_score > 0.62)]

# Filtro
anomaliasNovosDados = novosDados[indicesNovosDados$id, ]
normaisNovosDados = novosDados[-indicesNovosDados$id, ]

# Gráfico das previsões
ggplot() + 
  geom_point(data = normaisNovosDados, 
             mapping = aes(transacao1,transacao2), 
             col = "turquoise3", 
             alpha = 0.5) + 
  geom_point(data = anomaliasNovosDados, 
             mapping = aes(transacao1,transacao2), 
             col = "tomato3", 
             alpha = 0.8)

View(previsoesNovosDados)

# Arredondando a coluna 'anomaly_score' para 2 casas decimais
previsoesNovosDados <- previsoesNovosDados %>%
  mutate(anomaly_score = round(anomaly_score, 2))

View(previsoesNovosDados)

# Criando uma nova coluna com base na condição
previsoesNovosDados <- previsoesNovosDados %>%
  mutate(status = ifelse(anomaly_score > 0.62, "anomalia", "normal"))

View(previsoesNovosDados)

library(ggplot2)

# Criando o box plot
ggplot(previsoesNovosDados, aes(x = status, y = anomaly_score, fill = status)) +
  geom_boxplot() +
  labs(title = "Box Plot de Anomalias e Normais",
       x = "Status",
       y = "Anomaly Score") +
  theme_minimal() +
  scale_fill_manual(values = c("anomalia" = "red", "normal" = "blue")) +
  theme(legend.position = "none")

# Salva em disco
write.csv(previsoesNovosDados, "previsoesNovosDados.csv")


