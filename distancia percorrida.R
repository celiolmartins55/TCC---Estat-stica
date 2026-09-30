# Instalar e carregar os pacotes
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr) 
library(ggplot2)


# Carregar os dados
distancias <- read.csv("distanciapercorrida.csv")

# Criar e vizualizar a tabeça
tabela <- distancias %>% group_by(Ano) %>% 
  summarise(media = mean(Total_Viagens_KM),var = var(Total_Viagens_KM),
            min = min(Total_Viagens_KM), max = max(Total_Viagens_KM), mediana = median(Total_Viagens_KM))





ggplot(tabela, aes(x = Ano)) +
  geom_point(aes(y = min, fill = "Mínima"), shape = 21, size = 2) +
  geom_point(aes(y = max, fill = "Máxima"), shape = 21, size = 2) +
  geom_point(aes(y = media, fill = "Média"), shape = 21, size = 2) +
  geom_point(aes(y = mediana, fill = "Mediana"), shape = 21, size = 2) +
  geom_line(aes(y = min, color = "Mínima")) +
  geom_line(aes(y = max, color = "Máxima")) +
  geom_line(aes(y = media, color = "Média")) +
  geom_line(aes(y = mediana, color = "Mediana")) +
  scale_fill_manual(
    name = "Distância", 
    values = c("Mínima" = "blue", "Máxima" = "red", "Média" = "black", "Mediana" = "green")
  ) +
  scale_color_manual(
    name = "Distância", 
    values = c("Mínima" = "blue", "Máxima" = "red", "Média" = "black", "Mediana" = "green")
  ) +
  labs(y = "Distância") +
  theme_minimal()
  