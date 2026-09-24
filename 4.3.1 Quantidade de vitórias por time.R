# Ler e carregar os pacotes necessarios
install.packages("tidyr"); library(tidyr)
install.packages("dplyr"); library(dplyr)
install.packages("ggplot2"); library(ggplot2)


dados <- read.csv("DadosSerieA.csv")

# Criar a coluna "vencedor" com o nome do vencedor
dados <- dados %>%
  mutate(vencedor = case_when(
    gols_mandante > gols_visitante ~ time_mandante,
    gols_visitante > gols_mandante ~ time_visitante,
    TRUE ~ "Empate"
  ))

# Criar um data.frame com a contagem de vitórias
vitorias <- dados %>%
  filter(vencedor!="Empate") %>%
  count(vencedor, name = "vitorias_totais") %>%
  arrange(desc(vitorias_totais)) %>%
  select(vencedor, vitorias_totais)

# Gerar o histograma
ggplot(vitorias,aes(x=vitorias_totais))+
  geom_histogram(binwidth = 50,fill="steelblue",color="black",alpha=1,boundary = 0) +
  scale_x_continuous(breaks = seq(0,400,by = 100)) +
  scale_y_continuous(breaks = seq(0,15,by = 3)) +
  theme_minimal() +
  labs(x="Vitórias", y = "Frequência")
