#Ler os dados 
dados <- read.csv("DadosSerieA.csv")

# Instalar os pacotes
# install.packages("tidyr"); install.packages("dplyr"); install.packages("ggplot2"); install.packages("psych")
library(dplyr); library(psych); library(tidyr); library(ggplot2)


# Transformar para formato longo
df_gols_long <- dados %>%
  select(gols_mandante, gols_visitante) %>%
  pivot_longer(cols = everything(), names_to = "tipo", values_to = "gols") %>% 
  mutate(tipo = ifelse(tipo == "gols_mandante", "Mandante", "Visitante"))

#Criar o gráfico
ggplot(df_gols_long, aes(x = gols, fill = tipo)) +
  geom_bar(position = "dodge", alpha = 0.5, width = 0.8) +
  scale_fill_manual(values = c("Mandante" = "blue", "Visitante" = "red")) +
  labs(x = "Quantidade de Gols",
       y = "Frequência",
       fill = "Time") +
  geom_text(
    stat = "count",                     
    aes(label = after_stat(count)),     
    position = position_dodge(width = 1), 
    vjust = -0.5,                       
    size = 3,                         
    color = "black"                    
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = 0:10) +
  theme(
    axis.text = element_text(size = 16),     
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 17)
  )

# Criar um novo data frame com as médias de cada ano e seu gráfico
df_evolucao <- dados %>%
  group_by(ano_campeonato) %>%
  summarise(
    Media_Mandante = mean(gols_mandante, na.rm = TRUE),
    Media_Visitante = mean(gols_visitante, na.rm = TRUE),
    Media_Total = mean(gols_mandante + gols_visitante, na.rm = TRUE)
  )

ggplot(df_evolucao, aes(x = ano_campeonato)) +
  geom_line(aes(y = Media_Total, color = "Média Total"), size = 1) +
  geom_line(aes(y = Media_Mandante, color = "Mandante"), size = 1) +
  geom_line(aes(y = Media_Visitante, color = "Visitante"), size = 1) +
  scale_color_manual(values = c(
    "Média Total" = "black", 
    "Mandante" = "blue", 
    "Visitante" = "red"),
    breaks = c("Média Total", "Mandante", "Visitante"),
    labels = c("Média Total (Decresce)", "Mandante (Decresce)", "Visitante (Estacionário)")
  ) +
  labs(x = "Ano",
       y = "Média de Gols",
       color = "Legenda") + 
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),     
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 17)
  )

# Instalar os pacotes necessarios 
# install.packages("Kendall"); install.packages("trend"); install.packages("FSA"); install.packages("modifiedmk")
library(Kendall);library(trend);library(FSA); library(modifiedmk)


# Testar a independência
attach(df_evolucao)
Box.test(Media_Total,type = "Ljung-Box")
Box.test(Media_Mandante,type = "Ljung-Box")
Box.test(Media_Visitante,type = "Ljung-Box")

# Realizar o teste de Mann-Kendall apropriado para cada variável
mmkh(Media_Total)
mmkh(Media_Mandante)
summary(MannKendall(Media_Visitante))

# Cria uma nova variável com os periodos e para o total de gols
dados <- dados %>%
  mutate(Periodo = case_when(
    ano_campeonato <= 2008 ~ "2003-2008",
    ano_campeonato <= 2013 ~ "2009-2013",
    ano_campeonato <= 2018 ~ "2014-2018",
    TRUE                   ~ "2019-2024"
  ))

dados <- dados %>% 
  mutate(gols_total = gols_mandante + gols_visitante)

# Calcular a média pra cada período 
attach(dados)
tapply(gols_total, Periodo, mean)
tapply(gols_mandante, Periodo, mean)
tapply(gols_visitante, Periodo, mean)

# Criar o box-plot para cada um
boxplot(gols_total~Periodo,pch=19,
        ylab = "Gols total",xlab = "Período",cex.lab = 1.5,col="steelblue3");points(tapply(
          gols_total,Periodo,mean),pch=19, col="red")
boxplot(gols_mandante~Periodo,pch=19,
        ylab = "Gols do mandante",xlab = "Período",cex.lab = 1.5,col="steelblue3");points(tapply(
          gols_mandante,Periodo,mean),pch=19, col="red")
boxplot(gols_visitante~Periodo,pch=19,
        ylab = "Gols do visitante",xlab = "Período",cex.lab = 1.5,col="steelblue3");points(tapply(
          gols_visitante,Periodo,mean),pch=19, col="red")


# Aplicar o teste de Kruskal-Wallis em cada variável
kruskal.test(gols_total~Periodo) 
kruskal.test(gols_mandante~Periodo) 
kruskal.test(gols_visitante~Periodo)

# Aplicar o teste de Dunn com correção de bonferroni
dunnTest(gols_total~Periodo, method = "bonferroni",alpha=0.05)
dunnTest(gols_mandante~Periodo, method = "bonferroni",alpha=0.05)
dunnTest(gols_visitante~Periodo, method = "bonferroni",alpha=0.05)
