# Instalar e carregar os pacotes
install.packages("dplyr")
install.packages("ggplot2")
install.packages("scales")
library(dplyr) 
library(ggplot2)
library(scales)

# Carregar os dados
dados <- read.csv("DadosSerieA.csv")

# Criar uma tabela com valores para cada ano e criar o gráfico
media_valor <-  dados %>%
  group_by(ano_campeonato) %>%
  summarise(
    media_valor_ano = mean((valor_equipe_titular_mandante+valor_equipe_titular_visitante)/2, na.rm = TRUE)
  )

ggplot(data = media_valor, aes(x = ano_campeonato, y = media_valor_ano)) +
  geom_line(color = "dodgerblue4", linewidth = 1, alpha = 0.8) +
  geom_point(color = "firebrick2", size = 3) +
  scale_x_continuous(breaks = seq(2005, 2024, by = 1)) +
  scale_y_continuous(
    breaks = seq(10, 40, by = 10),
    labels = label_dollar(prefix = " € ", suffix = " mi")
  ) +
  labs(
    x = "Ano do Campeonato",
    y = "Valor médio dos times (em milhões de euros)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 12))






# Selecionar alguns anos e criar os histogramas
data_08 = filter(dados,ano_campeonato =='2008')
data_13 = filter(dados,ano_campeonato =='2013')
data_18 = filter(dados,ano_campeonato =='2018')
data_23 = filter(dados,ano_campeonato =='2023')
hist(c(data_08$valor_equipe_titular_mandante,
       data_08$valor_equipe_titular_visitante),
     ylab="Frequência",xlab="Valor",main="",ylim = c(0,300),col="steelblue", cex.lab=1.4, xlim= c(0,50))
hist(c(data_13$valor_equipe_titular_mandante,
       data_13$valor_equipe_titular_visitante),
     ylab="Frequência",xlab="Valor",main="",ylim = c(0,300),col="steelblue", cex.lab=1.4, xlim = c(0,50))
hist(c(data_18$valor_equipe_titular_mandante,
       data_18$valor_equipe_titular_visitante),
     ylab="Frequência",xlab="Valor",main="",ylim = c(0,300),col="steelblue", cex.lab=1.4,xlim = c(0,120))
hist(c(data_23$valor_equipe_titular_mandante,
       data_23$valor_equipe_titular_visitante),
     ylab="Frequência",xlab="Valor",main="",ylim = c(0,300),col="steelblue", cex.lab=1.4, xlim = c(0,120))


# Fazer o teste de normalidade para todos os anos
teste_normalidade <- dados %>%
  filter(ano_campeonato >= 2008) %>%
  # 2. Agrupamos por ano
  group_by(ano_campeonato) %>%
  summarise(
    estatistica_W = shapiro.test(c(valor_equipe_titular_mandante,valor_equipe_titular_visitante))$statistic,
    p_valor = shapiro.test(c(valor_equipe_titular_mandante,valor_equipe_titular_visitante))$p.value
  ) %>%
  mutate(
    distribuicao = ifelse(p_valor < 0.05, "Não Normal", "Normal")
  )

teste_normalidade
