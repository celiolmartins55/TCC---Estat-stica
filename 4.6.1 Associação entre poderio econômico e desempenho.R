# Instalar e carregar os pacotes
install.packages("dplyr")
install.packages("ggplot2")
install.packages("scales")
library(dplyr) 
library(ggplot2)
library(scales)

# Carregar os dados
valor_camp <- read.csv("valores_campeao.csv")

# Criar o gráfico
ggplot(valor_camp, aes(x = ano_campeonato, y = valor_medio_campeao)) +
  geom_segment(aes(x = ano_campeonato, xend = ano_campeonato, y = 0, yend = valor_medio_campeao), color = "darkgray") +
  geom_point(aes(color = campeao_top3), size = 4) +
  scale_color_manual(values = c("TRUE" = "#D4AF37", "FALSE" = "#7F8C8D"), 
                     labels = c("Não era um dos três mais ricos", "Era um dos três mais ricos")) +
  scale_x_continuous(breaks = valor_camp$ano_campeonato) +
  scale_y_continuous(breaks = seq(0, 100, 10),
                     labels = label_dollar(prefix = " € ", suffix = " mi"))+
  theme_minimal() +
  labs(x = "Ano do Campeonato",
       y = "Valor Médio (em milhões de euros)",
       color = "Status") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_line(data = valor_camp, aes(y=valor_medio_ano, linetype = "Média Anual"), 
        color = "darkslategrey",  linewidth = 1) +
  scale_linetype_manual(
        name = NULL, 
        values = c("Média Anual" = "solid"))  +
  geom_text(aes(label = time), vjust = -0.8, size = 3)
  
  
