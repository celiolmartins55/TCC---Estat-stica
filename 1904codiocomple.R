library(dplyr); library(psych); library(tidyr); library(ggplot2)
# sins
dados <- read.csv("C:\\Users\\Celinho\\Downloads\\dados_tcc\\seriea0324.csv")
dados <- read.csv("C:\\Users\\Celinho\\Downloads\\campeonato-brasileiro-full-com-ano.csv")
dados <- dados %>% 
  rename(gols_mandante = mandante_Placar,gols_visitante = visitante_Placar,ano_campeonato=ano)
distancias <- read.csv("C:\\Users\\Celinho\\Downloads\\dados_tcc\\distanciapercorrida")

attach(dados)
descri <- function(x) {
  cv <- sd(x,na.rm = TRUE) / mean(x,na.rm = TRUE) * 100
  return(c(min(x,na.rm = TRUE),max(x,na.rm = TRUE),mean(x,na.rm=TRUE),var(x,na.rm = TRUE),cv))
}
descri(gols_mandante)
descri(gols_visitante)
descri(class$Total_Participacoes)
descri(dados$publico)
descri(dados$valor_equipe_titular_mandante)
descri(dados$valor_equipe_titular_visitante)
descri(distancias$Total_Viagens_KM)
df_gols_long <- dados %>%
  select(gols_mandante, gols_visitante) %>%
  pivot_longer(cols = everything(), names_to = "tipo", values_to = "gols") %>% 
  mutate(tipo = ifelse(tipo == "gols_mandante", "Mandante", "Visitante"))
df_gols_long
ggplot(df_gols_long, aes(x = gols, fill = tipo)) +
  geom_bar(position = "dodge", alpha = 0.5, width = 0.8) +
  scale_fill_manual(values = c("Mandante" = "blue", "Visitante" = "red")) +
  labs(x = "Quantidade de Gols",
       y = "Frequência",
       fill = "Time") +
  geom_text(
    stat = "count",                     
    aes(label = after_stat(count)),     # Define que o texto (label) será essa contagem
    position = position_dodge(width = 1), # Alinha o texto exatamente com a barra
    vjust = -0.5,                       
    size = 3,                         
    color = "black"                    
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = 0:10) +
  theme(
    axis.text = element_text(size = 16),     # Tamanho dos números dos eixos
    axis.title = element_text(size = 18),    # Tamanho dos títulos dos eixos
    legend.text = element_text(size = 16),   # Tamanho dos itens da legenda
    legend.title = element_text(size = 17)   # Tamanho do título da legenda
  )

df_evolucao2 <- dados %>%
  group_by(ano_campeonato) %>%
  summarise(
    Media_Mandante = mean(gols_mandante, na.rm = TRUE),
    Media_Visitante = mean(gols_visitante, na.rm = TRUE),
    Media_Total = mean(gols_mandante + gols_visitante, na.rm = TRUE),
    dife = mean(gols_mandante - gols_visitante, na.rm = TRUE)
  )
ggplot(df_evolucao2, aes(x = ano_campeonato)) +
  
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
  theme_minimal()
install.packages("Kendall")
library(Kendall)
attach(df_evolucao2)
summary(MannKendall(Media_Total))
summary(MannKendall(Media_Mandante))
summary(MannKendall(Media_Visitante))
install.packages("trend");
library(trend)
sens.slope(Media_Total)
sens.slope(Media_Mandante)
sens.slope(Media_Visitante)
dados <- dados %>%
  mutate(Periodo = case_when(
    ano_campeonato <= 2008 ~ "2003-2008",
    ano_campeonato <= 2013 ~ "2009-2013",
    ano_campeonato <= 2018 ~ "2014-2018",
    TRUE                   ~ "2019-2024"
  ))
attach(dados)
dados <- dados %>% 
  mutate(gols_total = gols_mandante + gols_visitante)
dados <- dados[!is.na(dados$gols_total), ]
boxplot(gols_total~Periodo,pch=19,
        ylab = "Gols total",xlab = "Período",cex.lab = 1.4,cex.axis = 1.5,col="steelblue3");points(tapply(
          gols_total,Periodo,mean),pch=19, col="red")
boxplot(gols_mandante~Periodo,pch=19,
        ylab = "Gols do mandante",xlab = "Período",col="steelblue3",cex.lab = 1.4,cex.axis = 1.5,ylim= c(0,10));points(tapply(
          gols_mandante,Periodo,mean),pch=19, col="red")
boxplot(gols_visitante~Periodo,pch=19,
        ylab = "Gols do visitante",xlab = "Período",col="steelblue3",cex.lab = 1.4,cex.axis = 1.5,ylim=c(0,10));points(tapply(
          gols_visitante,Periodo,mean),pch=19, col="red")
tapply(gols_total, Periodo, mean)
tapply(gols_mandante, Periodo, mean)
tapply(gols_visitante, Periodo, mean)
kruskal.test(gols_total~Periodo) 
kruskal.test(gols_mandante~Periodo)$p.value 
kruskal.test(gols_visitante~Periodo)$p.value
dunnTest(gols_total~Periodo, method = "bonferroni",alpha=0.05)
dunnTest(gols_mandante~Periodo, method = "bonferroni",alpha=0.05)
dunnTest(gols_visitante~Periodo, method = "bonferroni",alpha=0.05)

dados <- dados %>%
  mutate(vencedor = case_when(
    gols_mandante > gols_visitante ~ time_mandante,
    gols_visitante > gols_mandante ~ time_visitante,
    TRUE ~ "Empate"
  ))
vitorias <- dados %>%
  filter(vencedor!="Empate") %>%
  count(vencedor, name = "vitorias_totais") %>%
  arrange(desc(vitorias_totais)) %>%
  select(vencedor, vitorias_totais)
write.csv(vitorias,"vitorias.csv")
hist(vitorias$vitorias_totais)
ggplot(vitorias,aes(x=vitorias_totais))+
  geom_histogram(binwidth = 50,fill="steelblue",color="black",alpha=1,boundary = 0) +
  scale_x_continuous(breaks = seq(0,400,by = 100)) +
  scale_y_continuous(breaks = seq(0,15,by = 3)) +
  theme_minimal() +
  labs(x="Vitórias", y = "Frequência") +
  theme(
    axis.text = element_text(size = 16),     # Tamanho dos números dos eixos
    axis.title = element_text(size = 18))

class <-  read.csv("C:\\Users\\Celinho\\Downloads\\class_com_regiao.csv")
attach(class)
shapiro.test(Total_Participacoes)
shapiro.test(media)
write.csv(distancia_total_anual,"distanciapercorrida")
