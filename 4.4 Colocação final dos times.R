# Instalar e carregar os pacotes necessarios
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)
# Ler o banco de dados
class <-  read.csv("class_com_regiao.csv",header = TRUE, check.names = FALSE)
# Calcular a média, mínimo e máximo das classificações finais
class <- class %>%  rowwise() %>% mutate(
  media = mean(c_across('2003':'2024'), na.rm = TRUE),
  min = min(c_across('2003':'2024'), na.rm = TRUE),
  max = max(c_across('2003':'2024'), na.rm = TRUE),
  dife = max(c_across('2003':'2024'), na.rm = TRUE) - min(c_across('2003':'2024'), na.rm = TRUE)
)

View(class[,c(1,24,25,27,28,29)])
write.csv(class[,c(1,24,25,27,28,29)], "Tabela 11 – Participações e medidas.csv")

# Gerar o gráfico
ggplot(class,aes(x=Total_Participacoes)) +
  geom_point(aes(y=media,fill = Regiao,shape=Regiao),size=3) +
  scale_fill_manual(values = c("red", "green", "blue","orange","purple"),name= "Região") +
  scale_shape_manual(values = c(21, 22, 23, 24, 25),name = "Região") +
  labs(x="Total de participações",y="Colocação final média") +
  theme_minimal() + 
  theme(axis.title = element_text(size = 16))

# Realizar o teste de correlação de Spearman
attach(class)
cor(media,Total_Participacoes,method = "spearman")
