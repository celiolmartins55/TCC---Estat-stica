# Instalar e carregar os pacotes necessarios
install.packages(c("Kendall","trend","gglot2"))
library(Kendall)
library(trend)
library(ggplot2)



# Carregar os dados
Ano = c(2003:2024)
Media_Publico = c(10468, 7556, 13600, 12401, 17461, 16992, 17869, 14839, 
                  14886, 12977, 14951, 16555, 17044, 15188, 15968, 18840, 
                  21236, 0, 14632, 20680, 26502, 25773)
pub <- data.frame(Ano,Media_Publico) 




# Criar o gráfico de barras
ggplot(data=pub,aes(x = factor(Ano), y = Media_Publico)) +
  geom_col(fill = "steelblue", width = 0.9) +
  labs(x="Ano",y="Média de publico") + 
  geom_text(aes(label = round(Media_Publico, 1)), vjust = -0.5, size =3) +
  theme_minimal()


# Retirar os anos de 2020 e 2021
Media_filtrada <- Media_Publico[c(-18,-19)]

# Realizar o teste de ljung-box
Box.test(Media_filtrada, lag = 10, type = "Ljung-Box")


# Aplicar o teste de Mann-Kendall e Sens Slope 
summary(MannKendall(Media_filtrada))
sens.slope(Media_filtrada)



