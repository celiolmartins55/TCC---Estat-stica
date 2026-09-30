# Instalar e carregar os pacotes
install.packages("ggplot2")
library(ggplot2)

# Carregar os dados
dist24 <-  read.csv("distancia_2024.csv")

# Calcula a média da distância por cidade
dist_por_cidade <- aggregate(Total_Viagens_KM ~ Cidade_UF, data = dist24, FUN = mean)

# Criar o gráfico de distância total percorrida por cidade em 2024
ggplot(dist_por_cidade, aes(x = reorder(Cidade_UF, Total_Viagens_KM), y = Total_Viagens_KM)) +
  geom_col(fill = "steelblue") +
  coord_flip() + 
  labs(
    x = "Cidade",
    y = "Distância Percorrida (km)") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14),   
        axis.title.y = element_text(size = 14)    
  )




# Criar o gráfico dos quartis de distância em 2024
ggplot(dist24,aes(x=Quartil_Num,y=Total_Viagens_KM)) +
  geom_point(aes(fill = Regiao,shape=Regiao),size=3) +
  geom_text(aes(label = Estado), hjust = -0.15, size = 3) +
  scale_fill_manual(values = c("red", "green", "blue","orange"),name= "Região") +
  scale_shape_manual(values = c(21, 22, 23, 24),name = "Região") +
  labs(y="Distância percorrida", x = "Quantil") +
  xlim(1, 4.5) +
  theme_minimal()+
  theme(axis.title = element_text(size = 16))

# Calcular as medidas para cada quartil
attach(dist24)
tapply(Vitorias, Quartil_Num, sum)
tapply(Empates, Quartil_Num, sum)
tapply(Derrotas, Quartil_Num, sum)
tapply(Gols_Marcados, Quartil_Num, sum)
tapply(Gols_Tomados, Quartil_Num, sum)
tapply(Colocacao_Final, Quartil_Num, mean)
tapply(Colocacao_Final, Quartil_Num, median)


# Criando a matriz de contingência apenas com as frequências de resultados
resultados_matriz <- matrix(c(78, 80, 56, 65,  # Vitórias
                              48, 54, 52, 48,  # Empates
                              64, 56, 82, 77), # Derrotas
                            nrow = 3,
                            byrow = TRUE)

# Nomeando as linhas e colunas para organizar a saída
rownames(resultados_matriz) <- c("Vitórias", "Empates", "Derrotas")
colnames(resultados_matriz) <- c("Baixo (Q1)", "Moderado (Q2)", "Alto (Q3)", "Extremo (Q4)")

# Visualizando a matriz
print(resultados_matriz)

# Realizando o teste qui-quadrado
teste_resultados <- chisq.test(resultados_matriz)

# Exibindo os resultados do teste
print(teste_resultados)

# Exibindo os valores esperados das medidas de resultado
teste_resultados$expected

# Criando a matriz de contingência apenas com as frequências de Gols
gols_matriz <- matrix(c(254, 243, 212, 220,  # Gols Pró
                        225, 205, 260, 239), # Gols Contra
                      nrow = 2,
                      byrow = TRUE)

# Nomeando as linhas e colunas
rownames(gols_matriz) <- c("Gols Pró", "Gols Contra")
colnames(gols_matriz) <- c("Baixo (Q1)", "Moderado (Q2)", "Alto (Q3)", "Extremo (Q4)")

# Visualizando a matriz
print(gols_matriz)

# Realizando o teste qui-quadrado
teste_gols <- chisq.test(gols_matriz)

# Exibindo os resultados do teste
print(teste_gols)

# Exibindo os valores esperados e os resíduos de gols marcados
teste_gols$expected
teste_gols$residuals
