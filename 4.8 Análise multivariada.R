# Instalar e carregar os pacotes
install.packages("dplyr")
install.packages("tibble")
install.packages("psych")
install.packages("corrplot")
install.packages("factoextra")
library(dplyr)
library(tibble)
library(psych)
library(corrplot)
library(factoextra)


# Carregar os dados
dados_brutos <- read.csv("dados_brutos_multivariada.csv",row.names = 1)
dados_multivariada <- dados_brutos %>%
  mutate(Identificador = paste(time, ano, sep = "_")) %>%
  column_to_rownames("Identificador") %>%
  select(-ano, -time, -Jogos_Fora, -Media_Viagem_KM, -empates)

nomes <- c("Vitórias", "Derrotas", "Gols pró", "Gols contra","Cartões amarelos","Cartões vermelhos",
           "Percurso percorrido","Valor médio","Idade média")
colnames(dados_multivariada) <- nomes
dados_padronizados <- scale(dados_multivariada)

# Criar a matriz de correlação dos dados
matriz_cor <- cor(dados_multivariada,method = "pearson")

# Realizando KMO e Teste de esfericidade de Bartlett, 
KMO(matriz_cor)
cortest.bartlett(matriz_cor,n=140)

# Criar o gráfico da matriz de correlação 
cores_suaves <- colorRampPalette(c("brown2", "#FFFFFF", "#2B6CB0"))(200)
corrplot(matriz_cor, method = "circle", 
         type = "lower",
         col = cores_suaves,
         addCoef.col = "black",
         tl.col = "black", 
         tl.srt = 45,
         number.cex = 0.85,
         tl.cex = 1.3)

# Fazer o PCA e definir o número de componentes pela variância acumulada
modelo_pca <- prcomp(dados_multivariada, scale. = TRUE)
summary(modelo_pca)
reduzido <- modelo_pca$x[,1:4]

# Definido como 4, obtemos os loadings e a variância acumulada 
modelo_pca$rotation[,1:4]
modelo_pca$sdev^2

# Realizar o método do cotovelo
## Método do cotovelo no método sem PCA
fviz_nbclust(dados_padronizados, kmeans, method = "wss") +
  labs(title = "", x = "Número de grupos", y ="WSS")
## Método do cotovelo no método com PCA
fviz_nbclust(reduzido, kmeans, method = "wss") +
  labs(title = "", x = "Número de grupos", y ="WSS")


# Sem PCA: Calcular a distâncias e fazer a divisão em 4 grupos
dist_sem_pca <- dist(dados_padronizados, method = "euclidean")
hierarquico_sem_pca <- hclust(dist_sem_pca, method = "ward.D2")
grupos_sem_pca <- cutree(hierarquico_sem_pca, 4)

# Sem PCA: Calcular média das medidas de cada um
table(grupos_sem_pca)
dados_grupo_sem_pca <- dados_multivariada %>% mutate(grupo=grupos_sem_pca)
resultado_sem_pca <- aggregate(. ~ dados_grupo_sem_pca$grupo, 
                       data = dados_grupo_sem_pca, 
                       FUN = mean, 
                       na.rm = TRUE) 
View(resultado_sem_pca) ##OBS: no pdf houve uma inversão entre o grupo 2 e 4 para melhor comparação


# Com PCA: Calcular a distâncias e fazer a divisão em 4 grupos
distancias_pca <- dist(reduzido, method = "euclidean")
hierarquico_pca <- hclust(distancias_pca, method = "ward.D2")
grupos_pca <- cutree(hierarquico_pca,4)

# Com PCA: Calcular média das medidas de cada um
table(grupos_pca)
dados_grupo_pca <- dados_multivariada %>% mutate(grupo=grupos_pca)
resultado_pca <- aggregate(. ~ dados_grupo_pca$grupo, 
                        data = dados_grupo_pca, 
                        FUN = mean, 
                        na.rm = TRUE) 
View(resultado_pca)


# Fazer a contagem da aparição de cada time usando a abordagem com PCA
contagem <- dados_brutos %>% mutate(grupo=grupos_pca) %>%  group_by(time) %>%
  summarise(n1 = sum(grupo==1),n2 = sum(grupo==2),n3 = sum(grupo==3),n4 = sum(grupo==4))
View(contagem)


