#Carregar os dados
dados <- read.csv("DadosSerieA.csv")
partic <- read.csv("classificacao_brasileirao.csv")
dist <-  read.csv("distanciapercorrida.csv")

# Criar uma função para facilitar 
descri <- function(x) {
  return(c(
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    Media  = mean(x, na.rm = TRUE),
    var  = var(x, na.rm = TRUE),
    cv  = sd(x, na.rm = TRUE)/mean(x,na.rm = TRUE)*100
  ))
}

# Calcular as estatísticas para cada variável 
attach(dados)
descri(gols_mandante);descri(gols_visitante)
descri(partic$Total_Participacoes)
descri(publico[publico!=0])
valor <- subset(dados, ano_campeonato >= 2008)
descri(valor$valor_equipe_titular_mandante);descri(valor$valor_equipe_titular_visitante)
descri(dist$Total_Viagens_KM)
