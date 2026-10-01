# Instalar e carregar os pacotes
install.packages("dplyr")
install.packages("tidyr")
install.packages("knitr")
library(dplyr)
library(tidyr)
library(knitr)

# Carregar os dados
dados <- read.csv("C:\\Users\\Celinho\\Documents\\DadosSerieA.csv")
dist <-  read.csv("C:\\Users\\Celinho\\Downloads\\dados_tcc\\distanciapercorrida.csv")

#Criar a função de calcular
descri <- function(x) {
  return(c(
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    Media  = mean(x, na.rm = TRUE),
    var  = var(x, na.rm = TRUE),
    cv  = sd(x, na.rm = TRUE)/mean(x,na.rm = TRUE)*100
  ))
}

# Criar a função de gerar o código LaTeX quando recebe o ano
gerar_tabela <- function(ano_desejado) {

  dados_filtrados <- dados %>%
    filter(ano_campeonato == ano_desejado)
  
  dist_filtrados <-  dist %>%
    filter(Ano == ano_desejado)
  
  estat_mandante  <- descri(dados_filtrados$gols_mandante)
  estat_visitante <- descri(dados_filtrados$gols_visitante)
  estat_publico   <- descri(dados_filtrados$publico[dados_filtrados$publico != 0])
  estat_valor_mandante <-  descri(dados_filtrados$valor_equipe_titular_mandante)
  estat_valor_visitante <-  descri(dados_filtrados$valor_equipe_titular_visitante)
  estat_dist   <- descri(dist_filtrados$Total_Viagens_KM)
  
  tabela <- rbind(
    "Gols Mandante"  = estat_mandante,
    "Gols Visitante" = estat_visitante,
    "Público presente"        = estat_publico,
    "Valor da equipe titular (casa)" = estat_valor_mandante,
    "Valor da equipe titular (fora)" = estat_valor_visitante,
    "Distância percorrida"  = estat_dist
  )
  codigo_latex <- kable(tabela, 
                        format = "latex", 
                        booktabs = TRUE, 
                        digits = 2,
                        format.args = list(decimal.mark = ",", big.mark = "."),
                        linesep = "",
                        caption = paste("Estatísticas Descritivas por Ano -", ano_desejado)
                        )
  return(codigo_latex)
}

# Testando a função
gerar_tabela(2008)


# Criar um arquivo para armazenar 
file.create("todas_as_tabelas.tex")

# Criar o loop que vai passar ano por ano
for (ano in 2003:2024) {
  codigo_latex <- gerar_tabela(ano)
  cat(codigo_latex, file = "todas_as_tabelas.tex", append = TRUE)
  cat("\n\\vspace{1cm}\n\n", file = "todas_as_tabelas.tex", append = TRUE)
}
