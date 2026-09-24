# Instalar e carregar os pacotes necessarios
install.packages(c("ggplot2", "sf", "geobr", "dplyr", "Kendall", "trend"))
library(ggplot2)
library(sf)
library(dplyr)
library(Kendall)
library(trend)

# Carregar os dados com o número de gols marcados por cada estado
dados_estados <- read.csv("gols_por_estado.csv")

# Carregar as geometrias dos estados através do pacote geobr
estados_br <- st_read("BR_UF_2020.shp")

# Unir os dados
mapa_dados <- estados_br %>%
  left_join(dados_estados, by = c("SIGLA_UF" = "estado"))

# Criar os mapas
ggplot() +
  geom_sf(data = mapa_dados, aes(fill = gols_como_mandante), color = "black", linewidth = 0.2) +
  scale_fill_gradient(
    low = "#A020F0",
    high = "#FFD700",
    na.value = "gray90", 
    name = "Gols",
    limits = c(0, 3500),               
    breaks = seq(500, 3500, by = 1000) 
  ) +
  theme_void()  +
  theme(
    legend.position = "right"
  )

ggplot() +
  geom_sf(data = mapa_dados, aes(fill = gols_como_visitante), color = "black", linewidth = 0.2) +
  scale_fill_gradient(
    low = "#A020F0",
    high = "#FFD700",
    na.value = "gray90", 
    name = "Gols",
    limits = c(0, 3500),               
    breaks = seq(500, 3500, by = 1000) 
  ) +
  theme_void()  +
  theme(
    legend.position = "right"
  )

# Carregar os dados de participações de cada estado 
partic_est <- read.csv("participacoes_por_estado.csv")

# Juntar os dados e calcular a média
medias <- dados_estados %>% left_join(partic_est, by =c("estado"="estado"))
medias <- medias %>% group_by(estado) %>% 
  summarise(golsmad = gols_como_mandante/participacoes, golsvis = gols_como_visitante/participacoes)
mapa_medias <- estados_br %>%
  left_join(medias, by = c("SIGLA_UF" = "estado"))

# Criar os novos mapas com gols em média
ggplot() +
  geom_sf(data = mapa_medias, aes(fill = golsmad), color = "black", linewidth = 0.2) +
  scale_fill_gradient(
    low = "#A020F0",
    high = "#FFD700",
    na.value = "gray90", 
    name = "Gols",
    limits = c(0, 40),               
    breaks = seq(0, 40, by = 10)  
  ) +
  theme_void()  +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "right"
  ) 

ggplot() +
  geom_sf(data = mapa_medias, aes(fill = golsvis), color = "black", linewidth = 0.2) +
  scale_fill_gradient(
    low = "#A020F0",
    high = "#FFD700",
    na.value = "gray90", 
    name = "Gols",
    limits = c(0, 40),               
    breaks = seq(0, 40, by = 10)     
  ) + 
  theme_void() +
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 17),
    legend.position = "right" 
  )


dadosano <- read.csv("Gols_estados_anos.csv")

# Tirar estados com menos de 5 participações
dadosano <- dadosano %>%
  group_by(estado) %>%
  filter(n_distinct(ano_campeonato) > 4) %>%
  ungroup()

# Aplicas Mann-Kendall e Sen's Slope a todos
resu <- dadosano %>% 
  arrange(estado, ano_campeonato) %>% 
  group_by(estado) %>% 
  summarise(
    p_value = MannKendall(media_geral)$sl[1],
    Sens_Slope = sens.slope(media_geral)$estimates,
    p_value2 = MannKendall(media_mandante)$sl[1],
    Sens_Slope2 = sens.slope(media_mandante)$estimates,
    p_value3 = MannKendall(media_visitante)$sl[1],
    Sens_Slope3 = sens.slope(media_visitante)$estimates,
    anos = n()                      
  )
print(resu)
