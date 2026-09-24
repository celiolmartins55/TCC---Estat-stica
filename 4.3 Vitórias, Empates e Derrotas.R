# Instalar e carregar os pacotes necessarios
install.packages(c("ggplot2", "sf", "geobr", "dplyr"))
library(ggplot2)
library(sf)
library(dplyr)

# Carregar os dados 
vdest <- read.csv("C:\\Users\\Celinho\\Downloads\\estatisticas_por_estado.csv") 
estados_br <- st_read("C:\\Users\\Celinho\\Documents\\tcc\\mapas\\BR_UF_2020.shp")

mapa_dados2 <- estados_br %>%
  left_join(vdest, by = c("SIGLA_UF" = "estado"))


# Gerar os mapas
## Vitórias
ggplot() +
  geom_sf(data = mapa_dados2, aes(fill = Vitoria), color = "black", linewidth = 0.2) +
  scale_fill_gradient(
    low = "#A020F0",
    high = "#FFD700",
    na.value = "gray90", 
    name = "Vitórias",
    limits = c(0, 1700),               
    breaks = seq(0, 1600, by = 400)
  ) +
  theme_void()  +
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 17),
    legend.position = "right"
  )

## Empates
ggplot() +
  geom_sf(data = mapa_dados2, aes(fill = Empate), color = "black", linewidth = 0.2) +
  scale_fill_gradient(
    low = "#A020F0",
    high = "#FFD700",
    na.value = "gray90", 
    name = "Empates",
    limits = c(0, 1600),               
    breaks = seq(0, 1600, by = 400)
  ) +
  theme_void() +
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 17),
    legend.position = "right"
  )

## Derrotas
ggplot() +
  geom_sf(data = mapa_dados2, aes(fill = Derrota), color = "black", linewidth = 0.2) +
  scale_fill_gradient(
    low = "#A020F0",
    high = "#FFD700",
    na.value = "gray90", 
    name = "Derrotas",
    limits = c(0, 1600),               
    breaks = seq(0, 1600, by = 400)
  ) +
  theme_void() +
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 17),
    legend.position = "right"
  )



# Calcular o aproveitamento
aprov <- vdest %>% group_by(estado) %>%
  summarise(Aproveitamento=(3*Vitoria+Empate)*100/(3*jogos)) 
aprov <- estados_br %>% 
  left_join(aprov, by = c("SIGLA_UF" = "estado"))

# Gerar o mapa
ggplot() +
  geom_sf(data = aprov, aes(fill = Aproveitamento), color = "black", linewidth = 0.2) +
  scale_fill_gradient(
    low = "#A020F0",
    high = "#FFD700",
    na.value = "gray90", 
    name = "Aproveitamento (%)",
    limits = c(10,50),
    breaks = seq(10, 50, by = 10)
  ) +
  theme_void() + 
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 17),
    legend.position = "right"
  )
