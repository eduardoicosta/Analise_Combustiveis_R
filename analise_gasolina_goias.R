# Instalar pacotes necessários
if(!require(readxl)) install.packages("readxl")
if(!require(tidyr)) install.packages("tidyr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(scales)) install.packages("scales")
if(!require(curl)) install.packages("curl")
if(!require(png)) install.packages("png")
if(!require(gifski)) install.packages("gifski")
if(!require(gganimate)) install.packages("gganimate")

# Carregar bibliotecas
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(curl)
library(png)
library(gifski)
library(gganimate)

# Ler dados da planilha Excel
dados_municipios <- read_excel("C:/Users/eduar/Downloads/mensal-municipios-jan2022-2024.xlsx", col_names = FALSE)

# Remover linhas de cabeçalho extras
dados_municipios <- dados_municipios[-(1:16), ]
colnames(dados_municipios) <- dados_municipios[1, ]
dados_municipios <- dados_municipios[-1, ]

# Converter e ajustar os tipos de dados
dados_municipios$MÊS <- as.Date(as.numeric(dados_municipios$MÊS), origin = "1899-12-30")
dados_municipios$'NÚMERO DE POSTOS PESQUISADOS' <- as.integer(dados_municipios$'NÚMERO DE POSTOS PESQUISADOS')
dados_municipios[7:17] <- lapply(dados_municipios[7:17], as.numeric)

# Verificar valores faltantes
sapply(dados_municipios, function(x) sum(is.na(x)))

# Filtrar dados para o estado de Goiás e produto "Gasolina Comum"
dados_municipios_goias <- dados_municipios %>% 
  filter(ESTADO == "GOIAS", PRODUTO == "GASOLINA COMUM", MÊS >= "2023-01-01") %>%
  select(-c(2,3,4,6,7,9:18)) %>% 
  arrange(MUNICÍPIO)
dados_municipios_goias$`PREÇO MÉDIO REVENDA` <- round(dados_municipios_goias$`PREÇO MÉDIO REVENDA`, 2)

# Ordenar e ranquear dados
dados_municipios_goias_rank <- dados_municipios_goias %>%
  arrange(MÊS, desc(`PREÇO MÉDIO REVENDA`), MUNICÍPIO) %>%
  group_by(MÊS) %>%
  mutate(rank = row_number(), 
         Value_rel = `PREÇO MÉDIO REVENDA`/`PREÇO MÉDIO REVENDA`[rank==1],
         Value_lbl = paste0(" R$ ", sprintf("%.2f", as.numeric(`PREÇO MÉDIO REVENDA`)))) %>%
  filter(rank <= 10)

# Definir cores personalizadas
cores_personalizadas_mg <- c("#012030", "#13678A", "#45C4B0", "#19543a", "#7bb0a8",
                          "#034159", "#005C53", "#45006e", "#554865", "#b59e67",
                          "#4C4A59", "#1B7F7A", "#0897B4", "#4CABA6", "#7A577A",
                          "#038C3E", "#337704")

# Criar animação com ggplot2 e gganimate
anim_municipios_goias <- ggplot(dados_municipios_goias_rank, aes(rank, group = MUNICÍPIO, fill = MUNICÍPIO, color = MUNICÍPIO)) +
  geom_tile(aes(y = `PREÇO MÉDIO REVENDA`/2, height = `PREÇO MÉDIO REVENDA`, width = 0.9), alpha = 0.8, color = NA) +
  theme(legend.position = "none") +
  geom_text(aes(y = 0, label = paste(MUNICÍPIO, " ")), vjust = 0.2, hjust = 1, size = 6) +
  geom_text(aes(y = `PREÇO MÉDIO REVENDA`, label = Value_lbl, hjust = 0), size = 6) +
  scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(), limits = c(0, 7)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  xlab("") + ylab("") +
  labs(title = 'Preço Médio de Revenda de Gasolina em Goiás : {closest_state}') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0, size = 18),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = margin(1,1,1,8, "cm")) +
  transition_states(MÊS, transition_length = 2, state_length = 2) +
  ease_aes() +
  scale_fill_manual(values = cores_personalizadas_mg) +
  scale_color_manual(values = cores_personalizadas_mg)

# Renderizar a animação
animate(anim_municipios_goias, height = 600, width = 800,
        nframes = 500,
        fps = 50,
        renderer = gifski_renderer("municipios_goias.gif"))