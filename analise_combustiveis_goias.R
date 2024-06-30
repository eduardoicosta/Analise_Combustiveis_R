# Instalar pacotes necessários
if(!require(readxl)) install.packages("readxl")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(lubridate)) install.packages("lubridate")
if(!require(gganimate)) install.packages("gganimate")
if(!require(hrbrthemes)) install.packages("hrbrthemes")

# Carregar bibliotecas
library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
library(gganimate)
library(hrbrthemes)

# Ler dados da planilha Excel
dados_estados <- read_excel("C:/Users/eduar/Downloads/mensal-estados-desde-jan2013.xlsx", col_names = FALSE)

# Remover linhas de cabeçalho extras
dados_estados <- slice(dados_estados, -c(1:16))
colnames(dados_estados) <- dados_estados[1, ]
dados_estados <- dados_estados[-1, ]

# Conversão de tipos
dados_estados$MÊS <- as.Date(as.numeric(dados_estados$MÊS), origin = "1899-12-30")
dados_estados$'NÚMERO DE POSTOS PESQUISADOS' <- as.integer(dados_estados$'NÚMERO DE POSTOS PESQUISADOS')
dados_estados[7:17] <- lapply(dados_estados[7:17], as.numeric)

# Filtrar dados de Goiás e ajustar nomes de produtos
dados_estados_goias <- dados_estados %>% 
  filter(ESTADO == "GOIAS", 
         PRODUTO != "GLP", 
         PRODUTO != "GNV", 
         PRODUTO != "GASOLINA ADITIVADA", 
         PRODUTO != "OLEO DIESEL S10", 
         MÊS >= "2014-05-01")
dados_estados_goias$PRODUTO <- str_replace(dados_estados_goias$PRODUTO, "OLEO DIESEL", "DIESEL")
dados_estados_goias$PRODUTO <- str_replace(dados_estados_goias$PRODUTO, "ETANOL HIDRATADO", "ETANOL")
dados_estados_goias$PRODUTO <- str_replace(dados_estados_goias$PRODUTO, "GASOLINA COMUM", "GASOLINA")

# Definir cores personalizadas
cores_personalizadas_eg <- c("#13678A", "#038C3E", "#7A577A")

# Criar gráfico animado
anim_estados_goias <- ggplot(dados_estados_goias, aes(x = MÊS, y = `PREÇO MÉDIO REVENDA`, color = PRODUTO)) +
  geom_line(size = 1.2) +
  geom_point(shape = 19, size = 4, alpha = 0.5) +
  geom_text(aes(label = paste0("R$ ", format(round(`PREÇO MÉDIO REVENDA`, 2), nsmall = 2))), 
            vjust = -0.5, hjust = 0, size = 5, show.legend = FALSE) +
  labs(title = "Preço Médio de Revenda de Combustíveis em Goiás",
       color = "Produto") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(limits = c(1, 8), breaks = seq(1, 8, by = 1)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 18),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    legend.key.size = unit(1.3, 'cm'),
    legend.position = "left",
    plot.margin = margin(1,1,1,1, "cm"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.margin = margin(0, 1, 0, 0, "cm")
  ) +
  transition_reveal(MÊS) +
  scale_fill_manual(values = cores_personalizadas_eg) +
  scale_color_manual(values = cores_personalizadas_eg)

# Renderizar a animação
animate(anim_estados_goias, width = 800, height = 600,
        nframes = 500,
        fps = 50,
        renderer = gifski_renderer("estado_goias.gif"))
