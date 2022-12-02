# Pedro Milreu Cunha - Doutorando em Economia Aplicada pelo PPGE-UFPB #
# Contato: pcunha.2107@gmail.com #

##########################################################################
###################** CURSO DE INTRODUÇÃO A R - LABIMEC **################
##########################################################################

#---- Script de exemplos nº1: MAPAS #----

# Bibliotecas ----

library(dplyr) # Manipulação de dados
library(readxl) # Importação de arquivos .xlsx

library(sf) # Importação e manipulação de dados geográficos (.shp)
library(tmap) # Criação de choropleth maps
library(RColorBrewer) # Paleta de cores 

# Comando especial ----

`%notin%` <- Negate(`%in%`)

# Importação dos dados geográficos ---- 

dados_geo <- st_read("data/shapefiles/BR_UF_2021.shp")
dados_geo$Sigla_UF <- c("RO", "AC", "AM", "RR", "PA", "AP",
                  "TO", "MA", "PI", "CE", "RN", "PB",
                  "PE", "AL", "SE", "BA", "MG", "ES",
                  "RJ", "SP", "PR", "SC", "RS", "MS",
                  "MT", "GO", "DF")

# Importação dos dados sobre subnotificações de estupros ---- 
# (exemplo retirado da minha dissertação de mestrado)

dados_sub <- read_excel("data/sub_estimadas_estupros.xlsx") %>%
             select(Sigla_UF, sub_lag_end) %>%
             group_by(Sigla_UF) %>%
             mutate(sub_lag_end = mean(sub_lag_end, na.rm = TRUE)) %>%
             unique()

for (uf in dados_geo$Sigla_UF) {
    
    if (uf %notin% dados_sub$Sigla_UF) {
        
        dados_sub[nrow(dados_sub) + 1, ] <- list(uf, NaN)
        
    }
}

# União dos dados ----

dados <- dados_geo %>%
            left_join(dados_sub, by = "Sigla_UF")

# Criação do mapa ----

mapa <- tm_shape(dados) + 
            tm_borders(lwd = 0.55) +
            tm_fill("sub_lag_end",
                    title = "",
                    textNA = "Dado ausente",
                    colorNA = "lightgrey", palette = "GnBu",
                    labels = c("< 10%", "10% - 20%",
                               "20% - 30%", "30% - 40%",
                               "40% - 50%", "50% - 60%",
                               "60% - 70%")) + 
            tm_text("Sigla_UF", size = 0.65, fontfamily = "serif",
                    fontface = "bold", col = "black") +
            tm_credits("Nota: Média do período jan/2012 - dez/2020.",
                       fontface = "italic", size = 0.7,
                       align = "right") +
            tm_credits("Fonte: Elaboração própria a partir dos dados das SSPs.",
                       size = 0.8, align = "right") + 
            tm_layout(main.title = paste("Distribuição espacial da subnotificação",
                                         "de estupros de mulheres no Brasil",
                                         sep = "\n"),
                      title.size = 1,
                      main.title.position = "center", fontfamily = "serif",
                      main.title.fontface = "italic", scale = 1, frame = FALSE,
                      legend.title.size = 1.5, legend.text.size = 0.8,
                      legend.outside.position = c("left", "bottom"))

tmap_save(mapa, "figures/mapa_sub_estupros.svg",
          width = 12, height = 12, units = "cm")
