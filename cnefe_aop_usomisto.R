## Chamada das bibliotecas
library(geobr); library(tidyverse); library(sf); library(arrow);
library(tictoc); library(aopdata); library(RColorBrewer); 
library(spdep); library(spatialreg)


#(0) Lendo os endereços de Salvador ####
## Site do IBGE para baixar os dados por município:
# https://www.ibge.gov.br/estatisticas/downloads-estatisticas.html?caminho=Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Coordenadas_enderecos/Municipio
## Lendo os dados do CNEFE como arrow table (substituir no parâmetro 'sources' 
## o código IBGE do município de interesse): 
end <- open_dataset(sources = '2927408.csv', format = "csv", delimiter = ';')

## Lendo o arquivo geográfico dos hexágonos do projeto AOP (IPEA):
## Substituir com o nome do município de interesse no parâmetro 'city'
ssa <- aopdata::read_landuse(city = 'Salvador', geometry = T)

ssa <- ssa %>% 
  select(id_hex,P001,T001) %>% # Somente as variáveis P001 e T001 foram coletadas
  filter(P001 > 0 | T001 > 0) # Somente hexágonos com população (P001) e emprego (T001) > 0


#(1) Geoprocessamento ####
## (1a) Transformando em tibble e depois sf para as operações de join:
end_tb <- as_tibble(end)
end_sf <- st_as_sf(end_tb, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

## (1b) Fazendo o join especial dos pontos do CNEFE com os hexágonos do AOP
tic()
points_in_hex <- end_sf %>% st_join(ssa) 
toc() # ~37 segundos (depende da máquina)

## (1c) Contagem dos pontos por tipo de endereço dentro de cada hexágono
tic()
counts <- points_in_hex %>% 
  st_drop_geometry() %>% 
  group_by(id_hex, COD_ESPECIE) %>%  
  summarise(count = n(), .groups = 'drop') 
toc() # 0.17 segundos

## (1d) Transformando em formato wide para ter uma coluna por tipo de endereço
wide_counts <- counts %>%
  pivot_wider(names_from = COD_ESPECIE, values_from = count,
              names_prefix = "COD_ESPECIE", values_fill = list(count = 0))

## (1e) Join da base de dados com o arquivo geográfico do projeto AOP
ssa_final <- ssa %>%
  left_join(wide_counts, by = "id_hex")  

# (2) Cálculo do indicador de Entropia para verificar uso misto do solo ####
ssa_final <- ssa_final %>% 
  rowwise() %>% 
  mutate(
    tot = {
      vals <- c_across(starts_with("COD_ESPECIE"))
      sum(vals) 
    },
    prop_res = COD_ESPECIE1/tot,
    prop_n_res = (tot - COD_ESPECIE1)/tot,
    entropy = {
      entropy <- -sum(c_across(starts_with('prop')) * 
                        log(c_across(starts_with('prop'))), na.rm = TRUE)/log(2)
      if (is.nan(entropy) | is.infinite(entropy)) 0 else entropy
    }
  ) %>% ungroup()

# (3) Produção dos mapas ####
## (3a) Mapa de Entropia
ssa_final %>% 
  ggplot() +
  geom_sf(aes(fill = entropy)) +
  scale_fill_viridis_c() +
  labs(fill = '', title = 'Índice de Entropia (Uso do Solo)',
       subtitle = '+ Homogêneo 0 |----------------| 1 + Heterogêneo') +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 10))
ggsave('entropia.jpg', dpi = 500, height = 6, width = 8)

## (3b) Mapa de Uso Residencial
ssa_final %>% 
  ggplot() +
  geom_sf(aes(fill = prop_res)) +
  scale_fill_distiller(palette = 'Spectral', direction = 1) +
  labs(fill = '', title = 'Proporção de Endereços Residenciais') +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5))
ggsave('resid_perc.jpg', dpi = 500, height = 6, width = 8)

# (3) Produzindo LISA ####
## Cria a matriz de vizinhança baseada na geometria hexagonal
mat_viz <- poly2nb(ssa_final, queen = FALSE, snap = 1e-5) # 'queen=FALSE' respeita a forma hexagonal

# Cria a lista de pesos espaciais
pesos_esp <- nb2listw(mat_viz, style = "W", zero.policy = TRUE)

# Calculeao índice LISA
lisa <- localmoran(ssa_final$entropy, pesos_esp)

# AdicionA os resultados do LISA ao seu data frame espacial
ssa_final$lisa <- lisa[,1]
ssa_final$lisa_cat <- attr(lisa, 'quadr')$pysal

# Visualize o mapa do LISA
ssa_final %>% 
  ggplot() + 
  geom_sf(aes(fill = lisa_cat)) + 
  scale_fill_manual(values = c("High-High" = "#DC3323", 
                               "High-Low" = "#E89E9B", 
                               "Low-Low" = "#341DF4", 
                               "Low-High" = "#9E9BF8", 
                               "Not Significant" = "white")) +
  theme_void() +
  labs(fill = "LISA Cluster")
ggsave('lisa_entropia.jpg', dpi = 500, height = 6, width = 8)
