# extrai dados de clorofila  "chlor_a" de arquivos .nc 
# download em https://oceancolor.gsfc.nasa.gov/l3/
# script adaptado de Antônio Olinto: https://stat.ethz.ch/pipermail/r-sig-geo/2015-September/023494.html 
# criação conjunta com Raul de Sá Durlo

library(lubridate)
library(tidyverse)

pasta <- '/home/usuario/.../requested_files_1_czcs/requested_files'
arquivos <- list.files(pasta, pattern = 'a\\.nc', full.names = TRUE)

# define área de estudo
lonmax<-25
lonmin<--60
latmax<-10
latmin<--45

extrai_dados_do_arquivo_nc <- function(caminho_do_arquivo) {
  
  # leitura do arquivo nc
  dado_nc <- nc_open(caminho_do_arquivo)
  
  # extrai latitude, longitude e clorofila value
  lon <- ncvar_get(dado_nc, "lon")
  lat <- ncvar_get(dado_nc, "lat")
  value <- ncvar_get(dado_nc, "chlor_a")
  
  # matrix para data.frame (valor da chlor_a para cada ponto lat lon)
  dimnames(value) <- list(lon = lon, lat = lat)
  dat.var <- melt(value, id = "lon", value.name = "chlor_a")
  
  # seleciona a área de estudo, remove dados NA e agrupa por lat lon a cada 1°x1°
  dado_final <- dat.var %>% 
    as_tibble() %>% 
    filter(
      lon <= lonmax & 
        lon >= lonmin &
        lat <= latmax &
        lat >= latmin &
        !is.na(chlor_a)
    ) %>% 
    mutate(
      ano = year(ncatt_get(dado_nc, 0, "time_coverage_end")$value),
      mes = month(ncatt_get(dado_nc, 0, "time_coverage_end")$value),
      lat_round = round(lat, digits = 0),
      lon_round = round(lon, digits = 0)
    ) %>% 
    group_by(ano, mes, lat = lat_round, lon = lon_round) %>% 
    summarise(
      chlor_a = mean(chlor_a),
      .groups = "drop"
    )
  
  return(dado_final)
  
}

# testando a função
extrai_dados_do_arquivo_nc(caminho_do_arquivo = arquivos[2])

# coloca a função no loop (map_df) e salva o csv
CZCS <- map_df(arquivos, extrai_dados_do_arquivo_nc)

write_csv(CZCS, path = str_glue("{pasta}/CZCS_clorofila.csv"))

