# extrai dados de clorofila  "chlor_a" de arquivos .nc 
# download em https://oceancolor.gsfc.nasa.gov/l3/
# script adaptado de Antônio Olinto: https://stat.ethz.ch/pipermail/r-sig-geo/2015-September/023494.html 
# criação conjunta com Raul de Sá Durlo
library(lubridate)
library(tidyverse)

pasta <- '/home/usuario/.../clorofila/requested_files_1_SeaWiFS/requested_files'
arquivos <- list.files(pasta, pattern = 'a\\.nc', full.names = TRUE)

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
  
  # define o ponto médio entre as datas inicial e final
  dateini <- ncatt_get(dado_nc, 0, "time_coverage_start")$value
  dateend <- ncatt_get(dado_nc, 0, "time_coverage_end")$value
  datemean <- mean(c(as.Date(dateend, "%Y-%m-%dT%H:%M:%OSZ"), as.Date(dateini, "%Y-%m-%dT%H:%M:%OSZ")))
  
  # matrix to data.frame (valor da clor_a para cada ponto lat lon)
  dimnames(value) <- list(lon = lon, lat = lat)
  dat.var <- melt(value, id = "lon", value.name = "chlor_a")
  
  # select data from the study area taking out missing data, and grouping by
  # rounded lat lon values
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
      ano = year(datemean),
      mes = month(datemean),
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
extrai_dados_do_arquivo_nc(caminho_do_arquivo = arquivos)

# coloca a função no loop (map_df) e salva o csv
SeaWiFS <- map_df(arquivos, extrai_dados_do_arquivo_nc)

write_csv(SeaWiFS , path = str_glue("{pasta}/SeaWiFS _clorofila.csv"))
