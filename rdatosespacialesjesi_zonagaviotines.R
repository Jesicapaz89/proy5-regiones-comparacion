library(tidyverse) #Incluye a ggplot2
library(rerddap)
library(raster)
servidores <- servers()
View(servidores)
#Revisemos las primeras opciones disponibles
head(servidores)

servidores %>%
  filter(str_detect(str_to_lower(name), "noaa"))#busca en la columna de nombres lo que contenga la palabra coastwatch también aclarando que me tome independiente de si es minúscula o mayúscula (obligo a que lea en minúscula)
#dice si son datos públicos o no

noaa_url <- servidores %>% 
  filter(short_name == "UAF") %>%
  #Ahora que sabemos el nombre corto, podemos utilizar esto para filtrar los servidores
    pull(url)#extrae la información que está en una celda

#Veamos el resultado
noaa_url #ya tengo el servidor

?ed_search
datos=ed_datasets('grid')

#Podemos hacer una busqueda sencilla de datos de temperatura de la superficie del mar (SST por sus siglas en ingles) usando la funcion `ed_search`.

temp_sup_res_ <- ed_search(query = "sst", 
                          which = "griddap",
                          url = noaa_url)
head(temp_sup_res_$info)

temp_sup_res_2 <- ed_search_adv(query = "sst",
                                #Datos en grilla
                                protocol = "griddap",
                                #Servidor erddap
                                url = noaa_url,
                                #Datos mensuales
                                keywords = "monthly",
                                #Limites espaciales
                                #Limites temporales
                                minTime = "2010",
                                maxTime = "2020")
View(temp_sup_res_2$info)







#aca para probar descargar los datos ORBIS

library(robis)
library(rgdal) # for `ogrInfo()` and `readOGR()`
library(tools) # for `file_path_sans_ext()`
library(dplyr) # for `inner_join()`, `filter()`, `summarise()`, and the pipe operator (%>%)
library(ggplot2) # for `fortify()` and for plotting
library(sp) # for `point.in.polygon()` and `spDists()`
library(tidyr) # for `gather()`
library(readr) # for `write_tsv()`
library(leaflet)
library(lubridate)

fortify.shape <- function(x){
  x@data$id <- rownames(x@data)
  x.f <- fortify(x, region = "id")
  x.join <- inner_join(x.f, x@data, by = "id")
}

subset.shape <- function(x, domain){
  x.subset <- filter(x, long > domain[1] & 
                       long < domain[2] & 
                       lat > domain[3] & 
                       lat < domain[4])
  x.subset
}


?occurrence


ACN_orbis=occurrence(scientificname = "Thalassarche melanophris", startdate = as.Date("2011-04-01"), enddate = as.Date("2015-10-01"))
str(ACN_orbis)



