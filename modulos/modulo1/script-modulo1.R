
###############################################################
###############################################################
#### Sesión 1: Introducción al manejo de datos espaciales  ####
####                                                       ####
#### Elaborado por: Sergio A. Barona                       ####
#### Laboratorio de Economía Aplicada                      ####
#### Departamento de Economía y Finanzas                   ####
#### Pontificia Universidad Javeriana - Cali               ####
####                                                       ####
#### Fecha: 21 de abril de 2025                            ####
###############################################################
###############################################################

##------------------------------------------##
## 1. Cargar librerías y definir directorio ##
##------------------------------------------##

# Si es necesario, instalar las librerías
# install.packages("tidyverse")
# install.packages("sf")
# install.packages("terra")

# cargar librerías
library(tidyverse)
library(sf)
library(terra)

# Consulta: ayuda
??raster

# Ayuda sobre paquetes y funciones (?, help)
?terra
help("terra")

# Definir el directorio de trabajo
# setwd("path")
setwd("C:\\Users\\sergio.barona\\Desktop\\SpatialData-Workshop\\")

##----------------------##
## 2. Datos vectoriales ##
##----------------------##

## Primer ejemplo
# Cargamos el shapefile con la función st_read() de sf
# Shapefile de los países del mundo
countries = st_read(dsn = "vector_data\\countries",
                    layer = "countries")

# Verificar la clase (extensión de un data.frame)
class(countries)

# Examinamos el objeto sf
head(countries)

# Revisar la estructura
str(countries)

# Resumen
summary(countries)

# Verificar el CRS
st_crs(countries)

# Examinar el gráfico
windows()
plot(countries)

# Podemos examinar únicamente la geometría
windows()
plot(st_geometry(countries))

# Note que, si queremos seleccionar un atributo, debemos hacer lo siguiente:
plot(countries$CONTINENT) # No funciona

# Dos opciones:
plot(countries[1])
plot(countries["CONTINENT"])

# Subconjunto de polígonos
# Podemos usar la sintaxis y funciones de dplyr
countries %>% 
  filter(CONTINENT == "Asia") %>%
  st_geometry() %>%
  plot(col="red", add=T) 

# Extraer la tabla de atributos
attr_countries <- st_drop_geometry(countries)

## Segundo ejemplo: selección 

# Leer capa de los departamentos de Colombia
mun.col = st_read(dsn = "vector_data\\DANE_geodata\\",
                   layer = "MGN_ANM_MPIOS_WGS84")
head(mun.col)
plot(st_geometry(mun.col))

# Seleccionar un área de interés
mun1.col = mun.col %>% filter(DPTO_CCDGO  == 76)

# Leer capa de ríos
rivers.data = st_read(dsn = "vector_data\\hydro_rivers",
                    layer = "HydroRIVERS_v10_sa")
head(rivers.data)

# Selección de los ríos en el área seleccionada
rivers.mun = rivers.data[mun1.col,] 

# Ríos en el área seleccionada
plot(st_geometry(mun1.col), col = NA, border = "black", 
     main = "Ríos principales en el área seleccionada")
plot(st_geometry(rivers.mun), col = "blue", add = TRUE)


# En la siguiente sesión... (este gráfico se debe mejorar)
ggplot() +
  geom_sf(data = mun1.col, color = "black") +
  geom_sf(
    data = rivers.mun,
    aes(linewidth = LENGTH_KM, alpha = LENGTH_KM),
    color = "blue"
  ) +
  scale_linewidth_continuous(range = c(0.02, 1.2)) +
  scale_alpha_continuous(range = c(0.4, 1)) +
  theme_minimal() +
  labs(
    title = "Ríos principales resaltados por longitud",
    linewidth = "Longitud (km)",
    alpha = "Longitud (km)"
  ) + guides(linewidth = "none", alpha = "none")



## EJERCICIO 1: proponer un ejercicio muy similar al ejemplo2
##
##
##

##----------------------##
## 2. Datos raster      ##
##----------------------##

# Podemos crear objetos SpatRaster usando la función
# rast()
# Supongamos un raster de 20x20
raster0 <- rast(nrows=20, ncols=20, # dimensión
          xmin=0, xmax=360) 

# Nótese que es de dimensión 20x20. Tiene una sola capa
# Tiene CRS definido: WGS84
raster0
ext(raster0)
res(raster0)
ncell(raster0)
nlyr(raster0)
crs(raster0)
crs(raster0, proj = TRUE)

plot(raster0) # ¿Qué pasa con la figura?
values(raster0) = runif(ncell(raster0))
plot(raster0)

# Leer datos (raster de una única capa)
pop2020 = rast("raster_data\\densidad_pop\\gpw_v4_population_density_rev11_2020_1_deg.tif")

# Número de capas y proyección
ext(pop2020)
res(pop2020)
nlyr(pop2020)
ncell(pop2020)
crs(pop2020)
crs(pop2020, proj = TRUE)
head(pop2020)
plot(pop2020)

# Distribución de los datos
summary(pop2020)
hist(pop2020, breaks = 60, xlim = c(0,2000))

# Leer datos (raster de más de una capa. Opción sencilla: Unir los datos en el tiempo de nightlights)
dmsp =  rast("raster_data\\dmsp.ols\\dmsp.ols.1992.2004.tif")

# Número de capas y proyección
ext(dmsp)
res(dmsp)
nlyr(dmsp)
ncell(dmsp)
crs(dmsp)
crs(dmsp, proj = TRUE)
head(dmsp)

# Distribución de los datos
summary(dmsp)
hist(dmsp, breaks = 60, xlim = c(0,2000))

# Visualizar las capas
plot(dmsp)

##---------------------------------------##
## 3. Sistemas de coordenadas (CRS)      ##
##---------------------------------------##

# Cargar y explorar la capa de los municipios de Colombia
col.mun <- st_read("vector_data/DANE_geodata/", layer = "MGN_ANM_MPIOS")
col.mun
st_crs(col.mun)
plot(col.mun["DPTO_CCDGO"], 
     main = "Municipios y departamentos de Colombia")


# Cargar y explorar la capa de los países del mundo
countries2 = st_read("vector_data/countries_54030/",
                     layer = "countries_epsg54030")
countries2
st_crs(countries2)
plot(st_geometry(countries2))

# Filtrar países fronterizos
border.col <- countries2 %>%
  filter(ISO3 %in% c("COL", "BRA", "PER", "VEN", "ECU"))

# Intentar superponer sin transformar CRS 
plot(st_geometry(border.col), 
     main = "Superposición sin transformar CRS",
     border = "red", lwd = 2)
plot(col.mun["DPTO_CCDGO"],add = TRUE) # No funciona

# ¿Cómo cambiar las coordenadas?
# Método 1: Usando la sintaxis PROJ4
col.mun.wgs84 <- st_transform(col.mun,
                              "+proj=longlat +datum=WGS84")

# Método 2: podemos utilizar el código EPSG
col.mun.wgs84 = st_transform(col.mun, 4326)

# O lo que es lo mismo
col.mun.wgs84 = st_transform(col.mun, "+init=EPSG:4326")

# Hacemos lo mismo con la capa de los países
countries.wgs84 = st_transform(countries2, "+init=EPSG:4326")
border.col.wgs84 = countries.wgs84 %>%
  filter(ISO3 %in% c("COL", "BRA", "PER", "VEN", "ECU"))


# Visualizar correctamente superpuestos
plot(st_geometry(border.col.wgs84), border = "red", lwd = 2,
     main = "Superposición con idéntico CRS")
plot(col.mun.wgs84["DPTO_CCDGO"], add = TRUE)


# Visualización con ggplot2
ggplot() +
  geom_sf(data = border.col.wgs84, fill = NA, color = "black", size = 1) +
  geom_sf(data = col.mun.wgs84, aes(fill = DPTO_CCDGO), color = "white", size = 0.2) +
  labs(title = "Municipios de Colombia y países fronterizos (WGS84)") +
  guides(fill = "none")


## Nota: analizar diferentes proyecciones globales
countries_mercator <- st_transform(countries, 3857) # EPSG:3857 - Web Mercator
countries_robinson <- st_transform(countries, "ESRI:54030") # Robinson

par(mfrow = c(1,2))
plot(st_geometry(countries_mercator), main = "Mercator (EPSG:3857)")
plot(st_geometry(countries_robinson), main = "Robinson (ESRI:54030)")

# Comparar un país en ambas proyecciones
greenland_mercator <- countries_mercator %>% filter(NAME == "Greenland")
greenland_robinson <- countries_robinson %>% filter(NAME == "Greenland")

par(mfrow = c(1,2))
plot(st_geometry(greenland_mercator), main = "Greenland - Mercator")
plot(st_geometry(greenland_robinson), main = "Greenland - Robinson")


# Prueba
# Prueba
# Prueba

# Transformar municipios a UTM zona 18N (EPSG:32618)
countries.utm18 <- st_transform(countries, 32618)
st_write(countries.utm18, 
         "vector_data/countries_32618/countries_epsg32618.shp")

border.col <- countries.utm18 %>%
  filter(ISO3 %in% c("COL", "BRA", "PER", "VEN", "ECU"))
st_crs(border.col)

plot(st_geometry(border.col), border = "red", lwd = 2,
     main = "Superposición con CRS diferentes")
plot(col.mun["DPTO_CCDGO"], add = TRUE)

border.col.wgs84 =  st_transform(border.col, "+init=EPSG:4326")

plot(st_geometry(border.col.wgs84), border = "red", lwd = 2,
     main = "Superposición con CRS ajustadas")
plot(col.mun.wgs84["DPTO_CCDGO"], add = TRUE)




