
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
library(tidyverse)  # ggplot2, dplyr, y otras herramientas
library(sf)         # Manejo de datos vectoriales
library(terra)      # Manejo de datos raster

# Consulta: ayuda
??raster

# Ayuda sobre paquetes y funciones (?, help)
?terra          # Documentación del paquete terra
help("terra")   # Equivalente

# Definir el directorio de trabajo
# IMPORTANTE: cambiar por su ruta específica
# setwd("path")
setwd("C:\\Users\\sergio.barona\\Desktop\\SpatialData-Workshop\\")

##----------------------##
## 2. Datos vectoriales ##
##----------------------##

## Primer ejemplo: países del mundo
# Cargamos shapefile con la función st_read() de sf
# Shapefile de los países del mundo
countries = st_read(dsn = "vector_data\\countries", # Directorio con los archivos
                    
                    layer = "countries"  # Nombre del archivo sin extensión
                    )

# Inspeccionar el objeto

class(countries)    # sf + data.frame 
head(countries)     # primeras filas (tabla de atributos)
str(countries)      # estructura (geometría + atributos)

# Resumen descriptivo
summary(countries)

# Verificar del CRS
st_crs(countries)

# Extraer la tabla de atributos
attr_countries <- st_drop_geometry(countries)

# Examinar el gráfico con todas las variables (lento)
windows()
plot(countries)

# Podemos examinar únicamente la geometría (más rápido)
windows()
plot(st_geometry(countries))

# Gráfico por atributo
plot(countries$CONTINENT) # No funciona

# Se puede seleccionar usando:
plot(countries["CONTINENT"])

# O indexando
plot(countries[14])    

# Subconjunto de polígonos
# Podemos filtrar usando la sintaxis y funciones de dplyr
countries_asia = countries %>% 
  filter(CONTINENT == "Asia") 

plot(st_geometry(countries))
plot(st_geometry(countries_asia), col="red", add = T) 

## Segundo ejemplo: datos de Colombia 
# Cargar municipios de Colombia (WGS84)
mun.col = st_read(dsn = "vector_data\\DANE_geodata\\",
                  layer = "MGN_ANM_MPIOS_WGS84")

# Inspeccionar el objeto
class(mun.col)    
head(mun.col)     
str(mun.col)      

summary(mun.col)
st_crs(mun.col)

# Examinar la geometría
plot(st_geometry(mun.col))

# Seleccionar un área de interés (Valle del Cauca, código 76)
mun1.col = mun.col %>% filter(DPTO_CCDGO  == 76)
plot(st_geometry(mun1.col))

# Cargar datos hidrográficos
rivers.data = st_read(dsn = "vector_data\\hydro_rivers",
                      layer = "HydroRIVERS_v10_sa")
class(rivers.data)    
head(rivers.data)     
str(rivers.data)      

summary(rivers.data)
st_crs(rivers.data)

# Selección de los ríos dentro del área seleccionada
rivers.mun = rivers.data[mun1.col,] 
head(rivers.mun)

# Visualización combinada
windows()
plot(st_geometry(mun1.col), border = "gray20", 
     main = "Ríos principales en el área seleccionada", lwd = 1.8)
plot(st_geometry(rivers.mun), col = "#0077BB", add = TRUE, lwd = 1.3)


## EJERCICIO 1: 
## Visualizar y comparar la distribución espacial de las sedes educativas
## por nivel educativo (Preescolar, Primaria y Media) en el área urbana´de Cali,
## Después: analizar la distribución espacial en la comuna 22

## Responder estas preguntas:
# 1. ¿Predomina algún nivel educativo?
# 2. ¿Hay concentraciones por nivel educativo?
# 3. ¿En este problema, la ubicación importa?


##----------------------##
## 2. Datos raster      ##
##----------------------##

# Podemos crear objetos SpatRaster usando la función
# rast()
# Supongamos un raster de 20x20
raster0 <- rast(nrows=20, ncols=20, # dimensión
                xmin=0, xmax=360)   # Extensión

# Nótese que es de dimensión 20x20. Tiene una sola capa
# Tiene CRS definido: WGS84
raster0

ext(raster0)        # Extensión 
res(raster0)        # Resolución 
ncell(raster0)      # Número de celdas
nlyr(raster0)       # Número de capas
names(raster0)      # Nombre de las capas
dim(raster0)        # Dimensión (filas, columnas, capas)
crs(raster0)        # CRS
crs(raster0, proj = TRUE) # CRS sintaxis PROJ4

plot(raster0) # ¿Qué pasa con la figura?
values(raster0) = runif(ncell(raster0))
plot(raster0)

# Ahora un conjunto de datos reales: Leer datos (raster de una única capa)
# Ejemplo: raster de densidad poblacional (2020)
pop2020 = rast("raster_data\\densidad_pop\\gpw_v4_population_density_rev11_2020_1_deg.tif")

# Número de capas y proyección
ext(pop2020)
res(pop2020)
ncell(pop2020)
nlyr(pop2020)
names(pop2020)
crs(pop2020)
crs(pop2020, proj = TRUE)
dim(pop2020)

# Resumen y distribución de los valores
summary(pop2020)
hist(pop2020, breaks = 60,
     xlim = c(0,2000), main = "Histograma de densidad poblacional")


## EJERCICIO 2: 
# Explorar raster de capas múltiples
# Leer datos
dmsp =  rast("raster_data\\dmsp.ols\\dmsp.ols.1992.2004.tif")


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
countries_robinson = st_read("vector_data/countries_54030/",
                     layer = "countries_epsg54030")
countries_robinson
st_crs(countries_robinson)
plot(st_geometry(countries_robinson))

# Filtrar países fronterizos
border.col <- countries_robinson %>%
  filter(ISO3 %in% c("COL", "BRA", "PER", "VEN", "ECU"))

# Intentar superposición CRS diferentes (ERROR) 
plot(st_geometry(border.col), 
     main = "Superposición CRS diferentes",
     border = "red", lwd = 2)
plot(col.mun["DPTO_CCDGO"],add = TRUE) # No funciona

# ¿Cómo cambiar el CRS?
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


# Superposición correcta
plot(st_geometry(border.col.wgs84), border = "red", lwd = 2,
     main = "Superposición con idéntico CRS (WGS84)")
plot(col.mun.wgs84["DPTO_CCDGO"], add = TRUE)


#####################################################
#####################################################
## Nota: analizar diferentes proyecciones globales
countries_mercator <- st_transform(countries, 3857) # EPSG:3857 - Web Mercator
countries_robinson <- st_transform(countries, "ESRI:54030") # Robinson

par(mfrow = c(1,2))
plot(st_geometry(countries_mercator), main = "Mercator (EPSG:3857)")
plot(st_geometry(countries_robinson), main = "Robinson (ESRI:54030)")

# Demostración de distorsión con Groenlandia
greenland_mercator <- countries_mercator %>% filter(NAME == "Greenland")
greenland_robinson <- countries_robinson %>% filter(NAME == "Greenland")

par(mfrow = c(1,2))
plot(st_geometry(greenland_mercator), main = "Greenland - Mercator")
plot(st_geometry(greenland_robinson), main = "Greenland - Robinson")

