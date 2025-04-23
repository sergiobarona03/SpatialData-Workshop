
###############################################################
###############################################################
#### Sesión 2: Introducción al manejo de datos espaciales  ####
####                                                       ####
#### Elaborado por: Sergio A. Barona                       ####
#### Laboratorio de Economía Aplicada                      ####
#### Departamento de Economía y Finanzas                   ####
#### Pontificia Universidad Javeriana - Cali               ####
####                                                       ####
#### Fecha: 22 de abril de 2025                            ####
###############################################################
###############################################################

##------------------------------------------##
## 1. Cargar librerías y definir directorio ##
##------------------------------------------##

# Instalar paquetes (sólo si no están instalados previamente)
# install.packages("tidyverse")
# install.packages("sf")
# install.packages("terra")

# cargar librerías
library(tidyverse)  # ggplot2, dplyr y otras herramientas
library(sf)         # Lectura y manejo de datos vectoriales
library(readxl)     # Para leer archivos de Excel

# Definir el directorio de trabajo
# IMPORTANTE: cambiar por su ruta específica
# setwd("path")
setwd("C:\\Users\\sergio.barona\\Desktop\\SpatialData-Workshop\\")

##----------------------------------##
## 2. Creación manual de objetos sf ##
##----------------------------------##

# Ejemplos de distintas geometrías

# Puntos
pt1 <- st_point(c(1, 1))
pt2 <- st_point(c(2, 3))

# Conjunto de puntos
mp  <- st_multipoint(rbind(c(4, 2), c(5, 3), c(4.5, 2.5), c(5.5, 2.2)))

# Polígono
poly <- st_polygon(list(rbind(c(7, 1), c(9, 1), c(8, 3), c(7, 1))))

# Línea
line <- st_linestring(rbind(c(3, 0.5), c(3.5, 2), c(5, 1.5)))

# Agrupar las geometrías en una columna (sfc)
geoms <- st_sfc(pt1, pt2, mp, poly, line)

# Crear atributos asociados a cada geometría
df <- data.frame(
  name = c("Alpha", "Beta", "Gamma", "Delta", "Epsilon"),
  type = c("Single Point", "Single Point", "MultiPoint", "Polygon", "Line"),
  value = c(10, 20, 30, 40, 50)
)

# Combinar atributos y geometrías en un objeto sf
sf_obj <- st_sf(df, geometry = geoms)


# Visualizar con la función base
plot(st_geometry(sf_obj), 
     col = c("red", "red", "blue", "green", "purple"), 
     pch = 16, 
     lwd = 2, 
     main = "Diferentes tipos de geometrías")

##-------------------------------------------------##
## 3. Cargar datos vectoriales reales (shapefiles) ##
##-------------------------------------------------##

# Shapefile con datos de NBI por departamento
dpto.nbi = st_read(dsn = "C:\\Users\\sergio.barona\\Desktop\\SpatialData-Workshop\\vector_data\\nbi_dptos\\",
                   layer = "dptoscol_nbi_cnpv2018")

# Explorar el objeto sf
class(dpto.nbi)  
names(dpto.nbi)
dim(dpto.nbi)
head(dpto.nbi)     
str(dpto.nbi)   
st_crs(dpto.nbi)
st_bbox(dpto.nbi)

# Resumen descriptivo
summary(dpto.nbi)

plot(st_geometry(dpto.nbi))

##---------------------------##
## 4. Selección de variables ##
##---------------------------##

# Seleccionar variable (vector y pierde la clase sf)
dpto.nbi$tasa_nb

# ¿Cómo conservar la clase sf?
# Usar corchetes simples y seleccionar por nombre
dpto.nbi["tasa_nb"]
dpto.nbi[5]           # Otra forma: indexación

# Mapa básico de tasa de NBI
plot(dpto.nbi["tasa_nb"])

##---------------------------------------##
## 5. Visualización con ggplot2: geom_sf ##
##---------------------------------------##

# Si bien la función plot() es útil, podemos realizar
# una visualización más detallada utilizando
# la función geom_sf() y funciones adicionales

# Visualización básica
# ggplot() + geom_sf(data = ..., aes(fill = ...,
# col = ..., ...),...)

ggplot(dpto.nbi) + geom_sf(aes(fill = tasa_nb))

# Elementos adicionales
# geom_sf_text() para agregar etiquetas (texto) sobre los polígonos
# coord_sf() asegura que todas las capas emplean un CRS común
# (por defecto: EPSG:4326)

ggplot(dpto.nbi) +
  geom_sf(aes(fill = tasa_nb), color = "grey80", size = 0.3) +
  geom_sf_text(
    data = dpto.nbi %>% filter(tasa_nb > 50),
    aes(label = dpto),
    size = 2, color = "red", fontface = "bold"
  ) +
  scale_fill_gradient(
    low = "#f7fbff",
    high = "#08306b",
    name = "Tasa NBI (%)"
  ) +
  theme_bw(base_size = 18) +
  theme(
    legend.position = "right",
    panel.grid.major = element_line(color = "grey90", size = 0.2),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 9, color = "grey40")
  ) +
  labs(
    title = "Tasa de NBI por Departamento en Colombia",
    subtitle = "Etiquetas solo para departamentos con NBI > 50%",
    caption = "Fuente: DANE (2018). CNPV",
    x = " ", y = " "
  ) +
  coord_sf()


##--------------------------------------------------##
## 6. Unión de datos externos con datos espaciales  ##
##--------------------------------------------------##

# Shapefile con datos de NBI a nivel municipal
nbi.mun = st_read(dsn = "vector_data\\NBI_CNPV2018\\",
                  layer = "NBIMUN")
ggplot() + geom_sf(data = nbi.mun, aes(fill = tasa_nb),
                   col = "black")

# Datos externos: ocupación (%)
ocup.mun = readxl::read_excel("vector_data\\CNPV_2018\\OCUP_MUN_CNPV2018.xlsx")

#Unir datos usando la variable llave "cod" (código del municipio)
nbi.ocup = nbi.mun %>% left_join(ocup.mun[c("cod", "percent_en_el_municipio")],
                                by = "cod") %>%
  rename(ocup = percent_en_el_municipio)

# Mapa: ocupación diferenciada por municipios
ggplot() + geom_sf(data = nbi.ocup, aes(fill = ocup),
                   col = "black")

#### Ejercicio 1: añadir la IPM
#### Instrucciones:
#### 1. Cargar archivo con indicadores de pobreza multidimensional (IPM)
#### 2. Unir IPM al shapefile de municipios
#### 3. Visualización del IPM diferenciado por municipios

##---------------------------------------##
## 5. Funciones útiles: funciones st_*() ##
##---------------------------------------##

#### Función st_union: unir múltiples geometrías en una sola
# Agrupar municipios del Valle del Cauca

# Filtrar municipios del Valle del Cauca
nbi.valle <- nbi.ocup %>% filter(nom_dpt  == "VALLE DEL CAUCA")

ggplot(nbi.valle) + geom_sf(aes(fill = tasa_nb))

# Unir la geometría de de los municipios 
# Para las variables restantes, debemos seleccionar una
# función para agregar los valores (e.g. promedio)

nbi.valle.union <- nbi.valle %>%
  summarise(nom_dpt = unique(nom_dpt),
            cod_dpt = unique(cod_dpt),
            tasa_nb = mean(tasa_nb, na.rm = T),
            ocup = mean(ocup, na.rm = T),
            geometry = st_union(geometry) # une la geometría
            )
nbi.valle.union
plot(st_geometry(nbi.valle.union))

# Esto se puede generalizar para todos los municipios
nbi.dptos <- nbi.ocup %>%
  group_by(nom_dpt) %>%
  summarise(nom_dpt = unique(nom_dpt),
            cod_dpt = unique(cod_dpt),
            tasa_nb = mean(tasa_nb, na.rm = T),
            ocup = mean(ocup, na.rm = T),
            geometry = st_union(geometry)
  )

ggplot(nbi.dptos) + geom_sf(aes(fill = tasa_nb))

#### Funciones st_centroid(), st_coordinates(), st_as_sf()
# Una aplicación común es la creación de objetos sf
# para añadir a nuestros datos

# Calcular centroides usando la función st_centroid()
cent.dptos <- st_centroid(nbi.dptos)

# Para extraer las coordenadas de los centroides, usamos la función st_coordinates() 
valle <- cent.dptos %>% filter(cod_dpt == 76) %>% st_coordinates()
bogota <- cent.dptos %>% filter(cod_dpt == 11) %>% st_coordinates()

# Creamos una sfc (columna que contiene la geometría, i.e. línea)
line_vb <- st_sfc(st_linestring(rbind(valle, bogota)),
                  crs = st_crs(nbi.dptos)) 

# Convertimos en un objeto sf
line_sf <- st_sf(geometry = line_vb)

# Primero: visualizar todos los centroides
ggplot() +
  geom_sf(data = nbi.dptos, fill = "white", color = "black") +
  geom_sf(data = cent.dptos,
          color = "red", size = 1)

# Segunda:  línea entre el centroide de Bogotá y Valle del Cauca
ggplot() +
  geom_sf(data = nbi.dptos, fill = "white", color = "black") +
  geom_sf(data = line_sf, color = "red", size = 1.2) +
  geom_sf(data = cent.dptos %>% filter(cod_dpt %in% c(76, 11)),
          color = "blue", size = 1) 

# También podemos añadir puntos específicos de interés
# Filtrar la geometría para Bogotá
bogota = mun.col %>% filter(cod_dpto == 11)

# Crear las coordenadas de interés:
aux.coord <- data.frame(
  place = c("Parque Simon Bolivar"),
  long = c(-74.09413442050968),
  lat = c(4.658164647475244)) %>% 
  st_as_sf(coords = c("long", "lat"))

# Asignar el CRS
st_crs(aux.coord) = st_crs(bogota)

# Visualizar
ggplot() + geom_sf(data = bogota) +
  geom_sf(data = aux.coord, col = "red") +
  geom_sf_text(data = aux.coord,
               aes(label = place),
               size = 2, vjust = -1, col = "red")

####  st_intersects()
# Podemos utilizar la función para 
# identificar intersecciones en objetos espaciales

# Crear puntos aleatorios
points <- st_sample(nbi.dptos, size = 100)

# Visualizar los puntos y los polígonos
ggplot() + geom_sf(data = nbi.dptos) + geom_sf(data = points,
                                              col = "red")

# ¿Qué puntos caen en qué polígonos?
inter <- st_intersects(nbi.dptos, points)

# Contar los puntos por polígono
nbi.dptos$count <- lengths(inter)

# Verificar el conteo:
ggplot() + geom_sf(data = nbi.dptos) +
  geom_sf(data = points,
          col = "red") +
  geom_sf_text(data =  nbi.dptos,
               aes(label = count))

# Creamos intervalos para visualizar mejor los resultados
nbi.dptos$count_cut = cut(nbi.dptos$count, 
                        breaks = c(0,1,4,8, 12),
                        include.lowest = T, right = F)
ggplot() +
  geom_sf(data = nbi.dptos, 
          aes(fill = count_cut),col = "black") + theme_bw()

### Ejercicio 2
### Ejercicio con barrios y las sedes educativas
### Instrucciones del ejercicio:
### Cargar la capa de barrios de Cali
### Cargar la capa de sedes educativas
### Cortar la capa de sedes educativas según la capa de barrios de Cali
### Visualizar ambas capas superpuestas
### ¿Qué puntos caen en qué polígonos?
### Contar las sedes educativas en cada barrio 
### Visualizar usando intervalos

