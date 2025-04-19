
# Datos vectoriales

# Imagen 1: Simple features
library(sf)
pt1 <- st_point(c(1, 1))
pt2 <- st_point(c(2, 3))
mp  <- st_multipoint(rbind(c(4, 2), c(5, 3), c(4.5, 2.5), c(5.5, 2.2)))
poly <- st_polygon(list(rbind(c(7, 1), c(9, 1), c(8, 3), c(7, 1))))
line <- st_linestring(rbind(c(3, 0.5), c(3.5, 2), c(5, 1.5)))

geoms <- st_sfc(pt1, pt2, mp, poly, line)

df <- data.frame(
  name = c("Alpha", "Beta", "Gamma", "Delta", "Epsilon"),
  type = c("Single Point", "Single Point", "MultiPoint", "Polygon", "Line"),
  value = c(10, 20, 30, 40, 50)
)

sf_obj <- st_sf(df, geometry = geoms)
library(ggplot2)
img1 = ggplot(sf_obj) + geom_sf(aes(col = type, fill = type), size = 3) + theme_bw() +
  theme(legend.title = element_blank(), legend.position = "bottom")

ggsave(file = "C:\\Users\\sergio.barona\\Desktop\\SpatialData-Workshop\\modulos\\\\modulo1\\img\\example_sf.jpeg",
       plot = img1, dpi = 300,
       height = 5, width = 9)

# Imagen 2: mapa interesante
img2 = st_read("C:\\Users\\sergio.barona\\Desktop\\SpatialData-Workshop\\vector_data\\countries\\",
               layer = "countries") %>% dplyr::filter(CONTINENT == "Asia")
img2_plot = ggplot(img2) + geom_sf(col = "black", fill = "gray80") +
  theme_bw()

ggsave(file = "C:\\Users\\sergio.barona\\Desktop\\SpatialData-Workshop\\modulos\\\\modulo1\\img\\example_countries.jpeg",
       plot = img2_plot, dpi = 300,
       height = 5, width = 9)


# Imagen raster 1
library(terra)
x <- rast(ncol=36, nrow=18, xmin=-1000, xmax=1000, ymin=-100, ymax=900)
res(x)
crs(x) <- "+proj=utm +zone=48 +datum=WGS84"
ncell(x)
values(x) = runif(ncell(x))
# Convert raster to data frame
df <- as.data.frame(x, xy = TRUE)

# Plot with ggplot2
raster1 = ggplot(df, aes(x = x, y = y, fill = lyr.1)) +
  geom_raster() +
  scale_fill_viridis_c() +
  coord_equal() +
  labs(
    fill = "Valor",
    x = " ",
    y = " ",
    title = "Raster (648 cells)"
  ) +
  theme_minimal() + scale_y_continuous(expand = c(0,0))
ggsave(file = "C:\\Users\\sergio.barona\\Desktop\\SpatialData-Workshop\\modulos\\\\modulo1\\img\\raster_648cells.jpeg",
       plot = raster1, dpi = 300,
       height = 5, width = 9)

# Imagen raster 2
raster2 <- rast("C:\\Users\\sergio.barona\\Desktop\\SpatialData-Workshop\\raster_data\\densidad_pop\\gpw_v4_population_density_rev11_2020_1_deg.tif")
df_raster2 <- as.data.frame(raster2, xy = TRUE)

# Check the value column name
value_col <- "gpw_v4_population_density_rev11_2020_1_deg"

# Create intervals (bins) for population density
# Example: 6 bins with custom breaks (modify as needed)
breaks <- c(0, 1, 10, 50, 100, 500, Inf)
labels <- c("0-1", "1-10", "10-50", "50-100", "100-500", ">500")

df_raster2$density_cat <- cut(
  df_raster2[[value_col]],
  breaks = breaks,
  labels = labels,
  include.lowest = TRUE,
  right = FALSE
)

# Plot with discrete color scale
plot_raster2 = ggplot(df_raster2, aes(x = x, y = y, fill = density_cat)) +
  geom_raster() +
  scale_fill_brewer(palette = "YlOrRd", na.value = "white", 
                    name = "Habitantes/km²") +
  coord_equal() +
  labs(
    x = " ",
    y = " ",
    title = " ",
    fill = " "
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file = "C:\\Users\\sergio.barona\\Desktop\\SpatialData-Workshop\\modulos\\\\modulo1\\img\\raster_habitanteskm2.jpeg",
       plot = plot_raster2, dpi = 300,
       height = 5, width = 9)
