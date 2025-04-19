
# Datos espaciales del NBI
# diferenciados según municipios

mun_shape <- st_read(dsn = "C:\\Users\\sergio.barona\\Desktop\\SpatialData-Workshop\\vector_data\\DANE_geodata\\",
                     layer = "MGN_ANM_MPIOS") %>% mutate(
                       cod = as.numeric(
                         paste0(DPTO_CCDGO, MPIO_CCDGO))
                     )


# Cargar datos NBI
nbi <- readxl::read_excel("C:\\Users\\sergio.barona\\Desktop\\SpatialData-Workshop\\vector_data\\CNPV_2018\\NBI_MUN_CNPV2018.xlsx")

# Cargar datos de población
pob <- readxl::read_excel("C:\\Users\\sergio.barona\\Desktop\\SpatialData-Workshop\\vector_data\\CNPV_2018\\Pob_MUN_CNPV2018.xlsx")


# Unir los datos
mun_data <- nbi[c("cod_dpto", "nom_dpto", "nom_mun",
                  "cod", "tasa_nbi", "tasa_miseria",
                  "comp_vivi", "comp_servi",
                  "comp_hacin", "comp_inasist",
                  "comp_dep_eco")] 

# Tasas
mun_data$tasa_nbi <- as.numeric(gsub(",", ".", mun_data$tasa_nbi))
mun_data$tasa_miseria <- as.numeric(gsub(",", ".", mun_data$tasa_miseria))
mun_data$comp_vivi <- as.numeric(gsub(",", ".", mun_data$comp_vivi))
mun_data$comp_servi <- as.numeric(gsub(",", ".", mun_data$comp_servi))
mun_data$comp_hacin <- as.numeric(gsub(",", ".", mun_data$comp_hacin))
mun_data$comp_inasist <- as.numeric(gsub(",", ".", mun_data$comp_inasist))
mun_data$comp_dep_eco <- as.numeric(gsub(",", ".", mun_data$comp_dep_eco))

mun.shp.nbi <-  mun_shape[c("cod",
                        "geometry")] %>%
  left_join(mun_data,by = "cod")

st_write(mun.shp.nbi,
         "C:\\Users\\sergio.barona\\Desktop\\SpatialData-Workshop\\vector_data\\NBI_CNPV2018\\NBIMUN.shp")


