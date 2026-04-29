# ============================================================
# MAPAS TEMÁTICOS PARA CHOTA- CAJAMARCA
# Usando tu polígono y tu DEM local
# ============================================================

library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyterra)
library(osmdata)
library(geodata)
library(patchwork)
library(fs)

library(sf)
library(ggplot2)
library(ggspatial)
library(raster)

Peru  <- getData('GADM', country='Peru', level=3) %>% st_as_sf()

Per  <- getData('GADM', country='Peru', level=0) %>% st_as_sf()
Amazonas =  subset(Peru , NAME_1 == "Amazonas")


peru <- geodata::gadm(country = "PER", level = 3, path = tempdir())
unique(peru$NAME_3)
MDD <- peru[peru$NAME_3 == "Chota", ]
# Si MDD es SpatVector
MDD_sf <- sf::st_as_sf(MDD)

# Ahora sí transformar
MDDD <- sf::st_transform(MDD_sf, crs = 4326)

write_sf(MDDD, "C:/Users/GORKY/Downloads/R para Facebbok/07_Mapa de Elevacion y caracteristicas/Poligono.shp")
library(elevatr)
elev = get_elev_raster(MDDD, z=13)
#plot(elev)

Poligo_alt    <- crop(elev, MDDD)                           #   
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, MDDD)
plot(Poligo_alt)

# Exportar
writeRaster(
  Poligo_alt,
  "C:/Users/GORKY/Downloads/R para Facebbok/07_Mapa de Elevacion y caracteristicas/DEM.tif",
  overwrite = TRUE
)


# ============================================================
# 1. CARPETA DE SALIDA
# ============================================================

dir_out <- "C:/Users/GORKY/Downloads/R para Facebbok/07_Mapa de Elevacion y caracteristicas"
dir_create(dir_out)

# ============================================================
# 2. CARGAR POLÍGONO Y DEM LOCAL
# ============================================================

distrito <- st_read(
  "C:/Users/GORKY/Downloads/R para Facebbok/07_Mapa de Elevacion y caracteristicas/Poligono.shp",
  quiet = TRUE
)

distrito <- st_make_valid(distrito)
distrito <- st_transform(distrito, 32719)

dem <- rast(
  "C:/Users/GORKY/Downloads/R para Facebbok/07_Mapa de Elevacion y caracteristicas/DEM.tif"
)

dem <- project(dem, "EPSG:32719")

dem <- crop(dem, vect(distrito))
dem <- mask(dem, vect(distrito))

names(dem) <- "Altitud"



# ============================================================
# 3. VARIABLES TOPOGRÁFICAS
# ============================================================

pendiente <- terrain(dem, v = "slope", unit = "degrees")
names(pendiente) <- "Pendiente"

orientacion <- terrain(dem, v = "aspect", unit = "degrees")
names(orientacion) <- "Orientacion"

tri <- terrain(dem, v = "TRI")
names(tri) <- "TRI"

tpi <- terrain(dem, v = "TPI")
names(tpi) <- "TPI"

curvatura <- terrain(dem, v = "roughness")
names(curvatura) <- "Curvatura"


# ============================================================
# 9. CLASIFICAR EN 5 CATEGORÍAS
# ============================================================
library(terra)

clasificar_5 <- function(r) {
  
  # Si tiene varias capas, usa solo la primera
  if (nlyr(r) > 1) {
    r <- r[[1]]
  }
  
  # Extraer valores
  vals <- values(r, na.rm = TRUE)
  
  # Calcular cuantiles
  qs <- quantile(
    vals,
    probs = c(0, 0.2, 0.4, 0.6, 0.8, 1),
    na.rm = TRUE
  )
  
  qs <- as.numeric(qs)
  
  # Evitar cortes repetidos
  qs <- unique(qs)
  
  # Si hay pocos valores únicos
  if (length(qs) < 6) {
    stop("El ráster tiene muy pocos valores distintos para crear 5 clases.")
  }
  
  # Matriz de reclasificación
  rcl <- matrix(
    c(
      qs[1], qs[2], 1,
      qs[2], qs[3], 2,
      qs[3], qs[4], 3,
      qs[4], qs[5], 4,
      qs[5], qs[6], 5
    ),
    ncol = 3,
    byrow = TRUE
  )
  
  rc <- classify(
    r,
    rcl = rcl,
    include.lowest = TRUE
  )
  
  return(rc)
}

alt_c <- clasificar_5(dem)

plot(alt_c)

alt_c   <- clasificar_5(dem)
slope_c <- clasificar_5(pendiente)
spi_c   <- clasificar_5(spi)
sti_c   <- clasificar_5(sti)
tri_c   <- clasificar_5(tri)
twi_c   <- clasificar_5(twi)
tpi_c   <- clasificar_5(tpi)
dd_c    <- clasificar_5(densidad_drenaje)
dist_c  <- clasificar_5(dist_vias)

# ============================================================
# 10. CURVATURA Y ORIENTACIÓN
# ============================================================

curv_c <- clasificar_5(curvatura)

aspect_c <- classify(
  orientacion,
  rcl = matrix(
    c(
      0, 22.5, 8,
      22.5, 67.5, 1,
      67.5, 112.5, 2,
      112.5, 157.5, 3,
      157.5, 202.5, 4,
      202.5, 247.5, 5,
      247.5, 292.5, 6,
      292.5, 337.5, 7,
      337.5, 360, 8
    ),
    ncol = 3,
    byrow = TRUE
  )
)

# ============================================================
# 11. PALETAS
# ============================================================

pal5 <- c(
  "Muy baja" = "#1a9850",
  "Baja"     = "#91cf60",
  "Media"    = "#ffffbf",
  "Alta"     = "#fdae61",
  "Muy alta" = "#d73027"
)

labs5 <- c("Muy baja", "Baja", "Media", "Alta", "Muy alta")

# ============================================================
# 12. FUNCIÓN MAPA 5 CLASES
# ============================================================

mapa_5 <- function(r, titulo) {
  
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "valor"
  
  df$valor <- factor(
    df$valor,
    levels = 1:5,
    labels = labs5
  )
  
  ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = valor)) +
    geom_sf(data = huepetuhe_utm, fill = NA, color = "gray30", linewidth = 0.3) +
    scale_fill_manual(values = pal5, drop = FALSE, name = titulo) +
    coord_sf(expand = FALSE) +
    theme_void() +
    theme(
      legend.position = c(0.18, 0.22),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 13),
      plot.title = element_text(size = 13, face = "bold"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    ) +
    annotation_scale(
      location = "bl",
      width_hint = 0.3,
      text_cex = 0.8
    ) +
    
    # 🧭 NORTE
    annotation_north_arrow(
      location = "tr",
      which_north = "true",
      style = north_arrow_fancy_orienteering,
      height = unit(1.2, "cm"),
      width = unit(1.2, "cm")
    )+
    labs(title = titulo)
}


mapa_aspect <- function(r) {
  
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "valor"
  
  df$valor <- factor(
    df$valor,
    levels = 1:8,
    labels = c(
      "Noreste", "Este", "Sureste", "Sur",
      "Suroeste", "Oeste", "Noroeste", "Norte"
    )
  )
  
  ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = valor)) +
    geom_sf(data = huepetuhe_utm, fill = NA, color = "gray30", linewidth = 0.3) +
    scale_fill_manual(
      values = c(
        "Noreste" = "#ff9900",
        "Este" = "#ffff00",
        "Sureste" = "#00ff00",
        "Sur" = "#00ffff",
        "Suroeste" = "#3399ff",
        "Oeste" = "#6600ff",
        "Noroeste" = "#ff00ff",
        "Norte" = "#ff0000"
      ),
      name = "Orientación"
    ) +
    coord_sf(expand = FALSE) +
    theme_void() +
    annotation_scale(
      location = "bl",
      width_hint = 0.3,
      text_cex = 0.8
    ) +
    
    # 🧭 NORTE
    annotation_north_arrow(
      location = "tr",
      which_north = "true",
      style = north_arrow_fancy_orienteering,
      height = unit(1.2, "cm"),
      width = unit(1.2, "cm")
    )+
    theme(
      legend.position = c(0.10, 0.30),
      legend.title = element_text(size = 8, face = "bold"),
      legend.text = element_text(size = 7),
      plot.title = element_text(size = 11, face = "bold"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    ) +
    labs(title = "Orientación")
}


p1  <- mapa_5(alt_c, "Altitud")
p2  <- mapa_5(slope_c, "Pendiente")
p3  <- mapa_aspect(aspect_c)



ggsave(
  filename = file.path(dir_out, "Altitud.png"),
  plot = p1 ,
  width = 7,
  height = 7,
  dpi = 500,
  bg = "white"
)

ggsave(
  filename = file.path(dir_out, "Pendiente.png"),
  plot = p2 ,
  width = 7,
  height = 7,
  dpi = 500,
  bg = "white"
)

ggsave(
  filename = file.path(dir_out, "Orientación.png"),
  plot = p3 ,
  width = 7,
  height = 7,
  dpi = 500,
  bg = "white"
)






# ============================================================
# CURVATURA EN 4 CLASES: CÓNCAVA, PLANA, CONVEXA
# ============================================================

# Curvatura real aproximada desde el DEM
curvatura <- terrain(dem, v = "flowdir")
names(curvatura) <- "Curvatura"

# Alternativa más visual: usar TPI como curvatura morfológica
curvatura_base <- terrain(dem, v = "TPI")
names(curvatura_base) <- "Curvatura"

# Definir umbral alrededor de 0
sd_curv <- global(curvatura_base, "sd", na.rm = TRUE)[1, 1]
umbral <- sd_curv * 0.25

curv_c <- classify(
  curvatura_base,
  rcl = matrix(
    c(
      -Inf, -umbral, 1,   # Cóncava
      -umbral, umbral, 2, # Plana
      umbral, Inf, 3      # Convexa
    ),
    ncol = 3,
    byrow = TRUE
  )
)

names(curv_c) <- "Curvatura"

mapa_curvatura <- function(r) {
  
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "valor"
  
  df$valor <- factor(
    df$valor,
    levels = 1:3,
    labels = c("Cóncava", "Plana", "Convexa")
  )
  
  ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = valor)) +
    geom_sf(
      data = huepetuhe_utm,
      fill = NA,
      color = "gray35",
      linewidth = 0.25
    ) +
    scale_fill_manual(
      values = c(
        "Cóncava" = "#2f80ed",
        "Plana"   = "#8c8c8c",
        "Convexa" = "#fff200"
      ),
      name = "Curvatura",
      drop = FALSE
    ) +
    coord_sf(expand = FALSE) +
    theme_void() +
    theme(
      legend.position = c(0.16, 0.22),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 12, face = "bold"),
      panel.border = element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.5
      )
    ) +
    annotation_scale(
      location = "bl",
      width_hint = 0.3,
      text_cex = 0.8
    ) +
    
    # 🧭 NORTE
    annotation_north_arrow(
      location = "tr",
      which_north = "true",
      style = north_arrow_fancy_orienteering,
      height = unit(1.2, "cm"),
      width = unit(1.2, "cm")
    )+
    labs(title = "Curvatura")
}

p4 <- mapa_curvatura(curv_c)

p4

ggsave(
  filename = file.path(dir_out, "Curvatura.png"),
  plot = p4 ,
  width = 7,
  height = 7,
  dpi = 500,
  bg = "white"
)




library(ggspatial)

# =============================
# PALETA EXACTA (como imagen)
# =============================
pal_ipt <- c(
  "Muy baja" = "#2c7bb6",  # azul
  "Baja"     = "#7fcdbb",  # celeste
  "Media"    = "#ffffbf",  # amarillo
  "Alta"     = "#fdae61",  # naranja
  "Muy alta" = "#d7191c"   # rojo
)

labs5 <- c("Muy baja", "Baja", "Media", "Alta", "Muy alta")

# =============================
# FUNCIÓN MAPA MEJORADA
# =============================
mapa_ipt <- function(r) {
  
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "valor"
  
  df$valor <- factor(df$valor,
                     levels = 1:5,
                     labels = labs5)
  
  ggplot() +
    
    geom_raster(data = df,
                aes(x = x, y = y, fill = valor)) +
    
    geom_sf(data = huepetuhe_utm,
            fill = NA,
            color = "gray30",
            linewidth = 0.3) +
    
    scale_fill_manual(
      values = pal_ipt,
      name = "Índice de\nPosición\nTopográfica (IPT)"
    ) +
    
    # 📏 ESCALA (como imagen)
    annotation_scale(
      location = "bl",
      width_hint = 0.3,
      text_cex = 0.8
    ) +
    
    # 🧭 NORTE
    annotation_north_arrow(
      location = "tr",
      which_north = "true",
      style = north_arrow_fancy_orienteering,
      height = unit(1.2, "cm"),
      width = unit(1.2, "cm")
    ) +
    
    coord_sf(expand = FALSE) +
    
    theme_void() +
    
    theme(
      legend.position = c(0.18, 0.25),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      plot.title = element_text(size = 14, face = "bold"),
      panel.border = element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.6
      )
    ) +
    
    labs(title = "Índice de Posición Topográfica")
}

# =============================
# CREAR MAPA
# =============================
p5 <- mapa_ipt(tpi_c)

p5

ggsave(
  filename = file.path(dir_out, "Índice de Posición Topográfica.png"),
  plot = p5 ,
  width = 7,
  height = 7,
  dpi = 500,
  bg = "white"
)





library(ggspatial)
library(grid)

# =============================
# PALETA IRT
# =============================

pal_irt <- c(
  "Muy baja" = "#b35838",  # marrón
  "Baja"     = "#f1a340",  # naranja
  "Media"    = "#7fbf3f",  # verde claro
  "Alta"     = "#1b9e77",  # verde oscuro
  "Muy alta" = "#253494"   # azul
)

labs5 <- c("Muy baja", "Baja", "Media", "Alta", "Muy alta")

# =============================
# MAPA IRT
# =============================

mapa_irt <- function(r) {
  
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "valor"
  
  df$valor <- factor(
    df$valor,
    levels = 1:5,
    labels = labs5
  )
  
  ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = valor)) +
    geom_sf(
      data = huepetuhe_utm,
      fill = NA,
      color = "gray30",
      linewidth = 0.3
    ) +
    scale_fill_manual(
      values = pal_irt,
      drop = FALSE,
      name = "Índice de\nRugosidad\ndel Terreno\n(IRT)"
    ) +
    annotation_scale(
      location = "br",
      width_hint = 0.28,
      text_cex = 0.8
    ) +
    annotation_north_arrow(
      location = "tr",
      which_north = "true",
      style = north_arrow_fancy_orienteering,
      height = unit(1.2, "cm"),
      width  = unit(1.2, "cm")
    ) +
    coord_sf(expand = FALSE) +
    theme_void() +
    theme(
      legend.position = c(0.12, 0.26),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text  = element_text(size = 9),
      plot.title   = element_blank(),
      panel.border = element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.5
      )
    )+
    
    labs(title = "Indice de Rugosidad delTerreno (IRT)")
}

# =============================
# CREAR Y EXPORTAR
# =============================

tri_c <- clasificar_5(tri)

p7 <- mapa_irt(tri_c)

p7
ggsave(
  filename = file.path(dir_out, "Indice_Rugosidad_Terreno_IRT.png"),
  plot = p7 ,
  width = 7,
  height = 7,
  dpi = 500,
  bg = "white"
)




flow_acc <- focal(
  dem,
  w = matrix(1, 5, 5),
  fun = mean,
  na.policy = "omit"
)

pend_rad <- pendiente * pi / 180
flow_acc <- abs(dem - flow_acc)
names(flow_acc) <- "Acumulacion"
twi <- log((flow_acc + 1) / (tan(pend_rad) + 0.001))
names(twi) <- "TWI"
twi_c   <- clasificar_5(twi)
p8  <- mapa_5(twi_c, "Índice de Humedad\nTopográfica (TWI)")



ggsave(
  filename = file.path(dir_out, "Índice de Humedad Topográfica (TWI).png"),
  plot = p8 ,
  width = 7,
  height = 7,
  dpi = 500,
  bg = "white"
)







# ============================================================
# 8. USO DEL SUELO - MAPBIOMAS 2024
# ============================================================

year <- 2024

url <- paste0(
  "https://storage.googleapis.com/mapbiomas-public/initiatives/peru/collection_3/LULC/peru_collection3_integration_v1-classification_",
  year,
  ".tif"
)

f_mapbiomas <- file.path(dir_out, paste0("mapbiomas_", year, ".tif"))

if (!file.exists(f_mapbiomas)) {
  download.file(url, f_mapbiomas, mode = "wb")
}

uso <- rast(f_mapbiomas)

distrito_wgs <- st_transform(huepetuhe_utm, crs(uso))

uso <- crop(uso, vect(distrito_wgs))
uso <- mask(uso, vect(distrito_wgs))
uso <- project(uso, "EPSG:32719", method = "near")

names(uso) <- "Uso del suelo"

mapa_uso <- function(r) {
  
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "clase"
  
  df <- df %>%
    mutate(
      uso = case_when(
        clase == 3  ~ "Bosque",
        clase == 15 ~ "Pastizal",
        clase == 21 ~ "Mosaico agropecuario",
        clase == 25 ~ "Sin vegetación",
        clase == 30 ~ "Minería",
        clase == 33 ~ "Río",
        TRUE ~ "Otros"
      )
    )
  
  ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = uso)) +
    geom_sf(data = huepetuhe_utm, fill = NA, color = "gray30", linewidth = 0.3) +
    scale_fill_manual(
      values = c(
        "Bosque" = "#1b7837",
        "Pastizal" = "#a6dba0",
        "Mosaico agropecuario" = "#fdae61",
        "Sin vegetación" = "#b2182b",
        "Minería" = "#ff0000",
        "Río" = "#2166ac",
        "Otros" = "#cccccc"
      ),
      name = "Uso del suelo"
    ) +
    coord_sf(expand = FALSE) +
    theme_void() +
    theme(
      legend.position = c(0.19, 0.22),
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 12),
      plot.title = element_text(size = 12, face = "bold"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    ) +
    labs(title = "Uso del suelo")+
    annotation_scale(
      location = "br",
      width_hint = 0.28,
      text_cex = 0.8
    ) +
    annotation_north_arrow(
      location = "tr",
      which_north = "true",
      style = north_arrow_fancy_orienteering,
      height = unit(1.2, "cm"),
      width  = unit(1.2, "cm")
    ) 
}

p12 <- mapa_uso(uso)


ggsave(
  filename = file.path(dir_out, "Uso del suelo.png"),
  plot = p12 ,
  width = 7,
  height = 7,
  dpi = 500,
  bg = "white"
)






