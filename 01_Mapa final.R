# ============================================================
# PANEL FINAL OPTIMIZADO - LEYENDAS CORREGIDAS
# Exportación: 28 x 16 cm
# ============================================================

library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(patchwork)
library(fs)
library(grid)

dir_out <- "C:/Users/GORKY/Downloads/R para Facebbok/07_Mapa de Elevacion y caracteristicas"
dir_create(dir_out)

# -----------------------------
# 1. Datos base
# -----------------------------

distrito <- st_read(file.path(dir_out, "Poligono.shp"), quiet = TRUE) |>
  st_make_valid() |>
  st_transform(32719)

dem <- rast(file.path(dir_out, "DEM.tif")) |>
  project("EPSG:32719")

dem <- crop(dem, vect(distrito)) |>
  mask(vect(distrito))

names(dem) <- "Altitud"
huepetuhe_utm <- distrito

# -----------------------------
# 2. Variables topográficas
# -----------------------------

pendiente   <- terrain(dem, "slope", unit = "degrees")
orientacion <- terrain(dem, "aspect", unit = "degrees")
tri         <- terrain(dem, "TRI")
tpi         <- terrain(dem, "TPI")

pend_rad <- pendiente * pi / 180
flow_acc <- abs(dem - focal(dem, w = matrix(1, 5, 5), fun = mean, na.policy = "omit"))
twi <- log((flow_acc + 1) / (tan(pend_rad) + 0.001))

# -----------------------------
# 3. Clasificación
# -----------------------------

clasificar_5 <- function(r) {
  
  if (nlyr(r) > 1) r <- r[[1]]
  
  qs <- quantile(
    values(r, na.rm = TRUE),
    probs = seq(0, 1, 0.2),
    na.rm = TRUE
  ) |> as.numeric()
  
  classify(
    r,
    matrix(c(
      qs[1], qs[2], 1,
      qs[2], qs[3], 2,
      qs[3], qs[4], 3,
      qs[4], qs[5], 4,
      qs[5], qs[6], 5
    ), ncol = 3, byrow = TRUE),
    include.lowest = TRUE
  )
}

alt_c   <- clasificar_5(dem)
slope_c <- clasificar_5(pendiente)
tpi_c   <- clasificar_5(tpi)
tri_c   <- clasificar_5(tri)
twi_c   <- clasificar_5(twi)

# -----------------------------
# 4. Curvatura
# -----------------------------

sd_curv <- global(tpi, "sd", na.rm = TRUE)[1, 1]
umbral  <- sd_curv * 0.25

curv_c <- classify(
  tpi,
  matrix(c(
    -Inf, -umbral, 1,
    -umbral, umbral, 2,
    umbral, Inf, 3
  ), ncol = 3, byrow = TRUE)
)

# -----------------------------
# 5. Orientación
# -----------------------------

aspect_c <- classify(
  orientacion,
  matrix(c(
    0, 22.5, 8,
    22.5, 67.5, 1,
    67.5, 112.5, 2,
    112.5, 157.5, 3,
    157.5, 202.5, 4,
    202.5, 247.5, 5,
    247.5, 292.5, 6,
    292.5, 337.5, 7,
    337.5, 360, 8
  ), ncol = 3, byrow = TRUE)
)

# -----------------------------
# 6. Uso del suelo MapBiomas
# -----------------------------

year <- 2024

url <- paste0(
  "https://storage.googleapis.com/mapbiomas-public/initiatives/peru/collection_3/LULC/peru_collection3_integration_v1-classification_",
  year, ".tif"
)

f_mapbiomas <- file.path(dir_out, paste0("mapbiomas_", year, ".tif"))

if (!file.exists(f_mapbiomas)) {
  download.file(url, f_mapbiomas, mode = "wb")
}

uso <- rast(f_mapbiomas)
distrito_wgs <- st_transform(huepetuhe_utm, crs(uso))

uso <- crop(uso, vect(distrito_wgs)) |>
  mask(vect(distrito_wgs)) |>
  project("EPSG:32719", method = "near")

# -----------------------------
# 7. Paletas
# -----------------------------

labs5 <- c("Muy baja", "Baja", "Media", "Alta", "Muy alta")

pal_general <- c(
  "Muy baja" = "#1a9850",
  "Baja"     = "#91cf60",
  "Media"    = "#ffffbf",
  "Alta"     = "#fdae61",
  "Muy alta" = "#d73027"
)

pal_ipt <- c(
  "Muy baja" = "#2c7bb6",
  "Baja"     = "#7fcdbb",
  "Media"    = "#ffffbf",
  "Alta"     = "#fdae61",
  "Muy alta" = "#d7191c"
)

pal_irt <- c(
  "Muy baja" = "#b35838",
  "Baja"     = "#f1a340",
  "Media"    = "#7fbf3f",
  "Alta"     = "#1b9e77",
  "Muy alta" = "#253494"
)

tema_mapa <- theme_void() +
  theme(
    plot.title = element_text(size = 8, face = "bold", hjust = 0),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.35),
    plot.margin = margin(1, 1, 1, 1),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key.size = unit(0.32, "cm"),
    legend.title = element_text(size = 6.2, face = "bold"),
    legend.text = element_text(size = 5.5),
    legend.spacing.y = unit(0.02, "cm"),
    legend.margin = margin(1, 1, 1, 1)
  )

# -----------------------------
# 8. Función mapas 5 clases
# -----------------------------

mapa_5 <- function(r, titulo, paleta = pal_general, pos = c(0.15, 0.23)) {
  
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "valor"
  df$valor <- factor(df$valor, levels = 1:5, labels = labs5)
  
  ggplot() +
    geom_raster(data = df, aes(x, y, fill = valor)) +
    geom_sf(data = huepetuhe_utm, fill = NA, color = "gray25", linewidth = 0.25) +
    scale_fill_manual(values = paleta, drop = FALSE, name = titulo) +
    annotation_scale(location = "bl", width_hint = 0.23, text_cex = 0.45) +
    annotation_north_arrow(
      location = "tr",
      style = north_arrow_fancy_orienteering,
      height = unit(0.55, "cm"),
      width  = unit(0.55, "cm")
    ) +
    coord_sf(expand = FALSE) +
    guides(fill = guide_legend(
      title.position = "top",
      ncol = 1,
      byrow = TRUE
    )) +
    tema_mapa +
    theme(legend.position = pos) +
    labs(title = titulo)
}

# -----------------------------
# 9. Orientación
# -----------------------------

mapa_aspect <- function(r) {
  
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "valor"
  
  df$valor <- factor(
    df$valor,
    levels = 1:8,
    labels = c("Noreste", "Este", "Sureste", "Sur",
               "Suroeste", "Oeste", "Noroeste", "Norte")
  )
  
  ggplot() +
    geom_raster(data = df, aes(x, y, fill = valor)) +
    geom_sf(data = huepetuhe_utm, fill = NA, color = "gray25", linewidth = 0.25) +
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
    annotation_scale(location = "bl", width_hint = 0.23, text_cex = 0.45) +
    annotation_north_arrow(
      location = "tr",
      style = north_arrow_fancy_orienteering,
      height = unit(0.55, "cm"),
      width  = unit(0.55, "cm")
    ) +
    coord_sf(expand = FALSE) +
    guides(fill = guide_legend(ncol = 1, title.position = "top")) +
    tema_mapa +
    theme(
      legend.position = c(0.13, 0.34),
      legend.text = element_text(size = 4.8),
      legend.key.size = unit(0.26, "cm")
    ) +
    labs(title = "Orientación")
}

# -----------------------------
# 10. Curvatura
# -----------------------------

mapa_curv <- function(r) {
  
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "valor"
  
  df$valor <- factor(
    df$valor,
    levels = 1:3,
    labels = c("Cóncava", "Plana", "Convexa")
  )
  
  ggplot() +
    geom_raster(data = df, aes(x, y, fill = valor)) +
    geom_sf(data = huepetuhe_utm, fill = NA, color = "gray25", linewidth = 0.25) +
    scale_fill_manual(
      values = c(
        "Cóncava" = "#2f80ed",
        "Plana"   = "#8c8c8c",
        "Convexa" = "#fff200"
      ),
      name = "Curvatura"
    ) +
    annotation_scale(location = "bl", width_hint = 0.23, text_cex = 0.45) +
    annotation_north_arrow(
      location = "tr",
      style = north_arrow_fancy_orienteering,
      height = unit(0.55, "cm"),
      width  = unit(0.55, "cm")
    ) +
    coord_sf(expand = FALSE) +
    tema_mapa +
    theme(legend.position = c(0.15, 0.25)) +
    labs(title = "Curvatura")
}

# -----------------------------
# 11. Uso del suelo
# -----------------------------

mapa_uso <- function(r) {
  
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "clase"
  
  df <- df |>
    mutate(uso = case_when(
      clase == 3  ~ "Bosque",
      clase == 21 ~ "Mosaico agropecuario",
      TRUE ~ "Otros"
    ))
  
  ggplot() +
    geom_raster(data = df, aes(x, y, fill = uso)) +
    geom_sf(data = huepetuhe_utm, fill = NA, color = "gray25", linewidth = 0.25) +
    scale_fill_manual(
      values = c(
        "Bosque" = "#1b7837",
        "Mosaico agropecuario" = "#fdae61",
        "Otros" = "#d9d9d9"
      ),
      name = "Uso del suelo"
    ) +
    annotation_scale(location = "br", width_hint = 0.23, text_cex = 0.45) +
    annotation_north_arrow(
      location = "tr",
      style = north_arrow_fancy_orienteering,
      height = unit(0.55, "cm"),
      width  = unit(0.55, "cm")
    ) +
    coord_sf(expand = FALSE) +
    tema_mapa +
    theme(
      legend.position = c(0.20, 0.25),
      legend.text = element_text(size = 5),
      legend.key.size = unit(0.30, "cm")
    ) +
    labs(title = "Uso del suelo")
}

# -----------------------------
# 12. Crear mapas
# -----------------------------

p1 <- mapa_5(alt_c,   "Altitud", pos = c(0.15, 0.24))
p2 <- mapa_5(slope_c, "Pendiente", pos = c(0.15, 0.24))
p3 <- mapa_aspect(aspect_c)
p4 <- mapa_curv(curv_c)

p5 <- mapa_5(tpi_c, "Índice de Posición Topográfica", pal_ipt, pos = c(0.16, 0.25))
p6 <- mapa_5(tri_c, "Índice de Rugosidad del Terreno", pal_irt, pos = c(0.16, 0.25))
p7 <- mapa_5(twi_c, "Índice de Humedad Topográfica (TWI)", pal_general, pos = c(0.16, 0.25))
p8 <- mapa_uso(uso)

# -----------------------------
# 13. Panel final
# -----------------------------

panel_final <- (p1 | p2 | p3 | p4) /
  (p5 | p6 | p7 | p8)

panel_final

# -----------------------------
# 14. Exportar
# -----------------------------

ggsave(
  filename = file.path(dir_out, "Panel_final_28x16cm_leyendas_corregidas.png"),
  plot = panel_final,
  width = 28,
  height = 16,
  units = "cm",
  dpi = 600,
  bg = "white"
)