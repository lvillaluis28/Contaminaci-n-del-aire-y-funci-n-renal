#Modelo contaminacion del aire
#Paquetes
library(raster)
library(ncdf4)
library(tidyverse)

#ESTE ES EL CORRECTO. Usa coordenadas exactas de estación en curicó y limites de poligono en molina

coord_curico_2 <- data.frame(x = -71.2336, y = -34.9757)
coord_molina_2 <- data.frame(
  x = mean(c(-71.3496, -71.2322)),
  y = mean(c(-35.1465, -35.0307)))

#función para extraer 2,5 de nc
leer_pm25_2 <- function(nombre_archivo, coord_curico_2, coord_molina_2) {
  r <- raster::raster(nombre_archivo, varname = "PM25")
  
  pm_curico_2 <- raster::extract(r, coord_curico_2)
  pm_molina_2 <- raster::extract(r, coord_molina_2)
  
  # Extraer fecha desde el nombre del archivo
  nombre_base_2 <- basename(nombre_archivo)
  fecha_str_2 <- stringr::str_extract(nombre_base_2, "\\d{6}-\\d{6}")
  fecha_ini_2 <- as.Date(paste0(substr(fecha_str_2, 1, 4), "-", substr(fecha_str_2, 5, 6), "-01"))
  
  data.frame(
    Fecha = fecha_ini_2,
    PM25_Global_Curico_2 = pm_curico_2,
    PM25_Global_Molina_2 = pm_molina_2
  )
}

#Aplicar la funcion a todos los archivos nc
setwd("C:/Users/LUIS VILLASEÑOR/Desktop/Magíster Epidemiología/TESIS/contaminación del aire/v6/")
archivos_nc <- list.files(pattern = "\\.nc$", full.names = TRUE)

# Aplicar la función a todos
datos_globales_2 <- map_dfr(archivos_nc, leer_pm25_2, coord_curico_2, coord_molina_2)



#preparar base curicó
# Renombrar y transformar
datos_globales_2$Mes <- format(datos_globales_2$Fecha, "%Y-%m")
curicómensual_2021 <- read.csv("C:/Users/LUIS VILLASEÑOR/Desktop/Magíster Epidemiología/TESIS/contaminación del aire/curicómensual_2021.csv", sep=";")

curico <- curicómensual_2021 %>%
  rename(
    Fecha_cod = FECHA..YYMMDD.,
    PM25_validados = Registros.validados
  ) %>%
  mutate(
    Fecha = as.Date(paste0("20", substr(Fecha_cod, 1, 2), "-", substr(Fecha_cod, 3, 4), "-01")),
    Mes = format(Fecha, "%Y-%m")
  ) %>%
  select(Mes, PM25_validados) %>%
  filter(!is.na(PM25_validados))

#unir las bases de datos
df_modelo_2 <- left_join(datos_globales_2, curico, by = "Mes")

df_modelo_2 <- df_modelo_2 %>%
  mutate(PM25_validados = as.numeric(gsub(",", ".", PM25_validados)))

# Ajustar modelo
modelo_cal_2 <- lm(PM25_validados ~ PM25_Global_Curico_2, data = df_modelo_2)
summary(modelo_cal_2)

df_modelo_2 <- df_modelo_2 %>%
  mutate(anio = lubridate::year(Fecha))

modelo_multi_2 <- lm(PM25_validados ~ PM25_Global_Curico_2 * factor(anio), data = df_modelo_2)
summary(modelo_multi_2)

#Predecir pero con molina global
df_modelo_2$PM25_Molina_Estimado_dg1_cal <- predict(modelo_cal_2, newdata = data.frame(PM25_Global_Curico_2 = df_modelo_2$PM25_Global_Molina_2))


#df_modelo_2$PM25_MolinaCuricó_Estimado <- predict(modelo_cal_2, newdata = df_modelo_2)



#Visualizar el modelo
library(ggplot2)

#Sin IC

df_largo_2 <- df_modelo_2 %>%
  select(Fecha,
         `PM2.5 Estimado (Molina)` = PM25_Molina_Estimado_dg1_cal,
         `PM2.5 Global (Curicó)` = PM25_Global_Curico_2,
         `PM2.5 Medido (Curicó)` = PM25_validados,,
         `PM2.5 Global (Molina)` = PM25_Global_Molina_2) %>%
  tidyr::pivot_longer(-Fecha, names_to = "Tipo", values_to = "PM25")

# Graficar
ggplot(df_largo_2, aes(x = Fecha, y = PM25, color = Tipo, linetype = Tipo)) +
  geom_line(size = 1.2) +
  labs(title = "Comparación de PM2.5 en Molina",
       y = "PM2.5 (µg/m³)",
       x = "Fecha",
       color = "Tipo de dato",
       linetype = "Tipo de dato") +
  theme_minimal()




#Comparación de curicó medido vs curicó global
df_largo_2 <- df_modelo_2 %>%
  select(Fecha,
         `PM2.5 Medido (Curicó)` = PM25_validados,,
         `PM2.5 Global (Curicó)` = PM25_Global_Curico_2) %>%
  tidyr::pivot_longer(-Fecha, names_to = "Tipo", values_to = "PM25")

# Graficar
ggplot(df_largo_2, aes(x = Fecha, y = PM25, color = Tipo, linetype = Tipo)) +
  geom_line(size = 1.2) +
  labs(title = "Comparación de PM2.5 en Curicó medido vs Curicó global: modelo lineal",
       y = "PM2.5 (µg/m³)",
       x = "Fecha",
       color = "Tipo de dato",
       linetype = "Tipo de dato") +
  theme_minimal()




#Comparar estimado Molina vs global Molina
df_largo_2 <- df_modelo_2 %>%
  select(Fecha,
         `PM2.5 Estimado (Molina)` = PM25_Molina_Estimado_dg1_cal,,
         `PM2.5 Global (Molina)` = PM25_Global_Molina_2) %>%
  tidyr::pivot_longer(-Fecha, names_to = "Tipo", values_to = "PM25")

# Graficar
ggplot(df_largo_2, aes(x = Fecha, y = PM25, color = Tipo, linetype = Tipo)) +
  geom_line(size = 1.2) +
  labs(title = "Comparación de PM2.5 estimado para Molina vs Global Molina: modelo lineal",
       y = "PM2.5 (µg/m³)",
       x = "Fecha",
       color = "Tipo de dato",
       linetype = "Tipo de dato") +
  theme_minimal()

#Comparar estimado molina vs medido curicó
df_largo_2 <- df_modelo_2 %>%
  select(Fecha,
         `PM2.5 Estimado (Molina)` = PM25_Molina_Estimado_dg1_cal,,
         `PM2.5 Medido (Curicó)` = PM25_validados) %>%
  tidyr::pivot_longer(-Fecha, names_to = "Tipo", values_to = "PM25")

# Graficar
ggplot(df_largo_2, aes(x = Fecha, y = PM25, color = Tipo, linetype = Tipo)) +
  geom_line(size = 1.2) +
  labs(title = "Comparación de PM2.5 estimado en Molina vs medido en Curicó: modelo lineal",
       y = "PM2.5 (µg/m³)",
       x = "Fecha",
       color = "Tipo de dato",
       linetype = "Tipo de dato") +
  theme_minimal()










#IC
pred <- predict(modelo_cal_2, newdata = data.frame(PM25_Global_Curico_2 = df_modelo_2$PM25_Global_Molina_2),
                interval = "prediction")

df_modelo_2 <- df_modelo_2 %>%
  mutate(
    PM25_Molina_Estimado_dg = pred[, "fit"],
    IC_bajo = pred[, "lwr"],
    IC_alto = pred[, "upr"]
  )

#Con IC
ggplot() +
  # Banda de incertidumbre (solo para PM2.5 Estimado en Molina)
  geom_ribbon(data = df_modelo_2,
              aes(x = Fecha, ymin = IC_bajo, ymax = IC_alto),
              fill = "grey70", alpha = 0.4) +
  
  # Líneas para todos los tipos de PM2.5
  geom_line(data = df_largo_2,
            aes(x = Fecha, y = PM25, color = Tipo, linetype = Tipo),
            size = 1.2) +
  
  labs(title = "Comparación de PM2.5 en Molina",
       y = "PM2.5 (µg/m³)",
       x = "Fecha",
       color = "Tipo de dato",
       linetype = "Tipo de dato") +
  theme_minimal(base_size = 13)






















ggplot(df_modelo_2, aes(x = PM25_Global_Curico_2, y = PM25_validados)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Revisión del ajuste lineal")
modelo_cal_2 <- lm(PM25_validados ~ PM25_Global_Curico_2, data = df_modelo_2)
plot(modelo_spline)


modelo_cal_2 <- lm(PM25_validados ~ PM25_Global_Curico_2, data = df_modelo_2)
qqnorm(resid(modelo_spline))

qqline(resid(modelo_spline), col = "red")
hist(resid(modelo_spline), breaks = 10, col = "lightblue", main = "Histograma de los residuos", xlab = "Residuos")
shapiro.test(resid(modelo_spline))

plot(fitted(modelo_spline), resid(modelo_spline))
abline(h = 0, col = "red")






#MODELO CON SPLINES
library(splines)
modelo_spline <- lm(PM25_validados ~ ns(PM25_Global_Curico_2, df = 3), data = df_modelo_2)
summary(modelo_spline)



df_modelo_2$pred_spline <- predict(modelo_spline)

ggplot(df_modelo_2, aes(x = PM25_Global_Curico_2, y = PM25_validados)) +
  geom_point() +
  geom_line(aes(y = pred_spline), color = "blue", size = 1.2) +
  labs(title = "Modelo con Splines Naturales",
       x = "PM2.5 Global Curicó (ACAG)",
       y = "PM2.5 validados Curicó")


#Predecir pero con molina global
df_modelo_2$PM25_Molina_Estimado_dg_spline <- predict(modelo_spline, newdata = data.frame(PM25_Global_Curico_2 = df_modelo_2$PM25_Global_Molina_2))


#df_modelo_2$PM25_MolinaCuricó_Estimado <- predict(modelo_cal_2, newdata = df_modelo_2)



#Visualizar el modelo
library(ggplot2)

#Sin IC

df_largo_2 <- df_modelo_2 %>%
  select(Fecha,
         `PM2.5 Estimado (Molina)` = PM25_Molina_Estimado_dg_spline,
         `PM2.5 Global (Curicó)` = PM25_Global_Curico_2,
         `PM2.5 Medido (Curicó)` = PM25_validados,,
         `PM2.5 Global (Molina)` = PM25_Global_Molina_2) %>%
  tidyr::pivot_longer(-Fecha, names_to = "Tipo", values_to = "PM25")

# Graficar
ggplot(df_largo_2, aes(x = Fecha, y = PM25, color = Tipo, linetype = Tipo)) +
  geom_line(size = 1.2) +
  labs(title = "Comparación de PM2.5 en Molina",
       y = "PM2.5 (µg/m³)",
       x = "Fecha",
       color = "Tipo de dato",
       linetype = "Tipo de dato") +
  theme_minimal()



#Comparación Molina estimado vs Molina global

df_largo_2 <- df_modelo_2 %>%
  select(Fecha,
         `PM2.5 Estimado (Molina)` = PM25_Molina_Estimado_dg_spline,,
         `PM2.5 Global (Molina)` = PM25_Global_Molina_2) %>%
  tidyr::pivot_longer(-Fecha, names_to = "Tipo", values_to = "PM25")

# Graficar
ggplot(df_largo_2, aes(x = Fecha, y = PM25, color = Tipo, linetype = Tipo)) +
  geom_line(size = 1.2) +
  labs(title = "Comparación de PM2.5 estimado en Molina vs global Molina: Modelo spline",
       y = "PM2.5 (µg/m³)",
       x = "Fecha",
       color = "Tipo de dato",
       linetype = "Tipo de dato") +
  theme_minimal()


#Comparar estimado molina vs medido curicó
df_largo_2 <- df_modelo_2 %>%
  select(Fecha,
         `PM2.5 Estimado (Molina)` = PM25_Molina_Estimado_dg_spline,,
         `PM2.5 Medido (Curicó)` = PM25_validados) %>%
  tidyr::pivot_longer(-Fecha, names_to = "Tipo", values_to = "PM25")

# Graficar
ggplot(df_largo_2, aes(x = Fecha, y = PM25, color = Tipo, linetype = Tipo)) +
  geom_line(size = 1.2) +
  labs(title = "Comparación de PM2.5 estimado Molina vs medido Curicó: modelo spline",
       y = "PM2.5 (µg/m³)",
       x = "Fecha",
       color = "Tipo de dato",
       linetype = "Tipo de dato") +
  theme_minimal()




#IC
pred <- predict(modelo_spline, newdata = data.frame(PM25_Global_Curico_2 = df_modelo_2$PM25_Global_Molina_2),
                interval = "prediction")

df_modelo_2 <- df_modelo_2 %>%
  mutate(
    PM25_Molina_Estimado_dg = pred[, "fit"],
    IC_bajo = pred[, "lwr"],
    IC_alto = pred[, "upr"]
  )

#Con IC
ggplot() +
  # Banda de incertidumbre (solo para PM2.5 Estimado en Molina)
  geom_ribbon(data = df_modelo_2,
              aes(x = Fecha, ymin = IC_bajo, ymax = IC_alto),
              fill = "grey70", alpha = 0.4) +
  
  # Líneas para todos los tipos de PM2.5
  geom_line(data = df_largo_2,
            aes(x = Fecha, y = PM25, color = Tipo, linetype = Tipo),
            size = 1.2) +
  
  labs(title = "Comparación de PM2.5 en Molina",
       y = "PM2.5 (µg/m³)",
       x = "Fecha",
       color = "Tipo de dato",
       linetype = "Tipo de dato") +
  theme_minimal(base_size = 13)









ggplot() +
  # Banda de incertidumbre para estimaciones en Molina
  geom_ribbon(data = df_modelo_2,
              aes(x = Fecha, ymin = IC_bajo, ymax = IC_alto),
              fill = "grey70", alpha = 0.4) +
  
  # Líneas suavizadas por tipo
  geom_smooth(data = df_largo_2,
              aes(x = Fecha, y = PM25, color = Tipo, linetype = Tipo),
              method = "loess", span = 0.3, se = FALSE, size = 1.2) +
  
  # Escalas de color y línea
  scale_color_manual(values = c(
    "PM2.5 Estimado (Molina)" = "firebrick",
    "PM2.5 Global (Curicó)" = "darkgreen",
    "PM2.5 Global (Molina)" = "steelblue",
    "PM2.5 Medido (Curicó)" = "purple"
  )) +
  scale_linetype_manual(values = c(
    "PM2.5 Estimado (Molina)" = "solid",
    "PM2.5 Global (Curicó)" = "dashed",
    "PM2.5 Global (Molina)" = "dotdash",
    "PM2.5 Medido (Curicó)" = "dotted"
  )) +
  
  # Etiquetas y tema
  labs(title = "Comparación de PM2.5 en Molina (2022–2023)",
       subtitle = "Estimaciones spline y datos globales/medidos",
       x = "Fecha", y = expression(PM[2.5]~(µg/m^3)),
       color = "Tipo de dato", linetype = "Tipo de dato") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )




geom_point(size = 2, alpha = 0.5)






#INTENTO MODELO GAM
library(mgcv)

modelo_gam <- gam(PM25_validados ~ s(PM25_Global_Curico_2), data = df_modelo_2)
summary(modelo_gam)
# Crear predicciones
df_modelo_2$pred_gam <- predict(modelo_gam)

ggplot(df_modelo_2, aes(x = PM25_Global_Curico_2, y = PM25_validados)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x), color = "blue", fill = "gray70") +
  labs(title = "Modelo GAM: PM2.5 Validados vs Global Curicó",
       x = "PM2.5 Global Curicó",
       y = "PM2.5 Validados en Curicó") +
  theme_minimal()

AIC(modelo_cal_2, modelo_spline, modelo_gam)
BIC(modelo_cal_2, modelo_spline, modelo_gam)
anova(modelo_spline, modelo_gam, test = "F")



















#Grilla
#USANDO MODELO CORRECTO
#Contaminación molina pero en grilla
leer_grilla_molina_2 <- function(archivo) {
  r <- raster::raster(archivo, varname = "PM25")
  
  # Extensión geográfica aproximada de la comuna de Molina
  extent_molina <- raster::extent(-71.3496, -71.2322, -35.1465, -35.0307)
  
  # Recorte espacial
  r_molina <- crop(r, extent_molina)
  
  # Fecha del archivo
  fecha_str <- stringr::str_extract(basename(archivo), "\\d{6}-\\d{6}")
  fecha_ini <- as.Date(paste0(substr(fecha_str, 1, 4), "-", substr(fecha_str, 5, 6), "-01"))
  
  # Convertir a data.frame con coordenadas
  df <- as.data.frame(r_molina, xy = TRUE)
  names(df)[3] <- "PM25_Global"
  df$Fecha <- fecha_ini
  
  return(df)
}
# Aplicar a todos los archivos
grilla_molina_2 <- purrr::map_dfr(archivos_nc, leer_grilla_molina_2)

#Predice
grilla_molina_2$PM25_Estimado <- predict(modelo_cal_2, newdata = data.frame(PM25_Global_Curico_2 = grilla_molina_2$PM25_Global))

#Ver modelo
library(ggplot2)

ggplot(grilla_molina_2 %>% filter(Fecha == as.Date("2022-05-01")), aes(x = x, y = y, fill = PM25_Estimado)) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_c() +
  labs(title = "PM2.5 estimado en Molina - Mayo 2022 (1 km x 1 km)",
       x = "Longitud", y = "Latitud", fill = "µg/m³") +
  theme_minimal()





#Poner en mapa
library(sf)
library(dplyr)
library(chilemapas)


# Cargar mapa y convertir a sf si es necesario
mapa_comunas <- chilemapas::mapa_comunas

# Forzar conversión si no es sf
if (!inherits(mapa_comunas, "sf")) {
  mapa_comunas <- st_as_sf(mapa_comunas)
}

st_geometry(mapa_comunas)

# Filtrar comuna de Molina (código 07108)
molina_shape <- mapa_comunas %>% filter(codigo_comuna == "07304")
curicó_shape <- mapa_comunas %>% filter(codigo_comuna == "07301")
maule_shape <- mapa_comunas %>% filter(codigo_region == "07")

ggplot() +
  geom_sf(data = molina_shape, fill = "lightblue", color = "black") +
  labs(title = "Comuna de Molina (07304) - Chile")
ggplot() +
  geom_sf(data = curicó_shape, fill = "lightblue", color = "black") +
  labs(title = "Comuna de Molina (07108) - Chile")


# Filtrar para junio de 2022
grilla_junio_2 <- grilla_molina_2 %>%
  filter(Fecha == as.Date("2023-06-01")) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)

molina_shape <- st_transform(molina_shape, crs = 4326)
grilla_molina_junio_rec <- st_intersection(grilla_junio_2, molina_shape)


ggplot() +
  geom_sf(data = molina_shape, fill = NA, color = "black", linewidth = 0.7) +
  geom_sf(data = grilla_molina_junio_rec, aes(color = PM25_Estimado), size = 1.5) +
  scale_color_viridis_c(option = "plasma") +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  labs(
    title = "PM2.5 estimado en Molina - Junio 2022",
    color = "µg/m³"
  )






#Analizando datos que son <0
mean(grilla_molina_2$PM25_Estimado < 0, na.rm = TRUE) * 100
sum(grilla_molina_2$PM25_Estimado < 0, na.rm = TRUE)


negativos <- grilla_molina_2 %>% 
  filter(PM25_Estimado < 0)
negativos_sf <- st_as_sf(negativos, coords = c("x", "y"), crs = 4326)
class(negativos_sf)
# Debería incluir "sf"

ggplot() +
  geom_sf(data = molina_shape, fill = NA, color = "black") +
  geom_sf(data = negativos_sf, aes(color = PM25_Estimado)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = -5) +
  labs(title = "Puntos con PM2.5 estimado negativo en Molina")
