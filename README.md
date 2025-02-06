# t7500180
# Paso 1: Definir los vectores
set.seed(123)  # Para reproducibilidad

energia <- rep(c("Renovable", "No Renovable"), each = 10)
consumo <- c(120, 135, 110, NA, 145, 155, 160, NA, 130, 140, 
             210, 190, NA, 220, 215, 230, NA, 205, 195, 225)
costo_kwh <- rep(c(0.12, 0.15), each = 10)  # Costos: 0.12 para renovable, 0.15 para no renovable

# Paso 2: Limpieza de datos - Reemplazar NA con la mediana por tipo de energía
library(dplyr)

df_temporal <- data.frame(energia, consumo)
mediana_consumo <- df_temporal %>% 
  group_by(energia) %>% 
  summarise(mediana = median(consumo, na.rm = TRUE))

df_temporal <- df_temporal %>% 
  left_join(mediana_consumo, by = "energia") %>% 
  mutate(consumo = ifelse(is.na(consumo), mediana, consumo)) %>% 
  select(-mediana)

# Paso 3: Crear el dataframe final
df_consumo <- data.frame(
  energia = df_temporal$energia,
  consumo = df_temporal$consumo,
  costo_kwh = costo_kwh
)

# Paso 4: Cálculos
df_consumo$costo_total <- df_consumo$consumo * df_consumo$costo_kwh

# Calcular el total de consumo y costo por tipo de energía
totales <- df_consumo %>%
  group_by(energia) %>%
  summarise(
    total_consumo = sum(consumo),
    total_costo = sum(costo_total)
  )

# Calcular la media del consumo diario por tipo de energía
media_consumo <- df_consumo %>%
  group_by(energia) %>%
  summarise(media_consumo = mean(consumo))

# Simulación de aumento de precio del 10%
df_consumo$ganancia <- df_consumo$costo_total * 1.1

# Paso 5: Resumen
df_ordenado <- df_consumo %>% arrange(desc(costo_total))

top_3_costos <- head(df_ordenado, 3)

resumen_energia <- list(
  "Dataframe Ordenado" = df_ordenado,
  "Total Consumo por Energía" = totales,
  "Costo Total por Energía" = totales,
  "Top 3 Costos" = top_3_costos
)
