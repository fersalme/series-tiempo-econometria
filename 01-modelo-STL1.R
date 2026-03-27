
# Ejemplo 1. descomposición y pronóstico de series de tiempo desempleo Colombia
# Utilizaremos datos de desempleo de Colombia para realizar la descomposición clásica y por STL
# Realizar el pronóstico de la serie para h=5 usando la descomposición.

# Librerias
library(tidyverse)
library(tsibble)
library(feasts)
library(fable)

# datos
desempleo <- read_csv2("data/Mercado laboral y población.csv", col_select = 1:2)

# temporalidad
desempleo <- desempleo |>
    mutate(`Periodo(MMM, AAAA)` = yearmonth(`Periodo(MMM, AAAA)`)) |>
    as_tsibble(index = `Periodo(MMM, AAAA)`)

# Grafico 
autoplot(desempleo)

# 1. Descomposición STL
# Ejecutar STL
desempleo_comp <- desempleo |>
  model(
    STL(`Tasa de desempleo - total nacional` ~ trend(window = 13) + season(window = "periodic"))
  ) |>
  components()

# Graficar componentes
desempleo_comp |> autoplot()

# 2. Pronóstico usando MCO (Modelo de Regresión Lineal)
# Desempleo_t = beta_0 + beta_1 Tendencia_t + \beta_k Estacionalidad_k + e_t

# Ajustar el modelo usando TSLM (Time Series Linear Model - MCO para series de tiempo)
fit_mco <- desempleo |>
  model(
    mco_lineal = TSLM(`Tasa de desempleo - total nacional` ~ trend() + season())
  )

# Generar el pronóstico para los próximos 12 meses
pronostico_mco <- fit_mco |>
  forecast(h = "12 months")

# Graficar el pronóstico
pronostico_mco |>
  autoplot(desempleo) +
  labs(title = "Pronóstico de Desempleo en Colombia (Modelo MCO)",
       subtitle = "Tendencia lineal + Estacionalidad fija",
       y = "Tasa de desempleo (%)") +
  theme_minimal()

# 3. Reporte del modelo
report(fit_mco)

# 4. Residuos del modelo MCO
fit_mco |> gg_tsresiduals()