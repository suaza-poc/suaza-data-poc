# ============================================================
# Indicador de Salud
# ============================================================
# Fuente: Gobernación del Huila - Observatorio de Salud
# Indicador: Mortalidad por suicidio
# Codigo indicador: SUICIDIO_HUILA
# Unidad: Tasa por 100.000 habitantes
# Nivel geografico: Subnacional / Local (Colombia - Huila / Suaza)
# Frecuencia: Anual
# Pais: Colombia
# Estratificador: Sexo
# ============================================================

library(here)
library(readxl)
library(dplyr)
library(arrow)
library(readr)
library(fs)
library(glue)

source(here("herramientas/descargas/descarga-url.R"))


process_mortalidad_suicidio <- function(output_dir = here("outputs")) {
  
  # ----------------------------------------------------------
  # 1. Descargar archivo fuente
  # ----------------------------------------------------------
  
  url <- paste0(
    "https://www.huila.gov.co/observatoriosalud/",
    "loader.php?lServicio=Tools2&lTipo=descargas&",
    "lFuncion=descargar&",
    "idHash=Lg1D3brhffVuAhWHumFe7zpySGk5eWRnPQ"
  )
  
  temp_file <- file.path(
    tempdir(),
    "mortalidad_huila.xlsx"
  )
  
  descargar_desde_url(
    url = url,
    dest_file = temp_file
  )
  
  
  # ----------------------------------------------------------
  # 2. Leer archivo
  # ----------------------------------------------------------
  
  suicidio_raw <- readxl::read_excel(
    temp_file
  )
  
  
  # ----------------------------------------------------------
  # 3. Calcular y estandarizar indicador
  # ----------------------------------------------------------
  
  suicidio <- suicidio_raw |>
    transmute(
      iso3 = "COL",
      territorio = as.character(Territorio),
      cod_subnacional = as.character(Regional),
      cod_local = as.character(`Cod - Terr`),
      anio = as.integer(Año),
      valor = (
        as.numeric(Suicidios) /
          as.numeric(Población)
      ) * 100000,
      sexo = as.character(Sexo),
      indicador = "Mortalidad por suicidio (100.000 hab.)"
    ) |>
    filter(
      !is.na(anio),
      !is.na(valor)
    ) |>
    arrange(
      territorio,
      anio,
      sexo
    )
  
  
  # ----------------------------------------------------------
  # 4. Crear directorios de salida
  # ----------------------------------------------------------
  
  dir_create(
    file.path(output_dir, "csv")
  )
  
  dir_create(
    file.path(output_dir, "parquet")
  )
  
  
  # ----------------------------------------------------------
  # 5. Guardar CSV y Parquet
  # ----------------------------------------------------------
  
  csv_file <- file.path(
    output_dir,
    "csv",
    "mortalidad-suicidio.csv"
  )
  
  parquet_file <- file.path(
    output_dir,
    "parquet",
    "mortalidad-suicidio.parquet"
  )
  
  write_csv(
    suicidio,
    csv_file,
    na = ""
  )
  
  write_parquet(
    suicidio,
    parquet_file
  )
  
  
  # ----------------------------------------------------------
  # 6. Eliminar archivo temporal
  # ----------------------------------------------------------
  
  if (file.exists(temp_file)) {
    file.remove(temp_file)
  }
  
  
  # ----------------------------------------------------------
  # 7. Resultado
  # ----------------------------------------------------------
  
  message(
    glue(
      "✅ Mortalidad por suicidio procesada: ",
      "{nrow(suicidio)} registros."
    )
  )
  
  message(glue("💾 CSV: {csv_file}"))
  message(glue("💾 Parquet: {parquet_file}"))
  
  
  return(
    list(
      data = suicidio,
      output_files = c(
        csv_file,
        parquet_file
      )
    )
  )
}


# ============================================================
# Ejecutar
# ============================================================

result <- process_mortalidad_suicidio()
