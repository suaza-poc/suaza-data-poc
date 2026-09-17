# ============================================================
# Descarga de Archivos desde URL Directa
# ============================================================

library(glue)
library(fs)
library(httr)

#' Descargar archivo desde una URL pública
#'
#' Descarga un archivo desde una URL pública, siguiendo
#' redirecciones y validando que la descarga se haya realizado.
#'
#' @param url URL directa de descarga del archivo.
#' @param dest_file Ruta y nombre del archivo local donde se guardará.
#'
#' @return Ruta del archivo guardado.
#' @export

descargar_desde_url <- function(url, dest_file) {
  
  message(
    glue("Descargando archivo desde: {url}...")
  )
  
  # ----------------------------------------------------------
  # Crear directorio destino
  # ----------------------------------------------------------
  
  fs::dir_create(
    dirname(dest_file)
  )
  
  
  # ----------------------------------------------------------
  # Eliminar archivo previo
  # ----------------------------------------------------------
  
  if (file.exists(dest_file)) {
    file.remove(dest_file)
  }
  
  
  # ----------------------------------------------------------
  # Descargar
  # ----------------------------------------------------------
  
  respuesta <- tryCatch(
    {
      httr::GET(
        url,
        httr::write_disk(
          dest_file,
          overwrite = TRUE
        ),
        httr::user_agent(
          "Mozilla/5.0"
        ),
        httr::timeout(60)
      )
    },
    error = function(e) {
      stop(
        glue(
          "Error al descargar desde {url}: ",
          "{conditionMessage(e)}"
        ),
        call. = FALSE
      )
    }
  )
  
  
  # ----------------------------------------------------------
  # Validar respuesta HTTP
  # ----------------------------------------------------------
  
  if (httr::http_error(respuesta)) {
    
    stop(
      glue(
        "Error HTTP {httr::status_code(respuesta)} ",
        "al descargar desde {url}."
      ),
      call. = FALSE
    )
  }
  
  
  # ----------------------------------------------------------
  # Validar archivo
  # ----------------------------------------------------------
  
  if (
    !file.exists(dest_file) ||
    file.info(dest_file)$size == 0
  ) {
    
    stop(
      "El archivo descargado está vacío o no se generó correctamente.",
      call. = FALSE
    )
  }
  
  
  # ----------------------------------------------------------
  # Información de descarga
  # ----------------------------------------------------------
  
  tipo_contenido <- httr::headers(respuesta)[["content-type"]]
  
  tamano <- file.info(dest_file)$size
  
  message(
    glue(
      "Archivo descargado exitosamente: {dest_file}"
    )
  )
  
  message(
    glue(
      "Tamaño: {round(tamano / 1024, 1)} KB"
    )
  )
  
  if (!is.null(tipo_contenido)) {
    
    message(
      glue(
        "Tipo de contenido: {tipo_contenido}"
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Retornar ruta
  # ----------------------------------------------------------
  
  return(dest_file)
}