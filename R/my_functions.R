concatena_archivos <- function(ruta_archivos) {
  nombre_archivos <- list.files(ruta_archivos)
  ruta_completa   <- paste0(ruta_archivos, "/",  nombre_archivos)
  contenido_archivos <- lapply(ruta_completa, read.csv)
  df <- do.call(rbind,contenido_archivos)
  names(df) <- tolower(names(df))
  return(df)
}

