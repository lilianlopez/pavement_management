concatena_archivos <- function(ruta_archivos) {
  nombre_archivos <- list.files(ruta_archivos)
  ruta_completa   <- paste0(ruta_archivos, "/",  nombre_archivos)
  contenido_archivos <- lapply(ruta_completa, read.csv)
  df <- do.call(rbind,contenido_archivos)
  names(df) <- tolower(names(df))
  return(df)
}

convierte_a_toneladas <- function(df) {
  # A TONELADAS
  #factor_libra_kg <- 0.454    0.000453592
  factor_tonelada <- 0.001
  columnas_toneladas <- c("gvw","wt1","wt2","wt3","wt4","wt5","wt6","wt7","wt8","wt9")
  
  df[columnas_toneladas] <- df[columnas_toneladas]*factor_tonelada
  
  return(df)
}

convierte_a_metros <- function(df) {
  #PIES A METROS
  factor_pie <- 0.3048
  columnas_pies <- c("wheelbase","spc1","spc2","spc3","spc4","spc5","spc6","spc7","spc8","spc9")
  df$spc4 <- df$spc4 %>% as.numeric()
  
  df[columnas_pies] <- df[columnas_pies]*factor_pie
  
  return(df)
  
}

convierte_a_kg <- function(df) {
  
  factor_ton_kg <- 1000
  
  columnas_kg <- c("gvw","wt1","wt2","wt3","wt4","wt5","wt6","wt7","wt8","wt9")
  df[columnas_kg] <- df[columnas_kg]*factor_ton_kg
  return(df)
  
  
}