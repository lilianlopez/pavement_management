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

clasifica_vehiculos <- function(df) {
  
  df["nom_12"] <- character(nrow(df))
  
  condicion_1 <- df["gvw"] <=19 & df["class"] ==4 & df["wheelbase"] <=8.04 & 
    df["num.axles"] == 2
  df[condicion_1,"nom_12"] <- "B2"
  
  condicion_2 <- df["gvw"]<=27.5 & df["class"] ==4 & df["wheelbase"]<11 &
    df["num.axles"]==3
  df[condicion_2,"nom_12"] <- "B3"
  
  condicion_3 <- df["gvw"]<=30.5 & df["class"] ==4 & df["wheelbase"]<11 &
    df["num.axles"]==4
  df[condicion_3,"nom_12"] <- "B4"
  
  condicion_4 <- df["gvw"]<=19 & df["class"] ==5 & df["wheelbase"]<11 &
    df["num.axles"]==2
  df[condicion_4,"nom_12"] <- "C2"
  
  condicion_5 <- df["gvw"]<=27.5 & df["class"] ==6 & df["wheelbase"]<11 &
    df["num.axles"]==3
  df[condicion_5,"nom_12"] <- "C3"
  
  condicion_6 <- df["gvw"]<=37.5 & df["class"] ==8 & df["wheelbase"]<28 &
    df["num.axles"]==4
  df[condicion_6,"nom_12"] <- "C2-R2"
  
  condicion_7 <- df["gvw"]<=51.5 & df["class"] ==10 & df["wheelbase"]<28 &
    df["num.axles"]==6
  df[condicion_7,"nom_12"] <- "C3-R3"
  
  condicion_8 <- df["gvw"]<=45.5 & df["class"] ==9 & df["wheelbase"]<28 &
    df["num.axles"]==5
  df[condicion_8,"nom_12"] <- "C2-R3"
  
  condicion_9 <- df["gvw"]<=44.5 & df["class"] ==9 & df["wheelbase"]<28 &
    df["num.axles"]==5
  df[condicion_9,"nom_12"] <- "C3-R2"
  
  
  #INICIAN LOS T-S  Y  LOS   T-S-R
  
  condicion_10 <- df["gvw"]<=30 & df["class"] ==8 & df["wheelbase"]<20 &
    df["num.axles"]==3
  df[condicion_10,"nom_12"] <- "T2-S1"
  
  condicion_11 <- df["gvw"]<=38.8 & df["class"] ==8 & df["wheelbase"]<20 &
    df["num.axles"]==4
  df[condicion_11,"nom_12"] <- "T2-S2"
  
  condicion_12 <- df["gvw"]<=45.5 & df["class"] ==9 & df["wheelbase"]<20 &
    df["num.axles"]==5
  df[condicion_12,"nom_12"] <- "T2-S3"
  
  condicion_13 <- df["gvw"]<=38.5 & df["class"] ==8 & df["wheelbase"]<20 &
    df["num.axles"]==4
  df[condicion_13,"nom_12"] <- "T3-S1"
  
  condicion_14 <- df["gvw"]<=46.5 & df["class"] ==9 & df["wheelbase"]<20 &
    df["num.axles"]==5
  df[condicion_14,"nom_12"] <- "T3-S2"
  
  condicion_15 <- df["gvw"]<=54 & df["class"] ==10 & df["wheelbase"]<20 &
    df["num.axles"]==6
  df[condicion_15,"nom_12"] <- "T3-S3"
  
  condicion_16 <- df["gvw"]<=47.5 & df["class"] ==11 & df["wheelbase"]<28 &
    df["num.axles"]==5
  df[condicion_16,"nom_12"] <- "T2-S1-R2"
  
  condicion_17 <- df["gvw"]<=54.5 & df["class"] ==12 & df["wheelbase"]<28 &
    df["num.axles"]==6 & df["spc1"] <=3.5 & df["spc2"] <=7 & df["spc3"]<=3 & 
    df["spc4"]<=6 & df["spc5"]<=1.5 
  df[condicion_17,"nom_12"] <- "T2-S1-R3"
  
  condicion_18 <- df["gvw"]<=54.5 & df["class"] ==12 & df["wheelbase"]<28 &
    df["num.axles"]==6 & df["spc1"] <=3.5 & df["spc2"]<=6 & df["spc3"]<=1.5 & 
    df["spc4"]<=3 & df["spc5"]<=7
  df[condicion_18,"nom_12"] <- "T2-S2-R2"
  
  condicion_19 <- df["gvw"]<=54.5 & df["class"] ==12 & df["wheelbase"]<28 &
    df["num.axles"]==6 & df["spc1"]<=2.5 & df["spc2"]<=1.5 & df["spc3"]<=7 & 
    df["spc4"]<=3 & df["spc5"]<=6
  df[condicion_19,"nom_12"] <- "T3-S1-R2"
  
  condicion_20 <- df["gvw"]<=60.5 & df["class"] ==13 & df["wheelbase"]<28 &
    df["num.axles"]==7 & df["spc1"]<=2.5 & df["spc2"]<=1.5 & df["spc3"]<=7 & 
    df["spc4"]<=3 & df["spc5"]<=6 & df["spc6"]<=1.5
  df[condicion_20,"nom_12"] <- "T3-S1-R3"
  
  condicion_21 <- df["gvw"]<=60.5 & df["class"] ==13 & df["wheelbase"]<28 &
    df["num.axles"]==7 & df["spc1"]<=2.5 & df["spc2"]<=1.5 & df["spc3"]<=6 & 
    df["spc4"]<=7 & df["spc5"]<=3 & df["spc6"]<=7
  df[condicion_21,"nom_12"] <- "T3-S2-R2"
  
  condicion_22 <- df["gvw"]<=66.5 & df["class"] ==13 & df["wheelbase"]<28 &
    df["num.axles"]==9
  df[condicion_22,"nom_12"] <- "T3-S2-R4"
  
  condicion_23 <- df["gvw"]<=63 & df["class"] ==13 & df["wheelbase"]<28 &
    df["num.axles"]==8
  df[condicion_23,"nom_12"] <- "T3-S2-R3"
  
  condicion_24 <- df["gvw"]<=60 & df["class"] ==12 & df["wheelbase"]<25 &
    df["num.axles"]==8
  df[condicion_24,"nom_12"] <- "T3-S3-R2"
  
  condicion_25 <- df["gvw"]<=51.5 & df["class"] ==13 & df["wheelbase"]<28 &
    df["num.axles"]==8
  df[condicion_25,"nom_12"] <- "T3-S3-S2"
  
  
  ##CONDICI?N PARA OTROS
  
  condicion_motos <- df["gvw"] <=0.6 & df["class"] ==1 & df["wheelbase"]<3 &
    df["num.axles"]==2
  df[condicion_motos,"nom_12"] <- "motos"
  
  condicion_otros_01 <- df["gvw"]<=9 & df["class"] >=1 & df["num.axles"]>=2 &
    df$nom_12==""
  df[condicion_otros_01,"nom_12"] <- "otros_01"
  
  condicion_otros_02 <- df["gvw"] >9 & df["class"] >=1 & df["num.axles"]>=2 &
    df$nom_12==""
  df[condicion_otros_02,"nom_12"] <- "otros_02"
  
  return(df)
  
}

cuenta_ejes <- function(df){
  
  
  n_df <- nrow(df)
  df$gvw_ton_sencillo <- double(n_df)
  df$gvw_ton_doble    <- double(n_df)
  df$gvw_ton_tandem   <- double(n_df)
  df$gvw_ton_tridem   <- double(n_df)
  
  vehiculos <- df$`nom_12` %>% unique()
  for (indice in vehiculos) { 
    condicion <- df["nom_12"] == indice
    if (indice == "B2") {
      df[condicion,]$gvw_ton_sencillo <- df[condicion,]$wt1 
      df[condicion,]$gvw_ton_doble    <- df[condicion,]$wt2 
    } else if (indice == "B3"){
      df[condicion,]$gvw_ton_sencillo <- df[condicion,]$wt1 
      df[condicion,]$gvw_ton_tandem   <- df[condicion,]$wt2 + df[condicion,]$wt3 
    } else if (indice == "B4"){
      df[condicion,]$gvw_ton_tandem  <- df[condicion,]$wt1 + df[condicion,]$wt2 + df[condicion,]$wt3 + df[condicion,]$wt4
    } else if (indice == "C2"){
      df[condicion,]$gvw_ton_sencillo <- df[condicion,]$wt1 
      df[condicion,]$gvw_ton_doble    <- df[condicion,]$wt2  
    } else if (indice == "C3"){
      df[condicion,]$gvw_ton_sencillo <- df[condicion,]$wt1 
      df[condicion,]$gvw_ton_tandem   <- df[condicion,]$wt2 + df[condicion,]$wt3
    } else if (indice == "C2-R2"){
      df[condicion,]$gvw_ton_sencillo <- df[condicion,]$wt1 
      df[condicion,]$gvw_ton_doble    <- df[condicion,]$wt2 + df[condicion,]$wt3 + df[condicion,]$wt4
    } else if (indice == "C3-R3"){
      df[condicion,]$gvw_ton_sencillo <- df[condicion,]$wt1
      df[condicion,]$gvw_ton_doble    <- df[condicion,]$wt4
      df[condicion,]$gvw_ton_tandem   <- df[condicion,]$wt2 + df[condicion,]$wt3 + df[condicion,]$wt5 + df[condicion,]$wt6
    } else if (indice == "C2-R3"){
      df[condicion,]$gvw_ton_sencillo <- df[condicion,]$wt1
      df[condicion,]$gvw_ton_doble    <- df[condicion,]$wt2 + df[condicion,]$wt3
      df[condicion,]$gvw_ton_tandem   <- df[condicion,]$wt4 + df[condicion,]$wt5   
    }else if (indice == "C3-R2"){
      df[condicion,]$gvw_ton_sencillo <- df[condicion,]$wt1
      df[condicion,]$gvw_ton_doble    <- df[condicion,]$wt4 + df[condicion,]$wt5
      df[condicion,]$gvw_ton_tandem   <- df[condicion,]$wt2 + df[condicion,]$wt3  
    }else if (indice == "T2-S1"){
      df[condicion,]$gvw_ton_sencillo <- df[condicion,]$wt1
      df[condicion,]$gvw_ton_doble    <- df[condicion,]$wt2 + df[condicion,]$wt3
    }else if (indice == "T2-S2"){
      df[condicion,]$gvw_ton_sencillo <- df[condicion,]$wt1
      df[condicion,]$gvw_ton_doble    <- df[condicion,]$wt2  
      df[condicion,]$gvw_ton_tandem   <- df[condicion,]$wt3 + df[condicion,]$wt4
    }else if (indice == "T2-S3"){
      df[condicion,]$gvw_ton_sencillo <- df[condicion,]$wt1
      df[condicion,]$gvw_ton_doble    <- df[condicion,]$wt2 
      df[condicion,]$gvw_ton_tridem   <- df[condicion,]$wt3 + df[condicion,]$wt4 + df[condicion,]$wt5
    }else if (indice == "T3-S1"){
      df[condicion,]$gvw_ton_sencillo <- df[condicion,]$wt1
      df[condicion,]$gvw_ton_doble    <- df[condicion,]$wt4 
      df[condicion,]$gvw_ton_tandem   <- df[condicion,]$wt2 + df[condicion,]$wt3
    }else if (indice == "T3-S2"){
      df[condicion,]$gvw_ton_sencillo <- df[condicion,]$wt1
      df[condicion,]$gvw_ton_tandem   <- df[condicion,]$wt2 + df[condicion,]$wt3 + df[condicion,]$wt4 + df[condicion,]$wt5
    }else if (indice == "T3-S3"){
      df[condicion,]$gvw_ton_sencillo <- df[condicion,]$wt1
      df[condicion,]$gvw_ton_tandem   <- df[condicion,]$wt2 + df[condicion,]$wt3
      df[condicion,]$gvw_ton_tridem   <- df[condicion,]$wt4 + df[condicion,]$wt5 + df[condicion,]$wt6
    }else if (indice == "T2-S1-R2"){
      df[condicion,]$gvw_ton_sencillo <- df[condicion,]$wt1
      df[condicion,]$gvw_ton_doble    <- df[condicion,]$wt2 + df[condicion,]$wt3 + df[condicion,]$wt4 + df[condicion,]$wt5
    }else if (indice == "T2-S1-R3"){
      df[condicion,]$gvw_ton_sencillo <- df[condicion,]$wt1
      df[condicion,]$gvw_ton_doble    <- df[condicion,]$wt2 + df[condicion,]$wt3 + df[condicion,]$wt4 + df[condicion,]$wt5
      df[condicion,]$gvw_ton_tandem   <- df[condicion,]$wt6 
    }else if (indice == "T2-S2-R2"){
      df[condicion,]$gvw_ton_sencillo <- df[condicion,]$wt1
      df[condicion,]$gvw_ton_doble    <- df[condicion,]$wt2 + df[condicion,]$wt5 + df[condicion,]$wt6  
      df[condicion,]$gvw_ton_tandem   <- df[condicion,]$wt3 + df[condicion,]$wt4
    }else if (indice == "T3-S1-R2"){
      df[condicion,]$gvw_ton_sencillo <- df[condicion,]$wt1
      df[condicion,]$gvw_ton_doble    <- df[condicion,]$wt4 + df[condicion,]$wt5 + df[condicion,]$wt6  
      df[condicion,]$gvw_ton_tandem   <- df[condicion,]$wt2 + df[condicion,]$wt3
    }else if (indice == "T3-S1-R3"){
      df[condicion,]$gvw_ton_sencillo <- df[condicion,]$wt1
      df[condicion,]$gvw_ton_doble    <- df[condicion,]$wt4 + df[condicion,]$wt5   
      df[condicion,]$gvw_ton_tandem   <- df[condicion,]$wt2 + df[condicion,]$wt3 + df[condicion,]$wt6 + df[condicion,]$wt7
    }else if (indice == "T3-S2-R2"){
      df[condicion,]$gvw_ton_sencillo <- df[condicion,]$wt1
      df[condicion,]$gvw_ton_doble    <- df[condicion,]$wt4 + df[condicion,]$wt5 
      df[condicion,]$gvw_ton_tandem   <- df[condicion,]$wt2 + df[condicion,]$wt3 + df[condicion,]$wt6 + df[condicion,]$wt7
    } else if (indice == "T3-S2-R4"){
      df[condicion,]$gvw_ton_sencillo <- df[condicion,]$wt1
      df[condicion,]$gvw_ton_tandem   <- df[condicion,]$wt2 + df[condicion,]$wt3 + df[condicion,]$wt4 + df[condicion,]$wt5 +  df[condicion,]$wt6 + df[condicion,]$wt7 + df[condicion,]$wt8 + df[condicion,]$wt9
      
    }else if (indice == "T3-S2-R3"){
      df[condicion,]$gvw_ton_sencillo <- df[condicion,]$wt1
      df[condicion,]$gvw_ton_doble    <- df[condicion,]$wt6
      df[condicion,]$gvw_ton_tandem   <- df[condicion,]$wt2 + df[condicion,]$wt3 + df[condicion,]$wt4  + df[condicion,]$wt5 + df[condicion,]$wt7 + df[condicion,]$wt8
      
    }else if (indice == "T3-S2-S2") {
      df[condicion,]$gvw_ton_sencillo <- df[condicion,]$wt1
      df[condicion,]$gvw_ton_doble    <- df[condicion,]$wt4 + df[condicion,]$wt5 
      df[condicion,]$gvw_ton_tandem   <- df[condicion,]$wt2 + df[condicion,]$wt3 + df[condicion,]$wt6 + df[condicion,]$wt7  
    }else if (indice == "T3-S3-R2"){
      df[condicion,]$gvw_ton_sencillo <- df[condicion,]$wt1
      df[condicion,]$gvw_ton_doble    <- df[condicion,]$wt4 + df[condicion,]$wt5 + 
        df[condicion,]$gvw_ton_tandem   <- df[condicion,]$wt2 + df[condicion,]$wt3 + df[condicion,]$wt6 + df[condicion,]$wt7 
    }else if (indice == "T3-S3-S2"){
      df[condicion,]$gvw_ton_sencillo <- df[condicion,]$wt1
      df[condicion,]$gvw_ton_doble    <- df[condicion,]$wt4 + df[condicion,]$wt5 + df[condicion,]$wt6 
      df[condicion,]$gvw_ton_tandem   <- df[condicion,]$wt2 + df[condicion,]$wt3 + df[condicion,]$wt7 + df[condicion,]$wt8
    }
    
  }
  
  
  df$eje_sencillo <- double(n_df)
  df$eje_doble <- double(n_df)
  df$eje_tandem <- double(n_df)
  df$eje_tridem <- double(n_df)
  
  
  
  for (indice in vehiculos) { 
    condicion <- df["nom_12"] == indice
    if (indice == "B2"){
      df[condicion,]$eje_sencillo <- 1
      df[condicion,]$eje_doble <- 1
    } else if (indice == "B3"){
      df[condicion,]$eje_sencillo <- 1 
      df[condicion,]$eje_tandem <- 1
    } else if (indice == "B4"){
      df[condicion,]$eje_tandem <- 2 
    } else if (indice == "C2"){
      df[condicion,]$eje_sencillo <- 1
      df[condicion,]$eje_doble <- 1
    } else if (indice == "C3"){
      df[condicion,]$eje_sencillo <- 1 
      df[condicion,]$eje_tandem <- 1
    } else if (indice == "C2-R2"){
      df[condicion,]$eje_sencillo <- 1 
      df[condicion,]$eje_doble <- 3
    } else if (indice == "C3-R3"){
      df[condicion,]$eje_sencillo <- 1 
      df[condicion,]$eje_doble <- 1
      df[condicion,]$eje_tandem <- 2
    } else if (indice == "C2-R3"){
      df[condicion,]$eje_sencillo <- 1
      df[condicion,]$eje_doble <- 2
      df[condicion,]$eje_tandem <- 1
    }else if (indice == "C3-R2"){
      df[condicion,]$eje_sencillo <- 1
      df[condicion,]$eje_doble <- 2
      df[condicion,]$eje_tandem <- 1
    }else if (indice == "T2-S1"){
      df[condicion,]$eje_sencillo <- 1
      df[condicion,]$eje_doble <- 2
    }else if (indice == "T2-S2"){
      df[condicion,]$eje_sencillo <- 1
      df[condicion,]$eje_doble <- 1
      df[condicion,]$eje_tandem <- 1
    }else if (indice == "T2-S3"){
      df[condicion,]$eje_sencillo <- 1
      df[condicion,]$eje_doble <- 1
      df[condicion,]$eje_tridem <- 1
    }else if (indice == "T3-S1"){
      df[condicion,]$eje_sencillo <- 1
      df[condicion,]$eje_doble <- 1
      df[condicion,]$eje_tandem <- 1
    }else if (indice == "T3-S2"){
      df[condicion,]$eje_sencillo <- 1
      df[condicion,]$eje_tandem <- 2
    }else if (indice == "T3-S3"){
      df[condicion,]$eje_sencillo <- 1 
      df[condicion,]$eje_tandem <- 1
      df[condicion,]$eje_tridem <- 1
    }else if (indice == "T2-S1-R2"){
      df[condicion,]$eje_sencillo <- 1
      df[condicion,]$eje_doble <- 4
    }else if (indice == "T2-S1-R3"){
      df[condicion,]$eje_sencillo <- 1
      df[condicion,]$eje_doble <- 3
      df[condicion,]$eje_tandem <- 1
    }else if (indice == "T2-S2-R2"){
      df[condicion,]$eje_sencillo <- 1
      df[condicion,]$eje_doble <- 3
      df[condicion,]$eje_tandem <- 1
    }else if (indice == "T3-S1-R2"){
      df[condicion,]$eje_sencillo <- 1
      df[condicion,]$eje_doble <- 3
      df[condicion,]$eje_tandem <- 1
    }else if (indice == "T3-S1-R3"){
      df[condicion,]$eje_sencillo <- 1
      df[condicion,]$eje_doble <- 2
      df[condicion,]$eje_tandem <- 2
    }else if (indice == "T3-S2-R2"){
      df[condicion,]$eje_sencillo <- 1
      df[condicion,]$eje_doble <- 2
      df[condicion,]$eje_tandem <- 2
    } else if (indice == "T3-S2-R4"){
      df[condicion,]$eje_sencillo <- 1
      df[condicion,]$eje_tandem <- 4 
    }else if (indice == "T3-S2-R3"){
      df[condicion,]$eje_sencillo <- 1
      df[condicion,]$eje_doble <- 1
      df[condicion,]$eje_tandem <- 3 
    }else if (indice == "T3-S3-R2"){
      df[condicion,]$eje_sencillo <- 1
      df[condicion,]$eje_tandem <- 2  
      df[condicion,]$eje_tridem <- 1
    }else if (indice == "T3-S3-S2"){
      df[condicion,]$eje_sencillo <- 1 
      df[condicion,]$eje_tandem <- 2
      df[condicion,]$eje_tridem <- 1
    }
  }
  
  return(df)
  
}


