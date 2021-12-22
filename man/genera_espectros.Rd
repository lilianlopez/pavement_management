\name{genera_espectros}
\alias{genera_espectros}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Genera espectros
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
genera_espectros(df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (df) 
{
    peso_maximo <- ceiling(df$gvw \%>\% max(na.rm = FALSE))
    print(getwd())
    print(peso_maximo)
    limite_inferior <- seq(0, peso_maximo - 1)
    limite_superior <- seq(1, peso_maximo)
    marca_de_clase <- (limite_inferior + limite_superior)/2
    eje_sencillo <- numeric()
    total_eje_sencillo <- df$eje_sencillo \%>\% sum()
    sencillo_porc <- double()
    eje_doble <- numeric()
    total_eje_doble <- df$eje_doble \%>\% sum()
    doble_porc <- double()
    eje_tandem <- numeric()
    total_eje_tandem <- df$eje_tandem \%>\% sum()
    tandem_porc <- double()
    eje_tridem <- numeric()
    total_eje_tridem <- df$eje_tridem \%>\% sum()
    tridem_porc <- double()
    for (i in 1:length((limite_inferior) - 1)) {
        condicion_peso_sencillo <- df$gvw_ton_sencillo > limite_inferior[i] & 
            df$gvw_ton_sencillo <= limite_superior[i]
        eje_sencillo[i] <- df[condicion_peso_sencillo, ]$eje_sencillo \%>\% 
            sum()
        sencillo_porc[i] <- (eje_sencillo[i]/total_eje_sencillo) * 
            100
        condicion_peso_doble <- df$gvw_ton_doble > limite_inferior[i] & 
            df$gvw_ton_doble <= limite_superior[i]
        eje_doble[i] <- df[condicion_peso_doble, ]$eje_doble \%>\% 
            sum()
        doble_porc[i] <- (eje_doble[i]/total_eje_doble) * 100
        condicion_peso_tandem <- df$gvw_ton_tandem > limite_inferior[i] & 
            df$gvw_ton_tandem <= limite_superior[i]
        eje_tandem[i] <- df[condicion_peso_tandem, ]$eje_tandem \%>\% 
            sum()
        tandem_porc[i] <- (eje_tandem[i]/total_eje_tandem) * 
            100
        condicion_peso_tridem <- df$gvw_ton_tridem > limite_inferior[i] & 
            df$gvw_ton_tridem <= limite_superior[i]
        eje_tridem[i] <- df[condicion_peso_tridem, ]$eje_tridem \%>\% 
            sum()
        tridem_porc[i] <- (eje_tridem[i]/total_eje_tridem) * 
            100
    }
    df_tipo_eje <- data.frame(Limite_Inferior_Ton = limite_inferior, 
        Marca_de_Clase_Ton = marca_de_clase, Limite_Superior_Ton = limite_superior, 
        Sencillo = eje_sencillo, sencillo_porcentaje = round(sencillo_porc, 
            3), doble = eje_doble, doble_porcentaje = round(doble_porc, 
            3), tandem = eje_tandem, tandem_porcentaje = round(tandem_porc, 
            3), tridem = eje_tridem, tridem_porcentaje = round(tridem_porc, 
            3))
    head(df_tipo_eje)
    df_sencillo <- df_tipo_eje[c("Limite_Inferior_Ton", "Marca_de_Clase_Ton", 
        "Limite_Superior_Ton", "Sencillo", "sencillo_porcentaje")]
    df_sencillo <- df_sencillo[df_sencillo$Sencillo != 0, ]
    df_doble <- df_tipo_eje[c("Limite_Inferior_Ton", "Marca_de_Clase_Ton", 
        "Limite_Superior_Ton", "doble", "doble_porcentaje")]
    df_doble <- df_doble[df_doble$doble != 0, ]
    df_tandem <- df_tipo_eje[c("Limite_Inferior_Ton", "Marca_de_Clase_Ton", 
        "Limite_Superior_Ton", "tandem", "tandem_porcentaje")]
    df_tandem <- df_tandem[df_tandem$tandem != 0, ]
    df_tridem <- df_tipo_eje[c("Limite_Inferior_Ton", "Marca_de_Clase_Ton", 
        "Limite_Superior_Ton", "tridem", "tridem_porcentaje")]
    df_tridem <- df_tridem[df_tridem$tridem != 0, ]
    list_of_data_frame <- list(df_sencillo = df_sencillo, df_tandem = df_tandem, 
        df_tridem = df_tridem, df_doble = df_doble)
    return(list_of_data_frame)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
