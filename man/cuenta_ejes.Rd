\name{cuenta_ejes}
\alias{cuenta_ejes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cuenta ejes
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cuenta_ejes(df)
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
    n_df <- nrow(df)
    df$gvw_ton_sencillo <- double(n_df)
    df$gvw_ton_doble <- double(n_df)
    df$gvw_ton_tandem <- double(n_df)
    df$gvw_ton_tridem <- double(n_df)
    vehiculos <- df$nom_12 \%>\% unique()
    for (indice in vehiculos) {
        condicion <- df["nom_12"] == indice
        if (indice == "B2") {
            df[condicion, ]$gvw_ton_sencillo <- df[condicion, 
                ]$wt1
            df[condicion, ]$gvw_ton_doble <- df[condicion, ]$wt2
        }
        else if (indice == "B3") {
            df[condicion, ]$gvw_ton_sencillo <- df[condicion, 
                ]$wt1
            df[condicion, ]$gvw_ton_tandem <- df[condicion, ]$wt2 + 
                df[condicion, ]$wt3
        }
        else if (indice == "B4") {
            df[condicion, ]$gvw_ton_tandem <- df[condicion, ]$wt1 + 
                df[condicion, ]$wt2 + df[condicion, ]$wt3 + df[condicion, 
                ]$wt4
        }
        else if (indice == "C2") {
            df[condicion, ]$gvw_ton_sencillo <- df[condicion, 
                ]$wt1
            df[condicion, ]$gvw_ton_doble <- df[condicion, ]$wt2
        }
        else if (indice == "C3") {
            df[condicion, ]$gvw_ton_sencillo <- df[condicion, 
                ]$wt1
            df[condicion, ]$gvw_ton_tandem <- df[condicion, ]$wt2 + 
                df[condicion, ]$wt3
        }
        else if (indice == "C2-R2") {
            df[condicion, ]$gvw_ton_sencillo <- df[condicion, 
                ]$wt1
            df[condicion, ]$gvw_ton_doble <- df[condicion, ]$wt2 + 
                df[condicion, ]$wt3 + df[condicion, ]$wt4
        }
        else if (indice == "C3-R3") {
            df[condicion, ]$gvw_ton_sencillo <- df[condicion, 
                ]$wt1
            df[condicion, ]$gvw_ton_doble <- df[condicion, ]$wt4
            df[condicion, ]$gvw_ton_tandem <- df[condicion, ]$wt2 + 
                df[condicion, ]$wt3 + df[condicion, ]$wt5 + df[condicion, 
                ]$wt6
        }
        else if (indice == "C2-R3") {
            df[condicion, ]$gvw_ton_sencillo <- df[condicion, 
                ]$wt1
            df[condicion, ]$gvw_ton_doble <- df[condicion, ]$wt2 + 
                df[condicion, ]$wt3
            df[condicion, ]$gvw_ton_tandem <- df[condicion, ]$wt4 + 
                df[condicion, ]$wt5
        }
        else if (indice == "C3-R2") {
            df[condicion, ]$gvw_ton_sencillo <- df[condicion, 
                ]$wt1
            df[condicion, ]$gvw_ton_doble <- df[condicion, ]$wt4 + 
                df[condicion, ]$wt5
            df[condicion, ]$gvw_ton_tandem <- df[condicion, ]$wt2 + 
                df[condicion, ]$wt3
        }
        else if (indice == "T2-S1") {
            df[condicion, ]$gvw_ton_sencillo <- df[condicion, 
                ]$wt1
            df[condicion, ]$gvw_ton_doble <- df[condicion, ]$wt2 + 
                df[condicion, ]$wt3
        }
        else if (indice == "T2-S2") {
            df[condicion, ]$gvw_ton_sencillo <- df[condicion, 
                ]$wt1
            df[condicion, ]$gvw_ton_doble <- df[condicion, ]$wt2
            df[condicion, ]$gvw_ton_tandem <- df[condicion, ]$wt3 + 
                df[condicion, ]$wt4
        }
        else if (indice == "T2-S3") {
            df[condicion, ]$gvw_ton_sencillo <- df[condicion, 
                ]$wt1
            df[condicion, ]$gvw_ton_doble <- df[condicion, ]$wt2
            df[condicion, ]$gvw_ton_tridem <- df[condicion, ]$wt3 + 
                df[condicion, ]$wt4 + df[condicion, ]$wt5
        }
        else if (indice == "T3-S1") {
            df[condicion, ]$gvw_ton_sencillo <- df[condicion, 
                ]$wt1
            df[condicion, ]$gvw_ton_doble <- df[condicion, ]$wt4
            df[condicion, ]$gvw_ton_tandem <- df[condicion, ]$wt2 + 
                df[condicion, ]$wt3
        }
        else if (indice == "T3-S2") {
            df[condicion, ]$gvw_ton_sencillo <- df[condicion, 
                ]$wt1
            df[condicion, ]$gvw_ton_tandem <- df[condicion, ]$wt2 + 
                df[condicion, ]$wt3 + df[condicion, ]$wt4 + df[condicion, 
                ]$wt5
        }
        else if (indice == "T3-S3") {
            df[condicion, ]$gvw_ton_sencillo <- df[condicion, 
                ]$wt1
            df[condicion, ]$gvw_ton_tandem <- df[condicion, ]$wt2 + 
                df[condicion, ]$wt3
            df[condicion, ]$gvw_ton_tridem <- df[condicion, ]$wt4 + 
                df[condicion, ]$wt5 + df[condicion, ]$wt6
        }
        else if (indice == "T2-S1-R2") {
            df[condicion, ]$gvw_ton_sencillo <- df[condicion, 
                ]$wt1
            df[condicion, ]$gvw_ton_doble <- df[condicion, ]$wt2 + 
                df[condicion, ]$wt3 + df[condicion, ]$wt4 + df[condicion, 
                ]$wt5
        }
        else if (indice == "T2-S1-R3") {
            df[condicion, ]$gvw_ton_sencillo <- df[condicion, 
                ]$wt1
            df[condicion, ]$gvw_ton_doble <- df[condicion, ]$wt2 + 
                df[condicion, ]$wt3 + df[condicion, ]$wt4 + df[condicion, 
                ]$wt5
            df[condicion, ]$gvw_ton_tandem <- df[condicion, ]$wt6
        }
        else if (indice == "T2-S2-R2") {
            df[condicion, ]$gvw_ton_sencillo <- df[condicion, 
                ]$wt1
            df[condicion, ]$gvw_ton_doble <- df[condicion, ]$wt2 + 
                df[condicion, ]$wt5 + df[condicion, ]$wt6
            df[condicion, ]$gvw_ton_tandem <- df[condicion, ]$wt3 + 
                df[condicion, ]$wt4
        }
        else if (indice == "T3-S1-R2") {
            df[condicion, ]$gvw_ton_sencillo <- df[condicion, 
                ]$wt1
            df[condicion, ]$gvw_ton_doble <- df[condicion, ]$wt4 + 
                df[condicion, ]$wt5 + df[condicion, ]$wt6
            df[condicion, ]$gvw_ton_tandem <- df[condicion, ]$wt2 + 
                df[condicion, ]$wt3
        }
        else if (indice == "T3-S1-R3") {
            df[condicion, ]$gvw_ton_sencillo <- df[condicion, 
                ]$wt1
            df[condicion, ]$gvw_ton_doble <- df[condicion, ]$wt4 + 
                df[condicion, ]$wt5
            df[condicion, ]$gvw_ton_tandem <- df[condicion, ]$wt2 + 
                df[condicion, ]$wt3 + df[condicion, ]$wt6 + df[condicion, 
                ]$wt7
        }
        else if (indice == "T3-S2-R2") {
            df[condicion, ]$gvw_ton_sencillo <- df[condicion, 
                ]$wt1
            df[condicion, ]$gvw_ton_doble <- df[condicion, ]$wt4 + 
                df[condicion, ]$wt5
            df[condicion, ]$gvw_ton_tandem <- df[condicion, ]$wt2 + 
                df[condicion, ]$wt3 + df[condicion, ]$wt6 + df[condicion, 
                ]$wt7
        }
        else if (indice == "T3-S2-R4") {
            df[condicion, ]$gvw_ton_sencillo <- df[condicion, 
                ]$wt1
            df[condicion, ]$gvw_ton_tandem <- df[condicion, ]$wt2 + 
                df[condicion, ]$wt3 + df[condicion, ]$wt4 + df[condicion, 
                ]$wt5 + df[condicion, ]$wt6 + df[condicion, ]$wt7 + 
                df[condicion, ]$wt8 + df[condicion, ]$wt9
        }
        else if (indice == "T3-S2-R3") {
            df[condicion, ]$gvw_ton_sencillo <- df[condicion, 
                ]$wt1
            df[condicion, ]$gvw_ton_doble <- df[condicion, ]$wt6
            df[condicion, ]$gvw_ton_tandem <- df[condicion, ]$wt2 + 
                df[condicion, ]$wt3 + df[condicion, ]$wt4 + df[condicion, 
                ]$wt5 + df[condicion, ]$wt7 + df[condicion, ]$wt8
        }
        else if (indice == "T3-S2-S2") {
            df[condicion, ]$gvw_ton_sencillo <- df[condicion, 
                ]$wt1
            df[condicion, ]$gvw_ton_doble <- df[condicion, ]$wt4 + 
                df[condicion, ]$wt5
            df[condicion, ]$gvw_ton_tandem <- df[condicion, ]$wt2 + 
                df[condicion, ]$wt3 + df[condicion, ]$wt6 + df[condicion, 
                ]$wt7
        }
        else if (indice == "T3-S3-R2") {
            df[condicion, ]$gvw_ton_sencillo <- df[condicion, 
                ]$wt1
            df[condicion, ]$gvw_ton_doble <- df[condicion, ]$wt4 + 
                df[condicion, ]$wt5 + df[condicion, ]$gvw_ton_tandem <- df[condicion, 
                ]$wt2 + df[condicion, ]$wt3 + df[condicion, ]$wt6 + 
                df[condicion, ]$wt7
        }
        else if (indice == "T3-S3-S2") {
            df[condicion, ]$gvw_ton_sencillo <- df[condicion, 
                ]$wt1
            df[condicion, ]$gvw_ton_doble <- df[condicion, ]$wt4 + 
                df[condicion, ]$wt5 + df[condicion, ]$wt6
            df[condicion, ]$gvw_ton_tandem <- df[condicion, ]$wt2 + 
                df[condicion, ]$wt3 + df[condicion, ]$wt7 + df[condicion, 
                ]$wt8
        }
    }
    df$eje_sencillo <- double(n_df)
    df$eje_doble <- double(n_df)
    df$eje_tandem <- double(n_df)
    df$eje_tridem <- double(n_df)
    for (indice in vehiculos) {
        condicion <- df["nom_12"] == indice
        if (indice == "B2") {
            df[condicion, ]$eje_sencillo <- 1
            df[condicion, ]$eje_doble <- 1
        }
        else if (indice == "B3") {
            df[condicion, ]$eje_sencillo <- 1
            df[condicion, ]$eje_tandem <- 1
        }
        else if (indice == "B4") {
            df[condicion, ]$eje_tandem <- 2
        }
        else if (indice == "C2") {
            df[condicion, ]$eje_sencillo <- 1
            df[condicion, ]$eje_doble <- 1
        }
        else if (indice == "C3") {
            df[condicion, ]$eje_sencillo <- 1
            df[condicion, ]$eje_tandem <- 1
        }
        else if (indice == "C2-R2") {
            df[condicion, ]$eje_sencillo <- 1
            df[condicion, ]$eje_doble <- 3
        }
        else if (indice == "C3-R3") {
            df[condicion, ]$eje_sencillo <- 1
            df[condicion, ]$eje_doble <- 1
            df[condicion, ]$eje_tandem <- 2
        }
        else if (indice == "C2-R3") {
            df[condicion, ]$eje_sencillo <- 1
            df[condicion, ]$eje_doble <- 2
            df[condicion, ]$eje_tandem <- 1
        }
        else if (indice == "C3-R2") {
            df[condicion, ]$eje_sencillo <- 1
            df[condicion, ]$eje_doble <- 2
            df[condicion, ]$eje_tandem <- 1
        }
        else if (indice == "T2-S1") {
            df[condicion, ]$eje_sencillo <- 1
            df[condicion, ]$eje_doble <- 2
        }
        else if (indice == "T2-S2") {
            df[condicion, ]$eje_sencillo <- 1
            df[condicion, ]$eje_doble <- 1
            df[condicion, ]$eje_tandem <- 1
        }
        else if (indice == "T2-S3") {
            df[condicion, ]$eje_sencillo <- 1
            df[condicion, ]$eje_doble <- 1
            df[condicion, ]$eje_tridem <- 1
        }
        else if (indice == "T3-S1") {
            df[condicion, ]$eje_sencillo <- 1
            df[condicion, ]$eje_doble <- 1
            df[condicion, ]$eje_tandem <- 1
        }
        else if (indice == "T3-S2") {
            df[condicion, ]$eje_sencillo <- 1
            df[condicion, ]$eje_tandem <- 2
        }
        else if (indice == "T3-S3") {
            df[condicion, ]$eje_sencillo <- 1
            df[condicion, ]$eje_tandem <- 1
            df[condicion, ]$eje_tridem <- 1
        }
        else if (indice == "T2-S1-R2") {
            df[condicion, ]$eje_sencillo <- 1
            df[condicion, ]$eje_doble <- 4
        }
        else if (indice == "T2-S1-R3") {
            df[condicion, ]$eje_sencillo <- 1
            df[condicion, ]$eje_doble <- 3
            df[condicion, ]$eje_tandem <- 1
        }
        else if (indice == "T2-S2-R2") {
            df[condicion, ]$eje_sencillo <- 1
            df[condicion, ]$eje_doble <- 3
            df[condicion, ]$eje_tandem <- 1
        }
        else if (indice == "T3-S1-R2") {
            df[condicion, ]$eje_sencillo <- 1
            df[condicion, ]$eje_doble <- 3
            df[condicion, ]$eje_tandem <- 1
        }
        else if (indice == "T3-S1-R3") {
            df[condicion, ]$eje_sencillo <- 1
            df[condicion, ]$eje_doble <- 2
            df[condicion, ]$eje_tandem <- 2
        }
        else if (indice == "T3-S2-R2") {
            df[condicion, ]$eje_sencillo <- 1
            df[condicion, ]$eje_doble <- 2
            df[condicion, ]$eje_tandem <- 2
        }
        else if (indice == "T3-S2-R4") {
            df[condicion, ]$eje_sencillo <- 1
            df[condicion, ]$eje_tandem <- 4
        }
        else if (indice == "T3-S2-R3") {
            df[condicion, ]$eje_sencillo <- 1
            df[condicion, ]$eje_doble <- 1
            df[condicion, ]$eje_tandem <- 3
        }
        else if (indice == "T3-S3-R2") {
            df[condicion, ]$eje_sencillo <- 1
            df[condicion, ]$eje_tandem <- 2
            df[condicion, ]$eje_tridem <- 1
        }
        else if (indice == "T3-S3-S2") {
            df[condicion, ]$eje_sencillo <- 1
            df[condicion, ]$eje_tandem <- 2
            df[condicion, ]$eje_tridem <- 1
        }
    }
    return(df)
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
