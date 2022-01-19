\name{clasifica_vehiculos}
\alias{clasifica_vehiculos}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Clasifica vehiculos
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
clasifica_vehiculos(df)
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
    df["nom_12"] <- character(nrow(df))
    condicion_1 <- df["gvw"] <= 19 & df["class"] == 4 & df["wheelbase"] <= 
        8.04 & df["num.axles"] == 2
    df[condicion_1, "nom_12"] <- "B2"
    condicion_2 <- df["gvw"] <= 27.5 & df["class"] == 4 & df["wheelbase"] < 
        11 & df["num.axles"] == 3
    df[condicion_2, "nom_12"] <- "B3"
    condicion_3 <- df["gvw"] <= 30.5 & df["class"] == 4 & df["wheelbase"] < 
        11 & df["num.axles"] == 4
    df[condicion_3, "nom_12"] <- "B4"
    condicion_4 <- df["gvw"] <= 19 & df["class"] == 5 & df["wheelbase"] < 
        11 & df["num.axles"] == 2
    df[condicion_4, "nom_12"] <- "C2"
    condicion_5 <- df["gvw"] <= 27.5 & df["class"] == 6 & df["wheelbase"] < 
        11 & df["num.axles"] == 3
    df[condicion_5, "nom_12"] <- "C3"
    condicion_6 <- df["gvw"] <= 37.5 & df["class"] == 8 & df["wheelbase"] < 
        28 & df["num.axles"] == 4
    df[condicion_6, "nom_12"] <- "C2-R2"
    condicion_7 <- df["gvw"] <= 51.5 & df["class"] == 10 & df["wheelbase"] < 
        28 & df["num.axles"] == 6
    df[condicion_7, "nom_12"] <- "C3-R3"
    condicion_8 <- df["gvw"] <= 45.5 & df["class"] == 9 & df["wheelbase"] < 
        28 & df["num.axles"] == 5
    df[condicion_8, "nom_12"] <- "C2-R3"
    condicion_9 <- df["gvw"] <= 44.5 & df["class"] == 9 & df["wheelbase"] < 
        28 & df["num.axles"] == 5
    df[condicion_9, "nom_12"] <- "C3-R2"
    condicion_10 <- df["gvw"] <= 30 & df["class"] == 8 & df["wheelbase"] < 
        20 & df["num.axles"] == 3
    df[condicion_10, "nom_12"] <- "T2-S1"
    condicion_11 <- df["gvw"] <= 38.8 & df["class"] == 8 & df["wheelbase"] < 
        20 & df["num.axles"] == 4
    df[condicion_11, "nom_12"] <- "T2-S2"
    condicion_12 <- df["gvw"] <= 45.5 & df["class"] == 9 & df["wheelbase"] < 
        20 & df["num.axles"] == 5
    df[condicion_12, "nom_12"] <- "T2-S3"
    condicion_13 <- df["gvw"] <= 38.5 & df["class"] == 8 & df["wheelbase"] < 
        20 & df["num.axles"] == 4
    df[condicion_13, "nom_12"] <- "T3-S1"
    condicion_14 <- df["gvw"] <= 46.5 & df["class"] == 9 & df["wheelbase"] < 
        20 & df["num.axles"] == 5
    df[condicion_14, "nom_12"] <- "T3-S2"
    condicion_15 <- df["gvw"] <= 54 & df["class"] == 10 & df["wheelbase"] < 
        20 & df["num.axles"] == 6
    df[condicion_15, "nom_12"] <- "T3-S3"
    condicion_16 <- df["gvw"] <= 47.5 & df["class"] == 11 & df["wheelbase"] < 
        28 & df["num.axles"] == 5
    df[condicion_16, "nom_12"] <- "T2-S1-R2"
    condicion_17 <- df["gvw"] <= 54.5 & df["class"] == 12 & df["wheelbase"] < 
        28 & df["num.axles"] == 6 & df["spc1"] <= 3.5 & df["spc2"] <= 
        7 & df["spc3"] <= 3 & df["spc4"] <= 6 & df["spc5"] <= 
        1.5
    df[condicion_17, "nom_12"] <- "T2-S1-R3"
    condicion_18 <- df["gvw"] <= 54.5 & df["class"] == 12 & df["wheelbase"] < 
        28 & df["num.axles"] == 6 & df["spc1"] <= 3.5 & df["spc2"] <= 
        6 & df["spc3"] <= 1.5 & df["spc4"] <= 3 & df["spc5"] <= 
        7
    df[condicion_18, "nom_12"] <- "T2-S2-R2"
    condicion_19 <- df["gvw"] <= 54.5 & df["class"] == 12 & df["wheelbase"] < 
        28 & df["num.axles"] == 6 & df["spc1"] <= 2.5 & df["spc2"] <= 
        1.5 & df["spc3"] <= 7 & df["spc4"] <= 3 & df["spc5"] <= 
        6
    df[condicion_19, "nom_12"] <- "T3-S1-R2"
    condicion_20 <- df["gvw"] <= 60.5 & df["class"] == 13 & df["wheelbase"] < 
        28 & df["num.axles"] == 7 & df["spc1"] <= 2.5 & df["spc2"] <= 
        1.5 & df["spc3"] <= 7 & df["spc4"] <= 3 & df["spc5"] <= 
        6 & df["spc6"] <= 1.5
    df[condicion_20, "nom_12"] <- "T3-S1-R3"
    condicion_21 <- df["gvw"] <= 60.5 & df["class"] == 13 & df["wheelbase"] < 
        28 & df["num.axles"] == 7 & df["spc1"] <= 2.5 & df["spc2"] <= 
        1.5 & df["spc3"] <= 6 & df["spc4"] <= 7 & df["spc5"] <= 
        3 & df["spc6"] <= 7
    df[condicion_21, "nom_12"] <- "T3-S2-R2"
    condicion_22 <- df["gvw"] <= 66.5 & df["class"] == 13 & df["wheelbase"] < 
        28 & df["num.axles"] == 9
    df[condicion_22, "nom_12"] <- "T3-S2-R4"
    condicion_23 <- df["gvw"] <= 63 & df["class"] == 13 & df["wheelbase"] < 
        28 & df["num.axles"] == 8
    df[condicion_23, "nom_12"] <- "T3-S2-R3"
    condicion_24 <- df["gvw"] <= 60 & df["class"] == 12 & df["wheelbase"] < 
        25 & df["num.axles"] == 8
    df[condicion_24, "nom_12"] <- "T3-S3-R2"
    condicion_25 <- df["gvw"] <= 51.5 & df["class"] == 13 & df["wheelbase"] < 
        28 & df["num.axles"] == 8
    df[condicion_25, "nom_12"] <- "T3-S3-S2"
    condicion_motos <- df["gvw"] <= 0.6 & df["class"] == 1 & 
        df["wheelbase"] < 3 & df["num.axles"] == 2
    df[condicion_motos, "nom_12"] <- "motos"
    condicion_otros_01 <- df["gvw"] <= 9 & df["class"] >= 1 & 
        df["num.axles"] >= 2 & df$nom_12 == ""
    df[condicion_otros_01, "nom_12"] <- "otros_01"
    condicion_otros_02 <- df["gvw"] > 9 & df["class"] >= 1 & 
        df["num.axles"] >= 2 & df$nom_12 == ""
    df[condicion_otros_02, "nom_12"] <- "otros_02"
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
