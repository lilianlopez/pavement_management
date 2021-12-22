\name{convierte_a_toneladas}
\alias{convierte_a_toneladas}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convierte a toneladas
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
convierte_a_toneladas(df)
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
    factor_tonelada <- 0.001
    columnas_toneladas <- c("gvw", "wt1", "wt2", "wt3", "wt4", 
        "wt5", "wt6", "wt7", "wt8", "wt9")
    df[columnas_toneladas] <- df[columnas_toneladas] * factor_tonelada
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
