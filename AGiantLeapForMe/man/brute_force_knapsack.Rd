\name{brute_force_knapsack}
\alias{brute_force_knapsack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
brute_force_knapsack(x, W)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{W}{
%%     ~~Describe \code{W} here~~
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
function (x, W) 
{
    if (any(W <= 0)) 
        stop("Max weight needs to be positive.")
    if (any(x[, 1] <= 0)) 
        stop("Weights in x needs to be positive")
    if (any(x[, 2] <= 0)) 
        stop("Values in x needs to be positive")
    if (typeof(x) != "list") 
        stop("x needs to be a data frame")
    best_value <- 0
    for (i in 1:(2^nrow(x))) {
        value <- 0
        weight <- 0
        index <- c()
        for (j in 1:nrow(x)) {
            if (bitwAnd(bitwShiftR(i, j), 1) == 1) {
                index <- append(index, j)
                value <- value + x[j, 2]
                weight <- weight + x[j, 1]
            }
        }
        if (weight <= W & value > best_value) {
            best_position <- index
            best_value <- value
        }
    }
    lst <- list(value = round(best_value), elements = best_position)
    return(lst)
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
