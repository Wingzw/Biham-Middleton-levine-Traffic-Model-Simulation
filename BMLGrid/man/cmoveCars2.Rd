\name{cmoveCars2}
\alias{cmoveCars2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Move cars
}
\description{
Function to move one kind of cars in one step
}
\usage{
cmoveCars2(g, color = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
An object of BML grid
}
  \item{color}{
1 means move red cars and 2 means move blue cars
}
}
\author{
Zhewen Shi
}
\examples{
g = createBMLGrid(10,10,0.3)
cmoveCars2(g, color = 1)


##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (g, color = 1) 
{
    nr = nrow(g)
    nc = ncol(g)
    v = 0
    out = .C("move2", nr = as.integer(nr), nc = as.integer(nc), 
        g = g, col = as.integer(color), v = as.numeric(v))
    return(list(grid = out$g, velocity = out$v))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
