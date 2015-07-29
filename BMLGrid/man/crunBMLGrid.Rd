\name{crunBMLGrid}
\alias{crunBMLGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Run BML grid
}
\description{
Function to simulate BML(Biham-Middleton-levine) movements for several times.
}
\usage{
crunBMLGrid(g, numSteps = 10000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
an object of BML class
}
  \item{numSteps}{
the number of runs
}
}
\details{
A list which contains all velocities at these time periods is returned. The final grid is also returned
}
\author{
Zhewen Shi
}
\examples{
##Run 10 times
u = createBMLGrid()
s = crunBMLGrid(u, 10)
plot(s$grid)


##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (g, numSteps = 10000) 
{
    if (numSteps > 0) {
        nr = nrow(g)
        nc = ncol(g)
        v = rep(0, numSteps)
        out = .C("runSteps", numSteps = as.integer(numSteps), 
            nr = as.integer(nr), nc = as.integer(nc), g = g, 
            v = as.numeric(v))
        return(list(grid = out$g, velocity = out$v))
    }
    else {
        return(list(grid = g, velocity = 0))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
