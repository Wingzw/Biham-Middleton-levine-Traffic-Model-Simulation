\name{runBMLGrid}
\alias{runBMLGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Run BML grid
}
\description{
Function to simulate BML(Biham-Middleton-levine) movements for several times.
}
\usage{
runBMLGrid(g, numSteps = 10000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
An object of BML class
}
  \item{numSteps}{
The number of runs
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
s = runBMLGrid(u, 10)
plot(s$grid)



##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (g, numSteps = 10000) 
{
    if (numSteps > 0) {
        velocity = rep(0, numSteps)
        for (i in 1:numSteps) {
            tmp = moveCars(g, i\%\%2 + 1)
            g = tmp$grid
            velocity[i] = tmp$velocity
        }
        return(list(grid = g, velocity = velocity))
    }
    return(list(grid = g, velocity = 0))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
