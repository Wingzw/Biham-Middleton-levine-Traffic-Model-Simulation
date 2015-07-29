\name{plot.BML}
\alias{plot.BML}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot
}
\description{
Function to plot the object of BML class
}
\usage{
\method{plot}{BML}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of BML class
}
  \item{\dots}{
other parameters
}
}
\author{
Zhewen Shi
}
\examples{
##Plot
u = createBMLGrid(r = 9, c = 9, ncars = 0.4)
plot(u, main = "grid")



##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, ...) 
{
    if (max(x) == 1) {
        image(t(x), col = c("white", "red"), axes = FALSE, ...)
    }
    else {
        image(t(x), col = c("white", "red", "blue"), axes = FALSE, 
            ...)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
