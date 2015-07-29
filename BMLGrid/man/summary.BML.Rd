\name{summary.BML}
\alias{summary.BML}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary
}
\description{
Function to return summary of an bml class object.
}
\usage{
\method{summary}{BML}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
An object of BML grid
}
  \item{\dots}{
other parameters
}
}
\details{
The summary contains the size of bml grid, the number of red cars and blue cars, the density of all cars, the velocity if red cars will move and the velocity if blue cars will move.
}
\author{
Zhewen Shi
}
\examples{
##Summary
u = createBMLGrid(r = 10, c = 10, ncars = 0.4)
summary(u)

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (object, ...) 
{
    r = nrow(object)
    col = ncol(object)
    nred = sum(object == 1)
    nblue = sum(object == 2)
    l1 = paste(" Number of rows:", r, "\n")
    l2 = paste("Number of columns:", col, "\n")
    l3 = paste("Number of red cars:", nred, "\n")
    l4 = paste("Number of blue cars:", nblue, "\n")
    dens = (nred + nblue)/(r * col)
    l5 = paste("Density of cars:", dens, "\n")
    redbml = cmoveCars(object, color = 1)
    bluebml = cmoveCars(object, color = 2)
    l6 = paste("If red Cars will move, the number of cars will be blocked:", 
        nred - round(nred * redbml$velocity), ";\n \t the number of cars can move:", 
        round(nred * redbml$velocity), ";\n \t the velocity will be:", 
        redbml$velocity, "\n")
    l7 = paste("If blue Cars will move, the number of cars will be blocked:", 
        nblue - round(nblue * bluebml$velocity), ";\n \t the number of cars can move:", 
        round(nblue * bluebml$velocity), ";\n \t the velocity will be:", 
        bluebml$velocity)
    cat(c(l1, l2, l3, l4, l5, l6, l7))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
