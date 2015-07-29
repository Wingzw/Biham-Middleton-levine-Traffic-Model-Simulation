\name{createBMLGrid}
\alias{createBMLGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create BML grid
}
\description{
Function to generate a BML(Biham-Middleton-levine) grid
}
\usage{
createBMLGrid(r = 100, c = 99, ncars = c(red = 2000, blue = 2000))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{
The number of rows of the BML grid
}
  \item{c}{
The number of columns of the BML grid
}
  \item{ncars}{
The number of red cars and the number of blue cars. It can also be the density of all cars.

}
}
\details{
If sum(ncar) < 1 or one kind of cars has negative numbers, error will occur and the function will stop.
}
\author{
Zhewen Shi
}
\examples{
##Generate a BML Grid and plot
u = createBMLGrid(r= 10, c = 10, c(red = 30, blue = 10))
plot(u)


##Generate a BML Grid and plot, ncars can be density of all cars
u = createBMLGrid(r= 10, c = 10, ncars = 0.3)
plot(u)


##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (r = 100, c = 99, ncars = c(red = 2000, blue = 2000)) 
{
    if (length(ncars) == 1 && ncars < 1) {
        ncars = rep(ceiling(r * c * ncars/2), 2)
        names(ncars) = c("red", "blue")
    }
    if (sum(ncars) < 1) {
        stop("No cars, no need to simulate")
    }
    if (ncars["red"] < 0 | ncars["blue"] < 0) {
        stop("There is one kind of car which number <0")
    }
    grid = matrix(as.integer(0), r, c)
    pos = sample(1:(r * c), sum(ncars))
    grid[pos] = sample(rep(c(as.integer(1), as.integer(2)), ceiling(ncars)))[seq(along = pos)]
    class(grid) = c("BML", class(grid))
    return(grid)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
