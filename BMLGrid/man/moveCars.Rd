\name{moveCars}
\alias{moveCars}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Move cars
}
\description{
Function to move one kind of cars in one step
}
\usage{
moveCars(g, color = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
An object of BML class
}
  \item{color}{
1 means move red cars and 2 means move blue cars,
}
}
\author{
Zhewen Shi
}
\examples{
g = createBMLGrid(10,10,0.3)
moveCars(g,color = 2)


##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (g, color = 1) 
{
    if (color == 1) {
        numCar = sum(g == 1)
        if (ncol(g) > 1) {
            temp = g[, c(ncol(g), 1:(ncol(g) - 1))]
            move = (temp == 1 & g == 0)
            numMove = sum(move)
            g[move] = 1
            delete = move[, c(2:ncol(g), 1)]
            g[delete] = 0
            velocity = numMove/numCar
        }
        else {
            velocity = 0
        }
    }
    else {
        numCar = sum(g == 2)
        if (nrow(g) > 1) {
            temp = g[c(nrow(g), 1:(nrow(g) - 1)), ]
            move = (temp == 2 & g == 0)
            numMove = sum(move)
            g[move] = 2
            delete = move[c(2:nrow(g), 1), ]
            g[delete] = 0
            velocity = numMove/numCar
        }
        else {
            velocity = 0
        }
    }
    return(list(grid = g, velocity = velocity))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
