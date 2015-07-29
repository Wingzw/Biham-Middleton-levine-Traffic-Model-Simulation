\name{BMLGrid-package}
\alias{BMLGrid-package}
\alias{BMLGrid}
\docType{package}
\title{
Simulate Biham-Middleton-levine traffic model.
}
\description{
Use this package to generate BML Grid, to simulate the movement of GML traffic model.
}
\details{
\tabular{ll}{
Package: \tab BMLGrid\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-05-13\cr
License: \tab GPL-2\cr
}

}
\author{
Zhewen Shi

Maintainer: Zhewen Shi <zwshi@ucdavis.edu>
}
\keyword{ package }
\examples{
g = createBMLGrid(r=10, c = 10, ncars = c(red = 20, blue = 20));
z = moveCars(g, color = 1);
z = cmoveCars(g, color = 1);
z = cmoveCars2(g, color = 1);
u = runBMLGrid(g, numSteps = 1000);
u = crunBMLGrid(g, numSteps = 1000);
u = crunBMLGrid2(g, numSteps = 1000);
plot(u$grid);
summary(g)
}
