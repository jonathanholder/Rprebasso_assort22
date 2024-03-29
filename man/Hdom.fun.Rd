% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Hdom_SI.R
\name{Hdom.fun}
\alias{Hdom.fun}
\title{Estimate dominant height based on average height}
\usage{
Hdom.fun(H.av = NA, SpeciesNo = NA, pCROBAS = pCROB)
}
\arguments{
\item{H.av}{Average height (m)}

\item{SpeciesNo}{The column number of the given species in pCROB}

\item{pCROBAS}{The matrix of CROBAS parameters}
}
\value{
The dominant height (m)
}
\description{
Estimate dominant height based on average height
}
\examples{
Hdom.fun(H.av=2:50,SpeciesNo=1)
}
