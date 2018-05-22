\name{setApproach}
\alias{setApproach}
\title{Creating and Setting the Properties of an Approach Object}
\description{
This function creates the approach object for processing by various functions of the capR program.
}
\usage{
setApproach(...)
}
\arguments{
  \item{station.id}{A string for the identification of the site, which is not used in the computations;}
  \item{station.name}{A string for the name (location) of the site, which is not used in the computations;}
  \item{Lapproach}{Length of the approach or distance from the approach to the culvert system inlet in length units. This value is used in function \command{HeadLoss12} for loss computations;}
  \item{nvalue}{The Manning's n-value of the approach or vector of n-values representing each subdivision;}
  \item{manningcor}{Manning's equation factor (1.486 or 1), which will be checked for consistencies with a same-named value in each culvert object;}
  \item{X}{A vector of horizontal stationing values;}
  \item{Y}{A vector of elevation values for each horizontal station;}
  \item{XY}{An optional data frame contain the vectors X and Y and so-named columns;}
  \item{subdivision}{A vector of horizontal stations of subdivision;}
  \item{additional.flow}{Additional flow known to arrive through the subdivision---the default of zero makes the most intuitive sense;}
  \item{fraction.of.flow}{A factor that reduces the fraction of flow to \dQuote{route} through the approach. This factor can be thought of as a cosine correction for survey skewness of the approach to the culvert system inlet; however, the correction can also conceptually accomodate the argument that only a fraction of the flow actually arrives normal to the culverts and the remainder arrives from the roadside ditches. This factor controls the computation of the velocity head in the approach.}
  \item{skew.angle}{The angle of skew or nonperpendicular alignment in degrees of the surveyed approach to the main axis of flow. The cosine of the skew is computed and multiplied to the stationing or X values of the cross section.}
  \item{time.series.in}{}
  \item{time.series.out}{}
  \item{forceEqualApproachHead}{A logical variable intended to force the use of a single velocity head of total flow through the total approach area for all culvert and road assemblies, default \code{TRUE}.}
  \item{forceIgnoreApproachHead}{A logical variable intended to force the treatment of all approach velocity heads (for the culvert and road assemblies) as zero, default \code{FALSE}.}
  \item{useApproachHeadApportioning}{A logical variable intended to apportion velocity had for a given culvert assembly by a fraction of total area multiplier associated with that culvert assembly, default \code{FALSE}.  The velocity head for the optional road assembly associated with the given assembly is computed by prorating the total velocity head (total flow and total approach area) by the fraction or ratio of the flow over the road to total flow. The fraction for the culvert is controlled by the \var{fraction.of.approach.area} attribute.}
}
\value{
  A bunch of stuff.
}
\note{
The truth of logical variables \code{forceEqualApproachHead}, \code{forceIgnoreApproachHead}, and \code{useApproachHeadApportioning} are tested in the indicated order. There is no provision in the internal logic to test whether there is more than one true setting. It is the user's responsibility to ensure that only one of these logical variables is true at any one time.
}
\references{
Bodhaine, G.L., 1968, Measurement of peak discharge at culverts by indirect methods: Techniques of Water-Resources Investigations of the U.S. Geological Survey, Book 3, Chapter A4, 60 p.

Hulsing, Harry, 1967, Measurement of peak discharge at dams by indirect method: Techniques of Water-Resources Investigations of the U.S. Geological Survey, Book 3, Chapter A5, 29 p.
}
\author{W.H. Asquith}
\seealso{}
\examples{
}
\keyword{misc}
