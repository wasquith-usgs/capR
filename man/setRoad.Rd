\name{setRoad}
\alias{setRoad}
\title{Creating and Setting the Properties of a Road Object}
\description{
This function creates the road object for processing by various functions of the capR program. The length units of the road object are implied and inherited from the culvert object that contains it.
}
\usage{
setRoad(...)
}
\arguments{
\item{crown.elev}{The elevation in length units of the crown or top of the roadway. For approach elevations less than this value, no attempt to compute flow-over-the-road is performed;}
\item{surface.type}{A category of road surface type;}
\item{road.width}{The width of the road in length units that is parallel to the direction of flow. Another way to consider this width is that it represents the width of the embankment as a person would see when driving on top of it. Also, from a hydraulics perspective, this width is the length of the broad-crested weir or length of the crest parallel to the flow;}
\item{crest.lengths}{A data frame of paired values of crest length (the flow width, b) and water-surface elevation, elev; each in the same length units. Vertical wall extrapolation (no further width of flow increase) is made for water-surface elevations greater than the maximum.}
}
\value{
  A bunch of stuff.
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
