\name{setCulvert}
\alias{setCulvert}
\title{Creating and Setting the Properties of a Culvert Object}
\description{
This function creates a single culvert object for processing by various functions of the capR program.
}
\usage{
setCulvert(...)
}
\arguments{
  \item{name}{A string for the identification of the culvert, which is not used in the computations;}
  \item{slope}{The dimensionless slope (length per length) of the culvert barrel;}
  \item{beveling}{The numerical value of the beveling of the entrance (see TWRI-B3ChA4-fig.22): \var{w/D} or \var{b/B}, so obviously specified as a dimensionless number;}
  \item{thetabev}{The angle of the beveling (see TWRI-B3ChA4-fig.22);}
  \item{rounding}{The rounding of the entrance (see TWRI-B3ChA4-fig.21): \var{r/D} or \var{r/B}, so obviously specified as a dimensionless number;}
  \item{USmiterlength}{The hypothenuse of the upstream mitering. This length is used in function \command{Lengths4MiteredPipe} to compute a length for flow types 1, 2, and 3;}
  \item{DSmiterlength}{The hypothenuse of the downstream mitering. This length is used in function \command{Lengths4MiteredPipe} to compute a length for flow types 1, 2, and 3;}
  \item{nvalue}{The Manning's n-value of the barrel;}
    \item{skew.angle}{The angle of skew or nonparallel alignment in degrees of the culvert flow axis to the main axis of flow. The cosine of the skew is computed and multiplied to the discharge as applicable in computation of velocity head \var{v1head};}
  \item{L}{The length of the barrel (not retained as an attribute of the culvert object, but this value used to set the \var{Ltop} and \var{Lbot} attributes);}
  \item{Ltop}{The length of the crown of the barrel. This value can be set equal to the \var{L} attribute if \var{L} is provided;}
  \item{Lbot}{The length of the invert of the barrel. This value can be set equal to the \var{L} attribute if \var{L} is provided;}
  \item{diameter}{The diameter or rise (replaces rise) of the barrel in length units. Thus, this value sets the crown of box or trapezoid barrels;}
  \item{width}{The width or span of the culvert in length units. This length is the total width of box barrels with optional webbing or the span of elliptical or pipe arch geometries. Trapezoid base width is set by this attribute, and if the value is zero then a triangular barrel is specified.  The width value sets the total width of box barrels and not the individual barrels;}
  \item{rise}{The rise or diameter (replaces diameter) of the barrel in length units;}
  \item{span}{The span or width (replaces width) of the barrel in length units;}
  \item{inlet.centerline}{The horizontal stationing in length units of the center line of the inlet---this value is not used in computations;}
  \item{outlet.centerline}{The horizontal stationing in length units of the center line of the outlet---this value is not used in computations;}
  \item{sideslope}{The side slope of a trapezoid in the run over rise notation;}
  \item{zusinvert}{The elevation or datum of the inlet invert in length units;}
  \item{zdsinvert}{The elevation or datum of the outlet invert in length units;}
  \item{inlet.depression}{The depth or length in length units that the inlet is submerged. This length is used to compute geometrical characteristics which are effectively removed from computations. For example, suppose that a 4-foot circular pipe has 1 foot of compacted (permanent) dirt in the inlet and suppose that the top of this dirt had an elevation of 2.45 feet. The \var{inlet.depression} would be set at 1 foot; the \var{zusinvert} would be set at 2.45; and \var{diameter} would be set at 4 feet;}
  \item{outlet.depression}{The depth of length in length units that the outlet is submerged (see discussion for \var{inlet.depression};}
  \item{min.recordable.hw}{The minimum recordable headwater elevation in length units. Use this value to force interruption of computations for headwater elevations below this value;}
  \item{min.recordable.tw}{The minimum recordable tailwater elevation in length units. Use this value to force interruption of computations for tailwater elevations below this value;}
  \item{type}{Geometry type and type to use in categorical lookup for discharge coefficients;}
  \item{altgeotype}{Alternative geotype to use for geometrical computations (area, topwidth, wetted perimeter, conveyance);}
  \item{pipearch.type}{The type of pipe arch geometry with a special category for the corner radius (CR). Note that the specific pairs of diameter (rise) and width (span) are the responsibility of the user;}
  \item{inlet}{Type of inlet category;}
  \item{theta}{Wingwall angle (see TWRI-B3ChA4-fig.24);}
  \item{theta.left}{Wingwall (left) angle (see TWRI-B3ChA4-fig.24);}
  \item{theta.right}{Wingwall (right) angle (see TWRI-B3ChA4-fig.24);}
  \item{web}{The number (integer) of webs (used in box and trapezoidal culvert geometries). For the unusual trapezoid case, the web is treated as full height and thus geometrically webs do not exist on the sideslope portion of the trapezoid. Shapes such as circles, ellipses, and pipearches ignore the setting of web;}
  \item{projectlength}{The length that the inlet projects upstream from the embankment and is used to set the k-factor for projection (\var{kproj});}
  \item{is.topedge.square}{Logical on the top edge of the inlet;}
  \item{is.bellmouthed}{Logical on the general shape of the inlet;}
  \item{is.tonguegroove}{Logical on the tongue and groove nature of the barrel material;}
  \item{pipearch.radius.bottom}{An optionally provided value in inches if \var{pipearch.radius.units} also is set to \sQuote{inches}. This radii normally is set by the function \command{CULPAD}, which was ported from CAP97.06;}
  \item{pipearch.radius.top}{An optionally provided value in inches if \var{pipearch.radius.units} also is set to \sQuote{inches}. This radii normally is set by the function \command{CULPAD}, which was ported from CAP97.06;}
  \item{pipearch.radius.corner}{An optionally provided value in inches if \var{pipearch.radius.units} also is set to \sQuote{inches}. This radii normally is set by the function \command{CULPAD}, which was ported from CAP97.06;}
  \item{pipearch.radius.units}{Length units of the pipearch radii, which when set to \sQuote{inches} causes function \command{CULPAD} to be bypassed and the user supplied values are used instead;}
  \item{number.of.barrels}{Number of barrels for this culvert object. These are treated as simple superpositioning of the single barrel computations (discharge in one barrel is multiplied by the number.of.barrels) with appropriate adjustment of approach velocity head and approach to inlet loss. Culverts with webbing (see attribute \var{web}) typically, but not exclusively, have this value set to 1. Recall that the \var{web} attribute takes over the additional wetted perimeter. However, even with webs, the \var{number.of.barrels} can still be set to greater than 1;}
  \item{ignore.approach.losses}{Logical to ignore losses from approach to inlet. This is the same as setting the length of the approach to zero.}
  \item{ignore.approach.velocity.head}{Logical to ignore the inclusion of the approach velocity head in the approach as additional energy;}
  \item{fraction.of.approach.area}{This numerical value on the interval zero to unity can be used to fine tune the approach velocity head computations for circumstances in which there are multiple overt assemblies.   For example, the user has a square culvert next to a circular culvert. These two are computed in isolation and superimposed to acquire total flow. The user may choose to use the total flow as the basis for approach velocity head or use the flow in the individual barrel along with the \var{fraction.of.approach.area} multiplied by the total approach area to compute the velocity head. If this feature is to be used,  it is the responsibility of the user to ensure that all of the \var{fraction.of.approach.area} for the culvert assemblies some to unity. Further it is the responsibility of the user to enable this feature using the \var{useApproachHeadApportioning} logical variable set to \var{TRUE} that is associated with the approach.}
  \item{material}{The material category of the barrel;}
  \item{is.barrel.rough}{Logical to set certain computations related to test validity of culvert flow types 5 or 6 flow through TWRI-B3ChA4-fig.16;}
  \item{materialthickness}{Categorical thickness of the barrel material;}
  \item{headwall}{Category of headwall;}
  \item{embankment}{Category of embankment;}
  \item{gravity}{Acceleration of gravity in english (32.2 feet per square second) or SI (9.81 meters per square second). This value is used to determine the Manning's equation factor (1.486 or 1);}
  \item{flowunits}{The units that discharges and conveyances are (or will be) reported in;}
  \item{lengthunits}{The units that lengths are (or will be) reported in;}
  \item{areaunits}{The units that areas are (or will be) reported in;}
  \item{use.datums.when.graphing}{Logical to determine behavior of culvert plotting functions. The datums of the culvert are determined by the inlet and outlet invert elevations (\var{zusinvert} and \var{zdsinvert});}
  \item{road}{The road object associated with this particular culvert object (see function \command{setRoad}).}
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
