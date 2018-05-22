"Lengths4MiteredPipe" <-
function(h1, flowtype=NULL, d2=NULL, d3=NULL, culvert=NULL, approach=NULL) {

  Lw <- NULL; L <- NULL;

  Lapproach     <- approach$Lapproach;
  USmiterlength <- culvert$USmiterlength;
  DSmiterlength <- culvert$DSmiterlength;
  Ltopculvert   <- culvert$Ltop;
  Lbotculvert   <- culvert$Lbot;
              D <- culvert$diameter;

  if(is.null(USmiterlength) || is.null(DSmiterlength)) {
    return(list(L=Ltopculvert, Lw=Lapproach));
  }

  # vertical angle from inlet culvert invert turning up
  # to the miter (IN RADIANS)
  thetaUS <- pi/2 - acos((D - culvert$inlet.depression)/USmiterlength);

  # vertical angle from outlet culvert invert turning up
  # (looking upstream) to the miter (IN RADIANS)
  thetaDS <- pi/2 - acos((D - culvert$outlet.depression)/DSmiterlength);

  if(flowtype == 1) {
    hzD <- HeadwtrDiaRatio(h1=h1, culvert=culvert);
  	ifelse(hzD >= 1, intoculvert <- 0,
  	                 intoculvert <- d2/tan(thetaUS));
  	L  <- "L not applicable because flow is Type1"
  	Lw <- Lapproach - intoculvert;
  } else if(flowtype == 2 || flowtype == 3) {
    Lw <- Lapproach;
    intoculvert     <- d2/tan(thetaUS);
    backintoculvert <- d3/tan(thetaDS);
    L <- Lbotculvert - intoculvert - backintoculvert;
  } else if(flowtype >= 4) {
    Lw <- Lapproach;
    L  <- Ltopculvert;
  } else {
    stop("flow type indeterminate");
  }

  #catme("L=",L);
  #catme("Lw=",Lw);
  #catme("Lapproach=",Lapproach);

  return(list(L=L, Lw=Lw));
}

#
#  other.lengths <-
#     Lengths4MiteredPipe(h1=h1, flowtype=flowtype,
#                         d2=d2, culvert=culvert,
#                         approach=approach)

