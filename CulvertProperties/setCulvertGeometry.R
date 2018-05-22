"setCulvertGeometry" <-
function(depth=NULL, culvert=NULL, location=c("inlet", "outlet"),
         depression=0, ...) {

  # First compute the properties as if the geometry was complete
  # note that the depression of zero condition results in a regular
  # computation of an undepressed culvert.
  xsec.properties <- culvert$geometry.func(depth=depth + depression,
                                           culvert=culvert);
  if(depression == 0) return(xsec.properties);


  location <- match.arg(location);

  if(location == "inlet") {
     Ad  <- culvert$inlet.depression.A;
     WPd <- culvert$inlet.depression.WP;
     TWd <- culvert$inlet.depression.TW;
  } else {
     Ad  <- culvert$outlet.depression.A;
     WPd <- culvert$outlet.depression.WP;
     TWd <- culvert$outlet.depression.TW;
  }
  A  <- xsec.properties$A  - Ad;
  WP <- xsec.properties$WP - WPd + TWd;
  HR <- A/WP;
  konvey <- Conveyance(A=A, R=HR, nvalue=culvert$nvalue,
                       manningcor=culvert$manningcor);
  xsec.properties$A      <- A;
  xsec.properties$WP     <- WP;
  xsec.properties$HR     <- HR;
  xsec.properties$KONVEY <- konvey;
  #print(xsec.properties);
  return(xsec.properties);
}

