"ARTRAP" <-
function(depth=NULL, culvert=NULL) {

  diameter   <- culvert$diameter;
  width      <- culvert$width;
  sideslope  <- culvert$sideslope;
  web        <- culvert$web;
  nvalue     <- culvert$nvalue;
  manningcor <- culvert$manningcor;

  if(is.null(depth) || depth > diameter) depth <- diameter;
  if(depth < 0) depth <- 0;

  # remember that diameter is the height!
  if(is.null(width)) width <- 0;
  topwidth <- width + 2*sideslope*depth;
  wettedperimeter <- width + 2*depth*(sqrt(1+sideslope^2) + web);
  if(abs(depth - diameter) <= 0.0001) {
    wettedperimeter <- wettedperimeter + topwidth;
  }
  area <- depth*(width + sideslope*depth);
  hydraulicradius <- area/wettedperimeter;
  konvey <- Conveyance(A=area, R=hydraulicradius,
                       nvalue=nvalue, manningcor=manningcor);
  z <- list(DEPTH=depth, D=diameter, B=width,
            WEB=web, N=nvalue, A=area, TW=topwidth,
            WP=wettedperimeter, HR=hydraulicradius,
            KONVEY=konvey);
  return(z);
}


"demo.ARTRAP" <- function(depth=0) {
  culvert <- list(diameter=8, width=8, sideslope=2, web=0, nvalue=0.015,
                  manningcor=1.486);
  return(ARTRAP(depth=depth, culvert=culvert));
}

