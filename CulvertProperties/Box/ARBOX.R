"ARBOX" <-
function(depth=NULL, culvert=NULL) {

  diameter <- culvert$diameter;
  width    <- culvert$width;
  web      <- culvert$web;
  nvalue   <- culvert$nvalue;
  manningcor <- culvert$manningcor;

  if(is.null(depth) || depth > diameter) depth <- diameter;
  if(depth < 0) depth <- 0;

  # remember that diameter is the height!
  if(is.null(width)) width <- diameter;
  wettedperimeter <- width + 2*(1+web)*depth;
  if(abs(depth - diameter) <= 0.0001) {
    wettedperimeter <- wettedperimeter + width;
  }
  area <- width*depth;
  topwidth <- width;
  hydraulicradius <- area/wettedperimeter;
  konvey <- Conveyance(A=area, R=hydraulicradius,
                       nvalue=nvalue, manningcor=manningcor);
  z <- list(DEPTH=depth, D=diameter, B=width,
            WEB=web, N=nvalue, A=area,
            TW=topwidth, WP=wettedperimeter,
            HR=hydraulicradius, KONVEY=konvey);
  return(z);
}


"demo.ARBOX" <- function(depth=0) {
  culvert <- list(diameter=8, width=8, web=1, nvalue=0.015,
                  manningcor=1.486);
  return(ARBOX(depth=depth, culvert=culvert));
}
#C=====ARBOX bof=============================================================
#C
#C-----Purpose:
#C       Computes geometric properties for box culverts
#C     Programmed by:
#C     Date:
#C     Modified by: JM Fulford
#C     Last modified:
#C
#      SUBROUTINE ARBOX
#     I                (DC,B,D,N,WEB,AREA,KONVEY,TW,WP)
#C
#C-----Arguments:
#      INTEGER WEB
#      REAL N,KONVEY
#C
#      RWEB=WEB
#      WP=B+2.0*(1.0+RWEB)*DC
#      IF (ABS(DC-D).LE.0.0001) WP=WP+B
#      AREA=B*DC
#      TW=B
#      HR=AREA/WP
#      KONVEY=1.49*HR**(2.0/3.0)*AREA/N
#C
#      RETURN
#      END
#C
#C=====ARBOX eof=============================================================
#
