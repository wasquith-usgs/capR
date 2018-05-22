"ARCIR" <-
function(depth=NULL, culvert=NULL) {

  diameter   <- culvert$diameter;
  nvalue     <- culvert$nvalue;
  manningcor <- culvert$manningcor;

  if(is.null(depth) || depth > diameter) depth <- diameter;
  if(depth < 0) depth <- 0;

  if(abs(depth - diameter) <= 0.000001) {
    area <- pi*0.25*diameter*diameter;
    wettedperimeter <- pi*diameter;
    topwidth <- 0;
    hydraulicradius <- area/wettedperimeter;
  } else {
    radius <- 0.5*diameter;
    RR <- radius*radius; RmD <- radius - depth;
    alpha <- acos(RmD/radius);
    AS <- alpha*RR;
    AT <- RmD*sqrt(RR - RmD^2);
    area <- AS - AT;
    wettedperimeter <- 2*radius*alpha;
    hydraulicradius <- area/wettedperimeter;
    topwidth <- 2*sqrt(RR - RmD^2);
  }
  konvey <- Conveyance(A=area, R=hydraulicradius,
                       nvalue=nvalue, manningcor=manningcor);
  z <- list(DEPTH=depth, D=diameter, N=nvalue, A=area,
            TW=topwidth, WP=wettedperimeter,
            HR=hydraulicradius, KONVEY=konvey);
  return(z);
}

"demo.ARCIR" <- function(depth=0) {
  culvert <- list(diameter=8, nvalue=0.015,
                  manningcor=1.486);
  return(ARCIR(depth=depth, culvert=culvert));
}
#C=====ARCIR bof=============================================================
#C
#C-----Purpose:
#C       Computes geometric properties for circular section culverts
#C     Programmed by:
#C     Date:
#C     Modified by: JM Fulford
#C     Last modified:
#C
#      SUBROUTINE ARCIR
#     I                (D,DEPTH,N,AREA,KONVEY,TW,WP)
#C
#C     + + + PURPOSE + + +
#C
#      REAL N,KONVEY
#C
#      IF (ABS(DEPTH-D).LE.0.000001) THEN
#        AREA=3.14159*0.25*D*D
#        WP=3.14159*D
#        TW=0.0
#        HR=AREA/WP
#      ELSE
#        R=0.5*D
#        ALP=ACOS((R-DEPTH)/R)
#        AS=ALP*R*R
#        AT=(R-DEPTH)*SQRT(R*R-(R-DEPTH)**2)
#        AREA=AS-AT
#        WP=2.0*R*ALP
#        HR=AREA/WP
#        TW=2.0*SQRT(R*R-(R-DEPTH)**2)
#      ENDIF
#      KONVEY=1.49*HR**(2.0/3.0)*AREA/N
#C
#      RETURN
#      END
#C
#C=====ARCIR eof=============================================================
#
