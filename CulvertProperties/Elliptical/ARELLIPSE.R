"ARELLIPSE" <-
function(depth=NULL, culvert=NULL) {

  # diameter and rise are SAME
  diameter   <- culvert$diameter;
  rise       <- culvert$rise;
  span       <- culvert$span;
  nvalue     <- culvert$nvalue;
  manningcor <- culvert$manningcor;

  if(is.null(depth) || depth > diameter) depth <- rise;
  if(depth < 0) depth <- 0;

  A <- span/2;  B <- rise/2;
  W <- depth - B;
  if(abs(depth - rise) <= 0.000001) {
    area <- pi*A*B;
    wettedperimeter <- 2 * pi * sqrt(0.5 * (A*A + B*B));
    topwidth <- 0;
    hydraulicradius <- area/wettedperimeter;
  } else {
    AB <- A*B; AA <- A*A; BB <- B*B; WW <- W*W; # precompute
    AoBtBBmWW <- (A/B)*sqrt(BB - WW); # precompute
    area <- W*AoBtBBmWW + AB*asin(W/B) + AB*pi/2;
    topwidth <- AoBtBBmWW;
    wettedperimeter <- 2*sqrt((AA + BB)/2)*acos(-W/sqrt(WW + topwidth^2));
    topwidth <- 2*topwidth;
    hydraulicradius <- area/wettedperimeter;
  }
  konvey <- Conveyance(A=area, R=hydraulicradius, nvalue=nvalue,
                       manningcor=manningcor);
  z <- list(DEPTH=depth,  D=diameter, RISE=rise,
            SPAN=span,    N=nvalue,      A=area,
            TW=topwidth, WP=wettedperimeter,
            HR=hydraulicradius, KONVEY=konvey);
  return(z);
}

"demo.ARELLIPSE" <-
function(depth,rise,span) {
  culvert <- list(rise=rise, span=span, diameter=rise,
                  nvalue=0.025, manningcor=1.486);
  return(ARELLIPSE(depth, culvert=culvert));
}

#C=====ELLIPSE BOF===============================================
#C
#C-----Purpose:
#C       Computes geometric properties for elliptical culverts
#C     Programmed by:  J.M. Fulford
#C     Date:  10.14.1998
#C
#      SUBROUTINE ELLIPSE
#     I                  (RISE,SPAN,DEPTH,N,AREA,KONVEY,TW,WP)
#C
#C
#      REAL RISE,SPAN,DEPTH,N,KONVEY,TW,WP
#C
#      REAL A,B,W,HR,RATIO
#C
#      W= DEPTH - (RISE/2.0)
#C      IF(RISE.GE.SPAN) THEN
#C-----vertical ellipse
#        A=SPAN*0.5
#        B=RISE*0.5
#C     ELSE
#C-----horizontal ellipse
#C        A=RISE*0.5
#C        B=SPAN*0.5
#C      ENDIF
#      IF(ABS(DEPTH-RISE).LE.0.000001) THEN
#        AREA = 3.14159*A*B
#        WP = 6.28318*SQRT((A*A + B*B)*0.5)
#        TW = 0.0
#        HR= AREA/WP
#      ELSE
#        AREA = A*W*SQRT(B*B-W*W)/B + A*B*ASIN(W/B) + A*B*3.14159*0.5
#        TW = A* SQRT(B*B - W*W)/B
#        RATIO = -1.0*W/SQRT(W*W + TW*TW)
#        TW=2.0*TW
#        WP = 2.0 * SQRT((A*A + B*B)/2.0)*ACOS(RATIO)
#        HR=AREA/WP
#      ENDIF
#      KONVEY=1.49*HR**(2.0/3.0)*AREA/N
#C
#      RETURN
#      END
#C
#C=====ELLIPSE eOF===============================================

