"KWINGWALL" <-
function(theta) {
  # vectorization to support asymmetric wingwalls
  kwing <- sapply(theta, function(theta) {
    if(theta < 0 | theta > 90) {
        stop("theta is not in [0,90]");
    }
    if(theta == 0) return(1);
    ang <- theta/180*pi;
    ang <- cos(ang);
    if(ang <= 0.5) {
      return(1.0594+0.3062*ang);
    } else {
      return(1.2402+(0.27173-0.79619*ang+0.28426*ang*ang)*ang);
    }
  })
  return(kwing);
}
# See figure 42 of TWRI8-A3

"demo.WINGWALL" <-
function() {
  plot(KWINGWALL(0:90), type="l")
}

#C=====WINGWALL #bof==============================================================
#C
#C-----Purpose:
#C       computes the discharge coefficient adjustment due to the
#C       wingwall angle. Computed when the value of angle is read.
#C     Programmed by: JM Fulford
#C     Date:
#C     Modified by:
#C     Last modified:
#C
#      REAL FUNCTION WINGWALL (THETA)
#C
#C-----Arguments:
#      REAL THETA
#C
#C-----Local variables:
#      REAL ANG
#C
#      IF(THETA.LT.0.0.OR.THETA.GT.90)THEN
#        WINGWALL=-1.0
#      ELSE IF (THETA.EQ.0) THEN
#        WINGWALL=1.0
#      ELSE
#        THETA=THETA/57.2958
#        ANG=COS(THETA)
#        IF(ANG.LT.0.5)THEN
#          WINGWALL=1.0594+0.3062*ANG
#        ELSE
#          WINGWALL=1.2402+(0.27173-0.79619*ANG+0.28426*ANG*ANG)*ANG
#        ENDIF
#      ENDIF
#C
#      RETURN
#      END
#C
#C=====WINGWALL #eof============================================================
