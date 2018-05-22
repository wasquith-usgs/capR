

Cval.5flared.h <- new.h();
set.h("c5vals",
      c(0.48, 0.50, 0.52, 0.53, 0.55, 0.56,
        0.57, 0.59, 0.61, 0.63, 0.65, 0.66),
      Cval.5flared.h);
set.h("hnode",
      c(1.4,  1.5,  1.6,  1.7,  1.8,  1.9,
        2.0, 2.5,  3.0,  3.5,  4.0,  5.0),
      Cval.5flared.h);



"C5FLARED" <-
function(hzD) {
  hnode    <- get.h("hnode",  Cval.5flared.h);
  c5vals   <- get.h("c5vals", Cval.5flared.h);
  c5flared <- approx(x=hnode, y=c5vals, xout=hzD,
                     yleft=c5vals[1], yright=c5vals[12]);
  return(c5flared$y);
}

"demo.C5FLARED" <-
function() {
  plot(C5FLARED(seq(1,10,by=0.1)), type="l");
}

#
#C=====C5FLARED eof===========================================================
#C
#C-----Purpose:
#C       interpolate tabled values of C5 for flared pipe ends from table on
#C       page 44 of the twri.
#C     Programmed by: JM Fulford
#C     Date: 12.96
#C
#      REAL FUNCTION C5FLARED(H)
#c
#C-----Arguments:
#      REAL H
#C
#C-----Local variables:
#      INTEGER I,NI
#      REAL HNODE(12),C5VALS(12),LX
#C
#C-----Initializations
#      DATA C5VALS /
#     # 0.48, 0.50, 0.52, 0.53, 0.55, 0.56, 0.57, 0.59
#     #,0.61, 0.63, 0.65, 0.66 /
#      DATA HNODE /
#     #  1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.5, 3.0,
#     #  3.5, 4.0, 5.0 /
#C
#      I=0
#      NI=12
# 10   CONTINUE
#      I=I+1
#      IF(HNODE(I).LT.H.AND.I.LT.NI) GO TO 10
#C
#      IF(I.EQ.1)THEN
#        C5FLARED=C5VALS(I)
#      ELSE IF(I.EQ.NI.AND.H.GE.HNODE(I))THEN
#        C5FLARED=C5VALS(I)
#      ELSE
#        X=H- HNODE(I-1)
#        LX=HNODE(I) - HNODE(I-1)
#        C5FLARED = C5VALS(I-1) + X*C5VALS(I)/LX
#      ENDIF
#C
#      RETURN
#      END
#C
#C=====C5FLARED bof===========================================================
#
