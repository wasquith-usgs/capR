"KRND" <-
function(rnd) {
  if(rnd <= 0.115) {
    return(1.0005 + 1.9662*rnd + 4.5275*rnd^2 - 61.792*rnd^3);
  } else if(rnd <= 0.133) {
    return(1.192 + 0.4444*(rnd - 0.115));
  } else {
    return(1.2);
  }
}


#C=====KRND bof=================================================================
#C
#C-----Purpose:
#C       interpolates rounding adjustment coefficients
#C     Programmed by: JM Fulford
#C
#      REAL FUNCTION KRND(RND)
#C
#C-----Arguments:
#      REAL RND
#C
#C-----Local variables
#C
#      IF(RND.LE.0.115) THEN
#        KRND=1.0005 + 1.9662*RND + 4.5275*RND*RND - 61.792*RND*RND*RND
#      ELSE IF (RND.LE.0.133)THEN
#        KRND=1.192 + 0.4444*(RND - 0.115)
#      ELSE
#        KRND=1.2
#      ENDIF
#C
#      RETURN
#      END
#C
#C=====KRND eof=============================================================
#C
#
