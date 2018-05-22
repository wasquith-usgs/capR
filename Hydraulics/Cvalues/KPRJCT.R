"KPRJCT" <-
function(projectratio=0) {
  if(projectratio < 0.1)  return(0.99909 - 0.78182*projectratio);
  return(0.92246 - 0.024299*projectratio);
}
# See TWRI p. 41-42

#C=====KPRJCT bof===============================================================
#C
#C-----Purpose:
#C       computes the coefficient adjustment for projecting pipes
#C-----Programmed by: JM Fulford
#C       
#      REAL FUNCTION KPRJCT(LPRJCT)
#C
#C-----Arguments:
#      REAL LPRJCT
#C
#C-----Local variables:
#C
#      IF(LPRJCT.LT.0.1)THEN
#        KPRJCT = 0.99909 - 0.78182*LPRJCT
#      ELSE IF(LPRJCT.GE.0.1)THEN
#        KPRJCT = 0.92246 - 0.024299*LPRJCT
#      ENDIF
#C
#      RETURN
#      END
#C
#C=====KPRJCT eof===============================================================
#
