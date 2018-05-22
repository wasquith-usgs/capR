"KBEV" <-
function(rnd=NULL, theta=NULL) {
  if(theta <= 0) {
    return(1);
  } else if(theta <= 30) {
    Y1 <- 0.99973 + 1.4707*rnd - 4.77*rnd^2 - 11.794*rnd^3;
    return(1.0 + theta*(Y1 - 1)/30);
  } else if(theta <= 45) {
    Y1 <- 0.99973 + 1.4707*rnd -   1.77*rnd^2 - 11.794*rnd^3;
    Y2 <- 0.99897 + 3.6457*rnd - 25.459*rnd^2 + 40.508*rnd^3;
    return(Y1 + (theta - 30)*(Y2-Y1)/15);
  } else if(theta <= 60) {
    Y2 <- 0.998997 + 3.6457*rnd - 25.459*rnd^2 + 40.508*rnd^3;
    Y3 <- 1        + 4.8351*rnd - 18.307*rnd^2 - 19.827*rnd^3;
    return(Y2 + (theta - 45)*(Y3-Y2)/15);
  } else  {
    return(1.0+4.8351*rnd - 18.307*rnd^2 - 19.827*rnd^3);
  }
}



#C=====KBEV bof=============================================================
#C
#C-----Purpose:
#C        compute coefficient adjustments for beveling of box or pipe
#C     Programmed by: JM Fulford
#C     Date: 12.96
#C
#      REAL FUNCTION KBEV(RND,THETA)
#C
#C-----Arguments:
#      REAL RND,THETA
#C
#C-----Local variables:
#      REAL Y,Y2,Y3
#C
#      IF(THETA.LE.0.0) THEN
#        KBEV=1.0
#      ELSE IF(THETA.LE.30.) THEN
#        Y=0.99973 + 1.4707*RND - 4.77*RND*RND - 11.794*RND*RND*RND
#        KBEV=1.0 + THETA*(Y - 1.0)/30.0
#      ELSE IF (THETA.LE.45.0) THEN
#        Y=0.99973 + 1.4707*RND - 1.77*RND*RND - 11.794*RND*RND*RND
#        Y2=0.99897 + 3.6457*RND - 25.459*RND*RND + 40.508*RND*RND*RND
#        KBEV = Y + (THETA - 30.0)*(Y2-Y)/15.0
#      ELSE IF(THETA.LE.60)THEN
#        Y2=0.998997 + 3.6457*RND - 25.459*RND*RND + 40.508*RND*RND*RND
#        Y3=1.0 + 4.8351*RND - 18.307*RND*RND -19.827*RND*RND*RND
#        KBEV=Y2 + (THETA-45.0)*(Y3-Y2)/15.0
#      ELSE IF (THETA.GT.60) THEN
#        KBEV=1.0+4.8351*RND - 18.307*RND*RND - 19.827*RND*RND*RND
#      ENDIF
#C
#      RETURN
#      END
#C
#C=====KBEV eof=================================================================
#
