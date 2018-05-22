"QC46" <- function(type=c("circle", "box", "ellipse", "pipearch", "nonstandard"),
                   kwr=NULL, rnd=NULL, theta=NULL,
                   inlet=c("flush", "mitered", "flared"), kproj=NULL) {
   # tc - culvert code type; 1-circular, 2-box, 3-pipe arch, 4-nonstand.
   # kproj  - projecting pipe adjustment to discharge coefficient (pg 42)
   # rnd    - rounding or beveling ratio
   inlet <- match.arg(inlet);
   type  <- match.arg(type);
   
   QC46 <- rep(1, length(theta)); # vectorization for asymmetric wingwalls
   for(i in 1:length(theta)) {
     if(inlet == "mitered") {
       QC46[i] <- 0.74
     } else if(inlet == "flared") {
       QC46[i] <- 0.90
     } else if(theta[i] < 75 | ( type != "circle" & type != "ellipse")) {
       C46 <- 0.84 + 2.125*rnd - 8.035*rnd*rnd
       if(theta[i] >= 30 & C46 < 0.87 &
                      (type == "circle" | type == "ellipse")) C46 <- 0.87
     } else if(theta[i] >= 75 & (type == "circle" | type == "ellipse")) {
       C46 <- (1.47 - 0.008*theta[i])*kwr
     }
     if(inlet != "flared") QC46[i] <- kproj*C46
   }
   return(mean(QC46));
}


#C=====QC46 bof=================================================================
#C
#C-----Purpose:
#C      computes default C46 discharge coefficients according to the TWRI
#C      chapter A3, "Measurement of Peak Discharge at Culverts by Indirect
#C      Methods", pg 42 and 43 using table 5.
#C     Programmed by: JM Fulford
#C
#      REAL FUNCTION QC46(TC,KWR,RND,THETA,INLET,KPROJ)
#c
#C-----Arguments:
#      INTEGER INLET,TC
#      REAL KWR,THETA,KPROJ
#C
#C-----Local variables:
#C
#      QC46=1.0
#      IF (INLET.EQ.2)THEN
#C       mitered pipe
#        QC46=0.74
#      ELSE IF(INLET.EQ.4)THEN
#C       flared pipe ends
#        QC46=0.90
#      ELSE IF(THETA.LT.75.0.OR.TC.GT.1)THEN
#        C46=0.84 + 2.125*RND - 8.035*RND*RND
#        IF(THETA.GE.30.AND.C46.LT.0.87.AND.TC.EQ.1) C46=0.87
#      ELSE IF(THETA.GE.75.0.AND.TC.EQ.1)THEN
#        C46=(1.47 - 0.008*THETA)*KWR
#      ENDIF
#      IF(INLET.NE.4)THEN
#        QC46=KPROJ*C46
#      ENDIF
#C
#      RETURN
#      END
#C
#C=====QC46 eof=================================================================
#
