

Cval.5table6.h <- new.h();
set.h("c5vals",
      matrix(
      c(0.44, 0.46, 0.49, 0.50, 0.50, 0.51, 0.51,
        0.46, 0.49, 0.52, 0.53, 0.53, 0.54, 0.54,
        0.47, 0.51, 0.54, 0.55, 0.55, 0.56, 0.56,
        0.48, 0.52, 0.55, 0.57, 0.57, 0.57, 0.57,
        0.49, 0.54, 0.57, 0.58, 0.58, 0.58, 0.58,
        0.50, 0.55, 0.58, 0.59, 0.60, 0.60, 0.60,
        0.51, 0.56, 0.59, 0.60, 0.61, 0.61, 0.62,
        0.54, 0.59, 0.62, 0.54, 0.64, 0.65, 0.66,
        0.55, 0.61, 0.64, 0.66, 0.67, 0.69, 0.70,
        0.57, 0.62, 0.65, 0.67, 0.69, 0.70, 0.71,
        0.58, 0.63, 0.66, 0.68, 0.70, 0.71, 0.72,
        0.59, 0.64, 0.67, 0.69, 0.71, 0.72, 0.73), byrow=TRUE, ncol=7),
      Cval.5table6.h);
set.h("rnode", c(  0, 0.02, 0.04, 0.06, 0.08, 0.10, 0.14),
      Cval.5table6.h);
set.h("hnode", c(1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.5, 3.0, 3.5, 4.0, 5.0),
      Cval.5table6.h);


"C5TABLE6" <-
function(hzD=NULL, rnd=NULL) {

   c5vals <- get.h("c5vals", Cval.5table6.h);
   rnode  <- get.h("rnode",  Cval.5table6.h);
   hnode  <- get.h("hnode",  Cval.5table6.h);

   n.hnode <- length(hnode);
   min.hnode <- min(hnode); max.hnode <- max(hnode);
   nodes <- 1:n.hnode; rnds <- seq(0,0.14, by=0.01);
   c5table6 <- rep(NA, length(hzD));
   for(i in 1:length(hzD)) {
   	 h <- hzD[i];
   	 if(h < min.hnode) {
   	   the.value <- approx(x=rnode, y=c5vals[1,], xout=rnd)$y;
   	 } else if(h > max.hnode) {
       the.value <- approx(x=rnode, y=c5vals[n.hnode,], xout=rnd)$y;
   	 } else {
   	   j <- nodes[hnode == h];
   	   if(length(j) != 0) {
   	     the.value <- approx(x=rnode, y=c5vals[j,], xout=rnd)$y;
   	   } else {
   	     j1 <- max(nodes[hnode < h]); j2 <- min(nodes[hnode > h]);
         y <- c(approx(x=rnode, y=c5vals[j1,], xout=rnd)$y,
                approx(x=rnode, y=c5vals[j2,], xout=rnd)$y);
         the.value <- approx(x=hnode[c(j1,j2)], y=y, xout=h)$y;
       }
     }
     c5table6[i] <- the.value;
   }
   return(c5table6)
}


"demo.C5TABLE6" <-
function() {
  out <- C5TABLE6(runif(10, min=1,max=6), 0.08)
  print(out)
}
#C=====C5TAB6 bof=============================================================
#C
#C-----Purpose:
#C      Interpolate from table 6 values of twri on culverts to determine C5
#C     Programmed by: JM Fulford
#C     Date: 12.96
#C
#      REAL FUNCTION C5TAB6(RND,H)
#C
#C-----Arguments:
#      REAL RND,H
#C
#C-----Local Variables:
#      INTEGER I,J
#      REAL RNODE(7),HNODE(12),C5VALS(7,12)
#      REAL LX,LY,A1,B2,X1,X2
#C
#C-----Initializations
#      DATA C5VALS /
#     #  0.44, 0.46, 0.49, 0.50, 0.50, 0.51, 0.51,
#     #  0.46, 0.49, 0.52, 0.53, 0.53, 0.54, 0.54,
#     #  0.47, 0.51, 0.54, 0.55, 0.55, 0.56, 0.56,
#     #  0.48, 0.52, 0.55, 0.57, 0.57, 0.57, 0.57,
#     #  0.49, 0.54, 0.57, 0.58, 0.58, 0.58, 0.58,
#     #  0.50, 0.55, 0.58, 0.59, 0.60, 0.60, 0.60,
#     #  0.51, 0.56, 0.59, 0.60, 0.61, 0.61, 0.62,
#     #  0.54, 0.59, 0.62, 0.54, 0.64, 0.65, 0.66,
#     #  0.55, 0.61, 0.64, 0.66, 0.67, 0.69, 0.70,
#     #  0.57, 0.62, 0.65, 0.67, 0.69, 0.70, 0.71,
#     #  0.58, 0.63, 0.66, 0.68, 0.70, 0.71, 0.72,
#     #  0.59, 0.64, 0.67, 0.69, 0.71, 0.72, 0.73 /
#      DATA RNODE/ 0, 0.02, 0.04, 0.06, 0.08, 0.10, 0.14 /
#      DATA HNODE /
#     #      1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0,
#     #      2.5, 3.0, 3.5, 4.0, 5.0 /
#C
#      I=0
# 10   CONTINUE
#      I=I+1
#      IF(RNODE(I).LT.RND.AND.I.LT.7) GO TO 10
#      J=0
# 20   CONTINUE
#      J=J+1
#      IF(HNODE(J).LT.H.AND.J.LT.12) GO TO 20
#C
#      IF(I.EQ.1)THEN
#        IF(J.GT.1.AND.J.LE.12)THEN
#          X1=C5VALS(I,J-1)
#          X2=C5VALS(I,J)
#        ELSE
#          IF(J.GT.12) THEN
#            X1=C5VALS(I,12)
#          ELSE
#            X1=C5VALS(I,J)
#          ENDIF
#          X2=X1
#        ENDIF
#      ELSE IF(I.GT.7) THEN
#        IF(J.LT.13.AND.J.GT.1)THEN
#          X1=C5VALS(7,J-1)
#          X2=C5VALS(7,J)
#        ELSE
#          IF(J.GT.12)THEN
#            X1=C5VALS(7,12)
#          ELSE
#            X1=C5VALS(7,J)
#          ENDIF
#          X2=X1
#        ENDIF
#      ELSE
#        LX=RNODE(I) - RNODE(I-1)
#        A1=(RND - RNODE(I-1))/LX
#        IF(J.GT.1.AND.J.LE.12)THEN
#          X1=C5VALS(I-1,J-1) + A1*(C5VALS(I,J-1) - C5VALS(I-1,J-1))
#          X2=C5VALS(I-1,J) + A1*(C5VALS(I,J) - C5VALS(I-1,J))
#        ELSE IF(J.LE.1)THEN
#          X1=C5VALS(I-1,1) + A1*(C5VALS(I,1) -C5VALS(I-1,1))
#          X2=X1
#        ELSE
#          X1=C5VALS(I-1,12) + A1*(C5VALS(I,12)-C5VALS(I-1,12))
#          X2=X1
#        ENDIF
#      ENDIF
#      IF(J.EQ.1)THEN
#        B2=0
#      ELSE IF(J.GT.12)THEN
#        B2=0
#      ELSE
#        LY=HNODE(J)-HNODE(J-1)
#        B2=(H - HNODE(J-1))/LY
#      ENDIF
#      C5TAB6=X1 + B2*(X2-X1)
#C
#      RETURN
#      END
#C
#C=====C5TAB6 bof=============================================================
#
