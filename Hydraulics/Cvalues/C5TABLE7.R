

Cval.5table7.h <- new.h();
set.h("c5vals",
      matrix(
      c(0.44, 0.44, 0.44, 0.43, 0.42, 0.39,
        0.44, 0.46, 0.46, 0.45, 0.43, 0.41,
        0.46, 0.47, 0.47, 0.46, 0.45, 0.42,
        0.47, 0.49, 0.49, 0.48, 0.46, 0.43,
        0.48, 0.50, 0.50, 0.48, 0.47, 0.44,
        0.49, 0.51, 0.51, 0.50, 0.48, 0.45,
        0.50, 0.52, 0.52, 0.51, 0.49, 0.46,
        0.51, 0.53, 0.53, 0.52, 0.49, 0.46,
        0.54, 0.56, 0.56, 0.54, 0.52, 0.49,
        0.55, 0.58, 0.58, 0.56, 0.54, 0.50,
        0.57, 0.60, 0.60, 0.58, 0.55, 0.52,
        0.58, 0.61, 0.61, 0.59, 0.56, 0.53,
        0.59, 0.62, 0.62, 0.60, 0.58, 0.54), byrow=TRUE, ncol=6),
      Cval.5table7.h);
set.h("tnode", c(  0,  30,  45,  60,  75,  90),
      Cval.5table7.h);
set.h("hnode", c(1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9,
                 2.0, 2.5, 3.0, 3.5, 4.0, 5.0),
      Cval.5table7.h);


"C5TABLE7" <-
function(hzD=NULL, theta=NULL) {

   c5vals <- get.h("c5vals", Cval.5table7.h);
   tnode  <- get.h("tnode",  Cval.5table7.h);
   hnode  <- get.h("hnode",  Cval.5table7.h);

   n.hnode <- length(hnode);
   min.hnode <- min(hnode); max.hnode <- max(hnode);
   nodes <- 1:n.hnode; thetas <- seq(0,90, by=1);
   c5table7 <- rep(NA, length(hzD));
   for(i in 1:length(hzD)) {
   	 h <- hzD[i];
   	 if(h < min.hnode) {
   	   c5table7[i] <- approx(x=tnode, y=c5vals[1,], xout=theta)$y;
   	 } else if(h > max.hnode) {
       c5table7[i] <- approx(x=tnode, y=c5vals[n.hnode,], xout=theta)$y;
   	 } else {
   	   j <- nodes[hnode == h];
   	   if(length(j) != 0) {
   	     c5table7[i] <- approx(x=tnode, y=c5vals[j,], xout=theta)$y;
   	   } else {
   	     j1 <- max(nodes[hnode < h]); j2 <- min(nodes[hnode > h]);
         y <- c(approx(x=tnode, y=c5vals[j1,], xout=theta)$y,
                approx(x=tnode, y=c5vals[j2,], xout=theta)$y);
         c5table7[i] <- approx(x=hnode[c(j1,j2)], y=y, xout=h)$y;
       }
     }
   }
   return(c5table7)
}

"demo.C5TABLE7" <-
function() {
  out <- C5TABLE7(runif(10, min=1,max=3), 30)
  print(out)
}


#C=====C5TAB7 bof=============================================================
#C
#C-----Purpose:
#C       interpolate from tabled values C5 using table 7 of the TWRI on
#C       culverts
#C     Programmed by: JM Fulford
#C     Date: 12.96
#C
#      REAL FUNCTION C5TAB7(THETA,H)
#C
#C     Arguments:
#      REAL THETA,H
#C
#C     Local variables
#      INTEGER I,J
#      REAL TNODE(6),HNODE(13),C5VALS(6,13)
#      REAL LX,LY,A1,B2
#C
#C-----Initializations
#      DATA C5VALS /
#     # 0.44,0.44,0.44,0.43,0.42,0.39,
#     # 0.44,0.46,0.46,0.45,0.43,0.41,
#     # 0.46,0.47,0.47,0.46,0.45,0.42,
#     # 0.47,0.49,0.49,0.48,0.46,0.43,
#     # 0.48,0.50,0.50,0.48,0.47,0.44,
#     # 0.49,0.51,0.51,0.50,0.48,0.45,
#     # 0.50,0.52,0.52,0.51,0.49,0.46,
#     # 0.51,0.53,0.53,0.52,0.49,0.46,
#     # 0.54,0.56,0.56,0.54,0.52,0.49,
#     # 0.55,0.58,0.58,0.56,0.54,0.50,
#     # 0.57,0.60,0.60,0.58,0.55,0.52,
#     # 0.58,0.61,0.61,0.59,0.56,0.53,
#     # 0.59,0.62,0.62,0.60,0.58,0.54  /
#      DATA TNODE / 0.0, 30., 45., 60., 75., 90. /
#      DATA HNODE /
#     #     1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.,
#     #     2.5, 3.0, 3.5, 4.0, 5.0 /
#
#C
#      I=0
# 10   CONTINUE
#      I=I+1
#      IF(TNODE(I).LT.THETA.AND.I.LT.6) GO TO 10
#      J=0
# 20   CONTINUE
#      J=J+1
#      IF(HNODE(J).LT.H.AND.J.LT.13) GO TO 20
#C
#      IF(I.EQ.1)THEN
#        IF(J.GT.1.AND.J.LT.14)THEN
#          X1=C5VALS(I,J-1)
#          X2=C5VALS(I,J)
#        ELSE
#          IF(J.GT.13) THEN
#            X1=C5VALS(I,13)
#          ELSE
#            X1=C5VALS(I,J)
#          ENDIF
#          X2=X1
#        ENDIF
#      ELSE IF(I.GT.6) THEN
#        IF(J.LT.14.AND.J.GT.1)THEN
#          X1=C5VALS(6,J-1)
#          X2=C5VALS(6,J)
#        ELSE
#          IF(J.GT.13)THEN
#            X1=C5VALS(6,13)
#          ELSE
#            X1=C5VALS(6,J)
#          ENDIF
#        ENDIF
#      ELSE
#        LX=TNODE(I) - TNODE(I-1)
#        A1=(THETA - TNODE(I-1))/LX
#        IF(J.GT.1.AND.J.LE.13)THEN
#          X1=C5VALS(I-1,J-1) + A1*(C5VALS(I,J-1) - C5VALS(I-1,J-1))
#          X2=C5VALS(I-1,J) + A1*(C5VALS(I,J) - C5VALS(I-1,J))
#        ELSE IF(J.LE.1)THEN
#          X1=C5VALS(I-1,J) + A1*(C5VALS(I,J) - C5VALS(I-1,J))
#          X2=X1
#        ELSE
#          X1=C5VALS(I-1,13) + A1*(C5VALS(I,13) - C5VALS(I-1,13))
#          X2=X1
#        ENDIF
#      ENDIF
#      IF(J.EQ.1)THEN
#        B2=0
#      ELSE IF(J.GT.13)THEN
#        B2=0
#      ELSE
#        LY=HNODE(J)-HNODE(J-1)
#        B2=(H - HNODE(J-1))/LY
#      ENDIF
#      C5TAB7=X1 + B2*(X2-X1)
#C
#      RETURN
#      END
#C
#C=====C5TAB7 eof=============================================================
#
