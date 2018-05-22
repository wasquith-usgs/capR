"QType6" <-
function(C=NULL, Ao=NULL, h1=NULL, h3=NULL, h23=NULL, g=NULL) {
  return( C*Ao*sqrt(2*g*(h1 - h3 - h23) ) );
}
# TWRI p.6


"culvert2QType6" <-
function(h1=NULL, h4=NULL, culvert=NULL, approach=NULL,
         digits=4, maxits=50, eps=1e-6, ...) {

  h1.elev <- h1; h4.elev <- h4;

  h1 <- h1 - culvert$zdsinvert;
  h4 <- h4 - culvert$zdsinvert;
  if(h4 < 0) h4 <- 0;

  Ao  <- culvert$Ao.inlet;
  Ko  <- culvert$Ko.inlet;
  g   <- culvert$gravity;
  A1     <- approach$geometry$AREATOTAL/approach$fraction.of.flow;
  A1frac <- culvert$fraction.of.approach.area;
  alpha1 <- approach$geometry$alpha;
  D   <- culvert$diameter;
  inlet.depression <- culvert$inlet.depression;
  d2 <- D - inlet.depression;
  L   <- culvert$Ltop; # TWRI p.8
  g2  <- 2*g;
  NB <- culvert$number.of.barrels;

  ifelse(culvert$ignore.approach.velocity.head, usev1  <- 0, usev1  <- 1);
  ifelse(culvert$ignore.approach.losses,        useh12 <- 0, useh12 <- 1);

  test56 <- isType5or6(h1=h1, culvert=culvert);

  C <- round(getCforType6(culvert=culvert), digits=digits);

  Qbyfig17 <- QType6byFig17(h1=h1, C=C, culvert=culvert);


  # initially guess that h3 is in accordance to TWRI p. 34
  # however, this is largely paying homage as these will
  # quickly be superceded in the iterations.
  ifelse(culvert$type == "box", h3 <- 0.65*d2,
                                h3 <- 0.75*d2);
  Q.old <- getDischargeAtCriticalDepth(h3, culvert=culvert,
                                       location="outlet",
                                       depression=culvert$outlet.depression);

  road <- culvert$road; # properties of the road
  approachflow <- approach$TMP.accumulated.flow; # TOTAL COMPUTED FLOW
                                                 # FROM AN EARLIER RUN

  # July 11, 2016: The following line seems bogus and causes too much velocity head
  # because approachflow will be summed back into the Qtotal within the iteration loop.
  if(approachflow == 0) {
     #approachflow <- Q.old*NB;
     #Q.previous.run <- approachflow;
  }
  Q.previous.run     <- culvert$TMP.Q.previous.run;
  Qroad.previous.run <- culvert$TMP.Qroad.previous.run;
  v1head.approach <- usev1*alpha1*(approachflow/A1)^2/g2;
  v1head <- v1head.road <- 0;
  if(approach$forceEqualApproachHead) {
      v1head <- v1head.road <- v1head.approach;
  } else if(approach$forceIgnoreApproachHead) {
      v1head <- v1head.road <- v1head.approach <- 0;
  } else if(approach$useApproachHeadApportioning & approachflow != 0) {
      v1head <- usev1*alpha1*(Q.previous.run/(A1*A1frac))^2/g2;
      v1head.road <- (Qroad.previous.run/approachflow) * v1head.approach;
  } else if(approachflow != 0) {
      v1head      <- (Q.previous.run/approachflow)     * v1head.approach;
      v1head.road <- (Qroad.previous.run/approachflow) * v1head.approach;
  }
  roadflow <- Qroad(h1=h1.elev, h4=h4.elev, v1head=v1head.road, road=road);

  #catme("Q.old=",Q.old);
  Q <- NULL;
  its <- 0;
  while(1) {
     its <- its + 1;

     h3 <- getH3forType6(h4=h4, discharge=Q.old, culvert=culvert);

     geo3 <- setCulvertGeometry(depth=h3, culvert=culvert,
                                location="outlet",
                                depression=culvert$outlet.depression);
     A3   <- geo3$A;
     K3   <- geo3$KONVEY;

     # Discharge apportioning of total head amongst the two possible
     # units conveying flow (these culverts and the number of barrels NB)
     # and the optional roadway, only if actually present.
     # Note that approachflow was the TOTAL Q in A1 from last outer iteration
     # of the while(1) loop that this comments are typed in.
     roadQ <- roadflow$Qroad;
     totalQ <- Q.old*NB - Q.previous.run     +
               roadQ    - Qroad.previous.run + approachflow;
     v1head.approach <- usev1*alpha1*(totalQ/A1)^2/g2;
     v1head <- v1head.road <- 0;
     if(approach$forceEqualApproachHead) {
        v1head <- v1head.road <- v1head.approach;
     } else if(approach$forceIgnoreApproachHead) {
        v1head <- v1head.road <- v1head.approach <- 0;
     } else if(approach$useApproachHeadApportioning) {
        v1head <- usev1*alpha1*(Q.old*NB/(A1*A1frac))^2/g2;
        v1head.road <- (roadQ/approachflow) * v1head.approach;
     } else {
        v1head      <- (Q.old*NB/approachflow) * v1head.approach;
        v1head.road <- (roadQ/approachflow)    * v1head.approach;
     }
     roadflow <- Qroad(h1=h1.elev, h4=h4.elev, v1head=v1head.road, road=road);

     lengths <- Lengths4MiteredPipe(h1=h1, flowtype=6, d2=NA, d3=NA,
                                    culvert=culvert, approach=approach);
     L  <- lengths$L;
     Lw <- lengths$Lw;

     h12 <- useh12*HeadLoss12(Q.old*NB, culvert=culvert,
                              approach=approach, d2=d2, otherL=Lw);

     h23 <- HeadLoss23(Q.old, culvert=culvert, d2=d2, d3=h3);

     dc3  <- getCriticalDepth(Q.old, culvert=culvert,
                              location="outlet",
                              depression=culvert$outlet.depression);

     H <- h1 + v1head - h12 - h3 - h23;
     if(H < 0) {
          # conclude not type 6
          Q <- list(Q=NA, error=NA, its=its,
                    C=C,
                    d2=d2,
                    Ao=Ao,
                    Ko=Ko,
                    h1=h1,
                    d3=NA,
                    A3=NA,
                    K3=NA,
                    d3c=NA,
                    h12=NA,
                    h23=NA,
                    v1head=v1head,
                    H=NA,
                    L=L,
                    g=g,
                    terminalarea=NA,
                    konvey2=NA,
                    isType6=FALSE,
                    valid=FALSE,
                    roadflow=roadflow,
                    message="H < 0, trapping sqrt(x<0)",
                    Qbyfig17=Qbyfig17);
        return(Q);
     }
     Q <- C*Ao*sqrt(g2*H);

     if(! is.finite(Q)) {
        stop("Infinite Q determined for Type 6, v1head likely growing unbounded, try larger approach section or reducing fraction of total flow normal to inlet");
     }

     if(abs(Q-Q.old)/Q.old < eps | its > maxits)  break;
     Q.old <- Q;
  }
  Q <- list(Q=round(Q*NB, digits=digits),
            error=(Q-Q.old)*NB,
            its=its,
            C=C,
            d2=d2,
            Ao=Ao,
            Ko=Ko,
            h1=h1,
            d3=h3,
            A3=A3,
            K3=K3,
            d3c=dc3,
            h12=h12,
            h23=h23,
            v1head=v1head,
            H=H,
            L=L,
            g=g,
            terminalarea=Ao,
            konvey2=Ko,
            isType6=test56$isType6,
            valid=test56$isType6,
            roadflow=roadflow,
            message="",
            Qbyfig17=Qbyfig17);
  return(Q);
  # Jain (2001, p. 313) reports h3 on the interval [D/2, D] because although
  # barrel is full, D is not a good estimate of pressure head
}



"printType6" <-
function(flow=NULL, culvert=culvert, approach=approach,
         splash=TRUE, checkvalid=TRUE) {
  if(checkvalid & ! flow$valid) return(NA);

  lunits <- culvert$lengthunits;
  qunits <- culvert$flowunits;
  aunits <- culvert$areaunits;

  catme("  Depth (at crown) at inlet", round(flow$d2, digits=3),lunits);
  catme("    Area (naught) at inlet", round(flow$Ao, digits=2),aunits);
  catme("    Conveyance (naught) at inlet",
                                      round(flow$Ko, digits=0),qunits);
  catme("  Depth at outlet", round(flow$d3, digits=3),lunits);
  catme("    Area at outlet", round(flow$A3, digits=2),aunits);
  catme("    Conveyance at outlet", round(flow$K3, digits=0),qunits);
  catme("    Critical depth at outlet", round(flow$d3c, digits=3),lunits);
  catme("  Length of culvert", round(flow$L, digits=2),lunits);
  catme("  Headloss between approach and inlet", round(flow$h12,
                                                       digits=3),lunits);
  catme("  Headloss between inlet and outlet", round(flow$h23,
                                                       digits=3),lunits);
  catme("  Approach velocity head", round(flow$v1head, digits=3),lunits);

  catme("  ***Results from figure 17 of TWRI based on Cap97.08c FORTRAN***");
  catme("    Discharge is",
                                    round(flow$Qbyfig17$Q, digits=4),qunits);
  catme("      Headwater divided by diameter is",
                                    round(flow$Qbyfig17$H.over.D, digits=2));
  catme("      Correction factor Kf is",
                          round(flow$Qbyfig17$Kf, digits=2),
              "from X term of", round(flow$Qbyfig17$X, digits=2));
  catme("      Q divided by (Ao * sqrt(diameter)) is",
                              round(flow$Qbyfig17$Q.over.AorootD, digits=2));

  pdffile <- paste(c("TMPcapR_culvert",
                     culvert$TMP.key, "_inletoutlet.pdf"), collapse="");
  pdf(pdffile);
  graphCulvert(discharge=flow$Q,
               depth=culvert$diameter-culvert$inlet.depression,
               culvert=culvert, location="inlet",
               depression=culvert$inlet.depression);
  graphCulvert(discharge=flow$Q, depth=flow$d3,
               culvert=culvert, location="outlet",
               depression=culvert$outlet.depression);
  dev.off();

}


"getH3forType6" <-
function(h4=NULL, discharge=NULL, culvert=NULL,
         byoutletpressure=TRUE, supportedjet=FALSE) {
   if(byoutletpressure) {
      ratio <- piezoLevelRatioType6(discharge=discharge,
                                    culvert=culvert,
                                    supportedjet=supportedjet);
      #catme("piezoLevelRatioType6",ratio);
      return(ratio*culvert$diameter);
   }

   D  <- culvert$diameter;
   D.over.2 <- D/2;

   yc  <- getCriticalDepth(discharge, culvert=culvert);
   if(is.na(yc)) { # guessing again to keep process running
     ifelse(culvert$type == "box", h3 <- 0.65*D, h3 <- 0.75*D);
   }
   ifelse(h4 > yc, h3 <- h4, h3 <- yc);

   ycD2 <- (yc + D)/2; # Sturm (2010, p. 256-257)
   #ifelse(h4 > ycD2, h3 <- h4, h3 <- ycD2);
   if(h3 < D.over.2) h3 <- D.over.2; # set minimum (Jain, 2001, p.313)
   #  # AND TWRI p. 6 and p. 34;

   #h3 <- D;

   #geo <- setCulvertGeometry(depth=h3, culvert=culvert);
   #A3  <- geo$A;
   #v3head  <- (discharge/A3)^2/(2*culvert$gravity);
   #catme("getH3forType6, Q",discharge);
   #catme("getH3forType6, A3",A3);
   #catme("getH3forType6, yc=",yc);
   #catme("getH3forType6, h4=",h4);
   #catme("getH3forType6, h3=",h3);
   #catme("getH3forType6, D=",D);
   #catme("getH3forType6, (yc+D)/2=",ycD2);
   #catme("getH3forType6, v3head=",v3head);

   return(h3);
}



"QType6byFig17" <-
function(h1=NULL, C=C, culvert=NULL, switchKF=TRUE, ...) {

  L <- culvert$Ltop;
  D <- culvert$diameter;
  Ao <- culvert$Ao.inlet;
  Ro <- culvert$Ro.inlet;
  g  <- culvert$gravity;
  nvalue <- culvert$nvalue;
  manningcor <- culvert$manningcor;

  # IMPLEMENTING FIGURE 17
  # note that in original fortran source that nvalue^2 is missing
  # in the discharg.f file of the Cap97.08c sources (other conversions
  # in culvert.f)
  X <- 2*g * nvalue^2 * L / (manningcor^2 * Ro^(4/3)); # step 3 of TWRI, p.31

  # USING TWRI example 8 with Q=209
  # NOTE THAT switchFW=TRUE provides a Q=216
  #           switchFW=FALSE provides a Q=221
  # my dynamic computation of h3 provides Q=217
  if(switchKF) { # FROM discharg.f
    KF1 <- 1.222081  + X*(-0.39096055 + X*(0.10127278 - X*0.010547583));
    KF2 <- 1.6475101 + X*(-0.56519116 + X*(0.12983789 - X*0.011096691));
  } else { # FROM culvert.f
    KF1 <- 1.2002 + X*(-0.37526  +
                    X*( 0.13410  +
                    X*(-0.034714 +
                    X*(0.0047746 - X*0.00023996))));
    KF2 <- 1.7713 + X*(-0.93433 +
                    X*( 0.48185 +
                    X*(-0.14896 +
                    X*( 0.02349 - X*0.0014455))));
  }


  if(C < 0.76) {
    KF <- KF1;
  } else if(C >= 1.0) { # but C can not be >= 1.0??
    KF <- KF2;
  } else {
    KF <- KF1 + 4.166666 * (C-0.76) * (KF2-KF1);
  }
  # KF completes step 4 of TWRI, p.31

  HQ1 <- h1/D; # step 1 of TWRI, p.31 (C provided as argument)


  QRAT <- HQ1*(4.7693-HQ1*(0.68351 - .044726*HQ1)) - 1.8368;
  QRAT <- HQ1*(7.2458-HQ1*(1.8350  - HQ1*(0.26087+0.014075*HQ1))) - 3.6183;
  # step 2 of TWRI, p.31 is completed

  QRAT1 <- QRAT*KF; # step 5 of TWRI, p. 31
      Q <- QRAT1*Ao*sqrt(D); # step 6 of TWRI, p. 31

  return(list(Q=Q, Kf=KF,
              H.over.D=HQ1,
              X=X,
              Q.over.AorootD=QRAT));
}



