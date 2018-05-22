"QType5" <-
function(C=NULL, Ao=NULL, h1=NULL, z=NULL, g=NULL) {
  return( C*Ao*sqrt(2*g*(h1 - z) ) );
}
# TWRI p.6

"culvert2QType5" <-
function(h1=NULL, h4=NULL, culvert=NULL, approach=NULL,
         digits=4, maxits=50, eps=1e-6, ...) {

  h1.elev <- h1; h4.elev <- h4;

  h1 <- h1 - culvert$zdsinvert;
  h4 <- h4 - culvert$zdsinvert;
  if(h4 < 0) h4 <- 0;

  z  <- culvert$z;
  Ao <- culvert$Ao.inlet;
  Ko <- culvert$Ko.inlet;
  L  <- culvert$Ltop;
  g  <- culvert$gravity;
  C  <- getCforType5(h1=h1, culvert=culvert);
  NB <- culvert$number.of.barrels;
  A1     <- approach$geometry$AREATOTAL/approach$fraction.of.flow;
  A1frac <- culvert$fraction.of.approach.area;
  alpha1 <- approach$geometry$alpha;
  D   <- culvert$diameter;
  inlet.depression <- culvert$inlet.depression;
  d2 <- D - inlet.depression;
  g2 <- 2*g;

  ifelse(culvert$ignore.approach.velocity.head, usev1  <- 0, usev1  <- 1);
  ifelse(culvert$ignore.approach.losses,        useh12 <- 0, useh12 <- 1);

  Q.old <- C*Ao*sqrt(g2*(h1 - z));

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

  its <- 0;
  while(1) {
     its <- its + 1;
     #catme("iteration:",its);

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


     lengths <- Lengths4MiteredPipe(h1=h1, flowtype=5, d2=NA, d3=NA,
                                    culvert=culvert, approach=approach);
     L  <- lengths$L;
     Lw <- lengths$Lw;

     h12 <- useh12*HeadLoss12(Q.old*NB, culvert=culvert,
                              approach=approach, d2=d2, otherL=Lw);

     #catme("v1head:",v1head);
     H <- h1 - z + v1head - h12;
     Q <- C*Ao*sqrt(g2*H);
     if(! is.finite(Q)) {
        stop("Infinite Q determined for Type 5, v1head likely growing unbounded, try larger approach section or reducing fraction of total flow normal to inlet");
     }
     if(abs(Q-Q.old)/Q.old < eps | its > maxits) break;
     Q.old <- Q;
  }

  test56 <- isType5or6(h1=h1+v1head, culvert=culvert);

  Q <- list(Q=round(Q*NB, digits=digits),
            error=(Q-Q.old)*NB, its=its,
            C=C,
            d2=d2,
            Ao=Ao,
            Ko=Ko,
            h1=h1,
            z=z,
            H=H,
            v1head=v1head,
            L=L,
            g=g,
            terminalarea=Ao,
            konvey2=Ko,
            isType5=test56$isType5,
            valid=test56$isType5,
            roadflow=roadflow,
            message="");
  return(Q);
}


"printType5" <-
function(flow=NULL, culvert=culvert, approach=approach,
         splash=TRUE, checkvalid=TRUE) {
  if(checkvalid & ! flow$valid) return(NA);

  lunits <- culvert$lengthunits;
  qunits <- culvert$flowunits;
  aunits <- culvert$areaunits;

  catme("  Depth (at crown) at inlet", round(flow$d2, digits=3),lunits);
  catme("  Area (naught)", round(flow$Ao, digits=2),aunits);
  catme("  Conveyance (naught)", round(flow$Ko, digits=0),qunits);
  catme("  Approach velocity head", round(flow$v1head, digits=3), lunits);

  pdffile <- paste(c("TMPcapR_culvert",
                     culvert$TMP.key, "_inletoutlet.pdf"), collapse="");
  pdf(pdffile);
  graphCulvert(discharge=flow$Q,
               depth=culvert$diameter-culvert$inlet.depression,
               culvert=culvert, location="inlet",
               depression=culvert$inlet.depression);
  dev.off();
}


"isType5or6" <-
function(type, h1=NULL, culvert=NULL) {
  So <- culvert$slope;
  D  <- culvert$diameter;
  nvalue <- culvert$nvalue;
  L  <- culvert$Ltop; # TWRI p.8
  g  <- culvert$gravity;
  Ro <- culvert$Ro.inlet;
  manningcor <- culvert$manningcor;
  z  <- culvert$z;

  L.over.D <- L/D;
  X <- 2*g * nvalue^2 * (h1-z) / (manningcor^2 * Ro^(4/3));

  isType5 <- isType6 <- FALSE;
  if((culvert$type == "circle" | culvert$type == "ellipse") &
    culvert$is.barrel.rough == TRUE) { # FIGURE 16
    fig16 <- TWRIfigure16(h1=h1, culvert=culvert);
    if(fig16 == 5) isType5 <- TRUE;
    if(fig16 == 6) isType6 <- TRUE;
  } else { # FIGURE 15
    fig15 <- TWRIfigure15(culvert=culvert);
    if(fig15 == 5) isType5 <- TRUE;
    if(fig15 == 6) isType6 <- TRUE;
  }

  return(list(isType5=isType5, isType6=isType6, L.over.D=L.over.D, X=X));
}


