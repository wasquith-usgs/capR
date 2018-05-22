"QType4" <-
function(C=NULL, Ao=NULL, h1=NULL, h4=NULL, n=NULL,
         L=NULL, Ro=NULL, g=32.2, manningcor=1.486) {
  tmp <- 2*g*(C*n)^2*L/(manningcor^2*Ro^(4/3)); tmp <- 1 + tmp;
  delh <- h1 - h4;
  return( C*Ao*sqrt(2*g*(delh)/tmp) );
}
# TWRI p.5

"culvert2QType4" <-
function(h1=NULL, h4=NULL, culvert=NULL, approach=NULL,
         digits=4, maxits=50, eps=1e-6, ...) {

  h1.elev <- h1; h4.elev <- h4;

  h1 <- h1 - culvert$zdsinvert;
  h4 <- h4 - culvert$zdsinvert;
  if(h4 < 0) h4 <- 0;

  Ao  <- culvert$Ao.inlet;
  Ro  <- culvert$Ro.inlet;
  Ko  <- culvert$Ko.inlet;
  z   <- culvert$z;
  D   <- culvert$diameter;
  inlet.depression <- culvert$inlet.depression;
  d2 <- D - inlet.depression;

  n   <- culvert$nvalue;
  L   <- culvert$Ltop; # TWRI p.8
  g   <- culvert$gravity;
  manningcor <- culvert$manningcor;
  NB <- culvert$number.of.barrels;
  A1     <- approach$geometry$AREATOTAL/approach$fraction.of.flow;
  A1frac <- culvert$fraction.of.approach.area;
  alpha1 <- approach$geometry$alpha;
  g2 <- 2*g;

  ifelse(culvert$ignore.approach.velocity.head, usev1  <- 0, usev1  <- 1);
  ifelse(culvert$ignore.approach.losses,        useh12 <- 0, useh12 <- 1);

  C <- round(getCforType4(culvert=culvert), digits=digits);
  #catme("h1",h1); catme("h4",h4);
  #catme("g",g);catme("n",n);catme("Ro",Ro);
  #catme("manningcor",manningcor);
  tmp <- g2*(C*n)^2*L/(manningcor^2*Ro^(4/3));
  Q.old <- C*Ao*sqrt(g2*(h1 - h4)/(1 + tmp));

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

     lengths <- Lengths4MiteredPipe(h1=h1, flowtype=4, d2=NA, d3=NA,
                                    culvert=culvert, approach=approach);
     L  <- lengths$L;
     Lw <- lengths$Lw;

     h12 <- useh12*HeadLoss12(Q.old*NB, culvert=culvert,
                              approach=approach, d2=d2, otherL=Lw);

     #catme("h1-z:",(h1-z));
     #catme("D:",D);
     vohead <- (Q.old/Ao)^2/(g2*C^2);
     #catme("vohead",vohead);

     Q <- C*Ao*sqrt(g2*(h1 - h4 + v1head - h12)/(1 + tmp));
     if(! is.finite(Q)) {
        stop("Infinite Q determined for Type 4, v1head likely growing unbounded, try larger approach section or reducing fraction of total flow normal to inlet");
     }
     # TWRI p. 36 For type4 h1-z must exceed D by an amount
     # equal to vohead (with C^2 term) to ensure full flow
     # at upstream end. WHA interprets this to be
     # "exceed by at least"
     #catme("h1",h1);catme("z",z);catme("vohead",vohead);
     #catme("D",D);catme("C",C);catme("Q",Q);
     #catme("Ao",Ao);

     #ifelse(h1 - z - vohead > D, valid <- TRUE, valid <- FALSE);
     ifelse(h1 - z - D >= vohead, valid <- TRUE, valid <- FALSE);

     if(abs(Q-Q.old)/Q.old < eps | its > maxits) break;
     Q.old <- Q;
  }

  # A few cases have been found in which valid is FALSE yet, things seem
  # SO submerged that surely barrel is running full at inlet. Use the 1.5
  # criteria to represent head that is so high that barrel will be full
  # regardless of what the relation between h1, z, D, and vohead tells us.
  if(valid == FALSE && (h1-z)/D > 1.5) valid <- TRUE;

  Q <- list(Q=round(Q*NB, digits=digits),
            error=(Q-Q.old)*NB,
            its=its,
            C=C,
            d2=d2,
            Ao=Ao,
            Ro=Ro,
            Ko=Ko,
            L=L,
            H=(h1-h4),
            h1=h1,
            vohead=vohead,
            v1head=v1head,
            g=g,
            terminalarea=Ao,
            konvey2=Ko,
            isType4=valid,
            valid=valid,
            roadflow=roadflow,
            message="");
  return(Q);
}




"printType4" <-
function(flow=NULL, culvert=culvert, approach=approach,
         splash=TRUE, checkvalid=TRUE) {
  if(checkvalid & ! flow$valid) return(NA);

  lunits <- culvert$lengthunits;
  qunits <- culvert$flowunits;
  aunits <- culvert$areaunits;

  catme("  Depth (at crown)", round(flow$d2, digits=2), lunits);
  catme("    Area (naught)", round(flow$Ao, digits=2), aunits);
  catme("    Hydraulic Radius (naught)", round(flow$Ro, digits=2), lunits);
  catme("    Conveyance (naught)", round(flow$Ko, digits=0), qunits);
  catme("  Length of culvert", round(flow$L, digits=2), lunits);
  catme("  Approach velocity head", round(flow$v1head, digits=3), lunits);

  pdffile <- paste(c("TMPcapR_culvert",
                     culvert$TMP.key, "_inletoutlet.pdf"), collapse="");
  pdf(pdffile);
  graphCulvert(discharge=flow$Q,
               depth=culvert$diameter-culvert$inlet.depression,
               culvert=culvert, location="inlet",
               depression=culvert$inlet.depression);
  graphCulvert(discharge=flow$Q,
               depth=culvert$diameter-culvert$outlet.depression,
               culvert=culvert, location="outlet",
               depression=culvert$outlet.depression);
  dev.off();

}

