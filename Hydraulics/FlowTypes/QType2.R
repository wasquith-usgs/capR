"QType2" <-
function(C=NULL, Ac=NULL, h1=NULL,
         a1=NULL, v1=NULL, dc=NULL, h12=NULL, h23=NULL, g=32.2) {
  vh <- a1*v1^2/(2*g);
  return(C*Ac*sqrt(h1 + vh - dc - h12 - h23));
}
# TWRI p.5


"culvert2QType2" <-
function(h1=NULL, h4=NULL, culvert=NULL, approach=NULL,
         digits=4, maxits=50, eps=1e-6, ...) {

  h1.elev <- h1; h4.elev <- h4;

  h1 <- h1 - culvert$zdsinvert;
  h4 <- h4 - culvert$zdsinvert;
  if(h4 < 0) h4 <- 0;

  So     <- culvert$So;
  z      <- culvert$z;
  g      <- culvert$gravity;
  D      <- culvert$diameter;
  A1     <- approach$geometry$AREATOTAL/approach$fraction.of.flow;
  A1frac <- culvert$fraction.of.approach.area;
  alpha1 <- approach$geometry$alpha;
  g2     <- 2*g;
  hzD    <- HeadwtrDiaRatio(h1=h1, culvert=culvert);
  NB     <- culvert$number.of.barrels;

  ifelse(culvert$ignore.approach.velocity.head, usev1  <- 0, usev1  <- 1);
  ifelse(culvert$ignore.approach.losses,        useh12 <- 0, useh12 <- 1);

  had.to.ignore.v1head.and.h12 <- NA;

  dc <- ((h1 - z)/2)*0.95;
  geo3 <- setCulvertGeometry(depth=dc, culvert=culvert, location="outlet",
                             depression=culvert$outlet.depression);
  A3 <- geo3$A;
  Q.old <- getDischargeAtCriticalDepth(depth=dc, culvert=culvert,
                                       location="outlet",
                                       depression=culvert$outlet.depression);
  v3 <- Q.old/A3;
  if(A3 == 0) v3 <- 0;

  d2 <- h1 - z - v3^2/g2; # TWRI p.25, simplification
  #catme("h1",h1);
  #catme("z",z);
  #catme("v3",v3);
     if(is.nan(d2)) {
        Q <- list(Q=0, error=0, its=0,
                  C=C,
                  Ac=NA,
                  Kc=NA,
                  A2=NA,
                  K2=NA,
                  h1=h1,
                  z=z,
                  v1head=0,
                  H=NA,
                  d2=NA,
                  d3=NA,
                  h12=NA,
                  h23=NA,
                  L=NA,
                  Lw=NA,
                  g=g,
                  terminalarea=NA,
                  konvey2=NA,
                  h4.over.hc=NA,
                  So.lt.Sc=NA,
                  isType2=FALSE,
                  valid=FALSE,
                  roadflow=culvert$road,
                  message="d2 is NaN, assume not Type 2",
                  had.to.ignore.v1head.and.h12=FALSE);
        return(Q);
     }
  #catme("Type 2 spinup d2",d2);
  inlet.depression <- culvert$inlet.depression;
  #catme("Type 2 interation D",D);
  #catme("Type 2 interation inlet.depression",inlet.depression);
  if(d2 > (D - inlet.depression)) d2 <- 0.95*(D - inlet.depression);
  if(d2 <= 0) d2 <- dc;

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

  lengths <- Lengths4MiteredPipe(h1=h1, flowtype=2,  d2=d2, d3=dc,
                                 culvert=culvert, approach=approach);
  L  <- lengths$L;
  Lw <- lengths$Lw;

  h12 <- useh12*HeadLoss12(Q.old*NB, culvert=culvert,
                           approach=approach, d2=d2, otherL=Lw);
  h23 <- HeadLoss23(Q.old, culvert=culvert, d2=d2, d3=dc, otherL=L);

  Q <- NULL; Ac <- A3;  # A3 is Ac
  its <- 0;

  #catme("z=",z);
  #catme("g=",g);
  #catme("A1=",A1);
  #catme("v1head=",v1head);
  #catme("d2=",d2);
  #catme("d3=",d3);
  #catme("h12=",h12);
  #catme("h23=",h23);
  #catme("Q.old=",Q.old);

  #Qdcrel <- getDischargeCriticalDepthRelation(culvert=culvert);
  #dc.at.minQ <- Qdcrel$dc.at.minQ;
  #minQ.at.dc <- Qdcrel$minQ.at.dc;
  #catme("Qc.dc.rel has minQ:", minQ.at.dc, "dc at minQ", dc.at.minQ);

  So.gt.Sc <- NULL;

  while(1) {
     its <- its + 1;
     #catme("at loop origin Q.old",Q.old);
     #catme("h1:",h1);
     #catme("v1head:",v1head);
     #catme("dc:",dc);
     #catme("h12:",h12);
     #catme("h23:",h23);
     H <- h1 + v1head - dc - h12 - h23;
     #catme("Htype2:",H);

     if(H < 0) {
       usev1  <- 0;
       useh12 <- 0;
       had.to.ignore.v1head.and.h12 <- TRUE;
       H <- h1 - dc - h23;
     }
     if(H < 0) {
        Q <- list(Q=NA, error=NA, its=its,
                   C=NA,
                   Ac=NA,
                   Kc=NA,
                   A2=NA,
                   K2=NA,
                   h1=h1,
                   z=z,
                   v1head=v1head,
                   H=H,
                   d2=NA,
                   d3=NA,
                   h12=NA,
                   h23=NA,
                   L=L,
                   Lw=Lw,
                   g=g,
                   terminalarea=NA,
                   konvey2=NA,
                   h4.over.hc=NA,
                   So.lt.Sc=NA,
                   isType2=FALSE,
                   valid=FALSE,
                   roadflow=roadflow,
                   message="H < 0, trapping sqrt(x<0)",
                   had.to.ignore.v1head.and.h12=had.to.ignore.v1head.and.h12);
         return(Q);
     }
     tmp <- sqrt(g2*H);

     C <- getCforType2(culvert=culvert, approach=approach,
                       hzD=hzD, A1=A1, Ac=Ac);
     C <- round(C, digits=digits);

     Q <- C*Ac*tmp;
     #cat(c("Qloop=",Q,"\n"));
     dc  <- getCriticalDepth(Q, culvert=culvert,
                             location="outlet",
                             depression=culvert$outlet.depression);
     if(is.na(dc)) {
        Q <- list(Q=NA, error=NA, its=its,
                  C=C,
                  Ac=NA,
                  Kc=NA,
                  A2=NA,
                  K2=NA,
                  h1=h1,
                  z=z,
                  v1head=v1head,
                  H=NA,
                  d2=NA,
                  d3=NA,
                  h12=NA,
                  h23=NA,
                  L=L,
                  Lw=Lw,
                  g=g,
                  terminalarea=NA,
                  konvey2=NA,
                  h4.over.hc=NA,
                  So.lt.Sc=NA,
                  isType2=FALSE,
                  valid=FALSE,
                  roadflow=roadflow,
                  message="could not compute dc, assume not type 2 flow",
                  had.to.ignore.v1head.and.h12=had.to.ignore.v1head.and.h12);
        return(Q);
     }

     geo <- setCulvertGeometry(depth=dc, culvert=culvert,
                               location="outlet",
                               depression=culvert$outlet.depression);
     Ac  <- geo$A;
     v3  <- Q/Ac;
     Kc  <- geo$KONVEY;

     d2 <- getSubcriticalInletDepth(Q.old, C=C, h1=h1, v1head=v1head,
                                    h12=h12, v3=v3, culvert=culvert, dc=dc);
     if(is.nan(d2)) {
        Q <- list(Q=0, error=0, its=its,
                  C=C,
                  Ac=NA,
                  Kc=NA,
                  A2=NA,
                  K2=NA,
                  h1=h1,
                  z=z,
                  v1head=0,
                  H=NA,
                  d2=NA,
                  d3=NA,
                  h12=NA,
                  h23=NA,
                  L=L,
                  Lw=Lw,
                  g=g,
                  terminalarea=NA,
                  konvey2=NA,
                  h4.over.hc=NA,
                  So.lt.Sc=NA,
                  isType2=FALSE,
                  valid=FALSE,
                  roadflow=roadflow,
                  message="d2 is NaN, assume not Type 2",
                  had.to.ignore.v1head.and.h12=FALSE);
        return(Q);
     }
     #catme("Type 2 interation d2",d2);
     #catme("Type 2 interation D",D);
     #catme("Type 2 interation inlet.depression",inlet.depression);
     if(d2 > (D - inlet.depression)) d2 <- 0.95*(D - inlet.depression);
     if(d2 <= 0) d2 <- dc;
     #cat(c("d2=",d2,"\n"));

     geo <- setCulvertGeometry(depth=d2, culvert=culvert,
                               location="inlet",
                               depression=culvert$inlet.depression);
     A2  <- geo$A;
     K2  <- geo$KONVEY;

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


     #d2 <- h1 - z + v1head - h12 - v3^2/(g2*C*C); # TWRI p.25 V2=V3;
     #cat(c("d2=",d2,"\n"));

     lengths <- Lengths4MiteredPipe(h1=h1, flowtype=2,  d2=d2, d3=dc,
                                 culvert=culvert, approach=approach);
     L  <- lengths$L;
     Lw <- lengths$Lw;

     h12 <- useh12*HeadLoss12(Q.old*NB, culvert=culvert,
                              approach=approach, d2=d2, otherL=Lw);

     h23 <- HeadLoss23(Q.old, culvert=culvert, d2=d2, d3=dc, otherL=L);

     #print(h1)
     #print(v1head)
     #print(dc)
     #print(h12)
     #print(h23)

     Sc <- (Q/Kc)^2;
     ifelse(So < Sc,              So.lt.Sc <- TRUE,   So.lt.Sc <- FALSE);
     ifelse(h4/dc < 1.0,        h4.over.hc <- TRUE, h4.over.hc <- FALSE);
     ifelse(h4.over.hc & So.lt.Sc, isType2 <- TRUE,    isType2 <- FALSE);

     if(abs(Q-Q.old)/Q.old < eps | its > maxits) break;
     Q.old <- Q;
  }

  ifelse(isType2, valid <- TRUE, valid <- FALSE);

  if(is.na(had.to.ignore.v1head.and.h12)) {
     had.to.ignore.v1head.and.h12 <- FALSE;
  } else {
     h12    <- "had to be ignored.";
     v1head <- "had to be ignored.";
  }

  Q <- list(Q=round(Q*NB, digits=4),
            error=(Q-Q.old)*NB,
            its=its,
            C=C,
            Ac=Ac,
            Kc=Kc,
            A2=A2,
            K2=K2,
            h1=h1,
            z=z,
            v1head=v1head,
            H=H,
            d2=d2,
            d3=dc,
            h12=h12,
            h23=h23,
            L=L,
            Lw=Lw,
            g=g,
            terminalarea=Ac,
            konvey2=K2,
            h4.over.hc=h4.over.hc,
            So.lt.Sc=So.lt.Sc,
            isType2=isType2,
            valid=valid,
            roadflow=roadflow,
            message="",
            had.to.ignore.v1head.and.h12=had.to.ignore.v1head.and.h12);
  return(Q);
}
# TWRI p.4




"printType2" <-
function(flow=NULL, culvert=culvert, approach=approach,
         splash=TRUE, checkvalid=TRUE) {
  if(checkvalid & ! flow$valid) return(NA);

  lunits <- culvert$lengthunits;
  qunits <- culvert$flowunits;
  aunits <- culvert$areaunits;


  h12 <- flow$h12;
  if(is.numeric(h12)) {
     h12 <- paste(c(round(h12, digits=3), " ", lunits), sep="", collapse="");
  }
  v1head <- flow$v1head;
  if(is.numeric(v1head)) {
     v1head <- paste(c(round(v1head, digits=3), " ", lunits),
                     sep="", collapse="");
  }

  catme("  Depth at inlet", round(flow$d2, digits=3),lunits);
  catme("    Area at inlet", round(flow$A2, digits=2),aunits);
  catme("    Conveyance at inlet", round(flow$K2, digits=0),qunits);
  catme("  Depth (critical) at outlet", round(flow$d3, digits=3),lunits);
  catme("    Area (critical) at outlet", round(flow$Ac, digits=2),aunits);
  catme("    Conveyance (critical) at outlet", round(flow$Kc, digits=0),qunits);
  catme("  Equivalent length of culvert", round(flow$L, digits=2),lunits);
  catme("  Equivalent length of approach", round(flow$Lw, digits=2),lunits);
  catme("  Headloss between approach and inlet", h12);
  catme("  Approach velocity head", v1head);
  catme("  Headloss between inlet and outlet", round(flow$h23,
                                                     digits=3),lunits);

  pdffile <- paste(c("TMPcapR_culvert",
                     culvert$TMP.key, "_inletoutlet.pdf"), collapse="");
  pdf(pdffile);
  graphCulvert(discharge=flow$Q, depth=flow$d2,
               culvert=culvert, location="inlet",
               depression=culvert$inlet.depression);
  graphCulvert(discharge=flow$Q, depth=flow$d3,
               culvert=culvert, location="outlet",
               depression=culvert$outlet.depression);
  dev.off();
}


