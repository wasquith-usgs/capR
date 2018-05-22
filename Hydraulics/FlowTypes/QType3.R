"QType3" <-
function(C=NULL, A3=NULL, h1=NULL, h3=NULL,
         a1=NULL, v1=NULL, h12=NULL, h23=NULL, g=32.2) {
  vh <- a1*v1^2/(2*g);
  return(C*A3*sqrt(h1 + vh - h3 - h12 - h23));
}
# TWRI p.5


"culvert2QType3" <-
function(h1=NULL, h4=NULL, culvert=NULL, approach=NULL,
         digits=4, maxits=50, eps=1e-6,...) {

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
  g2 <- 2*g;
  hzD <- HeadwtrDiaRatio(h1=h1, culvert=culvert);
  NB <- culvert$number.of.barrels;

  ifelse(culvert$ignore.approach.velocity.head, usev1  <- 0, usev1  <- 1);
  ifelse(culvert$ignore.approach.losses,        useh12 <- 0, useh12 <- 1);

  had.to.ignore.v1head.and.h12 <- NA;

  d3 <- ifelse(h4 == 0, 0.01*D, h4); # trap a zero tail water depth, which will yield A3=0
  geo3 <- setCulvertGeometry(depth=d3, culvert=culvert,
                             location="outlet",
                             depression=culvert$outlet.depression);
  A3   <- geo3$A;
  K3   <- geo3$KONVEY;
  TW3  <- geo3$TW;
  if(h4 > h1) {
     Q <- list(Q=0, error=0, its=0,
               C=C,
               A2=NA,
               K2=NA,
               A3=NA,
               K3=NA,
               h1=h1,
               z=z,
               v1head=0,
               H=NA,
                  d2=0,
                  d3=0,
                  h12=0,
                  h23=0,
                  L=NA,
                  Lw=NA,
                  g=g,
                  terminalarea=0,
                  konvey2=0,
                  h4.over.D=NA,
                  h4.over.hc=NA,
                  So.lt.Sc=NA,
                  isType3=FALSE,
                  valid=FALSE,
                  roadflow=culvert$road,
                  message="h4 > h1",
                  had.to.ignore.v1head.and.h12=FALSE);
        return(Q);
  }
  Q.old <- 0.95*A3*sqrt(g2*(h1-h4));
  v3 <- Q.old/A3;
  if(A3 == 0) v3 <- 0;
  #catme("A3=",A3);
  #catme("d3=",d3);
  #catme("v3=",v3);
  #catme("h1=",h1);
  #catme("h4=",h4);
  #catme("D=",D);
  d2 <- h1 - z - v3^2/g2; # TWRI p.25, simplification
  #catme("Type 3 spinup d2",d2);
  #catme("Type 3 spinup D",D);

  inlet.depression <- culvert$inlet.depression;
  #catme("Type 3 spinup inlet.depression",inlet.depression);

  if(d2 > (D - inlet.depression)) d2 <- 0.95*(D - inlet.depression);
  if(d2 <= 0) d2 <- d3; # trapping a bad spin up, one cause is not enough
  # head to push flow through the pipe given the "fair approximation" of Q.old

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

  lengths <- Lengths4MiteredPipe(h1=h1, flowtype=3,  d2=d2, d3=d3,
                                 culvert=culvert, approach=approach);
  L  <- lengths$L;
  Lw <- lengths$Lw;

  h12 <- useh12*HeadLoss12(Q.old*NB, culvert=culvert,
                           approach=approach, d2=d2, otherL=Lw);
  h23 <- HeadLoss23(Q.old, culvert=culvert, d2=d2, d3=d3, otherL=L);


  Q <- NULL;
  its <- 0;

  #catme("h1=",h1);
  #catme("h4=",h4);
  #catme("z=",z);
  #catme("g=",g);
  #catme("A1=",A1);
  #catme("v1head=",v1head);
  #catme("d2=",d2);
  #catme("d3=",d3);
  #catme("h12=",h12);
  #catme("h23=",h23);
  #catme("Q.old=",Q.old);

  So.gt.Sc <- NULL;

  while(1) {
     its <- its + 1;

     H <- h1 + v1head - d3 - h12 - h23;
     if(H < 0) {
       usev1  <- 0;
       useh12 <- 0;
       had.to.ignore.v1head.and.h12 <- TRUE;
       H <- h1 - d3 - h23;
     }
     if(H < 0) {
         Q <- list(Q=NA, error=NA, its=its,
                  C=NA,
                  A2=NA,
                  K2=NA,
                  A3=A3,
                  K3=K3,
                  h1=h1,
                  z=z,
                  v1head=v1head,
                  H=H,
                  d2=NA,
                  d3=d3,
                  h12=NA,
                  h23=NA,
                  L=L,
                  Lw=Lw,
                  g=g,
                  terminalarea=NA,
                  konvey2=NA,
                  h4.over.D=NA,
                  h4.over.hc=NA,
                  So.lt.Sc=NA,
                  isType3=FALSE,
                  valid=FALSE,
                  roadflow=roadflow,
                  message="H < 0, trapping sqrt(x<0)",
                  had.to.ignore.v1head.and.h12=had.to.ignore.v1head.and.h12);
        return(Q);
     }
     tmp <- sqrt(g2*H);

     Fr <- Froude(discharge=Q.old, area=A3, topwidth=TW3, g=g);
     C <- getCforType3(culvert=culvert, approach=approach,
                       froude=Fr, hzD=hzD, A1=A1, Ac=A3);
     C <- round(C, digits=digits);

     Q <- C*A3*tmp;
     v3 <- Q/A3;

     d2 <- getSubcriticalInletDepth(Q.old, C=C, h1=h1, v1head=v1head,
                                    h12=h12, v3=v3, culvert=culvert, dc=dc);
     #cat(c("d2=",d2,"\n"));
     if(is.nan(d2)) d2 <- d3;
     #catme("Type 3 interation d2",d2,d3);
     if(d2 > D) d2 <- 0.95*D;
     if(d2 <= 0) d2 <- d3;

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

     lengths <- Lengths4MiteredPipe(h1=h1, flowtype=3,  d2=d2, d3=d3,
                                    culvert=culvert, approach=approach);
     L  <- lengths$L;
     Lw <- lengths$Lw;

     h12 <- useh12*HeadLoss12(Q.old*NB, culvert=culvert,
                       approach=approach, d2=d2, otherL=Lw);

     h23 <- HeadLoss23(Q.old, culvert=culvert, d2=d2, d3=d3, otherL=L);

     #print(Q.old);
     #print(Q);
     #print(h1)
     #print(v1head)
     #print(h12)
     #print(h23)

     hc  <- getCriticalDepth(Q.old, culvert=culvert,
                             location="outlet",
                             depression=culvert$outlet.depression);
     if(is.na(hc)) {
        Q <- list(Q=NA, error=NA, its=its,
                  C=C,
                  A2=NA,
                  K2=NA,
                  A3=A3,
                  K3=K3,
                  h1=h1,
                  z=z,
                  v1head=v1head,
                  H=H,
                  d2=NA,
                  d3=d3,
                  h12=NA,
                  h23=NA,
                  L=L,
                  Lw=Lw,
                  g=g,
                  terminalarea=NA,
                  konvey2=NA,
                  h4.over.D=NA,
                  h4.over.hc=NA,
                  So.lt.Sc=NA,
                  isType3=FALSE,
                  valid=FALSE,
                  roadflow=roadflow,
                  message="could not compute hc, assume not type 3 flow",
                  had.to.ignore.v1head.and.h12=had.to.ignore.v1head.and.h12);
        return(Q);
     }
     geo <- setCulvertGeometry(depth=hc, culvert=culvert,
                               location="outlet",
                               depression=culvert$outlet.depression);
     Ac  <- geo$A;
     v3  <- Q/Ac;
     Kc  <- geo$KONVEY;

     Sc <- (Q/Kc)^2;
     ifelse(So < Sc,               So.lt.Sc <- TRUE,   So.lt.Sc <- FALSE);
     ifelse(h4/D <= 1.0,          h4.over.D <- TRUE,  h4.over.D <- FALSE);
     ifelse(h4/hc > 1.0,         h4.over.hc <- TRUE, h4.over.hc <- FALSE);
     ifelse(h4.over.D & h4.over.hc, isType3 <- TRUE,    isType3 <- FALSE);

     if(abs(Q-Q.old)/Q.old < eps | its > maxits) break;
     Q.old <- Q;
  }

  ifelse(isType3, valid <- TRUE, valid <- FALSE);

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
            A2=A2,
            K2=K2,
            A3=A3,
            K3=K3,
            h1=h1,
            z=z,
            v1head=v1head,
            H=H,
            d2=d2,
            d3=d3,
            h12=h12,
            h23=h23,
            L=L,
            Lw=Lw,
            g=g,
            terminalarea=A3,
            konvey2=K2,
            h4.over.D=h4.over.D,
            h4.over.hc=h4.over.hc,
            So.lt.Sc=So.lt.Sc,
            isType3=isType3,
            valid=valid,
            roadflow=roadflow,
            message="",
            had.to.ignore.v1head.and.h12=had.to.ignore.v1head.and.h12);
  return(Q);
}
# TWRI p.4





"printType3" <-
function(flow=NULL, culvert=culvert, approach=approach,
         splash=TRUE, checkvalid=TRUE) {
  if(checkvalid & ! flow$valid) return(NA);

  lunits <- culvert$lengthunits;
  qunits <- culvert$flowunits;
  aunits <- culvert$areaunits;

  h12 <- flow$h12;
  if(is.numeric(h12)) {
     h12 <- paste(c(round(h12, digits=3), " ", lunits),
                  sep="", collapse="");
  }
  v1head <- flow$v1head;
  if(is.numeric(v1head)) {
     v1head <- paste(c(round(v1head, digits=3), " ", lunits),
                     sep="", collapse="");
  }

  g  <- culvert$gravity;
  g2 <- 2*g;

  catme("  Depth at inlet", round(flow$d2, digits=3),lunits);
  catme("    Area at inlet", round(flow$A2, digits=2),aunits);
  catme("    Conveyance at inlet", round(flow$K2, digits=0),qunits);
  catme("    Velocity head at inlet",
                  round((flow$Q/flow$A2)^2/g2, digits=3), lunits);
  catme("  Depth at outlet", round(flow$d3, digits=3),lunits);
  catme("    Area at outlet", round(flow$A3, digits=2),aunits);
  catme("    Conveyance at outlet", round(flow$K3, digits=0),qunits);
  catme("    Velocity head at outlet",
                  round((flow$Q/flow$A3)^2/g2, digits=3),lunits);
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


