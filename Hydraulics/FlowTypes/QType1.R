"QType1" <-
function(C=NULL, Ac=NULL, h1=NULL, z=NULL,
         a1=NULL, v1=NULL, dc=NULL, h12=NULL, g=NULL) {
  vh <- a1*v1^2/(2*g);
  return(C*Ac*sqrt(h1 - z + vh - dc - h12));
}

"culvert2QType1" <-
function(h1=NULL, h4=NULL, culvert=NULL, approach=NULL,
         digits=4, maxits=50, eps=1e-6,...) {

  h1.elev <- h1; h4.elev <- h4;

  h1 <- h1 - culvert$zdsinvert;
  h4 <- h4 - culvert$zdsinvert;
  if(h4 < 0) h4 <- 0;

  So     <- culvert$So;
  z      <- culvert$z;
  g      <- culvert$gravity;
  A1     <- approach$geometry$AREATOTAL/approach$fraction.of.flow;
  A1frac <- culvert$fraction.of.approach.area;
  alpha1 <- approach$geometry$alpha;
  g2 <- 2*g;
  hzD <- HeadwtrDiaRatio(h1=h1, culvert=culvert);
  NB <- culvert$number.of.barrels;

  ifelse(culvert$ignore.approach.velocity.head, usev1  <- 0, usev1  <- 1);
  ifelse(culvert$ignore.approach.losses,        useh12 <- 0, useh12 <- 1);

  had.to.ignore.v1head.and.h12 <- NA;

  #geo.guess <- setCulvertGeometry(depth=0.66*(h1-z), culvert=culvert,
  #                                location="inlet",
  #                                depression=culvert$inlet.depression);
  #A.guess <- geo.guess$A; TW.guess <- geo.guess$TW;
  #Q.old <- sqrt(g*A.guess^3/TW.guess);

  Q.old <- getDischargeAtCriticalDepth(depth=0.66*(h1-z),
                                       culvert=culvert,
                                       location="inlet",
                                       depression=culvert$inlet.depression);

  road <- culvert$road; # properties of the road
  approachflow <- approach$TMP.accumulated.flow; # TOTAL COMPUTED FLOW
                                                 # FROM AN EARLIER RUN
  Q.previous.run     <- culvert$TMP.Q.previous.run;
  Qroad.previous.run <- culvert$TMP.Qroad.previous.run;

  # July 11, 2016: The following line seems bogus and causes too much velocity head
  # because approachflow will be summed back into the Qtotal within the iteration loop.
  if(approachflow == 0) {
     #approachflow <- Q.old*NB;
     #Q.previous.run <- approachflow;
  }
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

  Q <- NULL;
  its <- 0;

  #print(z);
  #print(g);
  #print(A1);
  #print(alpha1);
  #print(Q.old);

  #Qdcrel <- getDischargeCriticalDepthRelation(culvert=culvert);
  #dc.at.minQ <- Qdcrel$dc.at.minQ;
  #minQ.at.dc <- Qdcrel$minQ.at.dc;
  #catme("Qc.dc.rel has minQ:", minQ.at.dc, "dc at minQ", dc.at.minQ);

  So.gt.Sc <- NULL;

  while(1) {
     its <- its + 1;
     #catme("at loop origin Q.old",Q.old," at ",its);
     dc <- getCriticalDepth(Q.old, culvert=culvert, location="inlet",
                            depression=culvert$inlet.depression);

     if(is.na(dc)) {
        Q <- list(Q=NA, error=NA, its=its,
                  C=NA,
                  Ac=NA,
                  h1=h1,
                  z=z,
                  v1head=v1head,
                  H=NA,
                  d2=NA,
                  d3=NA,
                  h12=NA,
                  L=NA,
                  Lw=NA,
                  g=g,
                  terminalarea=NA,
                  konvey2=NA,
                  h4.over.hc=NA,
                  So.lt.Sc=NA,
                  isType1=FALSE,
                  valid=FALSE,
                  roadflow=roadflow,
                  message="could not compute dc, assume not type 1 flow",
                  had.to.ignore.v1head.and.h12=had.to.ignore.v1head.and.h12);
        return(Q);
     }

     geo <- setCulvertGeometry(depth=dc, culvert=culvert, location="inlet",
                               depression=culvert$inlet.depression );
     Ac  <- geo$A;
     TWc <- geo$TW;
     Kc  <- geo$KONVEY;

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


     lengths <- Lengths4MiteredPipe(h1=h1, flowtype=1,  d2=dc,
                                    culvert=culvert, approach=approach);
     L  <- lengths$L;
     Lw <- lengths$Lw;

     h12 <- useh12*HeadLoss12(Q.old*NB, culvert=culvert,
                              approach=approach, d2=dc, otherL=Lw);
     #cat("h12:", h12);
     H <- h1 - z + v1head - dc - h12;
     #catme("Htype1:",H);
     if(H < 0) {
       usev1  <- 0;
       useh12 <- 0;
       had.to.ignore.v1head.and.h12 <- TRUE;
       H <- h1 - z - dc;
     }
     if(H < 0) {
        Q <- list(Q=NA, error=NA, its=its,
                  C=NA,
                  Ac=NA,
                  Kc=NA,
                  h1=h1,
                  z=z,
                  v1head=v1head,
                  H=H,
                  d2=dc,
                  h12=h12,
                  L=L,
                  Lw=Lw,
                  g=g,
                  terminalarea=NA,
                  konvey2=NA,
                  h4.over.hc=NA,
                  So.gt.Sc=NA,
                  isType1=FALSE,
                  valid=FALSE,
                  roadflow=roadflow,
                  message="H < 0, trapping sqrt(x<0)",
                  had.to.ignore.v1head.and.h12=had.to.ignore.v1head.and.h12);
        return(Q);
     }
     tmp <- sqrt(g2*H);

     C <- getCforType1(culvert=culvert, approach=approach,
                       hzD=hzD, A1=A1, Ac=Ac);
     C <- round(C, digits=digits);

     Q <- C*Ac*tmp;

     Sc <- (Q/Kc)^2;
     ifelse(So > Sc,              So.gt.Sc <- TRUE,   So.gt.Sc <- FALSE);
     ifelse(h4/(dc+z) < 1.0,    h4.over.hc <- TRUE, h4.over.hc <- FALSE);
     ifelse(h4.over.hc & So.gt.Sc, isType1 <- TRUE,    isType1 <- FALSE);

     if(abs(Q-Q.old)/Q.old < eps | its > maxits) break;
     Q.old <- Q;
  }

  ifelse(isType1, valid <- TRUE, valid <- FALSE);

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
            h1=h1,
            z=z,
            v1head=v1head,
            H=H,
            d2=dc,
            h12=h12,
            L=L,
            Lw=Lw,
            g=g,
            terminalarea=Ac,
            konvey2=Kc,
            h4.over.hc=h4.over.hc,
            So.gt.Sc=So.gt.Sc,
            isType1=isType1,
            valid=valid,
            roadflow=roadflow,
            message="",
            had.to.ignore.v1head.and.h12=had.to.ignore.v1head.and.h12);
  return(Q);
}
# TWRI p.4



"printType1" <-
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

  catme("  Depth (critical) at inlet", round(flow$d2, digits=3),lunits);
  catme("    Area (critical) at inlet", round(flow$Ac, digits=2),aunits);
  catme("    Conveyance (critical) at inlet", round(flow$Kc, digits=0),qunits);
  catme("  Equivalent length of culvert", round(flow$L, digits=2),lunits);
  catme("  Equivalent length of approach", round(flow$Lw, digits=2),lunits);
  catme("  Headloss between approach and inlet", h12);
  catme("  Approach velocity head", v1head);

  pdffile <- paste(c("TMPcapR_culvert",
                     culvert$TMP.key, "_inletoutlet.pdf"), collapse="");
  pdf(pdffile);
  graphCulvert(discharge=flow$Q, depth=flow$d2,
               culvert=culvert, location="inlet",
               depression=culvert$inlet.depression);
  dev.off();
}



