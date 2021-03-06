"computeFlowSystem.singlepass" <-
function(culverts=NULL, approach=NULL, h1=NULL, h4=NULL,
         reset=TRUE, show.pass.results=TRUE,
         digits=4, ...) {

  if(! is.h(culverts)) {
     stop("Need hash of culverts to process");
  }

  keys <- keys.h(culverts);
  n.culverts <- length(keys);
  if(n.culverts < 1) {
     stop("*** Processing no culverts ***");
     return(NA);
  }

  if(reset) {
    resetTMPvarsApproach(approach);
    for(culvert.key in keys) {
       the.culvert <- get.h(culvert.key, culverts);
       the.culvert$TMP.key <- culvert.key;
       resetTMPvarsCulvert(the.culvert);
    }
  }

  Qs <- As <- Ks <- vector(mode="numeric", length=n.culverts);
  counter <- 0;

  if(show.pass.results) {
     cat("           **** Interating culvert key: ");
  }
  for(culvert.key in keys) {
     counter <- counter + 1;
     if(show.pass.results) {
        cat(culvert.key); cat(", ");
     }
     the.culvert <- get.h(culvert.key, culverts);
     flow <- computeFlow(h1=h1, h4=h4, culvert=the.culvert, reset=FALSE,
                         approach=approach, ...);
  }
  if(show.pass.results) {
       cat("**** \n");
  }

  i <- 0;
  for(culvert.key in keys) {
     i <- i + 1;
     the.culvert <- get.h(culvert.key, culverts);
     Qs[i] <- the.culvert$TMP.Q.previous.run +
              the.culvert$TMP.Qroad.previous.run;
     As[i] <- the.culvert$TMP.terminal.area.previous.run;
     Ks[i] <- the.culvert$TMP.konvey2.previous.run;
  }

  approach$TMP.accumulated.flow <- approach$additional.flow +
                                   sum(Qs, na.rm=TRUE);
  approach$TMP.terminal.area.previous.run <- sum(As, na.rm=TRUE);
  approach$TMP.konvey2.previous.run <- sum(Ks, na.rm=TRUE);

  a.culvert <- get.h(keys[1], culverts);
  lunits <- a.culvert$lengthunits;
  qunits <- a.culvert$flowunits;
  aunits <- a.culvert$areaunits;

  Q   <- approach$TMP.accumulated.flow;
  geo <- approach$geometry;
  A   <- geo$AREATOTAL;
  TW  <- geo$DBAR;
  g   <- a.culvert$gravity;
  F   <- Froude(discharge=Q, area=A, topwidth=TW, alpha=geo$alpha, g=g);
  F   <- round(F, digits=digits);
  ratio <- A/approach$TMP.terminal.area.previous.run;
  ratio <- round(ratio, digits=digits);
  if(show.pass.results) {
    catme("Discharge =",round(Q, digits=digits),
          "with Froude in approach of",F);
    catme("Ratio of areas (approach/'terminal area of culvert flow')",
          ratio);
  }

  z <- list(Qtotal=Q,
            qunits=qunits,
            froude.in.approach=F,
            contraction.ratio=ratio);
  return(z);
}


"computeFlowSystem.deforceEqualApproachHead" <-
function(...) {
   computeFlowSystem(forceEqualApproachHead=FALSE, ...)
}
"computeFlowSystem.forceIgnoreApproachHead" <-
function(...) {
   computeFlowSystem(forceIgnoreApproachHead=TRUE, ...)
}



"computeFlowSystem" <-
function(maxits=10, eps=1e-6, digits=4, show.pass.results=FALSE,
         forceEqualApproachHead=NULL, forceIgnoreApproachHead=NULL, ...) {

  flow <- computeFlowSystem.singlepass(reset=TRUE,
                                       show.pass.results=show.pass.results,
                                       forceEqualApproachHead=forceEqualApproachHead,
                                       forceIgnoreApproachHead=forceIgnoreApproachHead,
                                       ...);
  Q.old <- flow$Qtotal;
  its <- 0;
  error <- NA;
  if(Q.old == 0) {
    return(list(Qtotal=Q.old,
                error=error,
                iterations=its,
                flow.units=flow$qunits));
  }
  while(1) {
    its <- its + 1;
    flow <- computeFlowSystem.singlepass(reset=FALSE,
                                         show.pass.results=show.pass.results,
                                         forceEqualApproachHead=forceEqualApproachHead,
                                         forceIgnoreApproachHead=forceIgnoreApproachHead,
                                         ...);
    Q <- flow$Qtotal;
    error <- abs(Q-Q.old);
    if(abs(error/Q.old) < eps | its > maxits) break;
    Q.old <- Q;
  }
  #catme("TOTAL DISCHARGE: ", round(Q.old, digits=digits), flow$qunits);
  return(list(Qtotal=Q.old,
              error=error,
              its=its,
              flow.units=flow$qunits));
}




