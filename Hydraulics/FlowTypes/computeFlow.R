"computeFlow.deforceEqualApproachHead" <-
function(...) {
   computeFlow(forceEqualApproachHead=FALSE, ...)
}
"computeFlow.forceIgnoreApproachHead" <-
function(...) {
   computeFlow(forceIgnoreApproachHead=TRUE, ...)
}


"computeFlow" <-
function(culvert=NULL, approach=NULL, h1=NULL, h4=NULL,
         justQ=FALSE, verbose=FALSE, silent=FALSE,
         plotapproach=TRUE, reset=TRUE, ignore.early.out=FALSE,
         datetime="not provided",
         forceEqualApproachHead=NULL,
         forceIgnoreApproachHead=NULL, ...) {

  if(length(h1) > 1 || length(h4) > 1 || length(datetime) > 1) {
     warning("multiple h1 or h4 or datetime provided, using only the first values");
     h1 <- h1[1];
     h4 <- h4[1];
     datetime <- datetime[1];
  }

  if(silent) verbose <- FALSE;
  if(! silent & ! justQ) capRheader(approach=approach);

  options(device.ask.default=FALSE);

  early.out <- FALSE;
  if(! is.null(culvert$min.recordable.hw) &&
         h1 <= culvert$min.recordable.hw) {
     if(verbose) catme("\n*** h1 is at or below minimum recordable",
                        "headwater elevation, returning NA ***");
     early.out <- TRUE;
  }
  if(h1 == h4) {
     if(verbose) catme("\n*** h4 is equal to h1, returning NA ***");
     early.out <- TRUE;
  }
  if(h1 < h4) {
     if(verbose) catme("\n*** h4 is larger than h1, returning NA ***");
     early.out <- TRUE;
  }
  if(culvert$z >= h1 - culvert$zdsinvert) {
     if(verbose) catme("\n*** z is larger than h1, returning NA ***");
     early.out <- TRUE;
  }
  if(! ignore.early.out && early.out) {
     set.h("TMP.Q.previous.run", 0,             culvert);
     set.h("TMP.Qroad.previous.run", 0,         culvert);
     set.h("TMP.terminal.area.previous.run", 0, culvert);
     set.h("TMP.konvey2.previous.run", 0,       culvert);
     return(NA);
  }


  # It is critical that these original settings be preserved
  # in the case the the h1 <= approach$min.elevation conditions triggers
  # because we will need to modify the culvert and approach object on the
  # fly. Note that we must not modify the recorded approach$min.elevation.
  original.ignore.hf12   <- culvert$ignore.approach.losses;
  original.ignore.v1head <- culvert$ignore.approach.velocity.head;
  original.XY <- approach$xsec;
  original.forceEqualApproachHead <- approach$forceEqualApproachHead;
  original.forceIgnoreApproachHead <- approach$forceIgnoreApproachHead;
  if(! is.null(forceEqualApproachHead)) {
     approach$forceEqualApproachHead <- forceEqualApproachHead;
  }
  if(! is.null(forceIgnoreApproachHead)) {
     approach$forceIgnoreApproachHead <- forceIgnoreApproachHead;
  }

  if(h1 <= approach$min.elevation) {
     if(verbose) catme("\n*** h1 is at or below minimum elevation of",
                        "approach. ***");
     warning("h1 is at or below minimum elevation of approach. ",
             "The culvert is now being configured to ignore.approach.losses ",
             "and ignore.approach.velocity.head. In order to continue ",
             "processing, the XY of the approach is being modified so that ",
             "the minimum(Y) is set equal to 'just below' h1 or 'h1 - 0.001'. ",
             "This has the effect of keeping alive the approach geometry ",
             "subroutines, but the properties of the approach are ignored. ",
             "Expect to see extremely high to implausible Froude numbers in ",
             "output, ignore the output. Effectively 'ponded' conditions at ",
             "the inlet are assumed.");

     # These two truths have the effect of complete disregard for the approach
     # but geometry modification of the approach is needed to have approach
     # geometry functions continue to populate various fields
     culvert$ignore.approach.losses <- TRUE;
     culvert$ignore.approach.velocity.head <- TRUE;

     X <- approach$xsec$X; # extract the horizontal positions
     Y <- approach$xsec$Y; # extract the elevations
     ix <- 1:length(X); # vector of indices
     ix.min <- ix[Y == min(Y)]; # which indices are at the minimum
     Y[ix.min] <- h1 - 0.001; # reset those mins to just below h1
     approach$xsec <- data.frame(X=X, Y=Y); # repopulate
  }

  if(culvert$manningcor != approach$manningcor) {
     stop("Manning correction (1.486 | 1) mismatch in ",
          "approach and culvert objects");
  }

  if(reset) {
    resetTMPvarsApproach(approach);
    resetTMPvarsCulvert(culvert);
  }

  setApproachConditions(depth=h1, approach=approach,
                        plotem=plotapproach, ...);

  types <- TypeClassification(h1=h1, h4=h4, culvert=culvert, verbose=verbose);
  if(verbose) {
    catme("  Potential USGS flow types from TypeClassification():", types);
  }

  Qs <- Qroads <- TYPEs <- As <- Ks <- vector(mode="numeric");
  flows.h <- new.h();
  valid.Q.count <- 0;

  for(type in types) {
     if(verbose) catme("processing flow type", type);

     flow <- switch(type,
          culvert2QType1(h1=h1, h4=h4,
                         culvert=culvert, approach=approach),
          culvert2QType2(h1=h1, h4=h4,
                         culvert=culvert, approach=approach),
          culvert2QType3(h1=h1, h4=h4,
                         culvert=culvert, approach=approach),
          culvert2QType4(h1=h1, h4=h4,
                         culvert=culvert, approach=approach),
          culvert2QType5(h1=h1, h4=h4,
                         culvert=culvert, approach=approach),
          culvert2QType6(h1=h1, h4=h4,
                         culvert=culvert, approach=approach));

     culvert$Ltop <- culvert$TMP.Ltop;
     culvert$Lbot <- culvert$TMP.Lbot;
     culvert$zdsinvert <- culvert$TMP.zdsinvert;

     the.type <- type;
     if(type <= 3) {
        if(! is.na(flow$had.to.ignore.v1head.and.h12) &
                   flow$had.to.ignore.v1head.and.h12 == TRUE) {
           the.type <- -the.type;
        }
     }

     set.h(paste(c("type",the.type), sep="", collapse=""), flow, flows.h);

     if(verbose) print(flow);

     if(flow$valid) {
        valid.Q.count  <- valid.Q.count + 1;
        Qculvert <- flow$Q; # extract the culvert flow
        Qroad <- 0;
        if(length(flow$roadflow$is.flow) && flow$roadflow$is.flow) {
           Qroad <- flow$roadflow$Qroad;
        }
        Qs[valid.Q.count]     <- Qculvert;
        Qroads[valid.Q.count] <- Qroad;
        TYPEs[valid.Q.count]  <- the.type;
        As[valid.Q.count]     <- flow$terminalarea;
        Ks[valid.Q.count]     <- flow$konvey2;
        if(! silent) {
           if(justQ) {
              catme(h1, h4, Qculvert, type, Qroad, Qculvert+Qroad, type,
                    sep="  ");
           } else {
              printFlow(type, h1=h1, h4=h4, flow=flow, datetime=datetime,
                        culvert=culvert, approach=approach);
           }
        }
     }
  }

  if(valid.Q.count == 0) { # emergency 2-section slope area
     type <- "SAC";
     flow <- culvert2QTypeSAC(h1=h1, h4=h4, culvert=culvert, approach=approach);
     Qculvert <- flow$Q;
     Qroad <- 0;
     if(length(flow$roadflow$is.flow) && flow$roadflow$is.flow) {
       Qroad <- flow$roadflow$Qroad;
       Qculvert <- 0;
     }
     Qs[1]     <- Qculvert;
     Qroads[1] <- Qroad;
     TYPEs[1]  <- type;
     As[1]     <- flow$terminalarea;
     Ks[1]     <- flow$konvey2;
     if(! silent) {
        if(justQ) {
           catme(h1, h4, Qculvert, type, Qroad, Qculvert+Qroad, type,
                 sep="  ");
        } else {
           printFlow("SAC", h1=h1, h4=h4, flow=flow,
                     culvert=culvert, approach=approach);
        }
     }
  }

  if(verbose & valid.Q.count > 1) {
     catme("\n*** Two or more Q are valid",
           "returning mean of those found. ***\n");
  }
  if(length(Qs) == 0) {
     Qs <- Qroads <- As <- Ks <- c(NA);
     # used to trap min(), max() error messages
  }
  Qbar     <- mean(Qs,     na.rm=TRUE);
  Qroadbar <- mean(Qroads, na.rm=TRUE);
  Abar     <- mean(As,     na.rm=TRUE);
  Kbar     <- mean(Ks,     na.rm=TRUE);

  if(is.nan(Qbar))         Qbar <- 0;
  if(is.nan(Qroadbar)) Qroadbar <- 0;
  if(is.nan(Abar))         Abar <- 0;
  if(is.nan(Kbar))         Kbar <- 0;
  set.h("TMP.Q.previous.run",             Qbar,     culvert);
  set.h("TMP.Qroad.previous.run",         Qroadbar, culvert);
  set.h("TMP.terminal.area.previous.run", Abar,     culvert);
  set.h("TMP.konvey2.previous.run",       Kbar,     culvert);

  # Restore original values (rarely would these actually change)
  culvert$ignore.approach.losses <- original.ignore.hf12;
  culvert$ignore.approach.velocity.head <- original.ignore.v1head;
  approach$xsec <- original.XY;
  approach$forceEqualApproachHead <- original.forceEqualApproachHead;
  approach$forceIgnoreApproachHead <- original.forceIgnoreApproachHead;

  return(list(h1=h1, h4=h4,
              Qtotal=Qbar+Qroadbar,
              Qculvert=Qbar,
              Qroad=Qroadbar,
              Qmin=min(Qs),
              Qmax=max(Qs),
              Qsd=sd(Qs),
              Qs=Qs,
              A=Abar,
              K=Kbar,
              Types=TYPEs,
              type=TYPEs[1],
              flow.hash=flows.h));
}

