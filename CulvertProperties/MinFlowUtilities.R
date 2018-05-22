"setMinRecordables" <-
function(hwpin=NULL, twpin=NULL, culvert=NULL) {
   if(is.null(hwpin))    stop("Headwater pin elevation (min recordable) is NULL");
   if(is.null(twpin))    stop("Tailwater pin elevation (min recordable) is NULL");
   if(is.null(culvert))  stop("Culvert is NULL");

   if(! is.null(hwpin)) set.h("min.recordable.hw", hwpin, culvert);
   if(! is.null(twpin)) set.h("min.recordable.tw", twpin, culvert);
}


"computeMinFlowPin" <-
function(culvert=NULL, approach=NULL, del=0.001, verbose=TRUE, digits=2) {
   if(is.null(culvert))   stop("Culvert is NULL");
   if(is.null(approach))  stop("Approach is NULL");

   us.pin <- get.h("min.recordable.hw", culvert) + del;
   ds.pin <- get.h("min.recordable.tw", culvert) + del;
   min.recordable.flow <- computeFlow(h1=us.pin, h4=ds.pin, verbose=verbose,
                                      culvert=culvert, approach=approach);
   if(is.na(min.recordable.flow$Qsd)) {
      catme("*** Minimum recordable flow ('pin discharge') is",
            round(min.recordable.flow$Qtotal, digits=digits),
            culvert$flowunits,
            "of type", min.recordable.flow$type,"\n");
   } else {
      catme("*** Multiple solutions to minimum recordable flow",
            " ('pin discharge')---printing flow object\n");
      print(min.recordable.flow);
   }
   return(min.recordable.flow);
}





"computeMinFlowPivot" <-
function(h1=NULL, h4=NULL, culvert=NULL, approach=NULL, del=0.01,
         silent=TRUE, verbose=FALSE, digits=2) {
   if(is.null(culvert))   stop("Culvert is NULL");
   if(is.null(approach))  stop("Approach is NULL");

   zusinvert <- get.h("zusinvert", culvert);
   zdsinvert <- get.h("zdsinvert", culvert);
   us.pin <- get.h("min.recordable.hw", culvert);
   ds.pin <- get.h("min.recordable.tw", culvert);

   if(is.null(h1) && is.null(h4)) stop("Both h1 and h4 are NULL");

   if(is.null(h1) && us.pin > ds.pin && h4 > ds.pin) {
      print("Computation A:")
      top <- max(us.pin, zusinvert);
      bot <- max(h4, zdsinvert);
      h1s <- seq(top, bot, by=sign(bot - top)*del);
      i <- 0
      maxQ <- maxT <- maxh1 <- vector(mode="numeric")
      #print(h1s)
      for(h1 in h1s) {
         if(h1 <= zusinvert) next
         if(h4 >= h1) next
         flow <- NA
         try( flow <- computeFlow(h1=h1, h4=h4, verbose=verbose, ignore.early.out=TRUE,
                             culvert=culvert, approach=approach, silent=silent));
         Q <- max(flow$Qtotal, na.rm=TRUE);
         T <- paste(flow$Types, sep=",", collapse=";")
         i <- i + 1;
         maxQ[i] <- Q;
         maxT[i] <- T;
         maxh1[i] <- h1
      }
      if(length(Q) == 0) {
         stop("Ambiguous elevation conditions for a nonNULL h4, threshold discharge not available");
      }
      thresholdQ <- max(maxQ, na.rm=TRUE);
      ix <- seq(1,length(maxQ));
      DF <- data.frame(h1=maxh1, h4=rep(h4, length(maxQ)), Q=maxQ, Types=maxT)
      print(DF)
      plot(DF$h1, DF$Q, type="l",
           xlab="Headwater height, in length units",
           ylab="Maximum discharge for each headwater height, in length per time")
      thresholdh1 <- maxh1[ix[maxQ == thresholdQ]];
      typeh1 <- maxT[ix[maxQ == thresholdQ]]
      catme("ComputationA: The lowest recordable discharge threshold is",
            round(thresholdQ, digits=digits),  "if (when) h1 is",
            round(thresholdh1, digits=digits), "of type", typeh1, "\n");
      return(DF);
   }


   if(is.null(h4) && us.pin > zusinvert && us.pin > zdsinvert && ds.pin > zdsinvert) {
      print("Computation B:")
      bot <- min(ds.pin, zdsinvert);
      top <- max(ds.pin, zdsinvert);
      h4s <- seq(top, bot, by=-del);
      i <- 0
      maxQ <- maxT <- maxh4 <- vector(mode="numeric")
      for(h4 in h4s) {
         if(h4 <= zdsinvert) next
         if(h4 >= h1) next
         flow <- NA
         try( flow <- computeFlow(h1=h1, h4=h4, verbose=verbose, ignore.early.out=TRUE,
                             culvert=culvert, approach=approach, silent=silent));
         Q <- max(flow$Qtotal, na.rm=TRUE);
         T <- paste(flow$Types, sep=",", collapse=";")
         i <- i + 1;
         maxQ[i] <- Q;
         maxT[i] <- T;
         maxh4[i] <- h4;
         #cat(h1, h4, Q, T, sep="  ")
      }
      if(length(Q) == 0) {
         stop("Ambiguous elevation conditions for a nonNULL h1, threshold discharge not available");
      }
      thresholdQ <- max(maxQ, na.rm=TRUE);
      ix <- seq(1,length(maxQ))
      thresholdh4 <- maxh4[ix[maxQ == thresholdQ]];
      typeh4 <- maxT[ix[maxQ == thresholdQ]]
      DF <- data.frame(h1=rep(h1, length(maxh4)), h4=maxh4, Q=maxQ, Types=maxT)
      print(DF)
      plot(DF$h4, DF$Q, type="l",
           xlab="Tailwater height, in length units",
           ylab="Maximum discharge for each headwater height, in length per time")
      catme("ComputationB: The lowest recordable discharge threshold is",
            round(thresholdQ, digits=digits),  "if (when) h4 is",
            round(thresholdh4, digits=digits), "of type", typeh4, "\n");
      return(DF);
   }

   if(is.null(h4) && ds.pin < us.pin && h1 > us.pin) {
      print("Computation C:")
      bot <- min(ds.pin, zdsinvert);
      top <- max(us.pin, h1);
      h4s <- seq(top, bot, by=-del);
      i <- 0
      maxQ <- maxT <- maxh4 <- vector(mode="numeric")
      for(h4 in h4s) {
         if(h4 <= zdsinvert) next
         if(h4 >= h1) next
         flow <- NA
         try( flow <- computeFlow(h1=h1, h4=h4, verbose=verbose, ignore.early.out=TRUE,
                             culvert=culvert, approach=approach, silent=silent));
         Q <- max(flow$Qtotal, na.rm=TRUE);
         T <- paste(flow$Types, sep=",", collapse=";")
         i <- i + 1;
         maxQ[i] <- Q;
         maxT[i] <- T;
         maxh4[i] <- h4
      }
      if(length(Q) == 0) {
         stop("Ambiguous elevation conditions for a nonNULL h1, threshold discharge not available");
      }
      thresholdQ <- max(maxQ, na.rm=TRUE);
      ix <- seq(1,length(maxQ))
      thresholdh4 <- maxh4[ix[maxQ == thresholdQ]];
      typeh4 <- maxT[ix[maxQ == thresholdQ]]
      DF <- data.frame(h1=rep(h1, length(maxQ)), h4=maxh4, Q=maxQ, Types=maxT)
      print(DF)
      plot(DF$h4, DF$Q, type="l",
           xlab="Tailwater height, in length units",
           ylab="Maximum discharge for each headwater height, in length per time")
      thresholdh4 <- thresholdh4[length(thresholdh4)];
      catme("ComputationC: The lowest recordable discharge threshold is",
            round(thresholdQ, digits=digits),  "if (when) h4 is",
            round(thresholdh4, digits=digits), "of type", typeh4, "\n");
      return(DF);
   }

   stop("Ambiguous elevation conditions seemingly do not facilitate computation of threshold discharge")
}


"setMinRecordablesSystem" <-
function(hwpin=NULL, twpin=NULL, culverts=NULL) {
  if(! is.h(culverts)) {
     stop("Need hash of culverts to process");
  }

  keys <- keys.h(culverts);
  n.culverts <- length(keys);
  if(n.culverts < 1) {
     stop("*** Processing no culverts ***");
  }
  for(culvert.key in keys) {
     #catme("extracting culvert",culvert.key,"\n");
     the.culvert <- get.h(culvert.key, culverts);
     setMinRecordables(hwpin=hwpin, twpin=twpin, culvert=the.culvert);
  }
}


"computeMinFlowSystemPin" <-
function(culverts=NULL, approach=NULL, del=0.001, verbose=TRUE, digits=2) {
   if(! is.h(culverts)) {
     stop("Need hash of culverts to process");
   }
   if(is.null(approach)) stop("Approach is NULL");

   keys <- keys.h(culverts);
   us.pins <- ds.pins <- vector(mode="numeric");
   for(culvert.key in keys) {
     #catme("extracting culvert",culvert.key,"\n");
     the.culvert <- get.h(culvert.key, culverts);
     us.pin <- get.h("min.recordable.hw", the.culvert) + del;
     ds.pin <- get.h("min.recordable.tw", the.culvert) + del;
     us.pins <- c(us.pins, us.pin);
     ds.pins <- c(ds.pins, ds.pin);
   }
   us.sd.pins <- sd(us.pins);
   ds.sd.pins <- sd(ds.pins);
   if(us.sd.pins > 0 || ds.sd.pins > 0) {
      stop("Variability in the min.recordable.hw or min.recordable.tw ",
           "for the culvert system is present. The function ",
           "setMinRecordablesSystem() should be used to set these elevations ",
           "for all culverts in the system.");
   }

   us.pin <- us.pins[1];
   ds.pin <- ds.pins[1];

   min.recordable.flow <- computeFlowSystem(culverts=culverts, approach=approach,
                                            h1=us.pin, h4=ds.pin, verbose=verbose, show.pass.results=FALSE);
   if(is.na(min.recordable.flow$Qtotal)) {
      warning("Could not compute a minimum recordable flow for the ",
              "culvert system\n");
      return(NA);
   } else {
      catme("*** Minimum recordable flow ('pin discharge') for the",
            "culvert system is",
            round(min.recordable.flow$Qtotal, digits=digits),
            min.recordable.flow$flowunits,"\n");
   }
   return(min.recordable.flow);
}


"computeMinFlowSystemPivot" <-
function(h1=NULL, h4=NULL, culverts=NULL, approach=NULL, del=0.01,
         silent=TRUE, verbose=FALSE, digits=2) {

   if(! is.h(culverts)) {
     stop("Need hash of culverts to process");
   }
   if(is.null(approach)) stop("Approach is NULL");

   keys <- keys.h(culverts);
   us.pins <- ds.pins <- vector(mode="numeric");
   for(culvert.key in keys) {
     #catme("extracting culvert",culvert.key,"\n");
     the.culvert <- get.h(culvert.key, culverts);
     us.pin <- get.h("min.recordable.hw", the.culvert)
     ds.pin <- get.h("min.recordable.tw", the.culvert)
     us.pins <- c(us.pins, us.pin);
     ds.pins <- c(ds.pins, ds.pin);
   }
   us.sd.pins <- sd(us.pins);
   ds.sd.pins <- sd(ds.pins);
   if(us.sd.pins > 0 || ds.sd.pins > 0) {
      stop("Variability in the min.recordable.hw or min.recordable.tw ",
           "for the culvert system is present. The function ",
           "setMinRecordablesSystem() should be used to set these elevations ",
           "for all culverts in the system.");
   }
   us.pin <- us.pins[1];
   ds.pin <- ds.pins[1];
   message("us.pin=",us.pin,"  and ds.pin=",ds.pin)

   zusinverts <- zdsinverts <- vector(mode="numeric");
   for(culvert.key in keys) {
     #catme("extracting culvert",culvert.key,"\n");
     the.culvert <- get.h(culvert.key, culverts);
     us.invert <- get.h("zusinvert", the.culvert) + del;
     ds.invert <- get.h("zdsinvert", the.culvert) + del;
     zusinverts <- c(zusinverts, us.invert);
     zdsinverts <- c(zdsinverts, ds.invert);
   }
   zusinvert <- min(zusinverts);
   zdsinvert <- min(zdsinverts);
   message("zusinvert=",zusinvert,"  and zdsinvert=",zdsinvert)

   if(is.null(h1) && is.null(h4)) stop("Both h1 and h4 are NULL");

   if(is.null(h1) && us.pin > ds.pin && h4 > ds.pin) {
      message("ComputationA:")
      top <- max(us.pin, zusinvert);
      bot <- max(h4, zdsinvert);
      h1s <- seq(top, bot, by=-del);
      i <- 0
      maxQ <- maxT <- maxh1 <- vector(mode="numeric")
      #print(h1s)
      for(h1 in h1s) {
         if(h1 <= zusinvert) next
         if(h4 >= h1) next
         flow <- NA
         try( flow <- computeFlowSystem(h1=h1, h4=h4, verbose=verbose, ignore.early.out=TRUE,
                             culvert=culverts, approach=approach, silent=silent));
         Q <- max(flow$Qtotal, na.rm=TRUE);
         T <- paste(flow$Types, sep=",", collapse=";")
         i <- i + 1;
         maxQ[i] <- Q;
         maxT[i] <- T;
         maxh1[i] <- h1
         message("h1=",h1,"  h4=",h4,"  Q=",Q)
      }
      if(length(Q) == 0) {
         stop("Ambiguous elevation conditions for a nonNULL h4, threshold discharge not available");
      }
      thresholdQ <- max(maxQ, na.rm=TRUE);
      ix <- seq(1,length(maxQ));
      DF <- data.frame(h1=maxh1, h4=rep(h4, length(maxQ)), Q=maxQ, Types=maxT)
      print(DF)
      plot(DF$h1, DF$Q, type="l",
           xlab="Headwater height, in length units",
           ylab="Maximum discharge for each headwater height, in length per time")
      thresholdh1 <- maxh1[ix[maxQ == thresholdQ]];
      typeh1 <- maxT[ix[maxQ == thresholdQ]]
      catme("ComputationA: The lowest recordable discharge threshold is",
            round(thresholdQ, digits=digits),  "if (when) h1 is",
            round(thresholdh1, digits=digits), "of type", typeh1, "\n");
      return(DF);
   }


 if(is.null(h4) && us.pin > zusinvert && us.pin > zdsinvert && ds.pin > zdsinvert) {
      message("ComputationB:")
      bot <- min(ds.pin, zdsinvert);
      top <- max(ds.pin, zdsinvert);
      h4s <- seq(top, bot, by=-del);
      i <- 0
      maxQ <- maxT <- maxh4 <- vector(mode="numeric")
      for(h4 in h4s) {
         if(h4 <= zdsinvert) next
         if(h4 >= h1) next
         flow <- NA
         try( flow <- computeFlowSystem(h1=h1, h4=h4, verbose=verbose, ignore.early.out=TRUE,
                                        culvert=culverts, approach=approach, silent=silent));
         Q <- max(flow$Qtotal, na.rm=TRUE);
         T <- paste(flow$Types, sep=",", collapse=";")
         i <- i + 1;
         maxQ[i] <- Q;
         maxT[i] <- T;
         maxh4[i] <- h4;
         #cat(h1, h4, Q, T, sep="  ")
      }
      if(length(Q) == 0) {
         stop("Ambiguous elevation conditions for a nonNULL h1, threshold discharge not available");
      }
      thresholdQ <- max(maxQ, na.rm=TRUE);
      ix <- seq(1,length(maxQ))
      thresholdh4 <- maxh4[ix[maxQ == thresholdQ]];
      typeh4 <- maxT[ix[maxQ == thresholdQ]]
      DF <- data.frame(h1=rep(h1, length(maxh4)), h4=maxh4, Q=maxQ, Types=maxT)
      print(DF)
      plot(DF$h4, DF$Q, type="l",
           xlab="Tailwater height, in length units",
           ylab="Maximum discharge for each headwater height, in length per time")
      catme("ComputationB: The lowest recordable discharge threshold is",
            round(thresholdQ, digits=digits),  "if (when) h4 is",
            round(thresholdh4, digits=digits), "of type", typeh4, "\n");
      return(DF);
   }

   if(is.null(h4) && ds.pin < us.pin && h1 > us.pin) {
      message("ComputationC:")
      bot <- min(ds.pin, zdsinvert);
      top <- max(us.pin, h1);
      h4s <- seq(top, bot, by=-del);
      i <- 0
      maxQ <- maxT <- maxh4 <- vector(mode="numeric")
      for(h4 in h4s) {
         if(h4 <= zdsinvert) next
         if(h4 >= h1) next
         flow <- NA
         try( flow <- computeFlowSystem(h1=h1, h4=h4, verbose=verbose, ignore.early.out=TRUE,
                                        culvert=culverts, approach=approach, silent=silent));
         Q <- max(flow$Qtotal, na.rm=TRUE);
         T <- paste(flow$Types, sep=",", collapse=";")
         i <- i + 1;
         maxQ[i] <- Q;
         maxT[i] <- T;
         maxh4[i] <- h4
      }
      if(length(Q) == 0) {
         stop("Ambiguous elevation conditions for a nonNULL h1, threshold discharge not available");
      }
      thresholdQ <- max(maxQ, na.rm=TRUE);
      ix <- seq(1,length(maxQ))
      thresholdh4 <- maxh4[ix[maxQ == thresholdQ]];
      typeh4 <- maxT[ix[maxQ == thresholdQ]]
      DF <- data.frame(h1=rep(h1, length(maxQ)), h4=maxh4, Q=maxQ, Types=maxT)
      print(DF)
      plot(DF$h4, DF$Q, type="l",
           xlab="Tailwater height, in length units",
           ylab="Maximum discharge for each headwater height, in length per time")
      thresholdh4 <- thresholdh4[length(thresholdh4)];
      catme("ComputationC: The lowest recordable discharge threshold is",
            round(thresholdQ, digits=digits),  "if (when) h4 is",
            round(thresholdh4, digits=digits), "of type", typeh4, "\n");
      return(DF);
   }

   stop("Ambiguous elevation conditions seemingly do not facilitate computation of threshold discharge")
}


