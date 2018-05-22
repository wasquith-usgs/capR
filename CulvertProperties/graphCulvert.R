"graphCulvert" <-
function(discharge=NULL, depth=NULL, culvert=NULL, verbose=FALSE,
         location=c("inlet", "outlet"), depression=0, usedatums=FALSE) {

  key <- culvert$TMP.key;
  location <- match.arg(location);
  if(location == "inlet") {
     my.location <- paste(c("CULVERT ",key," INLET"), collapse="");
     my.datum    <- culvert$zusinvert;
     my.center   <- culvert$inlet.centerline;
  } else {
     my.location <- paste(c("CULVERT ",key," OUTLET"), collapse="");
     my.datum    <- culvert$zdsinvert;
     my.center   <- culvert$outlet.centerline;
  }
  usedatums <- culvert$use.datums.when.graphing;
  if(! usedatums) {
     my.datum  <- 0;
     my.center <- 0;
  }

  D <- culvert$diameter;
  if(is.null(depth)) depth <- D - depression;

  if(is.null(discharge)) {
    yo <- NA;
    yc <- NA;
  } else {
    discharge <- discharge/culvert$number.of.barrels;
    yo <- getNormalDepth(discharge,   culvert=culvert,
                         location=location, depression=depression);
    yc <- getCriticalDepth(discharge, culvert=culvert,
                           location=location, depression=depression);
  }

  TW2 <- 0.5*setCulvertGeometry(depth=depth,
                                culvert=culvert, location=location,
                                depression=depression)$TW;
  TW.at.depth.neg <- -TW2;
  TW.at.depth.pos <-  TW2;

  depths <- sort(c(seq(0, D - depression, by=0.01), depth));
  n <- length(depths);
  #catme("Depth and depression:", depth, depression);
  #catme("Depths for drawing A:", max(depths));
  TW.neg <- TW.pos <- vector(mode="numeric", length=n);
  for(i in 1:n) {
     TW2 <- 0.5*setCulvertGeometry(depth=depths[i],
                                   culvert=culvert, location=location,
                                   depression=depression)$TW;
     TW.neg[i] <- -TW2;
     TW.pos[i] <-  TW2;
  }
  #catme("TW.neg", TW.neg);
  #catme("TW.pos", TW.pos);

   ys <- c(rev(depths), depths, D - depression);
  #catme("Depths for drawing B:", max(ys));
  TWs <- c(rev(TW.neg), TW.pos);
  if(culvert$altgeotype == "trapezoid") {
     TWs[2*n+1] <- min(TW.neg);
  } else {
     TWs[2*n+1] <- rev(TW.neg)[1];
  }

  #idx <- sort(TWs, index.return=TRUE)$ix;
  #catme("TWs", TWs);
  #catme("ys", ys);
  ifelse(culvert$lengthunits == "feet",
         my.lunits <- ", IN FEET",
         my.lunits <- ", IN METERS");
  my.xlab <- paste(c("DISTANCE FROM CENTER LINE", my.lunits),
                   sep="", collapse="");
  if(usedatums) {
     my.ylab <- paste(c("ELEVATION FROM CULVERT DATUM", my.lunits),
                      sep="", collapse="");
  } else {
     my.ylab <- paste(c("ELEVATION FROM CULVERT INVERT", my.lunits),
                      sep="", collapse="");
  }
  rngy <- range(ys);
  rngx <- range(TWs);
  if(rngx[2] - rngx[1] > rngy[2] - rngy[1]) {
    xlim <- c(rngx[1], rngx[2]);
    ylim <- c(0, rngx[2] - rngx[1]);
  } else {
    xlim <- c(-D/2, D/2);
    ylim <- c(   0, D  );
  }

    plot(TWs + my.center,
         ys + my.datum,
         type="l", tcl=0.5, lwd=3,
         xlim=xlim + my.center,
         ylim=ylim + my.datum,
         xlab=my.xlab, ylab=my.ylab, col=1);
    polygon(TWs[ys <= depth] + my.center,
            ys[ys <= depth] + my.datum,
            col=rgb(0.95,0.95,0.95));
    lines(c(TW.at.depth.neg, TW.at.depth.pos) + my.center,
          c(depth, depth) + my.datum,
          lwd=3);

    if(! is.na(yo)) {
       TW <- setCulvertGeometry(depth=yo, culvert=culvert,
                                location=location, depression=depression)$TW;
       if(verbose) {
          catme("Topwidth=", TW, "at normal depth=", yo, "\n");
       }
       lines(c(-0.5*TW, 0.5*TW) + my.center,
             c(yo,yo) + my.datum,
             lwd=1, lty=2);
    }
    if(! is.na(yc)) {
       TW <- setCulvertGeometry(depth=yc, culvert=culvert,
                                location=location, depression=depression)$TW;
       if(verbose) {
          catme("Topwidth=", TW, "at critical depth=", yc, "\n");
       }
       lines(c(-0.5*TW, 0.5*TW) + my.center,
             c(yc,yc) + my.datum,
             lwd=1, lty=4, col=rgb(0.5,0.5,0.5));
    }
    mtext(my.location, side=3);
    legend(min(TWs) + my.center,
           0.95*ylim[2] + my.datum,
           c("Free surface",
             "Normal depth (if applicable)",
             "Critical depth (if applicable)"),
           lty=c(1,2,4), lwd=c(3,1,1), cex=0.75, col=c(1,1,rgb(0.5,0.5,0.5)),
           bty="o", bg="white", box.lwd=4, box.col="white");

}


