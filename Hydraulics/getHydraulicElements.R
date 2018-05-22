"getHydraulicElements" <-
function(culvert=NULL,
         location=c("inlet", "outlet"), depression=0,
         plotfile=NULL, dely=0.01, verbose=FALSE) {

   location <- match.arg(location);

           D <- culvert$diameter - depression;
          So <- culvert$So;
      sqrtSo <- ifelse(So <= 0, NA, sqrt(So));
  manningcor <- culvert$manningcor;
      nvalue <- culvert$nvalue;
           g <- culvert$gravity;

  if(depression != 0) {
     ifelse(location == "inlet", tmp <- culvert$inlet.depression,
                                 tmp <- culvert$outlet.depression);
     if(depression != tmp) {
        catme("Depression requested by function call does not match",
              "depression as set by either inlet.depression or",
              "outlet.depression in the culvert hash. This function",
              "requires for internal reasons that depression=0 or matches",
              "that specified by the culvert object");
        stop("depression != (outlet.depression | outlet.depression)");
     }
  }

  depths <- seq(dely, D, by=dely);
  n <- length(depths);
  A <- TW <- WP <- HR <- K <- Q <- V <- HD <- F <- vector(mode="numeric", length=n);
  for(i in 1:n) {
       geo <- setCulvertGeometry(depth=depths[i],
                                 culvert=culvert, location=location,
                                 depression=depression);
     A[i]  <- geo$A;
     TW[i] <- geo$TW;
     WP[i] <- geo$WP;
     HR[i] <- geo$HR;
     K[i]  <- geo$KONVEY;
     Q[i]  <- ifelse(is.na(sqrtSo), NA, geo$KONVEY * sqrtSo);
     V[i]  <- Q[i]/A[i];
     HD[i] <- A[i]/TW[i];
     F[i]  <- V[i]/sqrt(g*HD[i]);
  }
  area.full <-  A[n];
  hr.full   <- HR[n];
  wp.full   <- WP[n];
  k.full    <-  K[n];
  q.full    <-  Q[n];
  v.full    <-  V[n];
  f.full    <-  F[n];
  tw.max    <- max(TW);
  maxQ      <- max(Q);
  maxV      <- max(V);
  maxF      <- max(F);
  dimless.depth <- depths/D;
  dimless.area  <-  A/area.full;
  dimless.hr    <- HR/hr.full;
  dimless.tw    <- TW/tw.max;
  dimless.wp    <- WP/wp.full;
  dimless.k     <- K/k.full;
  dimless.q     <-  Q/q.full;
  dimless.v     <-  V/v.full;
  dimless.f     <-  F/maxF;
  x.max <- c(dimless.area, dimless.q,  dimless.v, dimless.k,
             dimless.hr,   dimless.tw, dimless.wp);
  x.max <- x.max[! is.na(x.max)];
  x.max <- max(x.max[! is.nan(x.max)]);
  #catme("x.max:",x.max);
  if(! is.null(plotfile)) pdf(plotfile);

  #catme("dimless.q", dimless.q);
  #catme("dimless.v", dimless.v);

  plot(dimless.area, dimless.depth, type="l", tcl=0.5,
       xlim=c(0,x.max), ylim=c(0,1), lty=3,
       xlab="Q/Qmax, V/Vmax, F/Fmax, A/Af, R/Rf, TW/TWmax, P/Pf, K/Kf, DIMENSIONLESS",
       ylab="DEPTH / MAXIMUM HEIGHT, DIMENSIONLESS");
  lines(c(1,1), c(0,1), col=8, lwd=0.75);

  maxdimless.q <- dimless.depth[Q == maxQ];
  maxdimless.v <- dimless.depth[V == maxV];
  maxdimless.f <- dimless.depth[F == maxF];
  if(! is.na(sqrtSo)) {
    lines(c(0,max(dimless.q)), c(maxdimless.q,maxdimless.q), col=8, lwd=0.5);
    points(max(dimless.q), maxdimless.q, pch=16, cex=1.5);
    lines(c(0,max(dimless.v)), c(maxdimless.v,maxdimless.v), col=8, lwd=0.5);
    points(max(dimless.v), maxdimless.v, pch=16, cex=1.5);
    lines(c(0,max(dimless.f)), c(maxdimless.f,maxdimless.f), col=8, lwd=0.5);
    points(max(dimless.f), maxdimless.f, pch=16, cex=1.5);

    lines(dimless.q,  dimless.depth, lwd=3);
    lines(dimless.v,  dimless.depth, lwd=3, lty=2);
    lines(dimless.f,  dimless.depth, lwd=3, lty=3);
  }
  lines(dimless.hr, dimless.depth, lty=2);
  lines(dimless.tw, dimless.depth, lty=1);
  lines(dimless.wp, dimless.depth, lty=4);
  lines(dimless.k,  dimless.depth, lty=6);



  legend(0, 0.75, c("Q/Qmax", "V/Vmax", "F/Fmax",
                    "A/Af", "R/Rf", "TW/TWmax", "P/Pf", "K/Kf"),
         lty=c(1,2,3,3,2,1,4,6), lwd=c(3,3,3,1,1,1,1,1),
         cex=0.75, bty="o", bg="white", box.lwd=4, box.col="white");

  if(! is.null(plotfile)) dev.off();

  z <- list(Qmax=maxQ, Qfull=q.full, Vfull=v.full, Vmax=maxV,
            diameter=D, width=tw.max, depths=depths,
            Area=A, HydraulicRadius=HR, Topwidth=TW,
            Conveyance=K, WettedPerimeter=WP, Froude=F, HydraulicDepth=HD);
  return(z);
}

