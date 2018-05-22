"geoXsecSimple" <-
function(h, xsec=NULL, nvalue=NULL, manningcor=1.486,
            plotem=FALSE, xfudge=0.001,
            pdffile="TMPgeoXsecSimple.pdf") {
  xs <- xsec$X;  ys <- xsec$Y;
  n <- length(ys);

  xs <- xs + xfudge*(1:n);

  xmin <- min(xs); xmax <- max(xs);
  ymin <- min(ys); ymax <- max(ys);
  if(h >= ys[1]) {
     txt <- paste(c("height of ",h," is greater than left end ",
                    "point of cross section, setting edge to height"),
                  collapse="");
     warning(txt);
     ys[1] <- h;
  }
  if(h >= ys[n]) {
     txt <- paste(c("height of ",h," is greater than right end ",
                    "point of cross section, setting edge to height"),
                    collapse="");
     warning(txt);
     ys[n] <- h;
  }
  if(h <= ymin) {
     txt <- paste(c("height of ",h," is less than minimum, ",
                    "setting height to minimum"),
                  collapse="");
     warning(txt);
     h <- ymin;
  }

  xs2 <- xs; ys2 <- ys;
  nx <- ny <- vector(mode = "numeric");
  e <- 0;
  for(i in 1:(n-1)) {
  	if(ys[i] > h & ys[i+1] < h) {
      e <- e + 1;
      if(xs[i] != xs[i+1]) {
    	nx[e] <- approx(c(ys[i], ys[i+1]), y=c(xs[i], xs[i+1]), h)$y
      } else {
        nx[e] <- xs[i];
      }
      ny[e] <- h;
  	} else if(ys[i] < h & ys[i+1] > h) {
  	  e <- e + 1;
      if(xs[i] != xs[i+1]) {
    	nx[e] <- approx(c(ys[i], ys[i+1]), y=c(xs[i], xs[i+1]), h)$y
      } else {
        nx[e] <- xs[i];
      }
      ny[e] <- h;
  	}
  }
  xs2 <- xs2[ys2 <= h]; ys2 <- ys2[ys2 <= h];
  xs2 <- c(xs2, nx); ys2 <- c(ys2, ny);
  idx <- sort(xs2, index.return=TRUE);
  xs2 <- xs2[idx$ix]; ys2 <- ys2[idx$ix];
  n2  <- length(ys2);

  if(n2 < 3) return(0);

  new.x.again <- new.y.again <- vector(mode = "numeric");
  j <- 0;
  for(i in 1:n2) {
  	j <- j + 1;
  	new.x.again[j] <- xs2[i];
  	new.y.again[j] <- ys2[i];
  	if(i < n2 & ys2[i] == h & ys2[i+1] == h) {
  		j <- j + 1;
  	  	new.x.again[j] <- new.y.again[j] <- NA;
  	}
  }
  xs2 <- new.x.again; ys2 <- new.y.again;

  rx <- xs2[! is.na(ys2)]; ry <- ys2[! is.na(ys2)];
  rn <- length(ry);
  area <- sum( (h - (ry[1:(rn-1)] + ry[2:rn    ])/2 ) *
                    (rx[2:rn    ] - rx[1:(rn-1)])   );

   #print(rx);
   #print(ry);
   #print(rn);

  "hypoth" <- function(i) {
  	y1 <- ys2[i]; y2 <- ys2[i+1];
    if( any( is.na( c(y1, y2) ) ) ) return(0);
  	ifelse(y2 <= h, return(sqrt((xs2[i+1] - xs2[i])^2 + (y2 - y1)^2)),
  	               return(0));
  }
  wettedperimeter <- sum(sapply(1:(rn-1),hypoth));

  "TW" <- function(i) {
     #print(ry[i]);
     #print(ry[i+1]);
     return(rx[i+1] - rx[i]);
     #ifelse((ry[i] = h & ry[i+1] == h), return(0), return(rx[i+1] - rx[i]));
  }
  topwidth <- sum(sapply(1:(rn-1),TW));


  if(plotem) {
  	pdf(pdffile);
    xc <- c(xs2, xs2[n], xs2[1]); yc <- c(ys2, h, h);
  	plot(xs,ys, type="b", lty=2, cex=2,
  	     xlab="STATION, IN FEET",
  	     ylab="HEIGHT, IN FEET");
    polygon(xc,  yc, col=rgb(0.95,0.95,0.95), border=NA);
    points(xs2, ys2, pch=16)
    lines(xs2,  ys2, lwd=3);
    dev.off();
  }
  hydraulicradius <- area/wettedperimeter;
  ifelse(is.null(nvalue), konvey <- NA,
            konvey <- Conveyance(A=area, R=hydraulicradius,
                                 nvalue=nvalue, manningcor=manningcor));

  z <- list(HEIGHT  = h,
            N      = nvalue,
            DBAR   = area/topwidth,
            A      = area,
            TW     = topwidth,
            WP     = wettedperimeter,
            HR     = hydraulicradius,
            KONVEY = konvey);
  return(z);
}


"demo.geoXsecSimple" <-
function(ask=TRUE) {
  if (ask) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }

  xsec <- data.frame(X=1:11, Y=c(12,11,5,6,7,8,9,0,4,7,12))
  print(geoXsec(runif(1,min=0, max=10),xsec, plotem=TRUE))

  xsec <- data.frame(X=c( 1,  1,  1, 1, 4, 4, 4,  4),
                     Y=c(13, 11, 10, 0, 0, 2, 6, 13))
  print(geoXsecSimple(runif(1,min=0, max=10),xsec, plotem=TRUE))


  xsec <- data.frame(X=c( 1,  1.05, 3.99,  4),
                     Y=c(13, 0, 0, 13))
  print(geoXsecSimple(runif(1,min=0, max=10),xsec, plotem=TRUE))


  xsec <- data.frame(X=c( 1,  10,  20),
                     Y=c(10, 0, 10))
  print(geoXsecSimple(runif(1,min=0, max=10),xsec, plotem=TRUE))

  xsec <- data.frame(X=c( 1,  10,  10),
                     Y=c(10, 0, 10))
  print(geoXsecSimple(runif(1,min=0, max=10),xsec, plotem=TRUE))


  xsec <- data.frame(X=c( 0,  10,  10,  15,  20, 30, 30),
                     Y=c(10,   0,    10,  10,  10, 0, 10))
  print(geoXsecSimple(runif(1,min=0, max=14),xsec, plotem=TRUE))

  x <- seq(-10,10,by=0.1)
  B <- 10
  h <- runif(1,min=0, max=14)
  xsec <- data.frame(X=x, Y=x^2)
  print(geoXsecSimple(h,xsec, plotem=TRUE))

  x <- seq(-10,10,by=0.1)
  B <- 10; h <- runif(1,min=0, max=14);
  xsec <- data.frame(X=c(x,x+20), Y=c(x^2,x^2))
  print(geoXsecSimple(h,xsec, plotem=TRUE))


}
