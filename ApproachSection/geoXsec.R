"geoXsec" <-
function(h, xsec=NULL, subdivision=NULL, nvalue=NULL, manningcor=1.486,
         plotem=FALSE, xfudge=0.001, tag="") {

   n.subs <- length(subdivision);
  n.nvals <- length(nvalue);

  mainpdf <- paste(c("TMPcapR_geoXsec",tag,".pdf"),sep="",collapse="");
  zall <- geoXsecSimple(h, xsec=xsec, nvalue=mean(nvalue),
                        plotem=plotem, xfudge=0.001,
                        pdffile=mainpdf);
  if(n.subs == 0) {
    if(is.list(zall)) {
      zall$alpha <- 1;
      zall$AREATOTAL   <- zall$A;
      zall$KONVEYTOTAL <- zall$KONVEY;
    }
  	return(zall);
  }

  A <- K <- vector(mode = "numeric");
  i <- 0; j <- 0;
  while(i < n.subs) {
      i <- i + 1;
      if(i == 1 | n.subs == 1) {
      print("FIRST SUBDIVISION");
      locxsec <- xsec[xsec$X <= subdivision[i], ];
      tmpx <- c(locxsec$X,
                locxsec$X[length(locxsec$X)],
                subdivision[i]);
      tmpy <- c(locxsec$Y,
                locxsec$Y[length(locxsec$Y)],
                h);
      locxsec <- data.frame(X=tmpx, Y=tmpy);
      #print(locxsec);
      txt <- paste(c("TMPcapR_geoXsecFIRST",tag,".pdf"), sep="", collapse="");
      z <- geoXsecSimple(h, locxsec,  nvalue=nvalue[i], manningcor=manningcor,
                         plotem=plotem, xfudge=0.001,
                         pdffile="TMPcapR_geoXsecFIRST.pdf");
      j <- j + 1;
      if(is.list(z)) {
        A[j] <- z$A; K[j] <- z$KONVEY;
      } else {
        A[j] <- NA;  K[j] <- NA;
      }
    }
    if(i == n.subs) {
      print("LAST SUBDIVISION");
      locxsec <- xsec[xsec$X >= subdivision[n.subs],];
      tmpx <- c(subdivision[i],
                locxsec$X);
      tmpy <- c(h,
                locxsec$Y);
      locxsec <- data.frame(X=tmpx, Y=tmpy);
      #print(locxsec);
      txt <- paste(c("TMPcapR_geoXsecLAST",tag,".pdf"), sep="", collapse="");
      z <- geoXsecSimple(h, locxsec,  nvalue=nvalue[i], manningcor=manningcor,
                   plotem=plotem, xfudge=0.001,
                   pdffile=txt);
      j <- j + 1;
      if(is.list(z)) {
        A[j] <- z$A; K[j] <- z$KONVEY;
      } else {
        A[j] <- NA;  K[j] <- NA;
      }
    } else {
      print("CENTRAL SUBDIVISION");
      locxsec <- xsec[xsec$X >= subdivision[i] &
                      xsec$X <= subdivision[i+1],];
      tmpx <- c(subdivision[i],
                locxsec$X,
                locxsec$X[length(locxsec$X)],
                subdivision[i+1]);
      tmpy <- c(h,
                locxsec$Y,
                locxsec$Y[length(locxsec$Y)],
                h);
      locxsec <- data.frame(X=tmpx, Y=tmpy);
      #print(locxsec);
      txt <- paste(c("TMPcapR_geoXsec",i,tag,".pdf"), sep="", collapse="");
      z <- geoXsecSimple(h, locxsec,  nvalue=nvalue[i], manningcor=manningcor,
                         plotem=plotem, xfudge=0.001,
                         pdffile=txt);
      if(is.list(z)) {
        A[j] <- z$A; K[j] <- z$KONVEY;
      } else {
        A[j] <- NA;  K[j] <- NA;
      }
    }
  }
  AREATOTAL   <- sum(A);
  KONVEYTOTAL <- sum(K);
  alpha <- Alpha(area=A, konvey=K);
  zall$alpha <- alpha;
  zall$AREATOTAL <- AREATOTAL;
  zall$KONVEYTOTAL <- KONVEYTOTAL;
  return(zall);
}


#xsec <- data.frame(X=1:11, Y=c(12,11,5,6,7,8,9,0,4,7,12))
#print(geoXsec(runif(1,min=0, max=10),xsec, nvalue=0.035, plotem=TRUE))
#


#xsec <- data.frame(X=1:11, Y=c(12,11,5,6,7,8,9,0,4,7,12))
#print(geoXsec(runif(1,min=0, max=10),xsec, subdivision=c(7.01), nvalue=c(0.050, 0.035), plotem=TRUE))
