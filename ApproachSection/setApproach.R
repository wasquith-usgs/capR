"setApproach" <-
function(station.id="",
         station.name="",
         Lapproach=0,
         nvalue=0.030,
         manningcor=1.486,
         X=c(0,0,20,20), Y=c(10,0,0,10), XY=NULL,
         subdivision=NULL,
         additional.flow=0,
         fraction.of.flow=1,
         skew.angle=0,
         time.series.in=NULL,
         time.series.out=NULL,
         forceEqualApproachHead=TRUE,
         forceIgnoreApproachHead=FALSE,
         useApproachHeadApportioning=FALSE,
         ...) {

  if(! is.null(XY)) {
     if(is.data.frame(XY)) {
       X <- XY$X;
       Y <- XY$Y;
     } else if(is.vector(XY)) {
       n <- length(XY);
       if(n %% 2 != 0) {
         stop("XY argument is not of even length for a paired X,Y requirement");
       }
       xs <- seq(1,n, by=2);
       ys <- xs+1;
       X <- XY[xs];
       Y <- XY[ys];
     }
     else {
        stop("XY argument given, but not data.frame or vector");
     }
  }
  if(length(X) != length(Y)) {
     stop("vectors of stationing and elevation on approach not equal length");
  }

  # There are as.numeric()s wrapped around the numerical arguments and
  # properties to provide a mode of protection immediately as the setting
  # of the approach. This was done in response to development of readApproach().
  X <- as.numeric(X); Y <- as.numeric(Y);
  skew.angle  <- as.numeric(skew.angle);
  skew.factor <- cos(skew.angle*pi/180);
  X.original <- as.numeric(X);
  Y.original <- as.numeric(Y);
  X <- X*skew.factor;

  tmp.h <- new.h();
  setchar.h("station.id",   station.id,   tmp.h);
  setchar.h("station.name", station.name, tmp.h);
  setchar.h("description",
        "Approach Section Properties", tmp.h);

  setnum.h("nvalue", nvalue,               tmp.h);
  setnum.h("manningcor", manningcor,       tmp.h);
  setnum.h("subdivision", subdivision,     tmp.h);
  setnum.h("Lapproach", Lapproach,         tmp.h);
  setnum.h("fraction.of.flow", fraction.of.flow,  tmp.h);

  set.h("skew.angle",  skew.angle,     tmp.h);
  set.h("skew.factor", skew.factor,    tmp.h);
  set.h("xsec",data.frame(X=X,Y=Y),    tmp.h);
  set.h("xsec.original",data.frame(X=X.original, Y=Y.original), tmp.h);
  setnum.h("min.elevation", min(Y),       tmp.h);

  set.h("time.series.in",  time.series.in,  tmp.h);
  set.h("time.series.out", time.series.out, tmp.h);

  setnum.h("additional.flow", additional.flow,      tmp.h);
  setnum.h("TMP.accumulated.flow", additional.flow, tmp.h);
  setnum.h("TMP.terminal.area.previous.run", 0,     tmp.h);
  setnum.h("TMP.konvey2.previous.run", 0,           tmp.h);

  set.h("forceEqualApproachHead",           forceEqualApproachHead, tmp.h);
  set.h("forceIgnoreApproachHead",         forceIgnoreApproachHead, tmp.h);
  set.h("useApproachHeadApportioning", useApproachHeadApportioning, tmp.h);

  nargs <- length(pairlist(...));
  if(nargs >= 1) {
     other.args <- pairlist(...);
     if(is.h(other.args$left.approach)) {
        set.h("left.approach",other.args$left.approach, tmp.h);
     }
     if(is.h(other.args$right.approach)) {
        set.h("right.approach",other.args$right.approach, tmp.h);
     }
  }
  return(tmp.h);
}



"readApproach" <-
function(file=NULL, sep="|", verbose=FALSE) {
  if(is.null(file) || ! file.exists(file)) {
     file <- file.choose();
  }
  message("Reading approach properties file ", file);
  D <- read.table(file, sep=sep, header=TRUE, as.is=TRUE);
  if(verbose) print(D);

  tmphash <- new.h(); # tmp hash, which will be used to form named arguments

  for(i in (1:length(D$KEY))) { # loop through each key
     key <- D$KEY[i];
     if(length(grep("^#", key, perl=TRUE, value=TRUE)) == 1) {
        message("   Skipping ",key);
        next;
     }
     val <- D$VALUE[i];

     # next two substitutions strip out leading and trailing whitespace
     val <- gsub("^\\s+", "", val, perl=TRUE);
     val <- gsub("\\+s$", "", val, perl=TRUE);

     message("Parsing on key '", key, "' and value '", val,"'");
     if(key == "X" || key == "Y") { # treating these different
       message("   Found either X or Y and appending previous values");
       was  <- get.h(key, tmphash);
       vals <- as.numeric(unlist(strsplit(val, '\\s+', perl=TRUE)));
        val <- c(was, vals); # combine old and then the new
     }
     if(key == "XY") { # treating this type even more different
       message("   Found XY key, splitting on spaces and then comma");
       xy.dataframe <- get.h(key, tmphash);
       xy <- unlist(strsplit(val, '\\s+', perl=TRUE));
       #print(xy);
       n <- length(xy);
       x.vec <- y.vec <- vector(length=n);
       for(i in 1:length(xy)) {
          each.xy <- as.numeric(unlist(strsplit(xy[i], ',', perl=TRUE)));
          if(length(each.xy) == 2) {
             #print(each.xy);
             x.vec[i] <- each.xy[1];
             y.vec[i] <- each.xy[2];
          }
       }
       message("      X vector= ",x.vec);
       message("      Y vector= ",y.vec);
       val <- data.frame(X=c(xy.dataframe$X, x.vec),
                         Y=c(xy.dataframe$Y, y.vec));
     }
     if(has.key("XY", tmphash)) {
        message("   Found and XY key, so deleting X and Y keys");
        del.h("X", tmphash);
        del.h("Y", tmphash);
     }
     set.h(key, val, tmphash); # ONLY LOCATION SETTING THE KEY=VALUE
  }

  # it took a lot of searching figureout a means to get user input
  # to function as formal named arguments to a function. This is apparently
  # the R idiom to do just that.
  approach <- do.call("setApproach", as.list(tmphash));

  # note that setApproach does create this key
  set.h("external.approachfile", file, approach);

  return(approach);
}





"editApproach.subnval" <-
function(approach=NULL) {
  if(! is.h(approach)) {
     warning("An approach hash is needed as argument");
     return();
  }
  nvalue <- get.h("nvalue",      approach);
  subdiv <- get.h("subdivision", approach);
  if(is.null(subdiv)) subdiv <- NA;

  edited.val <- de(nvalue, subdiv,
                  Modes=c("numeric", "numeric"),
                  Names=c("NVALUE", "SUBDIVISION"));

  nvalue <- edited.val$NVALUE;
  subdiv <- edited.val$SUBDIVISION;
  if(length(subdiv) == 1 && is.na(subdiv)) subdiv <- NULL;
  set.h("nvalue",      nvalue, approach);
  set.h("subdivision", subdiv, approach);
}


"editApproach.xy" <-
function(approach=NULL) {
  if(! is.h(approach)) {
     warning("An approach hash is needed as argument");
     return();
  }

  XY   <- get.h("xsec.original", approach);
  skew.angle <- get.h("skew.angle",    approach);
  X.original <- XY$X; Y.original <- XY$Y;
  skew.factor <- cos(skew.angle*pi/180);

  edited.XY <- de(X.original,Y.original,
                  Modes=c("numeric", "numeric"),
                  Names=c("STATION", "ELEVATION"));

  X.original <- edited.XY$STATION; Y.original <- edited.XY$ELEVATION;
  ok <- complete.cases(X.original, Y.original);
  X.original <- X.original[ok]; Y.original <- Y.original[ok];
  ix <- sort(X.original, index.return=TRUE)$ix;
  X.original <- X.original[ix]; Y.original <- Y.original[ix];


  X <- X.original*skew.factor; Y <- Y.original;
  set.h("xsec.original", data.frame(X=X.original, Y=Y.original), approach);
  set.h("xsec",          data.frame(X=X,          Y=Y),          approach);
}


# Three temporary summations are kept associated with the approach
# section. accumulated.flow is the total flow running through the approach,
# terminal.area.previous.run is the total area of the terminal sections of
# one or more culvert objects and is used in computing the contraction ratio
# of the flow, konvey2.previous.run is the conveyance for the computation
# of the headloss from section 1 (approach) to all inlets.
"resetTMPvarsApproach" <-
function(approach=NULL) {
   set.h("TMP.accumulated.flow", approach$additional.flow, approach);
   set.h("TMP.terminal.area.previous.run", 0,     approach);
   set.h("TMP.konvey2.previous.run", 0,           approach);
}


"setApproachConditions" <-
function(depth=NULL, approach=NULL, ...) {
       nvalue <- approach$nvalue;
   manningcor <- approach$manningcor;
  subdivision <- approach$subdivision;
         xsec <- approach$xsec;
          geo <- geoXsec(h=depth, xsec=xsec, subdivision=subdivision,
                         nvalue=nvalue, manningcor=manningcor, ...);

  if(is.h(approach$left.approach)) {
           tmp.h <- approach$left.approach;
          nvalue <- tmp.h$nvalue;
     subdivision <- tmp.h$subdivision;
            xsec <- tmp.h$xsec;
        left.geo <- geoXsec(h=depth, xsec=xsec, subdivision=subdivision,
                            nvalue=nvalue, manningcor=manningcor,
                            tag="left", ...);
      geo$AREATOTAL   <- geo$AREATOTAL   + left.geo$AREATOTAL;
      geo$KONVEYTOTAL <- geo$KONVEYTOTAL + left.geo$KONVEYTOTAL;
     set.h("geometry",left.geo, approach$left.approach);
  }
  if(is.h(approach$right.approach)) {
           tmp.h <- approach$right.approach;
          nvalue <- tmp.h$nvalue;
     subdivision <- tmp.h$subdivision;
            xsec <- tmp.h$xsec;
       right.geo <- geoXsec(h=depth, xsec=xsec, subdivision=subdivision,
                            nvalue=nvalue, manningcor=manningcor,
                            tag="right", ...);
     geo$AREATOTAL   <- geo$AREATOTAL   + right.geo$AREATOTAL;
     geo$KONVEYTOTAL <- geo$KONVEYTOTAL + right.geo$KONVEYTOTAL;
     set.h("geometry",right.geo, approach$right.approach);
  }

  set.h("geometry",geo, approach);
}


"demo.multiple.approaches" <- function() {
   app.left  <- setApproach(nvalue=0.01);
   app.right <- setApproach(nvalue=0.03);
   app <- setApproach(nvalue=0.02,
                      left.approach=app.left,
                      right.approach=app.right);
   setApproachConditions(depth=4, approach=app, plotem=TRUE);
   print(each.h(app));
   print(each.h(app$left.approach));
   print(each.h(app$right.approach));
}




"reset.approachlength" <- function(length, approach) {
    oldlength <- get.h("Lapproach", approach)
    set.h("Lapproach", length, approach);
    return(oldlength)
}


