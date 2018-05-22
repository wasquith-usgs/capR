"setRoad" <-
function(crown.elev=NULL,
         surface.type=c("paved", "gravel"),
         road.width=NULL,
         crest.lengths=data.frame(b=c(0,0), elev=c(0,0)),
         crest.length.reduction.per.unit.length=0) {

  surface.type <- match.arg(surface.type);

  if(is.null(road.width)) {
     stop("road.width is NULL");
  }

  if(is.null(crown.elev)) {
     stop("crown.elev is NULL");
  }

  if(! is.data.frame(crest.lengths)) {
     stop("a data frame of crest.lengths is not a data frame");
  } else {
     the.names <- names(crest.lengths);
     is.b    <- as.logical(length(grep('b', the.names)));
     is.elev <- as.logical(length(grep('elev', the.names)));
     if(! is.b)    stop("crest.length data frame needs a column named 'b'");
     if(! is.elev) stop("crest.length data frame needs a column named 'elev'");
  }

  tmp.h <- new.h();
  set.h("crown.elev",       crown.elev,  tmp.h);
  set.h("surface.type",   surface.type,  tmp.h);
  set.h("road.width",       road.width,  tmp.h);
  set.h("crest.lengths", crest.lengths,  tmp.h);
  set.h("crest.length.reduction.per.unit.length", crest.length.reduction.per.unit.length, tmp.h);
  return(tmp.h);
}


"Qroad" <-
function(h1=NULL, h4=NULL, v1head=0, road=NULL) {

  if(! is.h(road)) {
     return(list(Qroad=0, coes=NA, road.width=NA, crest.length=NA,
            H=NA, h=NA, ht=NA, is.flow=FALSE));
  }

  h  <- h1 - road$crown.elev;
  ht <- h4 - road$crown.elev;

  if(ht > h) {
    stop("*** ht > h after offsetting by crown elevation, flow would be backwards ***");
  }

  if(h <= 0) {
     return(list(Qroad=0, coes=NA, road.width=NA, crest.length=NA,
            H=NA, h=NA, ht=NA, is.flow=FALSE));
  }


  L <- road$road.width;
  surface.type <- road$surface.type;

  H <- h + v1head; # velocity head correction added

  if(is.null(road$crest.lengths)) {
    b <- 0.83333*H; # (5/6)*H, TWRI B3ChA5 p.26
  } else {
    # Note that we must use elevation in the table (h1) and *not* h
    b <- approx(road$crest.lengths$elev,
                y=road$crest.lengths$b, h1, rule=2)$y;
  }
  b <- b - b*road$crest.length.reduction.per.unit.length
  discharge.coefficients <- Croad(h=h, H=H, L=L, ht=ht, type=surface.type);

  Q <- discharge.coefficients$Croad*b*H^(3/2);

  return(list(Qroad=Q, coes=discharge.coefficients,
              road.width=L, crest.length=b,
              H=H, h=h, ht=ht, is.flow=TRUE));
}
# TWRI B3ChA5 p.26



"printRoadFlow" <-
function(roadflow, culvert=NULL, splash=TRUE) {

   lunits <- culvert$lengthunits;
   qunits <- culvert$flowunits;

   coes <- roadflow$coes;
   if(splash) splashbars();
   catme("FLOW OVER ROAD SUMMARY");
   catme("   Flow over road that is", round(roadflow$road.width), lunits,
        "wide is", round(roadflow$Qroad, digits=0), qunits);
   if(roadflow$is.flow) {
      catme("   with a weir coefficient of", round(coes$Croad, digits=3),
            "and a weir crest length of",
                          round(roadflow$crest.length, digits=2), lunits);
      catme("   Total head is", round(roadflow$H, digits=3), lunits,
            "with a hydraulic head of", round(roadflow$h, digits=3), lunits,
            "and a ht head of", round(roadflow$ht, digits=3), lunits);
   }
}



