"setCulvert" <-
function(name="",
         slope=NULL,
         beveling=0,
         thetabev=45,
         rounding=0,
         USmiterlength=NULL,
         DSmiterlength=NULL,
         nvalue=0.015,
         skew.angle=0,
         L=NULL,
         Ltop=NULL,
         Lbot=NULL,
         diameter=NULL,
         width=NULL,
         rise=NULL,
         span=NULL,
         inlet.centerline=0,
         outlet.centerline=0,
         sideslope=0,
         zusinvert=0,
         zdsinvert=0,
         inlet.depression=0,
         outlet.depression=0,
         min.recordable.hw=NULL,
         min.recordable.tw=NULL,
         type=c("circle", "box", "ellipse", "pipearch"),
         altgeotype=c(NA, "circle", "box", "ellipse", "pipearch",
                      "trapezoid"),
         pipearch.type=c(NA, "RCP", "ALUM 31.8inchCR",
                         "CMP <=18inchCR", "CMP 31inchCR", "CMP 47inchCR",
                         "custom"),
         inlet=c("flush", "mitered", "flared"),
         theta=0,
         theta.left=NULL,
         theta.right=NULL,
         web=0,
         projectlength=0,
         is.topedge.square=FALSE,
         is.bellmouthed=FALSE,
         is.tonguegroove=FALSE,
         pipearch.radius.bottom=NULL,
         pipearch.radius.top=NULL,
         pipearch.radius.corner=NULL,
         pipearch.radius.units=NULL,
         number.of.barrels=1,
         ignore.approach.losses=FALSE,
         ignore.approach.velocity.head=FALSE,
         fraction.of.approach.area=1,
         material=c("concrete", "corrugated-metal"),
         is.barrel.rough=FALSE,
         materialthickness=c("thin-walled"),
         headwall=c("vertical"),
         embankment=c("slope"),
         gravity=32.2,
         flowunits=c("cubic feet per second", "cubic meters per second"),
         lengthunits=c("feet", "meters"),
         areaunits=c("square feet","square meters"),
         velunits=c("feet per second", "meters per second"),
         use.datums.when.graphing=TRUE,
         road=NULL) {

  ifelse(gravity > 15, manningcor <- 1.486, manningcor <- 1);
  flowunits <- match.arg(flowunits);
  if(is.null(flowunits)) flowunits <- "cubic feet per second";
  lengthunits <- match.arg(lengthunits);
  if(is.null(lengthunits)) lengthunits <- "feet";
  areaunits <- match.arg(areaunits);
  if(is.null(areaunits)) areaunits <- "square feet";
  velunits <- match.arg(velunits);
  if(is.null(velunits)) velunits <- "feet per second";

  type <- match.arg(type);
  if(is.null(type)) type <- "circle";

  pipearch.type <- match.arg(pipearch.type);
  if(type == "pipearch" & is.na(pipearch.type)) {
     stop("pipearch specified, but pipearch.type is NULL");
  }

  altgeotype <- match.arg(altgeotype);
  if(is.na(altgeotype)) altgeotype <- type;


  inlet <- match.arg(inlet);
  if(is.null(inlet)) inlet <- "flush";
  if(! is.null(USmiterlength)) inlet <- "mitered";

  material <- match.arg(material);
  if(is.null(material)) material <- "concrete";
  if(material == "corrugated-material") is.barrel.rough <- TRUE;

  headwall <- match.arg(headwall);
  if(is.null(headwall)) headwall <- "vertical";

  embankment <- match.arg(embankment);
  if(is.null(embankment)) embankment <- "slope";

  geometry.func <- NULL
  if(type == "circle") {
    if(is.null(rise))     rise     <- diameter;
    if(is.null(diameter)) diameter <- rise;
    width <- span <- diameter;
    geometry.func <- ARCIR;
  } else if(type == "box") {
    if(is.null(rise))     rise     <- diameter;
    if(is.null(width) && is.null(span))  width    <- diameter;
    if(is.null(diameter)) diameter <- rise;
    if(is.null(span))     span     <- width;
    width <- span;
    if(altgeotype == "trapezoid") {
      geometry.func <- ARTRAP;
    } else {
      geometry.func <- ARBOX;
    }
  } else if(type == "ellipse") {
    if(is.null(diameter)) diameter <- rise;
    if(is.null(rise)) rise <- diameter;
    if(is.null(span)) span <- width;
    if(is.null(span)) span <- rise;
    width <- span;
    geometry.func <- ARELLIPSE;
  } else if(type == "pipearch") {
    if(is.null(diameter)) diameter <- rise;
    if(is.null(rise))     rise     <- diameter;
    if(is.null(span))     span     <- rise;
    width <- span;
    geometry.func <- ARPIPEARCH;
  } else {
    stop("can not determine culvert type");
  }

  if(! is.null(L) && is.null(Ltop)) Ltop <- L;
  if(! is.null(L) && is.null(Lbot)) Lbot <- L;
  L <- NULL;

  if(! is.null(slope)) {
     ifelse(is.null(Lbot), zusinvert <- zdsinvert,
                           zusinvert <- Lbot*slope + zdsinvert);
  }
  z <- zusinvert - zdsinvert;
  if(z == 0) {
     slope <- 0;
  } else {
     slope <- z/Lbot;
  }


  tmp.h <- new.h();

  setchar.h("name", name, tmp.h);

  setnum.h("So", slope,                    tmp.h); # cap96 variable So
  setnum.h("USmiterlength", USmiterlength, tmp.h);
  setnum.h("DSmiterlength", DSmiterlength, tmp.h);
  setnum.h("nvalue", nvalue,               tmp.h);
  setnum.h("diameter", diameter,           tmp.h); # cap96 variable D
  setnum.h("span", span,                   tmp.h);
  setnum.h("rise", rise,                   tmp.h);
  setnum.h("width", width,                 tmp.h);

  skew.factor <- cos(skew.angle*pi/180);
  setnum.h("skew.angle",   skew.angle,     tmp.h);
  setnum.h("skew.factor", skew.factor,     tmp.h);


  if(is.null(web) || is.na(web)) stop("Web can not be NULL or NA, make zero if no webbing");
  if(type != 'box' & web > 0)    stop("Geometry is non-'box', yet a web > 0 is specified");

  setint.h("web", web,         tmp.h);
  setnum.h("sideslope", sideslope,         tmp.h);
  setchar.h("type", type,                   tmp.h);
  setchar.h("pipearch.type", pipearch.type, tmp.h);
  setchar.h("altgeotype", altgeotype,       tmp.h);
  setchar.h("inlet", inlet,                 tmp.h);
  setnum.h("inlet.centerline",   inlet.centerline, tmp.h);
  setnum.h("outlet.centerline", outlet.centerline, tmp.h);
  setnum.h("z", z,                         tmp.h);
  setnum.h("zusinvert", zusinvert,         tmp.h);
  setnum.h("zdsinvert", zdsinvert,         tmp.h);
  setnum.h("min.recordable.hw", min.recordable.hw, tmp.h);
  setnum.h("min.recordable.tw", min.recordable.tw, tmp.h);
  setnum.h("Ltop", Ltop,                   tmp.h);
  setnum.h("Lbot", Lbot,                   tmp.h);
  set.h("geometry.func", geometry.func, tmp.h);
  setchar.h("material", material,           tmp.h);
  setchar.h("headwall", headwall,           tmp.h);
  setchar.h("embankment", embankment,       tmp.h);

  setnum.h("gravity", gravity,             tmp.h);
  setnum.h("manningcor", manningcor,       tmp.h);
  setchar.h("flowunits", flowunits,         tmp.h);
  setchar.h("lengthunits", lengthunits,     tmp.h);
  setchar.h("areaunits", areaunits,         tmp.h);
  setchar.h("velunits", velunits,           tmp.h);

  set.h("road", road,             tmp.h);

  if(type == "pipearch") {
    if(is.null(pipearch.radius.units)) {
      radii <- CULPAD(culvert=tmp.h);
      setnum.h("pipearch.radius.bottom", radii$radius.bottom, tmp.h);
      setnum.h("pipearch.radius.top",    radii$radius.top,    tmp.h);
      setnum.h("pipearch.radius.corner", radii$radius.corner, tmp.h);
      set.h("pipearch.radius.units",  radii$radius.units,  tmp.h);
    } else {
      if(pipearch.radius.units != "inches") {
         stop("pipearch radii set by user requires pipearch.radius.units equal to 'inches'");
      }
      setnum.h("pipearch.radius.bottom", pipearch.radius.bottom, tmp.h);
      setnum.h("pipearch.radius.top",    pipearch.radius.top,    tmp.h);
      setnum.h("pipearch.radius.corner", pipearch.radius.corner, tmp.h);
      setchar.h("pipearch.radius.units",  pipearch.radius.units,  tmp.h);
    }
  }

  if(inlet.depression < 0) {
    stop("inlet.depression is < 0, which is in error");
  }
  if(outlet.depression < 0) {
    stop("outlet.depression is < 0, which is in error");
  }
  if(inlet.depression >= diameter) {
    stop("inlet.depression is > diameter or rise, which is in error");
  }
  if(outlet.depression >= diameter) {
    stop("outlet.depression is > diameter or rise, which is in error");
  }
  setnum.h("inlet.depression",   inlet.depression, tmp.h);
  setnum.h("outlet.depression", outlet.depression, tmp.h);

  the.geometry <- setCulvertGeometry(depth=inlet.depression, culvert=tmp.h);
  inlet.depression.A  <- the.geometry$A;
  inlet.depression.WP <- the.geometry$WP;
  inlet.depression.TW <- the.geometry$TW;
  setnum.h("inlet.depression.A",   inlet.depression.A,  tmp.h);
  setnum.h("inlet.depression.WP",  inlet.depression.WP, tmp.h);
  setnum.h("inlet.depression.TW",  inlet.depression.TW, tmp.h);

  the.geometry <- setCulvertGeometry(depth=outlet.depression, culvert=tmp.h);
  outlet.depression.A  <- the.geometry$A;
  outlet.depression.WP <- the.geometry$WP;
  outlet.depression.TW <- the.geometry$TW;
  setnum.h("outlet.depression.A",   outlet.depression.A,  tmp.h);
  setnum.h("outlet.depression.WP",  outlet.depression.WP, tmp.h);
  setnum.h("outlet.depression.TW",  outlet.depression.TW, tmp.h);


  the.geometry <- setCulvertGeometry(depth=diameter-inlet.depression,
                                     culvert=tmp.h,
                                     depression=inlet.depression,
                                     location="inlet");
  setnum.h("Ao.inlet", the.geometry$A,       tmp.h);
  setnum.h("Po.inlet", the.geometry$WP,      tmp.h);
  setnum.h("Ro.inlet", the.geometry$HR,      tmp.h);
  setnum.h("Ko.inlet", the.geometry$KONVEY,  tmp.h);
  the.geometry <- setCulvertGeometry(depth=diameter-outlet.depression,
                                     culvert=tmp.h,
                                     depression=outlet.depression,
                                     location="outlet");
  setnum.h("Ao.outlet", the.geometry$A,      tmp.h);
  setnum.h("Po.outlet", the.geometry$WP,     tmp.h);
  setnum.h("Ro.outlet", the.geometry$HR,     tmp.h);
  setnum.h("Ko.outlet", the.geometry$KONVEY, tmp.h);

  number.of.barrels <- as.integer(number.of.barrels);
  if(number.of.barrels >= 1) {
     setnum.h("number.of.barrels", number.of.barrels, tmp.h);
  } else {
     stop("number.of.barrels is not >= 1");
  }

  projectratio <- projectlength/diameter;
  setnum.h("projectlength", projectlength, tmp.h);
  setnum.h("projectratio", projectratio,   tmp.h);
  kproj <- KPRJCT(projectratio=projectratio);
  setnum.h("kproj", kproj,                 tmp.h);

  setnum.h("rounding", rounding,           tmp.h); # cap96 variable RND
  krnd <- KRND(rnd=rounding); # FIGURE 21, TWRI p.39
  setnum.h("krnd", krnd,                   tmp.h);

  setnum.h("thetabev", thetabev,           tmp.h);
  setnum.h("beveling", beveling,           tmp.h); # cap96 variable BEV
  kbev <- KBEV(rnd=beveling, theta=thetabev); # FIGURE 22, TWRI p.40
  setnum.h("kbev", kbev,                   tmp.h);

  if(! is.null(theta.left) | ! is.null(theta.right)) {
     theta <- NULL;
     if(is.null(theta.left))  theta.left  <- theta.right;
     if(is.null(theta.right)) theta.right <- theta.left;
     ktheta <- c(KWINGWALL(theta.left), KWINGWALL(theta.right))
     # TWRI p.42 says to compute two C values and then mean the C values
     # so, we will track a vector of kthetas.
     if(length(ktheta) == 2) {
        if(ktheta[1] == ktheta[2]) ktheta <- ktheta[1]
     }
     setnum.h("theta", theta,              tmp.h);
     setnum.h("theta.left", theta.left,    tmp.h);
     setnum.h("theta.right", theta.right,  tmp.h);
     setnum.h("ktheta", ktheta,            tmp.h);
  } else {
     setnum.h("theta", theta,              tmp.h);
     ktheta <- KWINGWALL(theta); # FIGURE 24, TWRI p.42
     setnum.h("ktheta", ktheta,            tmp.h);
  }


  setnum.h("TMP.Q.previous.run", 0,             tmp.h);
  setnum.h("TMP.Qroad.previous.run", 0,         tmp.h);
  setnum.h("TMP.terminal.area.previous.run", 0, tmp.h);
  setnum.h("TMP.konvey2.previous.run", 0,       tmp.h);
  setnum.h("TMP.key", "",                       tmp.h);


  # The following three TMP storage variables contain the original
  # settings of the respective variables. These are stored so that
  # computeFlow() can recover the originals incase of a submerged outlet
  # and nonsubmerged inlet in which the lengths and the computation reference
  # datum (zdsinvert) is needed.
  setnum.h("TMP.Ltop", Ltop,           tmp.h);
  setnum.h("TMP.Lbot", Lbot,           tmp.h);
  setnum.h("TMP.zdsinvert", zdsinvert, tmp.h);

  setlog.h("is.topedge.square", is.topedge.square, tmp.h);
  setlog.h("is.bellmouthed",    is.bellmouthed,    tmp.h);
  setlog.h("is.tonguegroove",   is.tonguegroove,   tmp.h);
  setlog.h("is.barrel.rough",   is.barrel.rough,   tmp.h);

  setlog.h("use.datums.when.graphing",      use.datums.when.graphing,      tmp.h);
  setlog.h("ignore.approach.losses",        ignore.approach.losses,        tmp.h);
  setlog.h("ignore.approach.velocity.head", ignore.approach.velocity.head, tmp.h);

  if(fraction.of.approach.area > 1 || fraction.of.approach.area < 0) {
     stop("bad fraction.of.approach.area, must be in [0,1]");
  } else {
     setnum.h("fraction.of.approach.area", fraction.of.approach.area, tmp.h);
  }

  return(tmp.h);
}

"resetTMPvarsCulvert" <-
function(culvert) {
   set.h("TMP.Q.previous.run", 0,             culvert);
   set.h("TMP.Qroad.previous.run", 0,         culvert);
   set.h("TMP.terminal.area.previous.run", 0, culvert);
   set.h("TMP.konvey2.previous.run", 0,       culvert);
}


"changeBevelingCulvert" <-
function(culvert, bevs=list(beveling=0, thetabev=45)) {
   old.thetabev <- culvert$thetabev;
   old.beveling <- culvert$beveling;
   zz <- list(beveling=old.beveling, thetabev=old.thetabev, kbev=culvert$kbev);
   new.thetabev <- bevs$thetabev;
   new.beveling <- bevs$beveling;
   setnum.h("thetabev", new.thetabev, culvert);
   setnum.h("beveling", new.beveling, culvert);
   setnum.h("kbev",     KBEV(rnd=new.beveling, theta=new.thetabev), culvert); # FIGURE 22, TWRI p.40
   return(zz)
}



"changeRoundingCulvert" <-
function(culvert, rnd=0) {
   old.rounding <- culvert$rounding;
   zz <- list(rounding=old.rounding, krnd=culvert$krnd);
   new.rounding <- rnd;
   setnum.h("rounding", new.rounding, culvert);
   setnum.h("krnd",     KRND(rnd=new.rounding), culvert);  # FIGURE 21, TWRI p.39
   return(zz)
}
