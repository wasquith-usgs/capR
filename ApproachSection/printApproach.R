"printApproach" <-
function(discharge=0, approach=NULL, culvert=NULL,
         splash=TRUE, verbose=FALSE, ...) {

  lunits <- culvert$lengthunits;
  aunits <- culvert$areaunits;
  vunits <- culvert$velunits;
  qunits <- culvert$flowunits;
  
  g <- culvert$gravity;
  g2 <- 2*g;
  
  if(splash) splashbars();

  catme("APPROACH SUMMARY");
  catme("  Approach Attributes");
  catme("    Approach is", approach$Lapproach, lunits,
        "upstream of inlet, and normal-to-inlet the approach");
  catme("      conveys a", approach$fraction.of.flow, "fraction of total flow.",
        "The Manning's n-value is", approach$nvalue, "dimensionless.");
      
  geo <- approach$geometry;
  vel <- approach$fraction.of.flow*discharge/geo$AREATOTAL;
  
  Fr <- Froude(discharge=approach$fraction.of.flow*discharge,
               area=geo$AREATOTAL, topwidth=geo$TW,
               alpha=geo$alpha, g=g);
  flow.regime <- ifelse(Fr >= 1, "supercritical.", "subcritial.");
  
  catme("  Summary of Approach Geometry");
  catme("    Cross-sectional area is", round(geo$AREATOTAL,  digits=3),aunits,
        "for height (gage height) of",geo$HEIGHT, lunits);
  catme("    Hydraulic depth (A/TW) is",  round(geo$DBAR, digits=3),lunits,
        "for topwidth of",                round(geo$TW, digits=2),lunits);
  catme("    Hydraulic radius (A/WP) is", round(geo$HR, digits=3),lunits,
        "for wetted perimeter of",        round(geo$WP, digits=2),lunits);
  catme("    Conveyance is",              round(geo$KONVEYTOTAL, digits=0),
        qunits);
  catme("    Velocity is", round(vel, digits=3),vunits,
        "considering the fraction of flow.");
  catme("    Velocity head with alpha of", round(geo$alpha, digits=2),
       "is", round(geo$alpha*vel^2/g2, digits=3),lunits,
       "considering the fraction of flow.");
  catme("    Froude number is", round(Fr,digits=2),
        "dimensionless; therefore, flow is", flow.regime);
}

