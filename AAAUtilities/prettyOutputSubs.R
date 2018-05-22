"splashbars" <-
function(bar="-") {
  bars <- paste(rep(bar, options()$width), sep="", collapse="");
  catme(bars);
}

"capRheader" <-
function(approach=NULL, splash=TRUE) {
  splashbars();
  catme("    capR (TWRI Culvert Analysis Program in R)\n",
        "   by William H. Asquith, USGS, Lubbock, Texas");
  if(splash) splashbars();
  catme("Culvert system at", approach$station.id, approach$station.name);
  if(splash) splashbars();
}


"printCvalueSummary" <-
function(flow=NULL, culvert=NULL, splash=TRUE, verbose=FALSE, ...) {
  if(splash) splashbars();
  catme("kbev: ",culvert$kbev);
  if(splash) splashbars();
}


"printFlow" <-
function(type, h1=h1, h4=h4, flow=NULL, culvert=NULL, approach=NULL,
         splash=TRUE, verbose=FALSE, datetime="not provided", ...) {

  if(! flow$valid) {
    catme("FLOW IS NOT VALID, assume discharge = 0 and continue with output.");
    printApproach(discharge=0, approach=approach,
                  culvert=culvert, splash=splash);
    return(NA);
  }

  lunits <- culvert$lengthunits;
  qunits <- culvert$flowunits;
  aunits <- culvert$areaunits;

  Qculvert <- flow$Q; # extract the culvert flow
  Qroad <- 0;
  if(length(flow$roadflow$is.flow) && flow$roadflow$is.flow) {
     Qroad <- flow$roadflow$Qroad;
  }
  Qtotal   <- Qculvert + Qroad;

  catme("CULVERT FLOW SUMMARY:",culvert$name);
  catme("   Date of data",datetime);
  if(type == "SAC") {
    catme(" Type is SAC or 2-section slope-area based on horizontal\n",
          "projection of h1 and h4 into the inlet and outlet, respectively."); 
    catme(" Flow is",Qculvert, qunits,"Is this a reliable estimate?\n");
    catme(" Headwater (h1, gage height, no datum offset) is", h1, lunits, "\n",
          "Tailwater (h4, gage height, no datum offset) is", h4, lunits);
    catme(" -------");
    printTypeSAC(flow=flow, culvert=culvert, ...);  
  } else {
    err <- flow$error;
    catme(" Type", type,"culvert discharge is", Qculvert, qunits, "\n",
          "     with error=", round(err, digits=6), qunits,
          "in", flow$its, "iterations.");
    catme(" Discharge coefficient (C) is", flow$C, "dimensionless");
    catme(" Headwater (h1, gage height, no datum offset) is", h1, lunits, "\n",
          "Tailwater (h4, gage height, no datum offset) is", h4, lunits);
    catme(" Total head (H) is", round(flow$H, digits=3), lunits);
    catme(" -------");
    switch(type,
           printType1(flow=flow, culvert=culvert, ...),
           printType2(flow=flow, culvert=culvert, ...),
           printType3(flow=flow, culvert=culvert, ...),
           printType4(flow=flow, culvert=culvert, ...),
           printType5(flow=flow, culvert=culvert, ...),
           printType6(flow=flow, culvert=culvert, ...));
    if(culvert$ignore.approach.velocity.head) {
       catme("  Culvert is configured to ignore approach velocity head (v1head).");
    } else {
       catme("  Culvert is configured to use approach velocity head (v1head).");
    }
    if(culvert$ignore.approach.losses) {
       catme("  Culvert is configured to ignore approach losses (hf12).");
    } else {
       catme("  Culvert is configured to use approach losses (hf12).");
    }
  }
  printApproach(discharge=Qtotal, approach=approach,
                culvert=culvert, splash=splash);

  if(length(flow$roadflow$is.flow) && flow$roadflow$is.flow) {
     printRoadFlow(flow$roadflow, culvert=culvert, splash=splash);
     if(splash) splashbars();
     catme("TOTAL FLOW (Qculvert + Qroad) is",
            round(Qtotal, digits=4), qunits);
  }

  if(splash) splashbars();
}

