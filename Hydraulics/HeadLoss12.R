"HeadLoss12" <-
function(discharge, culvert=NULL, approach=NULL,
         d2=NULL, otherL=NULL, log2tmp=FALSE, ...) {
  ifelse(is.null(otherL), L <- approach$Lapproach, L <- otherL);
  konvey1 <- approach$geometry$KONVEYTOTAL;
  geo <- setCulvertGeometry(depth=d2,
                            culvert=culvert, location="inlet",
                            depression=culvert$inlet.depression);

  # Conveyance at section 2 is the sum of the TOTAL convenyance for
  # previously estimated system, which is stored in the approach
  # plus the conveyance for the currently processed culvert MINUS
  # the conveyance for said culvert from previous run
  konvey2 <- approach$TMP.konvey2.previous.run +
             geo$KONVEY - culvert$TMP.konvey2.previous.run;
  hf12 <- L*(approach$fraction.of.flow*discharge)^2/(konvey1*konvey2);
  if(is.nan(hf12)) hf12 <- 0;

  if(log2tmp) {
    sink("TMPcapR_HeadLoss12.txt", ...);
     catme("discharge=", discharge);
     catme("length=", L);
     catme("konvey1=", konvey1);
     catme("konvey2=", konvey2);
     catme("approachTMP.konvey2.previous.run",
           approach$TMP.konvey2.previous.run);
     catme("geoKONVEY", geo$KONVEY);
     catme("culvertTMP.konvey2.previous.run",
           culvert$TMP.konvey2.previous.run);
     catme("hf12=", hf12);
    sink();
  }
  return(hf12);
}




