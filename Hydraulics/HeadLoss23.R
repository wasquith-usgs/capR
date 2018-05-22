"HeadLoss23" <-
function(discharge, culvert=NULL, d2=NULL, d3=NULL,
         otherL=NULL, log2tmp=FALSE, ...) {
  ifelse(is.null(otherL), L <- culvert$Ltop, L <- otherL);
  geo2 <- setCulvertGeometry(depth=d2,
                             culvert=culvert, location="inlet",
                             depression=culvert$inlet.depression);
  geo3 <- setCulvertGeometry(depth=d3,
                             culvert=culvert, location="outlet",
                             depression=culvert$outlet.depression);
  konvey2 <- geo2$KONVEY;
  konvey3 <- geo3$KONVEY;
  hf23 <- L*discharge^2/(konvey2*konvey3);
  if(is.nan(hf23)) hf23 <- 0;
  if(log2tmp) {
     sink("TMPcapR_HeadLoss23.txt", ...);
        catme("discharge=",discharge);
        catme("length=",L);
        catme("konvey2=",konvey2);
        catme("konvey3=",konvey3);
        catme("hf23=",hf23);
     sink();
  }
  return(hf23);
}


