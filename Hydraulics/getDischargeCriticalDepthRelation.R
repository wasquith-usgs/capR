"getDischargeCriticalDepthRelation" <-
function(culvert=NULL, location=c("inlet", "outlet")) {
   ifelse(location == "inlet", the.depression <- culvert$inlet.depression,
                               the.depression <- culvert$outlet.depression);
   Doff <- culvert$diameter - the.depression;
   dme  <- seq(0.001*Doff, Doff, by=.001);
   Qme  <- sapply(dme, getDischargeAtCriticalDepth, culvert=culvert,
                 location="inlet", depression=the.depression);
   minQ <- min(Qme);
   dc.at.minQ <- dme[Qme == minQ];
   return(list(dc=dme, Qc=Qme, minQ.at.dc=minQ, dc.at.minQ=dc.at.minQ));
}

