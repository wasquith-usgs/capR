"getDischargeAtCriticalDepth" <-
function(depth, culvert=NULL,
         location=c("inlet", "outlet"), depression=0) {
  sec <- setCulvertGeometry(depth=depth, culvert=culvert,
                            location=location, depression=depression);
  discharge <- sec$A^(3/2)*sqrt(culvert$gravity/sec$TW);
  return(discharge);
}


