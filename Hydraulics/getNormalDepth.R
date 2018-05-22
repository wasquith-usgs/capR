"getNormalDepth" <-
function(discharge, culvert=NULL,
         location=c("inlet", "outlet"), depression=0) {
        g <- culvert$gravity;
        D <- culvert$diameter;
  nvalue  <- culvert$nvalue;
  manningcor <- culvert$manningcor;
  slope   <- culvert$So;

  if(slope <= 0) return(NA);
  LHS <- nvalue*discharge/(manningcor*sqrt(slope));
  #catme("LHS=",LHS);
  "afunc" <- function(depth) {
    sec <- setCulvertGeometry(depth, culvert=culvert,
                              location=location, depression=depression);
    RHS <- sec$A*sec$HR^(2/3);
    #catme("RHS=",RHS);
    return(LHS - RHS);
  }
  my.rt <- NULL;
  try(my.rt <- uniroot(afunc, interval=c(0.001*D,D-depression)), silent=TRUE);
  ifelse(is.null(my.rt), return(NA), return(my.rt$root));
}

