"getCriticalDepth" <-
function(discharge, culvert=NULL, min.depth=NA, max.depth=NA,
         location=c("inlet", "outlet"), depression=0) {
  g <- culvert$gravity;
  D <- culvert$diameter;
  LHS <- discharge^2/g;
  LHS <- discharge;
  #catme("LHS=",LHS);
  "afunc" <- function(depth) {
    sec <- setCulvertGeometry(depth, culvert=culvert,
                              location=location, depression=depression);
    #if(TW == 0) TW <- 0.0001*D;
    RHS <- sqrt(g*sec$A^3/sec$TW);
    #catme("RHS=",RHS," depth=",depth+depression,
    #      " and A=",sec$A," and TW=",sec$TW);
    return(LHS - RHS);
  }

  ifelse(is.na(max.depth), right <- D-depression, right <- max.depth);
  ifelse(is.na(min.depth), left  <- 0.001*right,  left  <- min.depth);

  my.rt <- NULL;
  try(my.rt <- uniroot(afunc, interval=c(left, right)), silent=TRUE);
  ifelse(is.null(my.rt), return(NA), return(my.rt$root));
}

