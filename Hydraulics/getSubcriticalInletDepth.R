"getSubcriticalInletDepth" <-
function(discharge, culvert=NULL, C=NULL, h1=NULL, dc=dc,
         v1head=NULL, h12=NULL, v3=NULL, searchMax=100, ...) {

  #catme("\n getSubcriticalInletDepth===============");

  z <- culvert$z;
  g <- culvert$gravity;
  D <- culvert$diameter;
  g2 <- 2*g; CC <- C^2;

  LHS <- h1 - z + v1head - (1/CC - 1)*v3^2/g2 - h12;

  depression <- culvert$inlet.depression;
  #catme("discharge=",discharge);
  #catme("LHS=",LHS);
  "afunc" <- function(depth) {
    sec <- setCulvertGeometry(depth, culvert=culvert,
                              location="inlet",
                              depression=depression);
    A2  <- sec$A;
    v2  <- discharge/A2;
    RHS <- depth + v2^2/g2;
    #catme("depth=",depth);
    #catme("RHS=",RHS);
    return(LHS - RHS);
  }
  my.rt <- NULL;
  try(my.rt <- uniroot(afunc, interval=c(dc,D-depression)), silent=TRUE);

  d2 <- h1 - z + v1head - h12 - v3^2/(g2*CC); # TWRI p.25 V2=V3
  if(is.null(my.rt)) return(d2);

  d2 <- my.rt$root;
  return(d2);
}
