"getCriticalSlope" <-
function(discharge, g=32.2, secfunc=NULL, ...) {
  afunc <- function(d, ...) {
    sec <- secfunc(d, ...);
    LHS <- discharge/g;
    RHS <- sec$A^3/sec$TW;
    return(LHS - RHS);	
  }
  rt <- uniroot(afunc, interval=c(0,searchMax), ...);
  return(rt$root);
}
