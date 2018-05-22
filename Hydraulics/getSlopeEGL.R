"getSlopeEGL" <-
function(discharge=NULL, depth=NULL, culvert=NULL) {
  nvalue     <- culvert$nvalue;
  manningcor <- culvert$manningcor;
  slope      <- culvert$So;
  secfunc    <- culvert$geometry.func;
  sec        <- secfunc(depth, culvert=culvert);
  Se <- ( (nvalue*discharge) / (manningcor*sec$A*HR^(2/3)) )^2;
  return(Se);
}

