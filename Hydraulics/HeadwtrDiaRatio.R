"HeadwtrDiaRatio" <-
function(h1=NULL, culvert=NULL, use.depression=TRUE) {
  z <- culvert$z;
  D <- culvert$diameter;
  if(use.depression) D <- D - culvert$inlet.depression;
  return((h1 - z)/D);
}
