"Froude" <-
function(discharge=NULL, area=NULL, topwidth=NULL, alpha=1, g=NULL) {
  velocity = discharge/area;
  hydraulicdepth = area/topwidth;
  return(velocity/sqrt(g*hydraulicdepth/alpha));
}


