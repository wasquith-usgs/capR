"SpecificEnergy" <-
function(depth, alpha=1, velocity=0, g=32.2) {
  return(depth + alpha*velocity^2/(2*g));
}
