"Conveyance" <-
function(A=NULL, R=NULL, nvalue=NULL, manningcor=NULL) {
  return( (manningcor/nvalue)*R^(2/3)*A );
}
