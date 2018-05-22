"CFIG19" <- 
function(C, areac=NULL, area1) {
  if(areac >= 0.2*area1 & area1 != 0) {
    return( 1.25 * (C - 0.196 + (areac * (0.98 - C)/area1)) );
  }
  return(C);
}


# FROM culrat.f
#C     adjustment for channel contraction
#      IF(AC.GE.0.2*A1.AND.A1.NE.0.0) C=1.25*(C-0.196+(AC*(0.98-C)/A1))
