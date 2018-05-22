"CFIG23" <-
function(froude=NULL) {
  if(froude == 1) return(0.95)
  return( 0.71364867 + froude*(0.38017909 - 0.14345278*froude) );
}

# FROM culrat.f
#C           box culverts - equation from fig.23 in TWRI
#            F=CQ/(AC*SQRT(D3*32.2))
#            C=0.71364867+F*(0.38017909-0.14345278*F)
