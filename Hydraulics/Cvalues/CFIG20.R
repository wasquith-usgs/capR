"CFIG20" <-
function(hzD=NULL) {
  return(0.88821+(0.21047-0.29299*hzD+0.078988*hzD*hzD)*hzD);
}

# do not forget to adjust for projection (KPROJ.R)
#C           adjust for projection if corrugated metal pipe

# FROM culrat.f
#C           pipe culverts with square entrance, flush with vertical headwall
#            C=0.88821+(0.21047-0.29299*H+0.078988*H*H)*H
