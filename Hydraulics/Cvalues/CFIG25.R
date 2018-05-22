"CFIG25" <-
function(hzD=NULL) {
  return(0.73620+(0.54049-0.49769*hzD+0.089097*hzD*hzD)*hzD);
}

# do not forget to adjust for projection (KPROJ.R)
#C           adjust for projection if corrugated metal pipe

# FROM culrat.f
#C           mitered entrance, flush with sloping embankment fig.25 TWRI
#            C=0.73620+(0.54049-0.49769*H+0.089097*H*H)*H
