"getCforType2" <-
function(culvert=NULL, approach=NULL, hzD=NULL, A1=0, Ac=0) {

  if(! is.null(culvert$C2master)) return(culvert$C2master);

  return(getCforTypes123(culvert=culvert, approach=approach,
                         froude=1, hzD=hzD, A1=A1, Ac=Ac,
                         attempted.flow.type="2"));
}

