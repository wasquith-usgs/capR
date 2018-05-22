"getCforType1" <-
function(culvert=NULL, approach=NULL, hzD=NULL, A1=0, Ac=0) {

  if(! is.null(culvert$C1master)) return(culvert$C1master);

  return(getCforTypes123(culvert=culvert, approach=approach,
                         froude=1, hzD=hzD, A1=A1, Ac=Ac,
                         attempted.flow.type="1"));
}

