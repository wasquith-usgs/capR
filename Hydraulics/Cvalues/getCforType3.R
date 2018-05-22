"getCforType3" <-
function(culvert=NULL, approach=NULL, froude=NULL, hzD=NULL, A1=0, Ac=0) {

  if(! is.null(culvert$C3master)) return(culvert$C3master);

  return(getCforTypes123(culvert=culvert, approach=approach,
                         froude=froude, hzD=hzD, A1=A1, Ac=Ac,
                         attempted.flow.type="3"));
}

