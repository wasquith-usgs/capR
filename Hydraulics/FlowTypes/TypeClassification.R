"TypeClassification" <-
function(h1=NULL, h4=NULL, culvert=NULL, ...) {

  h1 <- h1 - culvert$zdsinvert;
  h4 <- h4 - culvert$zdsinvert;

  if(h1 <= h4) stop("Upstream depth is <= downstream depth");

  d3 <- culvert$diameter - culvert$outlet.depression;
  HzD <- HeadwtrDiaRatio(h1=h1, culvert=culvert);
  if(h4/d3 > 1) {
     if(HzD >= 1.5)  return(c(4));
     So <- culvert$So;
     zdsinvert <- culvert$zdsinvert;
     if(So == 0) return(c(3)); # trapping division by zero
     L.prime <- (h4 - d3)/So;
     Ltop <- culvert$Ltop;
     Lbot <- culvert$Lbot;
     Ltop.new <- Ltop - L.prime;
     Lbot.new <- Lbot - L.prime;
     zdsinvert.new <- culvert$zdsinvert + L.prime*So;
     if(Ltop.new > 0 && Ltop.new < Ltop &&
        Lbot.new > 0 && Lbot.new < Lbot &&
        zdsinvert.new < zdsinvert) {
        culvert$Ltop <- Ltop.new;           # DANGER modifying the culvert
        culvert$Lbot <- Lbot.new;           # DANGER modifying the culvert
        culvert$zdsinvert <- zdsinvert.new; # DANGER modifying the culvert
     }
     #catme("new Ltop",Ltop.new);
     #catme("new Lbot",Lbot.new);
     #catme("new zdsinvert", zdsinvert.new);
     #catme("submerged outlet, but not inlet\n\n");
     return(c(1,3));
  }
  # remainder imply h4/d2 <= 1
  if(HzD <  1.5) return(c(1,2,3));
  if(HzD >= 1.5) return(c(5,6  ));
}
# TWRI p.3
