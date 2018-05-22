"getCforType5" <-
function(h1=NULL, culvert=NULL) {

   if(! is.null(culvert$C5master)) return(culvert$C5master);

   type          <- culvert$type;
   inlet         <- culvert$inlet;
   embankment    <- culvert$embankment;
   material      <- culvert$material;
   projectlength <- culvert$projectlength;
   rnd           <- culvert$rounding;
   bev           <- culvert$beveling;
   kproj         <- culvert$kproj;
   C <- NULL;

   hzD <- HeadwtrDiaRatio(h1=h1, culvert=culvert);

   # TODO not testing L/D < 6 and So < 0.03
   if(inlet == "flared") return(C5FLARED(hzD));

   Ctable6 <- C5TABLE6(hzD=hzD, rnd=culvert$rounding);
   if(is.null(culvert$theta)) {
      Ctable7 <- mean(C5TABLE7(hzD=hzD, theta=culvert$theta.left),
                      C5TABLE7(hzD=hzD, theta=culvert$theta.right));
   } else {
      Ctable7 <- C5TABLE7(hzD=hzD, theta=culvert$theta);
   }
   # Open question now, where is the wingwall correction for high-head
   # on non-box culverts?  TWRI is silent. The Ctable7 computation is
   # extra and only needed if type is box. But logic is just to get the
   # two tables computed and then deal with ramifications.
    
   if(projectlength > 0 &
        (material == "corrugated-metal" |
             type == "pipearch")) {
      C <- Ctable6*kproj;
      return(absMaxC(C));
   }
   # Note the above test is from TWRI p. 44 and this implies a 
   # projection correction is not applicable to circular concrete
   # pipes. This subroutine does not apply kproj in the concrete
   # circular case. (Should it?)  CAPGUI appears to do this.

   if(inlet == "mitered" & embankment == "slope") {
      C <- Ctable6*kproj*0.92; # TWRI p. 44
      return(absMaxC(C));
   }

   if(type == "box") {
      C7 <- Ctable7;
      if(rnd != 0 | bev != 0) {
        C6 <- Ctable6;
        ifelse(C6 < C7, C <- C7, C <- C6);
        return(absMaxC(C));
      }
      return(absMaxC(C7));
   }
   return(absMaxC(Ctable6));
}

