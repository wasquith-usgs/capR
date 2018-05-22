"getCforType4" <-
function(culvert=NULL) {

   if(! is.null(culvert$C4master)) return(culvert$C4master);

   type  <- culvert$type;
   inlet <- culvert$inlet;
   rnd   <- culvert$rounding;
   bev   <- culvert$beveling;
   if(bev > rnd) rnd <- bev;

   kproj <- culvert$kproj
   # TODO kwr????
   if(is.null(culvert$theta)) {
     C1 <- QC46(type=type, kwr=1, rnd=rnd, theta=culvert$theta.left, inlet=inlet, kproj=kproj);
     C2 <- QC46(type=type, kwr=1, rnd=rnd, theta=culvert$theta.right, inlet=inlet, kproj=kproj);
     C <- mean(C1, C2)
   } else {
     C <- QC46(type=type, kwr=1, rnd=rnd, theta=culvert$theta, inlet=inlet, kproj=kproj);
   }
   return(absMaxC(C));
}

