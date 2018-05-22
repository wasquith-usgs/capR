"getCforType6" <-
function(culvert=NULL) {

   if(! is.null(culvert$C6master)) return(culvert$C6master);

   type  <- culvert$type;
   inlet <- culvert$inlet;
   bev   <- culvert$beveling;
   rnd   <- culvert$rounding;
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

