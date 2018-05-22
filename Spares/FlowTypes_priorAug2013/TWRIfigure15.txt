TWRIfigure15.h <- new.h();
set.h("svals",
      matrix(
      c(-0.10,  -0.10,  -0.10,  -0.10,  -0.10,  -0.10, -0.10,
        -0.10,  -0.10,  -0.10,  -0.10,  -0.10,  -0.10, -0.10,
        -0.098, -0.098, -0.098, -0.097, -0.096, -0.091, 0.09,
        -0.081, -0.078, -0.074, -0.069, -0.056,  0.0,   0.094,
        -0.058, -0.049, -0.042, -0.029,  0.0,    0.015, 0.101,
        -0.041, -0.029, -0.019,  0.0,    0.029,  0.025, 0.105,
        -0.027, -0.012,  0.0,    0.009,  0.047,  0.034, 0.108,
        -0.017,  0.0,    0.003,  0.017,  0.058,  0.040, 0.111,
        -0.005,  0.002,  0.006,  0.023,  0.061,  0.047, 0.114,
         0.0,    0.003,  0.008,  0.030,  0.064,  0.051, 0.115,
         0.002,  0.010,  0.022,  0.042,  0.069,  0.100, 0.136,
         0.003,  0.011,  0.024,  0.043,  0.070,  0.102, 0.141,
         0.004,  0.012,  0.025,  0.044,  0.070,  0.104, 0.146,
         0.006,  0.013,  0.026,  0.045,  0.071,  0.108, 0.155,
         0.069,  0.073,  0.086,  0.105,  0.114,  0.294, 0.596),
      byrow=TRUE, ncol=7),
      TWRIfigure15.h);
set.h("rndnode", c(0.0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06),
      TWRIfigure15.h);
set.h("ldnode", c( 0.0,  4.4,  4.5,  5.5,  6.9,  7.9, 8.7, 9.3,
                  10.0, 10.3, 15.0, 20.0, 25.0, 35.0, 500),
      TWRIfigure15.h);


"TWRIfigure15" <-
function(culvert=NULL) {
   D    <- culvert$diameter - culvert$inlet.depression;
   L    <- culvert$Ltop;
   So   <- culvert$So;
   rnd  <- culvert$rounding;
   bev  <- culvert$beveling;
   if(bev > rnd) rnd <- bev;

   LD <- L/D;

   svals   <- get.h("svals", TWRIfigure15.h);
   rndnode <- get.h("rndnode", TWRIfigure15.h);
   if(rnd > max(rndnode)) rnd <- max(rndnode);

   ldnode <- get.h("ldnode", TWRIfigure15.h);

   n.ldnode <- length(ldnode)
   min.ldnode <- min(ldnode); max.ldnode <- max(ldnode);
   nodes <- 1:n.ldnode; #rnds <- seq(0,0.06, by=0.001);
   fig15 <- NA;
   #catme("FIGURE15 start LD:",LD);
   #catme("FIGURE15 rnd:",rnd);
   if(LD < min.ldnode) {
     #catme("FIGURE15 LD < min.ldnode");
     fig15 <- approx(x=rndnode, y=svals[1,], xout=rnd)$y;
   } else if(LD > max.ldnode) {
     #catme("FIGURE15 LD > max.ldnode");
     fig15 <- approx(x=rndnode, y=svals[n.ldnode,], xout=rnd)$y;
   } else {
     j <- nodes[ldnode == LD];
     #catme("FIGURE15 j",j);
   	 if(length(j) != 0) {
   	   fig15 <- approx(x=rndnode, y=svals[j,], xout=rnd)$y;
   	 } else {
   	    j1 <- max(nodes[ldnode < LD]); j2 <- min(nodes[ldnode > LD]);
        #catme("FIGURE15 j1/j2",j1,j2);
        y <- c(approx(x=rndnode, y=svals[j1,], xout=rnd)$y,
               approx(x=rndnode, y=svals[j2,], xout=rnd)$y);
       #catme("FIGURE15 y",y);
       fig15 <- approx(x=ldnode[c(j1,j2)], y=y, xout=LD)$y;
     }
   }
   #catme("FIGURE15 fig15",fig15);

   ifelse(So > fig15, type <- 5, type <- 6);
   #catme("FIGURE15 type",type);
   return(type);
}
