"TWRIfigure16" <-
function(h1=NULL, culvert=NULL) {
   D      <- culvert$diameter - culvert$inlet.depression;
   L      <- culvert$Ltop;
   So     <- culvert$So;
   g      <- culvert$gravity;
   z      <- culvert$z;
   Ro     <- culvert$Ro.inlet;
   nvalue <- culvert$nvalue;
   manningcor <- culvert$manningcor;


   rnd  <- culvert$rounding;
   bev  <- culvert$beveling;
   if(bev > rnd) rnd <- bev;

   if(rnd > 0.03) rnd <- 0.03;

   LD <- L/D;
   if(LD > 500) LD <- 500;
   #catme("FIGURE16 LD",LD);
   
   rndnode <- c(0, 0.01, 0.02, 0.03);
   
   X <- (2*g*nvalue^2*(h1-z)) / (manningcor^2 * Ro^(4/3));
   Xnode <- c(0, 0.10, 0.15, 0.20, 0.25, 0.30, 0.60);
   if(X > max(Xnode)) X <- max(Xnode);
   #catme("FIGURE16 X",X);

   SLOPES0.00 <- c(-0.037, -0.033, -0.031, -0.029, -0.027, -0.025, -0.013,
                   -0.015, -0.011, -0.009, -0.007, -0.005,  0.0,    0.030,
                   -0.012, -0.006, -0.003, -0.002,  0.0,    0.006,  0.041,
                   -0.011, -0.005,  0.002,  0.0,    0.002,  0.008,  0.044,
                   -0.006, -0.003,  0.0,    0.001,  0.003,  0.010,  0.048,
                   -0.004,  0.0,    0.002,  0.004,  0.007,  0.014,  0.054,
                   -0.001,  0.003,  0.005,  0.009,  0.013,  0.021,  0.068,
                      0.0,  0.010,  0.015,  0.024,  0.033,  0.044,  0.109,
                    0.002,  0.016,  0.025,  0.039,  0.053,  0.067,  0.151,
                    0.007,  0.019,  0.032,  0.048,  0.065,  0.082,  0.184,
                    0.010,  0.020,  0.035,  0.052,  0.069,  0.086,  0.188,
                    0.012,  0.020,  0.036,  0.053,  0.071,  0.089,  0.197,
                    0.015,  0.025,  0.040,  0.060,  0.090,  0.120,  0.250);


   svals0.00  <- matrix(SLOPES0.00, byrow=TRUE, ncol=7);
   ldnode0.00 <- c( 0.0,  5.5,  6.8,  7.2,  7.6, 8.4, 10.0,
                  15.0, 20.0, 25.0, 30.0, 35.0, 500.0);
               
               
   SLOPES0.01 <- c(-0.095, -0.082, -0.076, -0.069, -0.063, -0.056, -0.017,
                   -0.041, -0.028, -0.022, -0.017, -0.012,  0.0,    0.041,
                   -0.028, -0.016, -0.010, -0.006,  0.0,    0.013,  0.054,
                   -0.022, -0.010, -0.004,  0.0,    0.007,  0.016,  0.060,
                   -0.018, -0.006,  0.0,    0.050,  0.010,  0.019,  0.064,
                   -0.012,  0.0,    0.006,  0.010,  0.015,  0.023,  0.071,
                    0.004,  0.010,  0.013,  0.017,  0.022,  0.030,  0.078,
                    0.008,  0.018,  0.023,  0.033,  0.042,  0.053,  0.119,
                    0.010,  0.023,  0.033,  0.047,  0.060,  0.073,  0.151,
                    0.011,  0.027,  0.040,  0.056,  0.072,  0.088,  0.166,
                    0.013,  0.028,  0.043,  0.060,  0.077,  0.094,  0.194,
                    0.014,  0.028,  0.044,  0.061,  0.080,  0.099,  0.213,
                    0.025,  0.037,  0.053,  0.075,  0.102,  0.132,  0.283);

   svals0.01  <- matrix(SLOPES0.01, byrow=TRUE, ncol=7);
   ldnode0.01 <- c( 0.0,  5.3,  6.5,  7.1,  7.5, 8.1, 10.0,
                   15.0, 20.0, 25.0, 30.0, 35.0, 500.0);
                   
   
   SLOPES0.02 <- c(-0.357, -0.301, -0.273, -0.246, -0.218, -0.190, -0.024,
                   -0.146, -0.110, -0.089, -0.065, -0.036,  0.0,    0.037,
                   -0.104, -0.072, -0.052, -0.029,  0.0,    0.020,  0.057,
                   -0.070, -0.042, -0.022,  0.0,    0.015,  0.027,  0.061,
                   -0.045, -0.019,  0.0,    0.012,  0.021,  0.027,  0.069,
                   -0.024,  0.0,    0.012,  0.020,  0.026,  0.033,  0.075,
                    0.013,  0.021,  0.025,  0.030,  0.035,  0.044,  0.098,
                    0.017,  0.031,  0.038,  0.045,  0.055,  0.066,  0.132,
                    0.020,  0.036,  0.047,  0.060,  0.074,  0.088,  0.172,
                    0.023,  0.039,  0.053,  0.069,  0.085,  0.101,  0.197,
                    0.025,  0.041,  0.056,  0.074,  0.092,  0.110,  0.218,
                    0.026,  0.041,  0.057,  0.075,  0.093,  0.111,  0.219,
                    0.035,  0.048,  0.067,  0.090,  0.113,  0.143,  0.317);
   svals0.02  <- matrix(SLOPES0.02, byrow=TRUE, ncol=7);
   ldnode0.02 <- c( 0.0,  5.0,  6.0,  6.8,  7.4, 7.9, 10.0,
                   15.0, 20.0, 25.0, 30.0, 35.0, 500.0);

   SLOPES0.03 <- c(-1.121, -0.920, -0.820, -0.720, -0.619, -0.519, 0.083,
                   -0.498, -0.387, -0.308, -0.232, -0.160,  0.0,   0.090,
                   -0.280, -0.200, -0.128, -0.061,  0.0,    0.033, 0.093,
                   -0.202, -0.133, -0.064,  0.0,    0.023,  0.037, 0.094,
                   -0.124, -0.067,  0.0,    0.023,  0.029,  0.041, 0.095,
                   -0.046,  0.0,    0.023,  0.039,  0.036,  0.045, 0.096,
                    0.006,  0.022,  0.030,  0.035,  0.041,  0.049, 0.097,
                    0.032,  0.040,  0.044,  0.049,  0.055,  0.062, 0.104,
                    0.038,  0.050,  0.056,  0.064,  0.074,  0.084, 0.144,
                    0.035,  0.055,  0.065,  0.079,  0.093,  0.107, 0.191,
                    0.032,  0.060,  0.074,  0.088,  0.102,  0.116, 0.200,
                    0.028,  0.060,  0.076,  0.092,  0.108,  0.124, 0.220,
                    0.045,  0.060,  0.080,  0.105,  0.125,  0.155, 0.350);
   svals0.03  <- matrix(SLOPES0.03, byrow=TRUE, ncol=7);
   ldnode0.03 <- c( 0.0,  4.0,  5.4,  5.9,  6.4, 6.9, 7.5, 10.0,
                   15.0, 20.0, 27.5, 35.0, 500.0);
   
   
   "getfromFigs16ABCD" <-
   function(LD=NULL, X=NULL, svals=NULL, ldnode=NULL, Xnode=NULL) {
      n.ldnode <- length(ldnode)
      min.ldnode <- min(ldnode); max.ldnode <- max(ldnode);
      nodes <- 1:n.ldnode;
      if(LD < min.ldnode) {
        #catme("FIGURE16 LD < min.ldnode");
        fig16 <- approx(x=Xnode, y=svals[1,], xout=X)$y;
      } else if(LD > max.ldnode) {
        #catme("FIGURE16 LD > max.ldnode");
        fig16 <- approx(x=Xnode, y=svals[n.ldnode,], xout=X)$y;
      } else {
        j <- nodes[ldnode == LD];
        #catme("FIGURE16 j",j);
   	    if(length(j) != 0) {
   	      fig16 <- approx(x=Xnode, y=svals[j,], xout=X)$y;
   	    } else {
   	      j1 <- max(nodes[ldnode < LD]); j2 <- min(nodes[ldnode > LD]);
          #catme("FIGURE16 j1/j2",j1,j2);
          #catme("FIGURE16 svals[j1,]",svals[j1,]);
          #catme("FIGURE16 svals[j2,]",svals[j2,]);
          #catme("FIGURE16 Xnode",Xnode);
          y <- c(approx(x=Xnode, y=svals[j1,], xout=X)$y,
                 approx(x=Xnode, y=svals[j2,], xout=X)$y);
         #catme("FIGURE16 y",y);
         fig16 <- approx(x=ldnode[c(j1,j2)], y=y, xout=LD)$y;
        }
      }
      return(fig16);
   }
  
   
 
   slope0.00 <- getfromFigs16ABCD(LD=LD, X=X, svals=svals0.00, ldnode=ldnode0.00, Xnode=Xnode);
   slope0.01 <- getfromFigs16ABCD(LD=LD, X=X, svals=svals0.01, ldnode=ldnode0.01, Xnode=Xnode);
   slope0.02 <- getfromFigs16ABCD(LD=LD, X=X, svals=svals0.02, ldnode=ldnode0.02, Xnode=Xnode);
   slope0.03 <- getfromFigs16ABCD(LD=LD, X=X, svals=svals0.03, ldnode=ldnode0.03, Xnode=Xnode);
   #catme("FIGURE16 slope0.00",slope0.00);
   #catme("FIGURE16 slope0.01",slope0.01);
   #catme("FIGURE16 slope0.02",slope0.02);
   #catme("FIGURE16 slope0.03",slope0.03);
 
   fig16 <- approx(x=rndnode,
                   y=c(slope0.00, slope0.01, slope0.02, slope0.03),
                   xout=rnd)$y; 
  
   #catme("FIGURE16 fig16",fig16);
   ifelse(So > fig16, type <- 5, type <- 6);
   #catme("FIGURE16 type",type);
   return(type);
}
