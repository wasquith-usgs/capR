"piezoLevelRatioType6" <-
function(discharge=NULL, culvert=NULL,
         supportedjet=FALSE) { # FROM culvertd.for of FEQ
   g    <- culvert$gravity;
   D    <- culvert$diameter;
   Ao   <- culvert$Ao.inlet;
   type <- culvert$type;
  
   if(g > 15) { # OK IN ENGLISH
     a.factor <- 1;
   } else { # NOPE IN SI
      a.factor <- 1.81128; # 3.2808^3/(3.2808^(5/2));
   }
   
   if(type == "box") { # ratio is dimensionless **NO* correction (see below)
     RAT <- discharge/(Ao*sqrt(g*D));
     if(RAT < 1) {
        return(1.0 - 0.2*RAT);
     } else if(RAT < 5.0) {
        return(0.875 - 0.075*RAT);
     } else {
        return(0.5);
     }
   }

   # ALL OTHER CLASSES TREATED LIKE A PIPE CULVERT
   # ESTIMATE THE EQUIVALENT NUMBER OF PIPES OF DIAMETER D
   # TO ADJUST THE FLOW RATE
   NE <- 4*Ao/(pi*D^2);
   QE <- discharge/NE;
   RAT <- a.factor*QE/D^(5/2); # Figure 18 of TWRI **NOT** dimensionless
   # in the horizontal axis--therefore, use a correction
   
   if(RAT < 1.0) RAT <- 1.0;
 
   if(supportedjet) { # Discharge is supported.
      if(RAT <= 2.0) {
         return(1.0 - 0.1*(RAT - 1.0));
      } else if(RAT <= 3.0) {
         return(1.01 - 0.055*RAT);
      } else if(RAT <= 4.0) {
         return(1.061 - 0.0720*RAT);
      } else if(RAT <= 5.0) {
         return(1.129 - 0.089*RAT);
      } else if(RAT <= 6.0) {
         return(0.899 - 0.043*RAT);
      } else if(RAT <= 7.0) {
         return(0.797 - 0.026*RAT);
      } else if(RAT <= 8.0) {
         return(0.72 - 0.0152364*RAT);
      } else {
         return(0.5 + 0.52788/RAT^0.80925);
      }
   } else { # Discharge is unsupported.
      if(RAT <= 2.0) {
         return(1.058 - 0.058*RAT);
      } else if(RAT <= 3.0) {
         return(1.056 - 0.057*RAT);
      } else if(RAT <= 4.0) {
         return(1.188 - 0.101*RAT);
      } else if(RAT <= 5.0) {
         return(1.240 - 0.114*RAT);
      } else if(RAT <= 6.0) {
         return(0.940 - 0.054*RAT);
      } else if(RAT <= 7.0) {
         return(0.808 - 0.032*RAT);
      } else if(RAT <= 8.0) {
         return(0.85 - 0.038*RAT);
      } else {
        return(0.5 + 1.560/RAT^1.546);
      }
   }
}



