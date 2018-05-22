"getCforTypes123" <-
function(culvert=NULL, approach=NULL, froude=NULL, hzD=NULL, A1=0, Ac=0, 
         attempted.flow.type="not given") {

  #catme("attempted.flow.type=",attempted.flow.type);

  Ac <- Ac - culvert$TMP.terminal.area.previous.run + 
             approach$TMP.terminal.area.previous.run;

  type       <- culvert$type;
  inlet      <- culvert$inlet;
  embankment <- culvert$embankment;
  material   <- culvert$material;

  beveling <- culvert$beveling;

  krnd    <- culvert$krnd;
  kbev    <- culvert$kbev;
  kproj   <- culvert$kproj;
  kthetas <- culvert$ktheta;

  if(culvert$is.bellmouthed |
     culvert$is.tonguegroove) return(Cbelltonguegroove()); #TWRI p.40

  C <- sapply(kthetas, function(ktheta) {
    Cbase <- NULL;

    if(type == "circle" | type == "ellipse") {
       #catme("circle or ellipse");
       if(inlet == "mitered" & embankment == "slope") {
         Cbase <- CFIG25(hzD=hzD);       # TWRI Figure 25
         C <- Cbase*kproj;               # TWRI p. 42
       } else {
         Cbase <- CFIG20(hzD=hzD);       # TWRI Figure 20
         #catme("Cbase:",Cbase)
         #catme("krnd, kbev, ktheta:",krnd,kbev,ktheta)
         C <- Cbase*krnd*kbev;           # TWRI p. 39 (NO Ktheta, p. 41)
         if(beveling == 0) C <- C*kproj; # TWRI p. 42
       }
    } else if(type == "box") {
       #catme("box");
       Cbase  <- CFIG23(froude);         # TWRI Figure 23
       #catme("C123, box, Cbase",Cbase);
       C <- Cbase*krnd*kbev*ktheta;      # TWRI p. 41
       #catme("Cbase(froude):",Cbase,"(",froude,")")
       #catme("krnd, kbev, ktheta:",krnd,kbev,ktheta)
    } else {
       #catme("all else");
       C <- 0.95*krnd*kbev*ktheta*kproj; # ALL ELSE?
    }
    #catme("C (before contraction):",C);

    # A1 is area of approach section
    # Ac is "terminal" area
    # 1.25 is 1/0.80
    # finally adjust for contraction
    #catme("A1",A1);
    #catme("Ac",Ac);
    # Note that the implemented correction adjustment is not the
    # same as the one in the TWRI.
    if(A1 != 0 & Ac >= 0.2*A1) { # FROM Cap96.08c
       #catme("C:",C);
       #m <- 1 - Ac/A1; # TWRI p.38
       #C <- 0.98 - (0.98 - C)*m/0.80 # TWRI p.38
       #catme("C (aftercontraction):",C);
       C <- 1.25*(C - 0.196 + (Ac*(0.98-C)/A1) ); # FROM Cap96.08c
       #catme("C (fortran):",C);
    }
    return(C)
  })
  C <- mean(C) # TWRI P. 41
  return(absMaxC(C))
}

