"demo.TWRI.type1" <-
function(example=c("1","2")) {
  example <- match.arg(example);
  if(example == 1) { # EXAMPLE 1 IN TWRI
     US.depth <- 12;
     DS.depth <- 6;
     my.approach <- setApproach(Lapproach=10,
                                X=c(0,0,200,200), Y=c(15,0,0,15));
     my.culvert <- setCulvert(name="TWRI EXAMPLE 1",
                              type="circle",
                              material="corrugated-metal",
                              diameter=10,
                              nvalue=0.024,
                              rounding=0.006,
                              slope=0.02,
                              L=100);
    flow <- computeFlow(culvert=my.culvert, approach=my.approach,
                        h1=US.depth, h4=DS.depth);
    # 729 cfs in TWRI
    # 725 cfs in capR
  } else { # EXAMPLE 2 IN TWRI
    US.depth <- 10;
    DS.depth <- 4;
    my.approach <- setApproach(Lapproach=20,
                               X=c(0,0,200,200), Y=c(15,0,0,15));
    my.culvert <- setCulvert(name="TWRI EXAMPLE 2",
                             type="box",
                             material="concrete",
                             diameter=8,
                             width=8,
                             web=0,
                             nvalue=0.015,
                             rounding=0,
                             slope=0.02,
                             L=100);
    flow <- computeFlow(culvert=my.culvert, approach=my.approach,
                        h1=US.depth, h4=DS.depth);
    # 530 cfs in TWRI
    # 530 cfs in capR
  }
}



