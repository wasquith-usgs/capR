"Qslopearea2sec" <-
function(delh=0, L=NULL, A1=NULL, A2=NULL, K1=NULL, K2=NULL,
         alpha1=1, alpha2=1, g=NULL, treatexpansion=FALSE) {

      g2 <- 2*g;
  v1head <- 0;
  v2head <- 0;
       k <- 0;

     KoK <- K2/K1;
   KKoAA <- K2^2/(2*g*A2^2);
   AAoAA <- (A2/A1)^2;
   denom <- KoK*L + KKoAA*(-alpha1*AAoAA*(1-k) + alpha2*(1-k));
   Q1 <- K2*sqrt(delh/denom);

   v1head <- (Q1/A1)^2/g2;
   v2head <- (Q1/A2)^2/g2;
   if(treatexpansion) k <- ifelse(v1head - v2head > 0, 0.5, 0);

   delh2  <- delh + v1head - v2head;
   denom <- KoK*L + KKoAA*(-alpha1*AAoAA*(1-k) + alpha2*(1-k));
   Q2 <- K2*sqrt(delh2/denom);

  Q <- list(Q=Q2,  Q.no.velheads=Q1, delh=delh2, delh.no.velheads=delh);
  return(Q);
}


"culvert2QTypeSAC" <-
function(h1=NULL, h4=NULL, culvert=NULL, approach=NULL, digits=4) {

  h1.elev <- h1; h4.elev <- h4;

  h1 <- h1 - culvert$zdsinvert;
  h4 <- h4 - culvert$zdsinvert;
  if(h4 < 0) h4 <- 0;

  g  <- culvert$gravity;
  NB <- culvert$number.of.barrels;
  L   <- culvert$Lbot;

  A1     <- approach$geometry$AREATOTAL/approach$fraction.of.flow;
  alpha1 <- approach$geometry$alpha;
  g2     <- 2*g;

  road <- culvert$road; # properties of the road
  approachflow <- approach$TMP.accumulated.flow; # TOTAL COMPUTED FLOW
  v1head <- alpha1*(approachflow/A1)^2/g2;
  roadflow <- Qroad(h1=h1.elev, h4=h4.elev, v1head=v1head, road=road);

  geo2 <- setCulvertGeometry(depth=h1, culvert=culvert,
                             location="inlet",
                             depression=culvert$inlet.depression);
  geo3 <- setCulvertGeometry(depth=h4, culvert=culvert,
                             location="outlet",
                             depression=culvert$outlet.depression);
  A2  <- geo2$A;
  A3  <- geo3$A;
  K2  <- geo2$KONVEY;
  K3  <- geo3$KONVEY;

  if(roadflow$Qroad != 0) {
    Q <- list(Q=0,
              d2=h1,
              d3=h4,
              A2=NA,
              A3=NA,
              K2=NA,
              K3=NA,
              g=g,
              L=L,
              sac=NA,
              terminalarea=A2,
              konvey2=K2,
              isTypeSAC=FALSE,
              valid=FALSE,
              roadflow=roadflow,
              message="roadflow involved, but was trying slope-area flow");
     return(Q);
  }

  delh <- h1 - h4;
  sac <- Qslopearea2sec(delh=h1-h4, L=L, A1=A2, A2=A3, K1=K2, K2=K3, g=g)
  valid <- ifelse(is.na(sac$Q), FALSE, TRUE);

  Q <- list(Q=round(sac$Q*NB, digits=4),
            d2=h1,
            d3=h4,
            A2=A2,
            A3=A3,
            K2=K2,
            K3=K3,
            g=g,
            L=L,
            sac=sac,
            terminalarea=A3,
            konvey2=K2,
            isTypeSAC=valid,
            valid=valid,
            roadflow=roadflow,
            message="");
  return(Q);
}




"printTypeSAC" <-
function(flow=NULL, culvert=culvert, approach=approach,
         splash=TRUE, checkvalid=TRUE) {
  if(checkvalid & ! flow$valid) return(NA);

  lunits <- culvert$lengthunits;
  qunits <- culvert$flowunits;
  aunits <- culvert$areaunits;


  h12 <- flow$h12;
  if(is.numeric(h12)) {
     h12 <- paste(c(round(h12, digits=3), " ", lunits), sep="", collapse="");
  }
  v1head <- flow$v1head;
  if(is.numeric(v1head)) {
     v1head <- paste(c(round(v1head, digits=3), " ", lunits),
                     sep="", collapse="");
  }

  catme("  Depth at inlet", round(flow$d2, digits=3),lunits);
  catme("    Area at inlet", round(flow$A2, digits=2),aunits);
  catme("    Conveyance at inlet", round(flow$K2, digits=0),qunits);
  catme("  Depth at outlet", round(flow$d3, digits=3),lunits);
  catme("    Area at outlet", round(flow$A3, digits=2),aunits);
  catme("    Conveyance at outlet", round(flow$K3, digits=0),qunits);
  catme("  Bottom length of culvert", round(flow$L, digits=2),lunits);

  pdffile <- paste(c("TMPcapR_culvert",
                     culvert$TMP.key, "_inletoutlet.pdf"), collapse="");
  pdf(pdffile);
  graphCulvert(discharge=flow$Q, depth=flow$d2,
               culvert=culvert, location="inlet",
               depression=culvert$inlet.depression);
  graphCulvert(discharge=flow$Q, depth=flow$d3,
               culvert=culvert, location="outlet",
               depression=culvert$outlet.depression);
  dev.off();
}






