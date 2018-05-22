"ARPIPEARCH" <-
function(depth=NULL, culvert=NULL) {
  D <- culvert$diameter;
  AR  <- PIPE.ARCH(depth=depth, culvert=culvert);

  D <- culvert$diameter;
  B <- culvert$span;
  nvalue <- culvert$nvalue;

  z <- list(DEPTH=depth, D=D, B=B, N=nvalue, A=AR$A,
            TW=AR$TW, WP=AR$WP,  HR=AR$HR,  KONVEY=AR$KONVEY);
  return(z);
}

"PIPE.ARCH" <-
function(depth=NULL, culvert=NULL) {

  D <- culvert$diameter;
  B <- culvert$span;
  nvalue   <- culvert$nvalue;
  manningcor <- culvert$manningcor;
  g <- culvert$gravity;

  if(is.null(depth) || depth > D) depth <- D;
  if(depth < 0) depth <- 0;

  #ARADs <- CULPAD(culvert=culvert);
  #ARAD <- c(ARADs$radius.bottom,
  #          ARADs$radius.top,
  #          ARADs$radius.corner);
  ARAD <- c(culvert$pipearch.radius.bottom,
            culvert$pipearch.radius.top,
            culvert$pipearch.radius.corner);
  #catme("PIPEARCH ARAD: ",ARAD,"\n");
#C-----Arguments:
#      REAL B,D,ARAD(3)
#      REAL DEP,AREA,WP,TW,CONV,ROUGH
#C
#C     NOTE -- DIMENSIONS DIMENS MUST BE GIVEN IN INCHES,  DEP IN  FEET.
#C
   if(g > 15) {
      RISEQQ <- D*12;
      SPANQQ <- B*12;
      Y3 <- depth*12;
   } else {
     RISEQQ <- D*3.2808*12;  # meters to feet to inches
     SPANQQ <- B*3.2808*12;  # meters to feet to inches
     Y3 <- depth*3.2808*12;
  }
  if(Y3 <= 0)     Y3 <- 0.0001*RISEQQ;
  if(Y3 > RISEQQ) Y3 <- RISEQQ;

  BRQQ <- ARAD[1];
  TRQQ <- ARAD[2];
  CRQQ <- ARAD[3];

  SPHI1  <- (SPANQQ/2 - CRQQ) / (BRQQ - CRQQ);
  if(! is.finite(SPHI1)) {
    stop("SPHI1 is Inf, pipearch.bottom == pipearch.corner");
  }
  
  CPHI1  <- sqrt(1 - SPHI1^2);
  BDISQQ <- CRQQ + (BRQQ - CRQQ)*(1 - CPHI1);
  TPHI2  <- (SPANQQ/2 - CRQQ) / ( BDISQQ - ( RISEQQ-TRQQ ) );
  

  if(is.nan(CPHI1)) {
    stop("CPHI1 is NaN, review relation between pipearch.radius.bottom, pipearch.radius.corner, and span");
  }
  if(is.nan(TPHI2)) {
    stop("TPHI2 is NaN, review relation between pipearch.radius.bottom, pipearch.radius.top, pipearch.radius.corner, and rise and span");  
  }


  PHI1 <- atan(SPHI1/CPHI1);
  PHI2 <- atan(TPHI2);

  Y1 <- BRQQ   - BRQQ*cos(PHI1);
  Y2 <- BDISQQ + CRQQ*cos(PHI2);
  
  AREA1 <- 0;  AREA2 <- 0; AREA3 <- 0;
    WP1 <- 0;    WP2 <- 0;   WP3 <- 0;
    TOP <- 0;

  B1   <- 2*BRQQ;
  AK1  <- 1/(BRQQ*BRQQ);
  RAD1 <- sqrt(B1*B1);

  B2   <- 2*BDISQQ;
  A2   <- CRQQ*CRQQ - BDISQQ*BDISQQ;
  AK2  <- 1/CRQQ^2;
  RAD2 <- sqrt(B2*B2 - (-4*A2));

  B3   <- 2*(RISEQQ-TRQQ);
  A3   <- 2*RISEQQ*TRQQ - RISEQQ*RISEQQ;
  AK3  <- 1/TRQQ^2;
  RAD3 <- sqrt(B3*B3 - (-4*A3));


  # /144 * 2; # x2 for symmetry? /144 to in^2 to ft^2?

  # The original fortran sources in Cap96.08 use a cascade of
  # goto's. I have tried to keep the core lines in direct port
  # so the following three functions act like the line blocks.
   "PARCH.line325" <- function() {
   #  325 # ORIGINAL LINE LABEL ON NEXT LINE
       X3 <- abs(A3 + B3*Y3 - Y3*Y3);
      X33 <- A3 + B3*Y2 - Y2*Y2;
      subareas <- MARQUE(Y3, Y2, B3, X3, X33, AK3, RAD3);
      ASUB1 <- subareas$subarea1;
      ASUB2 <- subareas$subarea2;
        XX1 <- subareas$XX1;
        XX2 <- subareas$XX2;
      AREA3 <- (ASUB1 - ASUB2)/144 * 2;
        WP3 <- 2*TRQQ*(XX1-XX2);
      return(c(X3,AREA3,WP3));
   }

   "PARCH.line345" <- function() {
   #  345 # ORIGINAL LINE LABEL ON NEXT LINE
       X2 <- A2 + B2*Y2 - Y2*Y2;
      X22 <- A2 + B2*Y1 - Y1*Y1;
      subareas <- MARQUE(Y2, Y1, B2, X2, X22, AK2, RAD2);
      ASUB1 <- subareas$subarea1;
      ASUB2 <- subareas$subarea2;
        XX1 <- subareas$XX1;
        XX2 <- subareas$XX2;
      ASUB3 <- (SPANQQ/2 - CRQQ)*Y2 - (SPANQQ/2 - CRQQ)*Y1;
      AREA2 <- (ASUB1 - ASUB2 + ASUB3)/144 * 2;
        WP2 <- 2*CRQQ*(XX1-XX2);
      return(c(X2,AREA2,WP2));
   }

   "PARCH.line367" <- function() {
   #  367 # ORIGINAL LINE LABEL ON NEXT LINE
      X1 <- B1*Y1 - Y1*Y1;
      subareas <- MARQUE(Y1, 0, B1, X1, 0, AK1, RAD1);
      ASUB1 <- subareas$subarea1;
      ASUB2 <- subareas$subarea2;
        XX1 <- subareas$XX1;
        XX2 <- subareas$XX2;
      AREA1 <- (ASUB1 - ASUB2)/144 * 2;
        WP1 <- 2*BRQQ*(XX1-XX2);
      return(c(X1,AREA1,WP1));
   }


  if(Y3 >= Y2) {# GO TO 325 TODO
     zone3 <- PARCH.line325();
     zone2 <- PARCH.line345();
     zone1 <- PARCH.line367();
     X3 <- zone3[1]; AREA3 <- zone3[2]; WP3 <- zone3[3];
     X2 <- zone2[1]; AREA2 <- zone2[2]; WP2 <- zone2[3];
     X1 <- zone1[1]; AREA1 <- zone1[2]; WP1 <- zone1[3];
  } else {
     Y2 <- Y3;
     if(Y3 >= Y1) { # GO TO 345 TODO
       zone2 <- PARCH.line345();
       zone1 <- PARCH.line367();
       X2 <- zone2[1]; AREA2 <- zone2[2]; WP2 <- zone2[3];
       X1 <- zone1[1]; AREA1 <- zone1[2]; WP1 <- zone1[3];
     }
     else {
        Y1 <- Y3;
        if(Y3 > 0) { # GO TO 367 TODO
           zone1 <- PARCH.line367();
           X1 <- zone1[1]; AREA1 <- zone1[2]; WP1 <- zone1[3];
        } else {
          zone3 <- PARCH.line325();
          zone2 <- PARCH.line345();
          zone1 <- PARCH.line367();
          X3 <- zone3[1]; AREA3 <- zone3[2]; WP3 <- zone3[3];
          X2 <- zone2[1]; AREA2 <- zone2[2]; WP2 <- zone2[3];
          X1 <- zone1[1]; AREA1 <- zone1[2]; WP1 <- zone1[3];
        }
     }
  }

  AREA <- (AREA1 + AREA2 + AREA3); # already in square feet
    WP <- (WP1 + WP2 + WP3)/12; # cast into feet
  if(Y3 > Y2) {
    TOP <- 2*sqrt(X3);
  } else if(Y3 > Y1) {
    TOP <- 2*( sqrt(X2) + (SPANQQ/2 - CRQQ) );
  } else if(Y3 > 0) {
    TOP <- 2*sqrt(X1);
  }
  TW <- TOP/12; # cast into feet
  if(g < 15) { # cast into meters and square meters if needed
    TW   <- TW/3.2808;
    AREA <- AREA/(3.2808^2);
    WP   <- WP/3.2808;
  }

  hydraulicradius <- AREA/WP;
  if(TW <= 0) TW <- 1e-8;
  topwidth <- TW;
  wettedperimeter <- WP;

  konvey <- Conveyance(A=AREA, R=hydraulicradius,
                       nvalue=nvalue, manningcor=manningcor);


  z <- list(DEPTH=depth, D=D, B=B,
            N=nvalue, A=AREA,
            TW=topwidth, WP=wettedperimeter,
            HR=hydraulicradius, KONVEY=konvey);
  return(z);
}


"MARQUE" <-
function(UL, LL, BB, UX, LX, AK, RAD) {
#C=====MARQUE bof=============================================================
#C-----Purpose:
#C       computes culvert properties for pipe arch sections
#C     Programmed by: W Kirby original code from WSPRO
#C     Date:
#C     Modified by: JM Fulford
#C
#C
#C
#C
#C-----Arguments:
#      REAL LL,LX,UL,BB,AK,RAD,XX1,ASUB1,XX2,ASUB2
#C
   XX1 <- (2*UL - BB)/RAD;
   if(abs(XX1) > 1) {
      ifelse(XX1 >= 0, XX1 <-  1, XX1 <- -1);
      #XX1 <- SIGN(1.,XX1); # SEE BELOW
   }
   XX1 <- asin(XX1);
   ASUB1 <- (  ( (-2*UL+BB)*sqrt(UX) )*(-0.25)  ) + 1 / (2*AK) * XX1;
   XX2 <- (2*LL-BB)/RAD;
   if(abs(XX2) > 1) {
      ifelse(XX2 >= 0, XX2 <- 1, XX2 <- -1);
      #XX2 <- SIGN(1, XX2); # SEE BELOW
   }
   XX2 <- asin(XX2);
   ASUB2 <- (  ( (-2*LL+BB)*sqrt(LX) )*(-0.25)  ) + 1 / (2*AK) * XX2;
   return(list(subarea1=ASUB1, subarea2=ASUB2,
               area.units="inches", XX1=XX1, XX2=XX2));
}


#Sign Function
#The function sign in Fortran is called the sign transfer function. It is a function of two variables, and its definition involves two cases:
#
#CASE 1:   If y ≥ 0 then
#		sign(x,y) = abs(x)   ,
#CASE 2:   If y < 0 then
#		sign(x,y) = - abs(x)   .
#
#The practical effect is that sign(x,y) has the same absolute value as x, but it has the same sign as y; thus the sign of y is transferred to x. (The case y = 0 is a little special - it gives sign(x,y) always a plus sign.)
#
#Examples :
#
#sign(2,3) = 2,  sign(2, -3) = -2, sign(-2,3) = 2, sign(-2, -3) = -2


"CULPAD" <-
function(culvert=NULL) {
#C=====CULPAD bof=============================================================
#C-----Purpose:
#C       computes culvert properties for pipe arch sections
#C     Programmed by: W Kirby original code from WSPRO
#C     Date:
#C     Modified by: JM Fulford
#C                  6.9.97 - uses arctyp code in material code wspro location
#C     CULPAD -- PIPE-ARCH DIMENSIONS -- APPROXIMATE
#C        REGRESSION EQNS BASED ON DIMENSIONS TABULATED IN FHWA - CDS-4
#C  PARAMETERS --
#C     ARCTYP -- 2,4 = CMP .LE. 18"CR,  5 = CMP-31",
#C                 6 = CMP-47",  3 = ALUM. 31.8",   1 = R.C.P.
#C     ARAD(3,4,5) = APPROX BOTTOM, TOP, CORNER RADII  =  OUTPUT.
#C
#C-----Arguments:
#      INTEGER TCR
#      REAL ARAD(3),D,B
#C
#C-----Local variables:
#      INTEGER ARCTYP
#      REAL SPAN,RISE
#C
    g <- culvert$gravity;
    B <- culvert$span;
    D <- culvert$diameter;
    ARCTYP <- culvert$pipearch.type;

    ARAD <- vector(mode="numeric", length=3);

    if(g > 15) {
       SPAN <- B*12; # SPAN is in inches!
       RISE <- D*12; # RISE is in inches!
    } else {
       SPAN <- B*3.2808*12;  # meters to feet to inches
       RISE <- D*3.2808*12;  # meters to feet to inches
    }

    #ARCTYP <- as.integer((TCR - 300)/10); # TODO, WHAT IS TCR?
    # The as.integer() is my guess at getting something running.
    # TCR <- as.integer(TC/10); # in culrat.f
    # Now TC is a type of culvert, so there must be an interval within
    # the TC range that is reserved for pipearch?
    #C     ARCTYP -- 2,4 = CMP .LE. 18"CR,  5 = CMP-31",
    #C                 6 = CMP-47",  3 = ALUM. 31.8",  1 = R.C.P.
    if(ARCTYP == "CMP <=18inchCR") {
       ARAD[3] <- 18;
       if(RISE < 55) ARAD[3] <- 1.141 + 0.205*RISE;
       ARAD[2] <- 0.594 + 0.498*SPAN;
       ARAD[1] <- 7.000 - 2.036*RISE + 2.741*SPAN;
    } else if(ARCTYP == "CMP 31inchCR") {
       ARAD[3] <-   31;
       ARAD[2] <-   -0.346 +  0.505*SPAN;
       ARAD[1] <- -956.600 + 29.390*RISE - 13.49*SPAN;
    #} else if(ARCTYP == 4) { # TODO BUG IN CAP???? IT NEVER ASKED FOR TYPE 6
    } else if(ARCTYP == "CMP 47inchCR") { # Guessing type 6
       ARAD[3] <- 47; # see the 47 here
       ARAD[2] =   -3.27 +  0.521*SPAN;
       ARAD[1] = -982.30 + 18.440*RISE - 7.805*SPAN;
    } else if(ARCTYP == "ALUM 31.8inchCR") {
       ARAD[3] <- 31.8;
       ARAD[2] <-  -0.696 + 0.522*SPAN;
       ARAD[1] <- 363.300 - 9.639*RISE + 5.398*SPAN;
    } else if(ARCTYP == "RCP") {
       ARAD[3] <-   0.598 + 0.243*RISE;
       ARAD[2] <-   1.210 + 0.499*SPAN;
       ARAD[1] <- -60.130 + 2.106*SPAN + 0.583*abs(SPAN-95);
    } else {
       txt <- paste(c('Pipe archtype code ', ARCTYP,
                      ' is invalid.'),
                    sep=" ", collapse="");
       warning(txt);
    }

    ARAD <- ARAD; # UNIT CONVERSION OR NOT FROM INCHES?
    # WHA 10/28/2009 VOTES TO LEAVE IN INCHES PERIOD
    return(list(radius.bottom=ARAD[1],
                radius.top=ARAD[2],
                radius.corner=ARAD[3],
                radius.units="inches"));
}


