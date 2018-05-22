"Alpha" <-
function(area=NULL, konvey=NULL) {
  area   <- area[! is.na(area)];
  konvey <- konvey[! is.na(konvey)];
  AT2 <- sum(area)^2;   KT3 <- sum(konvey)^3;
   A2 <- area^2;         K3 <- konvey^3;
  return( sum(K3/A2)/(KT3/AT2) );
}

# This expression is based on the assumption that the discharge
# in a subsection is proportional to the conveyance. TWRI3/A1/p.29
