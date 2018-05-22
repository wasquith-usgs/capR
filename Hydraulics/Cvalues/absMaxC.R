"absMaxC" <-
function(C) {
  # TWRI p. 37
  ifelse(C > 0.98, return(0.98), return(C));
}

