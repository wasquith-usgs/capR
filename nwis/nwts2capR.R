"nwts2capR" <-
function(method=nwts2capR.ssh, ...) {
  return(method(...));
}

"nwts2capR.sql" <-
function() {
  stop("stub out");
}

"nwts2capR.nwisweb" <-
function() {
  stop("stub out");
}


"nwts2capR.ssh" <-
function(agency="USGS",
         host="somehost.cr.usgs.gov",
         path="./",
         program="capRwyuv.pl",
         user="someuser",
         station="08123620",
         stat="C",
         hw=1, tw=4,
         beg="1001", end="0930",
         wy=format(Sys.time(), "%Y"),
         yy=NULL,
         workingok=FALSE,
         compress=TRUE,
         test=FALSE,
         noget=FALSE,
         unlink=TRUE,
         verbose=FALSE) {

   # if the yy is not provided, use the wy from the argument
   # this way the "program" is not called with TWO separate
   # year arguments
   if(! is.null(yy)) {
      year <- paste(c("-yy=",yy," "), collapse="");
   } else {
      year <- paste(c("-wy=",wy," "), collapse="");
   }
   # compress the ssh session, good for slow networks
   compress <- ifelse(compress, "-z ",     " ");

   # get working record as well as approved?
   working  <- ifelse(workingok, "-workingok",  " ");
   # do not actually make the ssh retrieval, pretend that it
   # has happened and continue processing
   noget    <- ifelse(noget,    "-noget ", " ");
   com <- paste(c(path,program," ",
                  "-user=",user," ",
                  "-host=",host," ",
                  year,
                  "-a=",agency," ",
                  "-s=",stat," ",
                  "-hw=",hw," ",
                  "-tw=",tw," ",
                  "-b=",beg," ",
                  "-e=",end," ",
                  compress,
                  working,
                  noget,
                  station), collapse="");

   if(verbose || test) {
      message(com); # show the command, helpful in debugging
      message("Unit values of headwater in TMP4capR_uvHW.rdb");
      message("Unit values of tailwater in TMP4capR_uvTW.rdb");
      message("Standard error of external command is in TMP4capR_STDERR.txt");
      if(test) return(NA);
   }

   system(com); # EXTERNAL COMMAND
   if(unlink) {
      message("Unlinking the temporary files created by this function");
      try(unlink("TMP4capR_uvHW.rdb")  );
      try(unlink("TMP4capR_uvTW.rdb")  );
      try(unlink("TMP4capR_STDERR.txt"));
   }
}


"read.nwts2capR" <-
function(unlink=TRUE, savefile="TMP4capR_read_nwts2capR.RData") {
   HW <- TW <- HWTW <- DT <- NULL;
   try(HW    <- read.table("TMP4capR_HW.txt", sep="|", header=TRUE, stringsAsFactors=FALSE));
   if(is.null(HW)) {
      warning("Headwater file was not found");
      warning("Try manually changing directory with setwd()?");
      warning("Assuming tail, head and tail, and date files not available, returning NA");
      return(NA);
   }
   try(TW    <- read.table("TMP4capR_TW.txt",        sep="|", header=TRUE, stringsAsFactors=FALSE));
   try(HWTW  <- read.table("TMP4capR_HWTWinter.txt", sep="|", header=TRUE, stringsAsFactors=FALSE));
   try(DT    <- read.table("TMP4capR_alldates.txt",  sep="|", header=TRUE, stringsAsFactors=FALSE));
   if(unlink) {
      message("Unlinking the temporary files created by this function");
      try(unlink("TMP4capR_HW.txt")        );
      try(unlink("TMP4capR_TW.txt")        );
      try(unlink("TMP4capR_HWTWinter.txt") );
      try(unlink("TMP4capR_alldates.txt")  );

      message("Unlinking the savefile");
      try(unlink(savefile));
   }
   # Head water and tail water retrievals are now loaded, at this point
   # there is no guarantee that they have the same dates, hopefully
   # quite close in time (few minutes). The union of the dates is used
   # for the xout and approx() does linear interpolation with flat line
   # edge estimation (rule=2).
   HWapprox  <- approx(HW$DAYS, y=HW$HW, DT$DAYS, rule=2);
   TWapprox  <- approx(TW$DAYS, y=TW$TW, DT$DAYS, rule=2);
   HWTWunion <- data.frame(DATE=DT$DATE,
                           DAYS=round(DT$DAYS, digits=5),
                           HW=round(HWapprox$y, digits=2),
                           TW=round(TWapprox$y, digits=2));
   nwts2capR.RData <- list(headwater=HW,
                           tailwater=TW,
                           intersection.of.head.and.tailwater=HWTW,
                           union.of.dates=DT,
                           union.of.head.and.tailwater=HWTWunion,
                           message="");
   if(! file.exists(savefile)) {
      message("The savefile does not exist, testing whether to create one");
      if(! is.na(savefile)) {
        message(paste(c("savefile= ",savefile," now being written."),
                      collapse=""));
        try(save(nwts2capR.RData, file=savefile));
      } else {
        message("savefile=NA, so it was not written");
      }
   }
   return(nwts2capR.RData);
}




"write.capR2nwts" <-
function(uv, file="capR2nwts.txt", capograph=NA,
             station="NNNNNNNN", col.names=FALSE, append=FALSE) {
   uv$STATION <- rep(station, length(uv[,1]))
   uv$HW   <- sprintf("%2.2f", uv$HW)
   uv$TW   <- sprintf("%2.2f", uv$TW)
   uv$DAYS <- sprintf("%5.5f", uv$DAYS)
   uv$TYPE <- capograph$abstypes
   uv$Q    <- sprintf("%2.2f", capograph$Qtotal)
   uv <- as.data.frame(uv)
   #uv <- uv[,c(5,1:4,7,6)] # reshuffle the header
   write.table(uv, file=file, row.names=FALSE, quote=FALSE,
                   append=append, col.names=col.names)

   uv$DAYS <- as.numeric(uv$DAYS)
   uv$HW   <- as.numeric(uv$HW)
   uv$TW   <- as.numeric(uv$TW)
   uv$Q    <- as.numeric(uv$Q)

   return(uv)
}

