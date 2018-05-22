capR <- new.env(hash = TRUE);
message("Please select the capR.src file to sys.source() to capR.rxe");
my.file <- file.choose();
my.basename <- basename(my.file);
my.dirname  <-  dirname(my.file);
sys.source(my.file, envir = capR, chdir = TRUE,
           keep.source = getOption("keep.source.pkgs"))
setwd(my.dirname);
save(capR, file="capR.rxe");
message("The binary file ",my.dirname,"/capR.rxe is available");
rm(my.file);
rm(my.basename);
rm(my.dirname);
attach(capR)



