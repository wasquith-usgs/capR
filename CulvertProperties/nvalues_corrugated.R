"nvalues_corrugated" <-
function(corrugation=c("annular", "spiral"),
         diameter=c(  "1ft",  "2ft",  "3ft",  "4ft",  "5ft",
                      "6ft",  "7ft",  "8ft",  "9ft", "10ft",
                     "11ft", "12ft", "16ft", "18ft", "21ft"),
         pitchrise=c("2-2/3x1/2", "3x1", "5x1", "6x1",
                     "6x2", "9x2-1/2"),
         hashback=FALSE) {
         	
  corrugation <- match.arg(corrugation);
  diameter    <- match.arg(diameter);
  pitchxrise  <- match.arg(pitchrise);
  
  nval.h <- new.h();
  nval.annular.h <- new.h();
  nval.spiral.h  <- new.h();
  
  set.h( "1ft", new.h(), nval.annular.h);
  set.h( "2ft", new.h(), nval.annular.h);
  set.h( "3ft", new.h(), nval.annular.h);
  set.h( "4ft", new.h(), nval.annular.h);
  set.h( "5ft", new.h(), nval.annular.h);
  set.h( "6ft", new.h(), nval.annular.h);
  set.h( "7ft", new.h(), nval.annular.h);
  set.h( "8ft", new.h(), nval.annular.h);
  set.h( "9ft", new.h(), nval.annular.h);
  set.h("10ft", new.h(), nval.annular.h);
  set.h("11ft", new.h(), nval.annular.h);
  set.h("12ft", new.h(), nval.annular.h);
  set.h("16ft", new.h(), nval.annular.h);
  set.h("18ft", new.h(), nval.annular.h);
  set.h("21ft", new.h(), nval.annular.h);
  
  set.h("2-2/3x1/2", 0.027, get.h( "1ft", nval.annular.h));
  set.h("2-2/3x1/2", 0.025, get.h( "2ft", nval.annular.h));
  set.h("2-2/3x1/2", 0.024, get.h( "3ft", nval.annular.h));
  set.h("2-2/3x1/2", 0.024, get.h( "4ft", nval.annular.h));
  set.h("2-2/3x1/2", 0.024, get.h( "5ft", nval.annular.h));
  set.h("2-2/3x1/2", 0.023, get.h( "6ft", nval.annular.h));
  set.h("2-2/3x1/2", 0.023, get.h( "7ft", nval.annular.h));
  set.h("2-2/3x1/2", 0.023, get.h( "8ft", nval.annular.h));
  set.h("2-2/3x1/2", 0.023, get.h( "9ft", nval.annular.h));
  set.h("2-2/3x1/2", 0.022, get.h("10ft", nval.annular.h));
  set.h("2-2/3x1/2", 0.022, get.h("11ft", nval.annular.h));
  
  set.h("3x1", 0.028, get.h( "3ft", nval.annular.h));
  set.h("3x1", 0.028, get.h( "4ft", nval.annular.h));
  set.h("3x1", 0.028, get.h( "5ft", nval.annular.h));
  set.h("3x1", 0.028, get.h( "6ft", nval.annular.h));
  set.h("3x1", 0.028, get.h( "7ft", nval.annular.h));
  set.h("3x1", 0.028, get.h( "8ft", nval.annular.h));
  set.h("3x1", 0.028, get.h( "9ft", nval.annular.h));
  set.h("3x1", 0.027, get.h("10ft", nval.annular.h));
  set.h("3x1", 0.027, get.h("11ft", nval.annular.h));
  set.h("3x1", 0.027, get.h("12ft", nval.annular.h));
  set.h("3x1", 0.026, get.h("16ft", nval.annular.h));

  set.h("5x1", 0.026, get.h( "4ft", nval.annular.h));
  set.h("5x1", 0.026, get.h( "5ft", nval.annular.h));
  set.h("5x1", 0.026, get.h( "6ft", nval.annular.h));
  set.h("5x1", 0.026, get.h( "7ft", nval.annular.h));
  set.h("5x1", 0.025, get.h( "8ft", nval.annular.h));
  set.h("5x1", 0.025, get.h( "9ft", nval.annular.h));
  set.h("5x1", 0.025, get.h("10ft", nval.annular.h));
  set.h("5x1", 0.025, get.h("11ft", nval.annular.h));
  set.h("5x1", 0.024, get.h("12ft", nval.annular.h));
  set.h("5x1", 0.023, get.h("16ft", nval.annular.h));

  set.h("6x1", 0.025, get.h( "3ft", nval.annular.h));
  set.h("6x1", 0.024, get.h( "4ft", nval.annular.h));
  set.h("6x1", 0.024, get.h( "5ft", nval.annular.h));
  set.h("6x1", 0.024, get.h( "6ft", nval.annular.h));
  set.h("6x1", 0.023, get.h( "7ft", nval.annular.h));
  set.h("6x1", 0.023, get.h( "8ft", nval.annular.h));
  set.h("6x1", 0.023, get.h( "9ft", nval.annular.h));
  set.h("6x1", 0.023, get.h("10ft", nval.annular.h));
  set.h("6x1", 0.022, get.h("11ft", nval.annular.h));
  set.h("6x1", 0.022, get.h("12ft", nval.annular.h));
  set.h("6x1", 0.021, get.h("16ft", nval.annular.h));
 
  set.h("6x2", 0.035, get.h( "5ft", nval.annular.h));
  set.h("6x2", 0.035, get.h( "6ft", nval.annular.h));
  set.h("6x2", 0.035, get.h( "7ft", nval.annular.h));
  set.h("6x2", 0.034, get.h( "8ft", nval.annular.h));
  set.h("6x2", 0.034, get.h( "9ft", nval.annular.h));
  set.h("6x2", 0.034, get.h("10ft", nval.annular.h));
  set.h("6x2", 0.034, get.h("11ft", nval.annular.h));
  set.h("6x2", 0.033, get.h("12ft", nval.annular.h));
  set.h("6x2", 0.033, get.h("21ft", nval.annular.h));
 
  set.h("9x2-1/2", 0.036, get.h( "5ft", nval.annular.h));
  set.h("9x2-1/2", 0.035, get.h( "6ft", nval.annular.h));
  set.h("9x2-1/2", 0.034, get.h( "7ft", nval.annular.h));
  set.h("9x2-1/2", 0.034, get.h( "8ft", nval.annular.h));
  set.h("9x2-1/2", 0.034, get.h( "9ft", nval.annular.h));
  set.h("9x2-1/2", 0.034, get.h("10ft", nval.annular.h));
  set.h("9x2-1/2", 0.033, get.h("11ft", nval.annular.h));
  set.h("9x2-1/2", 0.033, get.h("12ft", nval.annular.h));
  set.h("9x2-1/2", 0.033, get.h("18ft", nval.annular.h));

  set.h("4ft", 0.020, nval.spiral.h);
  set.h("5ft", 0.022, nval.spiral.h);
  set.h("6ft", 0.023, nval.spiral.h);
  set.h("7ft", 0.023, nval.spiral.h);
  
  set.h("annular", nval.annular.h, nval.h);
  set.h("spiral",  nval.spiral.h,  nval.h);
  
  if(hashback) return(nval.h);
  if(corrugation == "spiral" & pitchrise == "2-2/3x1/2") {
  	if(has.key(diameter, nval.spiral.h)) {
       return(get.h(diameter, nval.spiral.h));
    }
  }
  tmp.h <- get.h(diameter, nval.annular.h);
  return(get.h(pitchrise, tmp.h));
}

#Manning n for Corrugated Metal Culverts 
#
#In Reply Refer To:                                  June 22, 1993 
#Mail Stop 415 
#
#OFFICE OF SURFACE WATER TECHNICAL MEMORANDUM NO. 93.17 
#
#Subject:  Manning n for Corrugated Metal Culverts 
#
#Several types of corrugated metal now used for culvert pipe are 
#not discussed in Techniques of Water-Resources Investigations 
#(TWRI), Book 3, Chapter A3, Measurement of Peak Discharge at 
#Culverts by Indirect Methods.  Laboratory studies conducted by 
#Utah State University for the National Corrugated Steel Pipe 
#Association provide n values for the new types of corrugation.  
#These studies have caused the Federal Highway Administration to 
#revise culvert roughness tables in the manual, Hydraulic Design of 
#Highway Culverts (Hydraulic Design Series No. 5), and provides 
#sufficient basis to revise n values for multiplate culverts as 
#given in TWRI, Book 3, Chapter A3, pages 10 and 11.  The values 
#given herein should be used for all future culvert computations. 
#
#The Office of Surface Water (OSW) also recommends that previous 
#computations for flow through multiplate culverts be reviewed if 
#the following conditions are met: 
#
#     1.  The n value used in the computation differs by 
#         0.003 or more from the value in this memorandum, and 
#     2.  discharges from types 2, 3, 4, or 6 computations using 
#         n values from TWRI, Book 3, Chapter A3, or from ratings 
#         based on such computations, have been published. 
#
#Ratings that were based on the old n values and are still in use 
#should be reviewed and revised if use of the revised n values 
#change any part of the rating by 5 percent or more.  Published 
#discharges do not need to be revised unless they meet the criteria 
#for revisions given in Novak (1985, p. 103-104, WRD data reports 
#preparation guide) and the water-surface elevations and field 
#conditions on which the computation is based provide a high degree 
#of reliability to the computed discharge.  The following material 
#supersedes the discussion in Standard riveted section and 
#Multiplate section in the part of the manual entitled "Corrugated 
#Metal" under Roughness Coefficients on pageJ10 of TWRI, Book 3, 
#Chapter A3. 
#
#                           Corrugated Metal 
#Corrugated pipes and arches are made in riveted, spiral, and 
#structural-plate styles.  The riveted and spiral styles are used 
#in small pipes of less than 9-foot diameter.  Spiral corrugations 
#have the same pitch and depth as that used in riveted 
#construction, but the plates are wound to form a continuous pipe.  
#Because of its greater strength, structural-plate (also called 
#multiplate) commonly is used for pipes that are more than 6 feet 
#in diameter.  Multiplate is made in sheets that are bolted
#together. 
#
#                     Standard Riveted Sections 
#The corrugated metal most commonly used in riveted pipes and 
#arches has a 2 2/3-inch pitch with a rise of 1/2 inch.  This is 
#frequently referred to as standard corrugated metal.  According to 
#laboratory tests, n values for full pipe flow vary from 0.0266 for 
#a 1-foot-diameter pipe to 0.0224 for an 8-foot-diameter pipe for 
#velocities normally encountered in culverts.  The American Iron 
#and Steel Institute recommends that a single value of 0.024 be 
#used in design of both partly-full and full-pipe flow for any size 
#of pipe.  This value may be satisfactory for many computations of 
#discharge.  However, more precise values are given in the 
#accompanying table, which shows values derived from tables and 
#graphs published by the Federal Highway Administration for culvert 
#design and that apply to both annular and spiral corrugations, as 
#noted in the table.  Values from this table should be used by 
#U.S. Geological Survey offices in computation of discharge through 
#culverts. 
#
#Riveted pipes are also made from corrugated metal with a 1-inch 
#rise and 3-, 5-, and 6-inch pitch.  Experimental data show a 
#slight lowering of the n value as pitch increases.  The n values 
#for these three types of corrugation are also given in the table. 
#
#                   Structural Plate (Multiplate) 
#Structural-plate metal used in multiplate construction has much 
#larger corrugations than does that used in riveted pipes.  
#Multiplate construction is used with both steel and aluminum.  The 
#steel has a 6-inch pitch and a 2-inch rise; aluminum has a 9-inch 
#pitch and a 2.5-inch rise.  Tests show somewhat higher n values 
#for this metal and type of construction than for riveted 
#construction.  Average n values range from 0.035 (steel) or 0.036 
#(aluminum) for 5-foot-diameter pipes to 0.033 for pipes of 18 feet 
#or greater diameter.  The n values for various diameters of pipe 
#are given in the following table. 
#
#Revised Roughness Coefficients for Corrugated Metal (May 1993)  
#  Pipe    |            n value for Indicated Corrugation Size   
#Diameter  |                                    |  Structural-plate 
#   ft     |       Riveted Construction         |    Construction        
#          |             Corrugation, Pitch x Rise, inches 
#          |2-2/3 x 1/2   3 x 1   5 x 1   6 x 1   6 x 2   9 x 2-1/2 
#           Annular Corrugations 
#    1          0.027 
#    2          0.025 
#    3          0.024     0.028           0.025 
#    4          0.024     0.028   0.026   0.024 
#    5          0.024     0.028   0.026   0.024   0.035     0.036 
#    6          0.023     0.028   0.026   0.024   0.035     0.035 
#    7          0.023     0.028   0.026   0.023   0.035     0.034 
#    8          0.023     0.028   0.025   0.023   0.034     0.034 
#    9          0.023     0.028   0.025   0.023   0.034     0.034 
#   10          0.022     0.027   0.025   0.023   0.034     0.034 
#   11          0.022     0.027   0.025   0.022   0.034     0.033 
#   12                    0.027   0.024   0.022   0.033     0.033 
#   16                 (a)0.026(a)0.023(a)0.021
#   18                                                   (a)0.033 
#   21                                         (a)0.033 
#           Spiral Corrugations 
#    4          0.020                    Use values for annular 
#    5          0.022                    corrugations for all other 
#    6          0.023                    corrugation sizes and pipe 
#    7          0.023                    diameters. 
#Range of pipe diameter in feet commonly encountered with the above 
#indicated corrugation size: 
#                <9       3-13    5-13    3-13    5-25      5-25 
#(a)Extrapolated beyond Federal Highway Administration curves.1 
#Note:  n values apply to pipes in good condition.  Severe 
#       deterioration of metal and misalignment of pipe sections 
#       may cause slightly higher values. 
#1See page 16 HDS-5 for extrapolation. 
#                                 Charles W. Boning, Chief 
#                                 Office of Surface Water 
#WRD DISTRIBUTION:  A, B, FO, PO 
#





