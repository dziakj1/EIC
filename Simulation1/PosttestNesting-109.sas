LIBNAME save "Data-PosttestNesting-109";
DATA save.Settings;
     fractional =  0 ;
     d =  0.3 ;
     n =  400 ;
     clustersize =  10 ;
     icc =  0.2 ;
     nsims =  5000 ;
     dropoutrate = .2;
RUN;
%INCLUDE "DoTheWork-Posttest-Complete.sas";