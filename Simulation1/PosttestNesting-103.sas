LIBNAME save "Data-PosttestNesting-103";
DATA save.Settings;
     fractional =  0 ;
     d =  0.5 ;
     n =  300 ;
     clustersize =  10 ;
     icc =  0.2 ;
     nsims =  5000 ;
     dropoutrate = .2;
RUN;
%INCLUDE "DoTheWork-Posttest-Complete.sas";
