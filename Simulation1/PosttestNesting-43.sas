LIBNAME save "Data-PosttestNesting-43";
DATA save.Settings;
     fractional =  0 ;
     d =  0.2 ;
     n =  400 ;
     clustersize =  10 ;
     icc =  0.1 ;
     nsims =  5000 ;
     dropoutrate = .2;
RUN;
%INCLUDE "DoTheWork-Posttest-Complete.sas";
