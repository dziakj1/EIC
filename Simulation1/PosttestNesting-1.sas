LIBNAME save "Data-PosttestNesting-1";
DATA save.Settings;
     fractional =  0 ;
     d =  0 ;
     n =  300 ;
     clustersize =  5 ;
     icc =  0.1 ;
     nsims =  5000 ;
     dropoutrate = .2;
RUN;
%INCLUDE "DoTheWork-Posttest-Complete.sas";
