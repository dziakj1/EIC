LIBNAME save "Data-PosttestNesting-95";
DATA save.Settings;
     fractional =  0 ;
     d =  0.5 ;
     n =  600 ;
     clustersize =  5 ;
     icc =  0.2 ;
     nsims =  5000 ;
     dropoutrate = .2;
RUN;
%INCLUDE "DoTheWork-Posttest-Complete.sas";
