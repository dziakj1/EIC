LIBNAME save "Data-PosttestNesting-86";
DATA save.Settings;
     fractional =  1 ;
     d =  0.3 ;
     n =  500 ;
     clustersize =  5 ;
     icc =  0.2 ;
     nsims =  5000 ;
     dropoutrate = .2;
RUN;
%INCLUDE "DoTheWork-Posttest-Fractional.sas";
