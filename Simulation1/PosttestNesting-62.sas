LIBNAME save "Data-PosttestNesting-62";
DATA save.Settings;
     fractional =  1 ;
     d =  0.3 ;
     n =  600 ;
     clustersize =  10 ;
     icc =  0.1 ;
     nsims =  5000 ;
     dropoutrate = .2;
RUN;
%INCLUDE "DoTheWork-Posttest-Fractional.sas";
