LIBNAME save "Data-PosttestNesting-80";
DATA save.Settings;
     fractional =  1 ;
     d =  0.5 ;
     n =  400 ;
     clustersize =  5 ;
     icc =  0.2 ;
     nsims =  5000 ;
     dropoutrate = .2;
RUN;
%INCLUDE "DoTheWork-Posttest-Fractional.sas";
