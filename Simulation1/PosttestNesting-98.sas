LIBNAME save "Data-PosttestNesting-98";
DATA save.Settings;
     fractional =  1 ;
     d =  0 ;
     n =  300 ;
     clustersize =  10 ;
     icc =  0.2 ;
     nsims =  5000 ;
     dropoutrate = .2;
RUN;
%INCLUDE "DoTheWork-Posttest-Fractional.sas";