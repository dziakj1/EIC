LIBNAME save "Data-PosttestNesting-30";
DATA save.Settings;
     fractional =  1 ;
     d =  0.3 ;
     n =  600 ;
     clustersize =  5 ;
     icc =  0.1 ;
     nsims =  5000 ;
     dropoutrate = .2;
RUN;
%INCLUDE "DoTheWork-Posttest-Fractional.sas";
