LIBNAME save "Data-PartialNesting-86";
DATA save.Settings;
     fractional =  1 ;
     d =  0 ;
     n =  400 ;
     clustersize =  5 ;
     icc =  0.2 ;
     allocation =  0.7 ;
     unequalVariances =  0 ;
     nsims =  5000 ;
     dropoutrate = .2;
RUN;
%INCLUDE "DoTheWork-Partial-Fractional.sas";
