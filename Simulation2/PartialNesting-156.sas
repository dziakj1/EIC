LIBNAME save "Data-PartialNesting-156";
DATA save.Settings;
     fractional =  1 ;
     d =  0.3 ;
     n =  500 ;
     clustersize =  5 ;
     icc =  0.2 ;
     allocation =  0.6 ;
     unequalVariances =  1 ;
     nsims =  5000 ;
     dropoutrate = .2;
RUN;
%INCLUDE "DoTheWork-Partial-Fractional.sas";
