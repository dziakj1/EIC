LIBNAME save "Data-PartialNesting-65";
DATA save.Settings;
     fractional =  0 ;
     d =  0 ;
     n =  300 ;
     clustersize =  5 ;
     icc =  0.1 ;
     allocation =  0.7 ;
     unequalVariances =  0 ;
     nsims =  5000 ;
     dropoutrate = .2;
RUN;
%INCLUDE "DoTheWork-Partial-Complete.sas";