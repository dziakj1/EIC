LIBNAME save "Data-PartialNesting-121";
DATA save.Settings;
     fractional =  0 ;
     d =  0 ;
     n =  500 ;
     clustersize =  5 ;
     icc =  0.2 ;
     allocation =  0.5 ;
     unequalVariances =  1 ;
     nsims =  5000 ;
     dropoutrate = .2;
RUN;
%INCLUDE "DoTheWork-Partial-Complete.sas";
