LIBNAME save "Data-PartialNesting-25";
DATA save.Settings;
     fractional =  0 ;
     d =  0 ;
     n =  500 ;
     clustersize =  5 ;
     icc =  0.2 ;
     allocation =  0.5 ;
     unequalVariances =  0 ;
     nsims =  5000 ;
     dropoutrate = .2;
RUN;
%INCLUDE "DoTheWork-Partial-Complete.sas";
