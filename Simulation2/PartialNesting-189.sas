LIBNAME save "Data-PartialNesting-189";
DATA save.Settings;
     fractional =  0 ;
     d =  0 ;
     n =  600 ;
     clustersize =  5 ;
     icc =  0.2 ;
     allocation =  0.7 ;
     unequalVariances =  1 ;
     nsims =  5000 ;
     dropoutrate = .2;
RUN;
%INCLUDE "DoTheWork-Partial-Complete.sas";
