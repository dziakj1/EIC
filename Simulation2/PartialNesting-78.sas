LIBNAME save "Data-PartialNesting-78";
DATA save.Settings;
     fractional =  1 ;
     d =  0 ;
     n =  600 ;
     clustersize =  5 ;
     icc =  0.1 ;
     allocation =  0.7 ;
     unequalVariances =  0 ;
     nsims =  5000 ;
     dropoutrate = .2;
RUN;
%INCLUDE "DoTheWork-Partial-Fractional.sas";
