LIBNAME source "C:\Documents and Settings\jjd264\My Documents\Paper-ShaniDziakCollins\ShaniSim1\";
PROC EXPORT DATA= SOURCE.ALLANSWERS 
            OUTFILE= "C:\Documents and Settings\jjd264\My Documents\Paper-ShaniDziakCollins\ShaniSim1\allanswers.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
