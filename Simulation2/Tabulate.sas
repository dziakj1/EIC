LIBNAME target "C:\Users\jjd264\Documents\Papers\EIC\ShaniSim2\";
DATA target.allTests; RUN;
DATA target.allCovParms; RUN;
DATA target.allTestsEV; RUN;
DATA target.allCovParmsEV; RUN;
%MACRO GetAllAnswers;
    %DO i = 1 %TO 192;
        LIBNAME source "C:\Users\jjd264\Documents\Papers\EIC\ShaniSim2\Data-PartialNesting-&i";
        DATA temp; RUN;
        DATA temp; SET source.allTestsAnswers; Condition = &i; RUN;
        DATA target.allTests; SET target.allTests temp; RUN;
        DATA temp; RUN;
        DATA temp; SET source.allCovParmsAnswers; Condition = &i; RUN;
        DATA target.allCovParms; SET target.allCovParms temp; RUN;
        DATA temp; RUN;
        DATA temp; SET source.allTestsAnswersEV; Condition = &i; RUN;
        DATA target.allTestsEV; SET target.allTestsEV temp; RUN;
        DATA temp; RUN;
        DATA temp; SET source.allCovParmsAnswersEV; Condition = &i; RUN;
        DATA target.allCovParmsEV; SET target.allCovParmsEV temp; RUN;
    %END;
%MEND;
%GetAllAnswers; 
PROC EXPORT DATA= TARGET.allTests 
            OUTFILE= "C:\Users\jjd264\Documents\Papers\EIC\ShaniSim2\allTests.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN; 
PROC EXPORT DATA= TARGET.allCovParms 
            OUTFILE= "C:\Users\jjd264\Documents\Papers\EIC\ShaniSim2\allCovParms.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN; 
PROC EXPORT DATA= TARGET.allTestsEV 
            OUTFILE= "C:\Users\jjd264\Documents\Papers\EIC\ShaniSim2\allTestsEV.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN; 
PROC EXPORT DATA= TARGET.allCovParmsEV 
            OUTFILE= "C:\Users\jjd264\Documents\Papers\EIC\ShaniSim2\allCovParmsEV.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN; 
