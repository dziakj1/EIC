LIBNAME target "C:\Documents and Settings\jjd264\My Documents\Paper-ShaniDziakCollins\ShaniSim1";
DATA target.AllAnswers; RUN;
%MACRO GetAllAnswers;
    %DO i = 1 %TO 128;
	    LIBNAME source "C:\Documents and Settings\jjd264\My Documents\Paper-ShaniDziakCollins\ShaniSim1\Data-PosttestNesting-&i";
	   DATA temp; RUN;
        DATA temp;
            SET source.answers;
			Condition = &i;
		RUN;
       DATA target.AllAnswers; SET target.AllAnswers temp; RUN;
	   DATA temp; RUN;
	%END;
%MEND;
%GetAllAnswers;

PROC MEANS DATA=target.AllAnswers; RUN;
PROC FREQ DATA=target.AllAnswers; TABLE Condition; RUN;
