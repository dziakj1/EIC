 
/*********************************************************/
/* Generate some datasets containing needed information  */
/*********************************************************/
DATA all; rep = 0; RUN;
DATA allcpt; rep = 0; RUN; 
DATA save.Settings;
	SET save.Settings;
    numClusters = n/clustersize;
    CALL SYMPUT("fractional",fractional);
	CALL SYMPUT("d",d);
	CALL SYMPUT("n",n);
	CALL SYMPUT("clustersize",clustersize);
	CALL SYMPUT("icc",icc);
	CALL SYMPUT("nsims",nsims);
	CALL SYMPUT("df",df);
	CALL SYMPUT("numSingletons",numSingletons);
	CALL SYMPUT("numClusters",numClusters);
	CALL SYMPUT("dropoutRate",dropoutRate); 
RUN;  
PROC FACTEX;
    FACTORS x1 x2 x3 x4 x5;
    OUTPUT OUT=completeFactorial;
QUIT; 
DATA save.allConditions;
    SET completeFactorial;
    %LET numConditions = 32; 
    cond = _N_;
	%LET probTable = (1/32),(1/32),(1/32),(1/32),
                     (1/32),(1/32),(1/32),(1/32),
                     (1/32),(1/32),(1/32),(1/32),
                     (1/32),(1/32),(1/32),(1/32),
                     (1/32),(1/32),(1/32),(1/32),
                     (1/32),(1/32),(1/32),(1/32),
                     (1/32),(1/32),(1/32),(1/32),
                     (1/32),(1/32),(1/32),(1/32);
RUN;
%MACRO doloop;
    %DO rep = 1 %TO &nsims; 
		DATA clusters; 
		    numSetsOfClusters = FLOOR(&numclusters/&numConditions);
			numExtraClusters = &numClusters-numSetsOfClusters*&numConditions;
			DO set = 1 TO numSetsOfClusters;
			    DO cond = 1 TO &numConditions;
		             uj = SQRT(&icc/(1-&icc))*RAND("NORMAL");
			        TauSqdU = &icc/(1-&icc);
		            OUTPUT;
		        END; 
			END;
			DO extra = 1 TO numExtraclusters;
			    cond = RAND("TABLE",&probTable);
		        uj = SQRT(&icc/(1-&icc))*RAND("NORMAL");
				TauSqdU = &icc/(1-&icc);
				OUTPUT;
		    END;
			DROP numSetsOfClusters numExtraClusters set extra;
		RUN;
		DATA clusters; SET clusters;
			j = _N_;
		RUN;
		DATA members;
		    DO j = 1 TO &numClusters;
		        DO memberID = 1 TO &clustersize;
					OUTPUT;
				END;
			END;
		RUN;
		DATA wide;
		    MERGE clusters members;
			BY j;
		RUN;
		PROC SORT DATA=wide; BY cond; RUN;
		DATA wide;
		    MERGE wide save.allConditions;
			BY cond;
		RUN; 
		DATA wide;
		    SET wide;
			i = _N_;
		    Pij = RAND("NORMAL");
			gammaP =  .65 ;
			estarij = SQRT(1 - gammaP**2) * RAND("NORMAL");
			SigmaSqdEStar = 1 - gammaP**2;
			Yij = uj + (&d/2)*x1 + (&d/2)*x3 + (&d/2)*x1*x3 + gammaP*Pij + estarij; 
			dropout = RAND("BERNOULLI",&dropoutRate);
		RUN; 
		PROC MIXED DATA=wide NOCLPRINT;
		    CLASS  x1 x2 x3 x4 x5 j;
		    MODEL Yij = Pij x1 x2 x3 x4 x5
		                    x1*x2 x1*x3 x1*x4 x1*x5
		                    x2*x3 x2*x4 x2*x5
		                    x3*x4 x3*x5
		                    x4*x5 / DDFM=SATTERTHWAITE; 
			RANDOM INTERCEPT / SUBJECT = j(x1 x2 x3 x4 x5);
			ODS OUTPUT TESTS3=OutputAncova COVPARMS=cp;
		    WHERE dropout = 0;
		QUIT;
		DATA OutputAncova;
		    SET OutputAncova;
		    method="Ancova";
		    rep = &rep;
		    power = ProbF<.05;
		    IF ProbF = . THEN power = .;
		RUN;
		PROC TRANSPOSE DATA=cp OUT=cpt; RUN; 
		DATA cpt; SET cpt; rep = &rep; RUN;
		DATA all; SET all OutputAncova; RUN; 
		DATA allcpt; SET allcpt cpt; RUN;  
	%END;
%MEND;
%DoLoop;
PROC SORT DATA=all; BY effect; RUN;
PROC MEANS DATA=all; WHERE rep>0; BY effect; OUTPUT OUT=answers; RUN;
PROC PRINT DATA=answers; RUN;
PROC MEANS DATA=allcpt; WHERE rep>0; OUTPUT OUT=answersCPs; RUN;
PROC PRINT DATA=answersCPs; RUN;
DATA save.all; SET all; RUN;
DATA save.answers; SET answers; RUN;
DATA save.allcpt; SET allcpt; RUN;
DATA save.answersCPs; SET answersCPs; RUN;
DATA save.wide; SET wide; RUN;
