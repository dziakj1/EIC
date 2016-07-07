OPTIONS NONOTES;  
/*********************************************************/
/* Generate some datasets containing needed information  */
/*********************************************************/
ods noresults;
ods exclude all;
DATA AllTests; rep = 0; RUN;
DATA AllCovParms; rep = 0; RUN; 
DATA AllTestsEV; rep = 0; RUN;
DATA AllCovParmsEV; rep = 0; RUN; 
DATA save.Settings;
    SET save.Settings;
    numSingletons = FLOOR(n*(1-allocation));
    numClusters = FLOOR(n*allocation/clustersize);
    CALL SYMPUT("fractional",fractional);
    CALL SYMPUT("d",d);
    CALL SYMPUT("n",n);
    CALL SYMPUT("clustersize",clustersize);
    CALL SYMPUT("icc",icc);
    CALL SYMPUT("allocation",allocation);
    CALL SYMPUT("nsims",nsims);
    CALL SYMPUT("df",df);
    CALL SYMPUT("numSingletons",numSingletons);
    CALL SYMPUT("numClusters",numClusters);
    CALL SYMPUT("dropoutRate",dropoutRate); 
    CALL SYMPUT("unequalVariances",unequalVariances); 
RUN;  
PROC FACTEX;
    FACTORS x1 x2 x3 x4 x5;
    SIZE DESIGN = MINIMUM;
    MODEL ESTIMATE = (x1 | x2 | x3 | x4 | x5 @ 2)  ;
    OUTPUT OUT=halfFactorial;
QUIT;
DATA save.allConditions;
    SET halfFactorial;
    %LET numConditions = 16; 
    cond = _N_;
    %LET probTable = (1/8),(1/8),(1/8),(1/8),
                     (1/8),(1/8),(1/8),(1/8) ;
RUN;
%MACRO doloop;
    %DO rep = 1 %TO &nsims; 
        DATA singletons; 
            numSetsOfSingletons = FLOOR(&numSingletons/(&numConditions/2));
            numExtraSingletons = &numSingletons-numSetsOfSingletons*(&numConditions/2);
            DO set = 1 TO numSetsOfSingletons;
                DO cond = 1 TO (&numConditions/2);      /* Only the first half of the conditions have X1=-1 and therefore are unclustered. */
                    OUTPUT;
                END; 
            END;
            DO extra = 1 TO numExtraSingletons;
                cond = RAND("TABLE",&probTable);
                OUTPUT;
            END;
            DROP numSetsOfSingletons numExtraSingletons set extra;
        RUN;
        DATA singletons; SET singletons;
            singletonID = _N_;
            uj = 0;
        RUN;
        DATA clusters; 
            numSetsOfClusters = FLOOR(&numclusters/(&numConditions/2));
            numExtraClusters = &numClusters-numSetsOfClusters*(&numConditions/2);
			unequalVariances = &unequalVariances;
            IF unequalVariances = 0 THEN TauSqdU = &icc/(1-&icc);
            IF unequalVariances = 1 THEN TauSqdU = .8075 * &icc/(1-&icc);
            DO set = 1 TO numSetsOfClusters;
                DO cond = ((&numConditions/2)+1) TO (&numConditions);  /* Only the first half of the conditions have X1=-1 and therefore are unclustered. */
                     uj = SQRT(TauSqdU)*RAND("NORMAL");
                    OUTPUT;
                END; 
            END;
            DO extra = 1 TO numExtraclusters;
                cond = (&numConditions/2)+RAND("TABLE",&probTable);   /* Randomly assign conditions to any extra clusters left over after evenly assigning clusters to conditions.*/
                uj = SQRT(&icc/(1-&icc))*RAND("NORMAL");
                TauSqdU = &icc/(1-&icc);
                OUTPUT;
            END;
            DROP numSetsOfClusters numExtraClusters set extra;
        RUN;
        DATA clusters; SET clusters;
            clusterID = _N_;
        RUN;
        DATA members;
            DO clusterID = 1 TO &numClusters;
                DO memberID = 1 TO &clustersize;
                    OUTPUT;
                END;
            END;
        RUN;
        DATA members;
            MERGE clusters members;
            BY clusterID;
        RUN;
        DATA wide;
            SET singletons members;
            IF clusterID = . THEN clustered = 0; ELSE clustered = 1;
            IF clustered = 1 THEN j = clusterID; ELSE j = &numClusters + singletonID;
        RUN;
        PROC SORT DATA=wide; BY cond; RUN;
        DATA wide;
            MERGE wide save.allConditions;
            BY cond;
            DROP clusterID memberID singletonID;
        RUN; 
        DATA wide;
            SET wide;
            i = _N_;
            Pij = RAND("NORMAL");
            gammaP =  .65 ;
            unequalVariances = &unequalVariances;
            IF unequalVariances = 0 then errorVariance = 1 - gammaP**2;
            IF unequalVariances = 1 then errorVariance = (1 - gammaP**2)*((4/3)-(2/3)*clustered);
            eij = SQRT(errorVariance) * RAND("NORMAL");
            SigmaSqdEStar = 1 - gammaP**2;
            Yij = uj + (&d/2)*x1 + (&d/2)*x3 + (&d/2)*x1*x3 + gammaP*Pij + eij; 
            dropout = RAND("BERNOULLI",&dropoutRate);
        RUN; 
		/* Analysis with equal variance assumption */
        PROC MIXED DATA=wide NOCLPRINT;
            CLASS  x1 x2 x3 x4 x5 j;
            MODEL Yij = Pij x1 x2 x3 x4 x5
                            x1*x2 x1*x3 x1*x4 x1*x5
                            x2*x3 x2*x4 x2*x5
                            x3*x4 x3*x5
                            x4*x5 / DDFM=SATTERTHWAITE;  
            RANDOM clustered / SUBJECT = j(x1 x2 x3 x4 x5); 
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
        DATA allTestsEV; SET allTestsEV OutputAncova; RUN; 
        DATA allCovParmsEV; SET allCovParmsEV cpt; RUN;  
        PROC DATASETS NOLIST NOWARN;
            DELETE OutputAncova cp cpt;
        RUN; 
        DATA save.allTestsEV; SET allTestsEV; RUN;
        DATA save.allCovParmsEV; SET allCovParmsEV; RUN; 
		/* Analysis without equal variance assumption */
        PROC MIXED DATA=wide NOCLPRINT;
            CLASS  x1 x2 x3 x4 x5 j;
            MODEL Yij = Pij x1 x2 x3 x4 x5
                            x1*x2 x1*x3 x1*x4 x1*x5
                            x2*x3 x2*x4 x2*x5
                            x3*x4 x3*x5
                            x4*x5 / DDFM=SATTERTHWAITE;  
            RANDOM clustered / SUBJECT = j(x1 x2 x3 x4 x5);
            REPEATED / SUB=i LOCAL=EXP(clustered) TYPE=VC; 
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
        DATA allTests; SET allTests OutputAncova; RUN; 
        DATA allCovParms; SET allCovParms cpt; RUN;  
        PROC DATASETS NOLIST NOWARN;
            DELETE OutputAncova cp cpt;
        RUN; 
        DATA save.allTests; SET allTests; RUN;
        DATA save.allCovParms; SET allCovParms; RUN; 
		DATA save.wide; SET wide; RUN;
    %END;
%MEND;
%DoLoop;

ods exclude none;
ods results;

PROC SORT DATA=allTestsEV; BY effect; RUN;
PROC MEANS DATA=allTestsEV; WHERE rep>0; BY effect; OUTPUT OUT=allTestsAnswersEV; RUN;
PROC PRINT DATA=allTestsAnswersEV; RUN; 
PROC MEANS DATA=allCovParmsEV; WHERE rep>0; OUTPUT OUT=allCovParmsAnswersEV; RUN;
PROC PRINT DATA=allCovParmsAnswersEV; RUN;

PROC SORT DATA=allTests; BY effect; RUN;
PROC MEANS DATA=allTests; WHERE rep>0; BY effect; OUTPUT OUT=allTestsAnswers; RUN;
PROC PRINT DATA=allTestsAnswers; RUN; 
PROC MEANS DATA=allCovParms; WHERE rep>0; OUTPUT OUT=allCovParmsAnswers; RUN;
PROC PRINT DATA=allCovParmsAnswers; RUN;

DATA save.allTestsAnswersEV; SET allTestsAnswersEV; RUN;
DATA save.allCovParmsAnswersEV; SET allCovParmsAnswersEV; RUN;
DATA save.allTestsAnswers; SET allTestsAnswers; RUN;
DATA save.allCovParmsAnswers; SET allCovParmsAnswers; RUN;
