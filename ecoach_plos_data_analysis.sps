***This file includes analysis for E2Coach data of 
winter 2012, fall 2012, and winter 2013 (three full length courses) 
and those who finished the class. It is to be used in conjunction with the
data published at https://github.com/ecoach/plos_dataset. Some of the analyses
presented here are published in the accompanying PLOS paper.


FREQUENCIES VARIABLES=UserGroups Course Term Gender
  /STATISTICS=MINIMUM MAXIMUM MEAN SUM
  /ORDER=ANALYSIS.

SORT CASES  BY UserGroups.
SPLIT FILE SEPARATE BY UserGroups.

FREQUENCIES VARIABLES= Gender
  /STATISTICS=MINIMUM MAXIMUM MEAN SUM
  /ORDER=ANALYSIS.


DESCRIPTIVES VARIABLES=SAT_Math ACT_Math HS_Math HS_Physics
  /STATISTICS=MEAN STDDEV MIN MAX.

SPLIT FILE OFF.

*ANOVA and planned contrasts of ECoach usage effects on BTE score.

UNIANOVA BTE_Quad_Reg BY UserGroups
  /CONTRAST(UserGroups)=Simple(1)
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /POSTHOC=UserGroups(TUKEY) 
  /PLOT=PROFILE(UserGroups)
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(UserGroups) COMPARE ADJ(LSD)
  /PRINT=ETASQ HOMOGENEITY DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=UserGroups.


ONEWAY BTE_Quad_Reg BY UserGroups
  /POLYNOMIAL=1
  /CONTRAST=-3 1 1 1
  /CONTRAST=-1 1 0 0
  /CONTRAST=-1 0 1 0
  /CONTRAST=-1 0 0 1
  /CONTRAST=0 -1 1 0
  /CONTRAST=0 0 -1 1
  /CONTRAST=0 -1 0 1
  /CONTRAST=-1 -1 -1 3 
  /CONTRAST=0 -1 -1 2
  /STATISTICS HOMOGENEITY  
  /MISSING ANALYSIS.

***Final graph used with 1 SE error bars.

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=UserGroups MEANSE(BTE_Quad_Reg, 
    1)[name="MEAN_BTE_Quad_Reg" LOW="MEAN_BTE_Quad_Reg_LOW" HIGH="MEAN_BTE_Quad_Reg_HIGH"] 
    MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: UserGroups=col(source(s), name("UserGroups"), unit.category())
  DATA: MEAN_BTE_Quad_Reg=col(source(s), name("MEAN_BTE_Quad_Reg"))
  DATA: LOW=col(source(s), name("MEAN_BTE_Quad_Reg_LOW"))
  DATA: HIGH=col(source(s), name("MEAN_BTE_Quad_Reg_HIGH"))
  GUIDE: axis(dim(1), label("User Groups"))
  GUIDE: axis(dim(2), label("Mean BTE_Quad_Reg"))
  GUIDE: text.footnote(label("Error Bars: +/- 1 SE"))
  SCALE: cat(dim(1), include(".00", "1.00", "2.00", "3.00"))
  SCALE: linear(dim(2), include(0))
  ELEMENT: interval(position(UserGroups*MEAN_BTE_Quad_Reg), shape.interior(shape.square))
  ELEMENT: interval(position(region.spread.range(UserGroups*(LOW+HIGH))), 
    shape.interior(shape.ibeam))
END GPL.


*Regression with controls.

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA CHANGE
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT BTE_Quad_Reg
  /METHOD=ENTER Gender Term Course
  /METHOD=ENTER UserGroups.

***Checking controls.
*Comparing types of students between terms.

ONEWAY BTE_Quad_Reg BY Term
  /POLYNOMIAL=1
  /STATISTICS DESCRIPTIVES HOMOGENEITY 
  /MISSING ANALYSIS.

ONEWAY BTE_Quad_Reg BY Course
  /POLYNOMIAL=1
  /STATISTICS DESCRIPTIVES HOMOGENEITY 
  /MISSING ANALYSIS.

ONEWAY BTE_Quad_Reg BY Gender
  /POLYNOMIAL=1
  /STATISTICS DESCRIPTIVES HOMOGENEITY 
  /MISSING ANALYSIS.


*Regression with controls.

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA CHANGE
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT BTE_Quad_Reg
  /METHOD=ENTER HS_Math HS_Physics ACT_Math
  /METHOD=ENTER UserGroups.


*Testing interaction between ECoach usage and gender.


UNIANOVA BTE_Quad_Reg BY Gender UserGroups
  /CONTRAST(Gender)=Simple(1)
  /CONTRAST(UserGroups)=Simple(1)
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PLOT=PROFILE(UserGroups*Gender)
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(Gender) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(UserGroups) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(Gender*UserGroups) 
  /PRINT=ETASQ HOMOGENEITY DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=Gender UserGroups Gender*UserGroups.


*Only considering users, excluding nonusers.

UNIANOVA BTE_Quad_Reg BY UserGroups_onlyusers Gender
  /CONTRAST(UserGroups_onlyusers)=Simple(1)
  /CONTRAST(Gender)=Simple(1)
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PLOT=PROFILE(UserGroups_onlyusers*Gender)
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(UserGroups_onlyusers) 
  /EMMEANS=TABLES(Gender) 
  /EMMEANS=TABLES(UserGroups_onlyusers*Gender) 
  /PRINT=ETASQ HOMOGENEITY DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=UserGroups_onlyusers Gender UserGroups_onlyusers*Gender.



*Frequencies.

USE ALL. 
EXECUTE. 
FREQUENCIES VARIABLES=Gender Term Course GPA filter_$ UserGroups 
  /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN SUM 
  /ORDER=ANALYSIS.

*Winter 2012*

USE ALL.
COMPUTE filter_$=(Term = 0).
VARIABLE LABELS filter_$ 'Term = 0 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

SORT CASES  BY UserGroups.
SPLIT FILE SEPARATE BY UserGroups.

FREQUENCIES VARIABLES=User Term Sum_Clicks_All Weeks_Visited_All
  /FORMAT=NOTABLE
  /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN SUM
  /ORDER=ANALYSIS.

*Fall 2012*

USE ALL.
COMPUTE filter_$=(Term = 1).
VARIABLE LABELS filter_$ 'Term = 1  (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

SORT CASES  BY UserGroups.
SPLIT FILE SEPARATE BY UserGroups.

FREQUENCIES VARIABLES=User Term Sum_Clicks_All Weeks_Visited_All
  /FORMAT=NOTABLE
  /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN SUM
  /ORDER=ANALYSIS.

*Winter 2013*

USE ALL.
COMPUTE filter_$=(Term = 3).
VARIABLE LABELS filter_$ 'Term = 3 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

SORT CASES  BY UserGroups.
SPLIT FILE SEPARATE BY UserGroups.

FREQUENCIES VARIABLES=User Term Sum_Clicks_All Weeks_Visited_All
  /FORMAT=NOTABLE
  /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN SUM
  /ORDER=ANALYSIS.


SPLIT FILE OFF.
USE ALL. 
EXECUTE. 



USE ALL.
COMPUTE filter_$=(UserGroups = 1 | UserGroups = 2 | UserGroups = 3).
VARIABLE LABELS filter_$ 'Term = 3 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

EXAMINE VARIABLES=SAT_Math ACT_Math BY Gender
  /PLOT NONE
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.

CROSSTABS
  /TABLES=HS_Physics BY Gender
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT ROW COLUMN 
  /COUNT ROUND CELL.


**Recode into those who didn't take calc, those who took AB, and those who took BC or higher**

RECODE HS_Math (1=1) (2=1) (3=1) (4=2) (5=3) (6=3) (7=3) (8=3) (9=3) INTO HSMathLevels.
VARIABLE LABELS  HSMathLevels 'What level math did they take in high school?'.
EXECUTE.


CROSSTABS
  /TABLES=HSMathLevels BY Gender
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT ROW COLUMN 
  /COUNT ROUND CELL.


USE ALL. 
EXECUTE. 


EXAMINE VARIABLES=GPA BTE_Quad_Reg Grade_Percent_Reg BY UserGroups
  /PLOT NONE
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.

CROSSTABS
  /TABLES=UserGroups BY Gender
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT ROW COLUMN 
  /COUNT ROUND CELL.



