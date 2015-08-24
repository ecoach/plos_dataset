***This file includes statistical analysis for E2Coach data of 
winter 2012, fall 2012, and winter 2013 (three full length courses) 
and those who finished the class. It is to be used in conjunction with the
data published at https://github.com/ecoach/plos_dataset.

ONEWAY GPA BY UserGroups
  /POLYNOMIAL=1
  /STATISTICS DESCRIPTIVES HOMOGENEITY 
  /MISSING ANALYSIS.


FREQUENCIES VARIABLES=UserGroups
  /STATISTICS=MINIMUM MAXIMUM MEAN SUM
  /ORDER=ANALYSIS.

**Recode into those E2Coach users math levels into who didn't take calc (1), those who took AB (2), and those who took BC or higher (3)**

RECODE HS_Math (1=1) (2=1) (3=1) (4=2) (5=3) (6=3) (7=3) (8=3) (9=3) INTO HSMathLevels.
VARIABLE LABELS  HSMathLevels 'What level math did they take in high school?'.
EXECUTE.

SORT CASES  BY UserGroups.
SPLIT FILE SEPARATE BY UserGroups.

FREQUENCIES VARIABLES= Gender
  /STATISTICS=MINIMUM MAXIMUM MEAN SUM
  /ORDER=ANALYSIS.

DESCRIPTIVES VARIABLES=SAT_Math ACT_Math
  /STATISTICS=MEAN STDDEV MIN MAX.

SPLIT FILE OFF.

CROSSTABS
  /TABLES=UserGroups BY HSMathLevels HS_Physics
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT ROW COLUMN 
  /COUNT ROUND CELL.


**Comparing physics courses and semesters in which the class was taken**

ONEWAY BTE_Quad_Reg BY Term
  /POLYNOMIAL=1
  /STATISTICS DESCRIPTIVES HOMOGENEITY 
  /MISSING ANALYSIS.

ONEWAY BTE_Quad_Reg BY Course
  /POLYNOMIAL=1
  /STATISTICS DESCRIPTIVES HOMOGENEITY 
  /MISSING ANALYSIS.


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


*Regression with controls. DV: BTE score. IV: HS math, physics, and ACT schore.

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA CHANGE
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT BTE_Quad_Reg
  /METHOD=ENTER HS_Math HS_Physics ACT_Math
  /METHOD=ENTER UserGroups.

*Testing ECoach usage and gender.

ONEWAY BTE_Quad_Reg BY Gender
  /POLYNOMIAL=1
  /STATISTICS DESCRIPTIVES HOMOGENEITY 
  /MISSING ANALYSIS.

*Testing the interaction between ECoach usage and gender.

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



USE ALL.
COMPUTE filter_$=(UserGroups = 1 | UserGroups = 2 | UserGroups = 3).
VARIABLE LABELS filter_$ 'users only (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


UNIANOVA BTE_Quad_Reg BY Gender HSMathLevels
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /CRITERIA=ALPHA(0.05)
  /DESIGN=Gender HSMathLevels Gender*HSMathLevels.


UNIANOVA BTE_Quad_Reg BY Gender HS_Physics
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /CRITERIA=ALPHA(0.05)
  /DESIGN=Gender HS_Physics Gender*HS_Physics.


**Table 7 descriptives**

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


