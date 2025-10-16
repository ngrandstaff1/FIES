***************************
** Predicting Old People **
***************************
clear all
sysuse auto
** Use "foreign" as a stand-in for "safe."

** This is what I would do.  It might not be perfect, but it is tractable.

** T-test **
* Can write a loop over a list of variables for this:
	*LOOP BEGINS: forvars in (var1 var2 ... )
		ttest price, by(foreign)
		return list
		scalar foreign_p = r(p)
		// etc. for extracting to a balance table
	*LOOP ENDS

** Test via Conditional Means (regression) **
* Predict Model #1
	reg foreign price headroom mpg
	predict xb1 // linear prediction of foreign based on (price headroom mpg)
	gen predict_1 = 0
	replace predict_1 = 1 if xb1 > 0.5
	tab foreign
	tab predict_1

* Predict Model #2
	reg foreign price headroom mpg length turn // etc. covariates
	predict xb2 // linear prediction of foreign based on (price headroom mpg)
	gen predict_2 = 0
	replace predict_2 = 1 if xb2 > 0.5
	tab foreign
	tab predict_2

// etc.
// etc.

** These could also be fit using probit/logit and the associate predict command.  Either way, you can set safe/unsafe and compare t-tests on observed data and the predicted values.  OLS (without ", robust") comes with its own set of assumptions (linearity if OLS and not probit/logit) and symmetric and normal error distributions. 

** Relevant post-estimation coding page for Stata to quickly make this comparison into a table is:
https://www.stata.com/manuals/restatclassification.pdf#restatclassification











