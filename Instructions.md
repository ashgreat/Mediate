## Instructions for Usage

1. Upload your data set to the app. The file must be in one of the following formats: Excel (.xls, .xlsx), Text (.csv), SPSS (.sav), Stata (.dta), and SAS (.sas7bdat). Once you upload the file, "Setup" window on right shows first 6 observations from your data set.
2. Choose your PROCESS model from the dropdown list.
3. Select your indepedent variable (X-variable).
4. Select your mediating variable. 
5. Select your dependent variable (Y-variable).
6. Choose covariates foe the mediating variable.
7. If you chose Model 4, skip to step 10.
8. If you chose Model 6, select a second mediating variable.
9. If you chose Model 8, select a moderating variable.
10. Optionally, Mediate App lets you adjust the following 3 bootstarp parameters:
	- Random number generator seed (default is 123456)
    - Number of bootstrap samples (default is 1000)
    - Confidence level (default is 95%)
11. All the model setup inputs are visible in the Setup tab. Once you are satisfied with your these inputs, click "Run Mediation".
12. The tab automatically switches from Setup to "Results". Depending on the number of bootstrap samples you requested, the app will take some time to show results. Please be patient.
13. In the results tab you will have 3 to 4 tables.


### Results structure

1. All the 3 models will have the first table for mediating variable regression.
2. For Model 6, the second table will be for 2nd mediating variable regression.
3. For Model 4 and Model 8, the second table will be for dependent variable regression. For Model 6, dependent variable regression will be in Table 3.
3. Finally, the last table will show the mediation effects and their bootstrap confidence intervals.


### Important notes about Model 8

1. Model 8 requires a binary moderator. It can have any numeric values. Mediate App will internally convert moderating variable to a dummy variable with values equaling 0 and 1. Mediate App will print a message showing which levels it converted to 0 and 1.
2. Model 8 will have 2 indirect effects and two total effects corresponding to two values of the dummy moderating variable.

## Download Reports

1. Mediate App allows you to download the model setup and the results as PDF and Word documents. Formatting for PDF is much more refined compared to Word document due to the way Pandoc handles documents. However, if you plan to copy and paste the tables in other documents, Word format will give you better flexibility.
