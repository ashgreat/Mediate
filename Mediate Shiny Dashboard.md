## Mediate Shiny Dashboard
Mediate is an experimental web-based application, or app, for doing mediation analysis and obtaining bootstrap confidence intervals. I, Ashwin Malshe, am the sole developer of the app. For any suggestions, comments, concerns, etc., please contact me on ashwin.malshe@utsa.edu

### Why use Mediate?

* As Mediate is web-based, all it requires is a computer with Internet connection and a modern browser (e.g., Safari, Chrome, Firefox).
* Mediate is free. It uses the open-source R software to perform mediation analysis. (*It requires some funds to host the app on the web but I can afford to pay for it.*)
* Mediate has a functional graphic user interface (GUI), which gives you granular control over your analysis without writing any code.
* Mediate shows you all the parameters you used for the analysis including the file name, variable names, random number seed, number of bootstrap samples, and confidence interval. You can simply copy this in a text file for easy replicability.
* Mediate makes mediation plots.
* Mediate allows you to export tables and plots in PDF and Word formats.
* Mediate is secure because it doesn't store your data. After every session, your data is purged from the servers. Mediate currently allows log-in free operation. (*I may add the ability to create accounts in future.*)
* Mediate can read data files in various formats including Comma-Separated Values (.csv), Microsoft Excel (.xls, .xlsx), Stata (.dta), SAS (.sas7bdat), and SPSS (.sav). This makes Mediate software-agnostic.

### Limitations (as of 2018-03-26)
* It can handle only 3 models from PROCESS (4, 6, and 8). However, these are the 3 most often used models in consumer research. I did a quick check of all the *Journal of Consumer Research* articles mentioning using PROCESS macro published since 2010 and found that almost all the articles used one of the above 3 models. I will add more models as we go forward.
* Model 4 and 8 can handle only 1 mediator at this point. Model 6 can handle 2 mediators in sequence.
* Model 8 can handle only 1 moderator at this point. It has to be a binary variable (having only 2 levels).


### Future plans
* More mediators in existing models
* More models
* Handle multiple levels in a moderator
* Include the "Floodlight Analysis" for continuous moderators

### Acknowledgements

I thanks Arash Talebi and Yuan Li for their help.

Arash Talebi, a marketing PhD student at ESSEC Business School, helped me extensively in testing the app and comparing it with PROCESS macro. 

Yuan (Susan) Li, a marketing PhD student at UTSA, helped me in initial testing of the app.
