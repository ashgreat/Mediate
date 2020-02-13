## A Simple Example

I have created a toy data set in Excel format for you to try out the app. There are 100 observations and a few variables. The variable names make it easy for you to identify what their purpose is.

### Data set description

The data has following variables:
* *x* - The independent variable
* *y* - The dependent variable
* *med1* and *med2* - The mediating variables. For Model 4 and 8 you need to use only one mediating variable.
* *covar1-covar3* - Optional covariates. The covariates are only for the mediating variable regressions and not for the dependent variable.
* *mod1-mod3* - Moderating variables. You will use only one of these for Model 8. However, there is a reason for including 3 of them. *mod1* is binary and uses dummy coding: 0 and 1. *mod2* is not binary and has 3 levels. If you use this, the app should give you an error. Finally, *mod3* is binary but its coding is 1 and 2 instead of 0 and 1. The app will still use it but internally convert the levels to 0 and 1. In the output, this is specified at the bottom of Table 3.

[Download](https://www.dropbox.com/s/17q97ljxink5n9s/DemoData.xls?dl=0) the data in Excel format.
