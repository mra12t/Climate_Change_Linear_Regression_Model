Climate Change
================
2022-12-15

# Overview

The aim of this study is to investigate the connection between the
average global temperature and other variables. It has been well
documented that the average global temperature has been on the rise for
the past 100 years. If this trend continues, it will have disastrous
consequences, including rising sea levels and an increase in extreme
weather events. These changes will impact billions of people around the
world.

# About the Data set

The file climate_change.csv contains climate data from May 1983 to
December 2008. It includes the year and month of the observation, as
well as the difference in temperature between the average global
temperature in that period and a reference value. The file also includes
data on atmospheric concentrations of carbon dioxide (CO2), nitrous
oxide (N2O), methane (CH4), trichlorofluoromethane (CFC-11), and
dichlorodifluoromethane (CFC-12), as well as the mean stratospheric
aerosol optical depth and the total solar irradiance. The file also
includes data on the multivariate El Nino Southern Oscillation index
(MEI), a measure of the strength of the El Nino/La Nina-Southern
Oscillation weather effect.

# Creating the First Model

``` r
# loading the data set
climate = read.csv("climate_change.csv")
# Sub-setting the data set into training and testing data set with regards of the year
train = subset(climate, Year <= 2006)
test = subset(climate, Year > 2006)
# Creating Linear Regression Model
climatelm = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = train)
```

### Finding $R^2$ of the Model and the Significant Variables of the Model

``` r
summary(climatelm)
```

    ## 
    ## Call:
    ## lm(formula = Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + 
    ##     TSI + Aerosols, data = train)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.25888 -0.05913 -0.00082  0.05649  0.32433 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -1.246e+02  1.989e+01  -6.265 1.43e-09 ***
    ## MEI          6.421e-02  6.470e-03   9.923  < 2e-16 ***
    ## CO2          6.457e-03  2.285e-03   2.826  0.00505 ** 
    ## CH4          1.240e-04  5.158e-04   0.240  0.81015    
    ## N2O         -1.653e-02  8.565e-03  -1.930  0.05467 .  
    ## CFC.11      -6.631e-03  1.626e-03  -4.078 5.96e-05 ***
    ## CFC.12       3.808e-03  1.014e-03   3.757  0.00021 ***
    ## TSI          9.314e-02  1.475e-02   6.313 1.10e-09 ***
    ## Aerosols    -1.538e+00  2.133e-01  -7.210 5.41e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.09171 on 275 degrees of freedom
    ## Multiple R-squared:  0.7509, Adjusted R-squared:  0.7436 
    ## F-statistic: 103.6 on 8 and 275 DF,  p-value: < 2.2e-16

$R^2$ = 0.7509.  
Here, I consider a variable to be significant only if the p-value is
below 0.05. Which means variables *MEI*, *CO2*, *CFC.11*, *CFC.12*,
*TSI*, and *Aerosols* are all significant.

### Understanding the Model

According to current scientific opinion, nitrous oxide (N2O) and CFC-11
are greenhouse gases that contribute to the warming of the Earth by
trapping heat from the sun. However, the regression coefficients for
both N2O and CFC-11 are negative, indicating that higher concentrations
of these gases in the atmosphere are associated with lower global
temperatures.  
To try to explain this we can look at the correlation between these two
variables and the rest of the independent variables.

``` r
library(knitr)
z = cor(train)
kable(z)
```

|          |       Year |      Month |        MEI |        CO2 |        CH4 |        N2O |     CFC.11 |     CFC.12 |        TSI |   Aerosols |       Temp |
|:---------|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|
| Year     |  1.0000000 | -0.0279420 | -0.0369877 |  0.9827494 |  0.9156595 |  0.9938452 |  0.5691064 |  0.8970117 |  0.1703020 | -0.3452467 |  0.7867971 |
| Month    | -0.0279420 |  1.0000000 |  0.0008847 | -0.1067325 |  0.0185687 |  0.0136315 | -0.0131112 |  0.0006751 | -0.0346062 |  0.0148895 | -0.0998567 |
| MEI      | -0.0369877 |  0.0008847 |  1.0000000 | -0.0411472 | -0.0334193 | -0.0508198 |  0.0690004 |  0.0082855 | -0.1544919 |  0.3402378 |  0.1724708 |
| CO2      |  0.9827494 | -0.1067325 | -0.0411472 |  1.0000000 |  0.8772796 |  0.9767198 |  0.5140597 |  0.8526896 |  0.1774289 | -0.3561548 |  0.7885292 |
| CH4      |  0.9156595 |  0.0185687 | -0.0334193 |  0.8772796 |  1.0000000 |  0.8998386 |  0.7799040 |  0.9636162 |  0.2455284 | -0.2678092 |  0.7032550 |
| N2O      |  0.9938452 |  0.0136315 | -0.0508198 |  0.9767198 |  0.8998386 |  1.0000000 |  0.5224773 |  0.8679308 |  0.1997567 | -0.3370546 |  0.7786389 |
| CFC.11   |  0.5691064 | -0.0131112 |  0.0690004 |  0.5140597 |  0.7799040 |  0.5224773 |  1.0000000 |  0.8689852 |  0.2720460 | -0.0439212 |  0.4077103 |
| CFC.12   |  0.8970117 |  0.0006751 |  0.0082855 |  0.8526896 |  0.9636162 |  0.8679308 |  0.8689852 |  1.0000000 |  0.2553028 | -0.2251312 |  0.6875575 |
| TSI      |  0.1703020 | -0.0346062 | -0.1544919 |  0.1774289 |  0.2455284 |  0.1997567 |  0.2720460 |  0.2553028 |  1.0000000 |  0.0521165 |  0.2433827 |
| Aerosols | -0.3452467 |  0.0148895 |  0.3402378 | -0.3561548 | -0.2678092 | -0.3370546 | -0.0439212 | -0.2251312 |  0.0521165 |  1.0000000 | -0.3849137 |
| Temp     |  0.7867971 | -0.0998567 |  0.1724708 |  0.7885292 |  0.7032550 |  0.7786389 |  0.4077103 |  0.6875575 |  0.2433827 | -0.3849137 |  1.0000000 |

Here, we can confirm our initial hypothesis by observing that N2O is
highly correlated with CO2, CH4, and CFC.12. Furthermore, CFC-11 is also
highly correlated with both CH4 and CFC.12. (The threshold at which a
variable become highly correlated to another one has been choosen at
0.7)

### Simplifying the Model

Now that we have a deep understanding about our model, we can start make
changes and optimizing it.

``` r
simplifiedlm = lm(Temp ~ MEI + CO2 + N2O + TSI + Aerosols, data = train)
summary(simplifiedlm)
```

    ## 
    ## Call:
    ## lm(formula = Temp ~ MEI + CO2 + N2O + TSI + Aerosols, data = train)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.26454 -0.06015 -0.00656  0.05830  0.34365 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -1.168e+02  1.990e+01  -5.867 1.26e-08 ***
    ## MEI          6.252e-02  6.567e-03   9.521  < 2e-16 ***
    ## CO2          7.344e-03  2.309e-03   3.181  0.00163 ** 
    ## N2O          8.231e-03  5.525e-03   1.490  0.13745    
    ## TSI          8.184e-02  1.466e-02   5.584 5.60e-08 ***
    ## Aerosols    -1.602e+00  2.168e-01  -7.390 1.73e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.09395 on 278 degrees of freedom
    ## Multiple R-squared:  0.7357, Adjusted R-squared:  0.731 
    ## F-statistic: 154.8 on 5 and 278 DF,  p-value: < 2.2e-16

### Comparing the two Models

In our new model we can see that $R^2$ has become 0.7357 which is a
0.0152 drop of the previous 0.7509 of the first model. However, the sign
of N2O has flipped and the model hasn’t lost its explanatory power. This
drop will occur because of the high correlation between the variables
which is normal given these gases are produced as a result of human’s
industrial revolution.

### Automating the Optimization Process

The step() function uses the Akaike information criterion (AIC) to find
a good compromise between model simplicity and $R^2$. The AIC is a
measure of the quality of the model that takes into account the number
of variables in the model. In general, a model with a lower AIC is
considered to be a better model, because it is a simpler model that
still explains the data well. The step function will try different
combinations of variables and select the one that results in the lowest
AIC.

``` r
StepModel = step(climatelm)
```

    ## Start:  AIC=-1348.16
    ## Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols
    ## 
    ##            Df Sum of Sq    RSS     AIC
    ## - CH4       1   0.00049 2.3135 -1350.1
    ## <none>                  2.3130 -1348.2
    ## - N2O       1   0.03132 2.3443 -1346.3
    ## - CO2       1   0.06719 2.3802 -1342.0
    ## - CFC.12    1   0.11874 2.4318 -1335.9
    ## - CFC.11    1   0.13986 2.4529 -1333.5
    ## - TSI       1   0.33516 2.6482 -1311.7
    ## - Aerosols  1   0.43727 2.7503 -1301.0
    ## - MEI       1   0.82823 3.1412 -1263.2
    ## 
    ## Step:  AIC=-1350.1
    ## Temp ~ MEI + CO2 + N2O + CFC.11 + CFC.12 + TSI + Aerosols
    ## 
    ##            Df Sum of Sq    RSS     AIC
    ## <none>                  2.3135 -1350.1
    ## - N2O       1   0.03133 2.3448 -1348.3
    ## - CO2       1   0.06672 2.3802 -1344.0
    ## - CFC.12    1   0.13023 2.4437 -1336.5
    ## - CFC.11    1   0.13938 2.4529 -1335.5
    ## - TSI       1   0.33500 2.6485 -1313.7
    ## - Aerosols  1   0.43987 2.7534 -1302.7
    ## - MEI       1   0.83118 3.1447 -1264.9

``` r
summary(StepModel)
```

    ## 
    ## Call:
    ## lm(formula = Temp ~ MEI + CO2 + N2O + CFC.11 + CFC.12 + TSI + 
    ##     Aerosols, data = train)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.25770 -0.05994 -0.00104  0.05588  0.32203 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -1.245e+02  1.985e+01  -6.273 1.37e-09 ***
    ## MEI          6.407e-02  6.434e-03   9.958  < 2e-16 ***
    ## CO2          6.402e-03  2.269e-03   2.821 0.005129 ** 
    ## N2O         -1.602e-02  8.287e-03  -1.933 0.054234 .  
    ## CFC.11      -6.609e-03  1.621e-03  -4.078 5.95e-05 ***
    ## CFC.12       3.868e-03  9.812e-04   3.942 0.000103 ***
    ## TSI          9.312e-02  1.473e-02   6.322 1.04e-09 ***
    ## Aerosols    -1.540e+00  2.126e-01  -7.244 4.36e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.09155 on 276 degrees of freedom
    ## Multiple R-squared:  0.7508, Adjusted R-squared:  0.7445 
    ## F-statistic: 118.8 on 7 and 276 DF,  p-value: < 2.2e-16

Here, we can see that the step() function has found a model with an
$R^2$ of 0.7508 and by excluding CH4 from the independent variables of
the model.

# Testing the Final Model

``` r
prid = predict(StepModel, newdata = test)
# Error Sum of Squares 
SSE = sum((prid-test$Temp)^2)
# Total Sum of Squares
SST = sum((mean(train$Temp)-test$Temp)^2)
#R-Squared
R2 = 1 - SSE/SST
R2
```

    ## [1] 0.6286051

$R^2$ is equal to 0.6286051 which is fair given that our model is a
simple Linear Regression Model
