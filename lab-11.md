Lab 11 - Grading the professor, Pt. 2
================
Ben Waggener
04/11/2025

## Load packages and data

``` r
library(tidyverse) 
```

    ## Warning: package 'tidyverse' was built under R version 4.4.3

    ## Warning: package 'ggplot2' was built under R version 4.4.3

    ## Warning: package 'purrr' was built under R version 4.4.3

``` r
library(tidymodels)
```

    ## Warning: package 'tidymodels' was built under R version 4.4.3

    ## Warning: package 'broom' was built under R version 4.4.3

    ## Warning: package 'dials' was built under R version 4.4.3

    ## Warning: package 'infer' was built under R version 4.4.3

    ## Warning: package 'modeldata' was built under R version 4.4.3

    ## Warning: package 'parsnip' was built under R version 4.4.3

    ## Warning: package 'recipes' was built under R version 4.4.3

    ## Warning: package 'rsample' was built under R version 4.4.3

    ## Warning: package 'tune' was built under R version 4.4.3

    ## Warning: package 'workflows' was built under R version 4.4.3

    ## Warning: package 'workflowsets' was built under R version 4.4.3

    ## Warning: package 'yardstick' was built under R version 4.4.3

``` r
library(openintro)
```

    ## Warning: package 'openintro' was built under R version 4.4.3

    ## Warning: package 'airports' was built under R version 4.4.3

    ## Warning: package 'cherryblossom' was built under R version 4.4.3

    ## Warning: package 'usdata' was built under R version 4.4.3

\##Exercise 1

“Fit a linear model (one you have fit before): m_bty, predicting average
professor evaluation score based on average beauty rating (bty_avg)
only. Write the linear model, and note the  
R2 and the adjusted R2.”

*score = 3.88 + .066(beauty average) R2 = .035 adjusted = .033*

``` r
evals <- openintro::evals

m_bty <- lm(score ~ bty_avg, data = evals)
summary(m_bty)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9246 -0.3690  0.1420  0.3977  0.9309 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.88034    0.07614   50.96  < 2e-16 ***
    ## bty_avg      0.06664    0.01629    4.09 5.08e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5348 on 461 degrees of freedom
    ## Multiple R-squared:  0.03502,    Adjusted R-squared:  0.03293 
    ## F-statistic: 16.73 on 1 and 461 DF,  p-value: 5.083e-05

## Exercise 2

“Fit a linear model (one you have fit before): m_bty_gen, predicting
average professor evaluation score based on average beauty rating
(bty_avg) and gender. Write the linear model, and note the  
R 2 and the adjusted R2.”

*score = 3.75 + .074(average beauty score) + .17(gender_male) R2 = .059,
adjusted = .055*

``` r
m_bty_gen <- lm(score ~ bty_avg + gender, data = evals)
summary(m_bty_gen)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + gender, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8305 -0.3625  0.1055  0.4213  0.9314 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.74734    0.08466  44.266  < 2e-16 ***
    ## bty_avg      0.07416    0.01625   4.563 6.48e-06 ***
    ## gendermale   0.17239    0.05022   3.433 0.000652 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5287 on 460 degrees of freedom
    ## Multiple R-squared:  0.05912,    Adjusted R-squared:  0.05503 
    ## F-statistic: 14.45 on 2 and 460 DF,  p-value: 8.177e-07

## Exercise 3

*for this linear model, the incept represents the evaluation score for a
hypothetical person who received a 0 for their average beauty rating and
is a woman. The slope for bty_avg means that increasing a beauty average
score by 1 results in a greater evaluation score by .074–on average. The
slope for gender means that being male, compared to female, increases
the evaluation score by .17–on average*

## Exercise 4

almost 6% of the variability is explained by this model accounting for
average beauty and gender.

\##Exercise 5

score = 3.92 + .074(average beauty score)— model for men

## Exercise 6

If two people have the same beauty rating, on average, the male
professor would get higher evaluation scores if the other person is
female.

\##Exercise 7 /“How does the relationship between beauty and evaluation
score vary between male and female professors?”

for female professors, a one point increase in beauty scores only
increase their overall evaluation by .031 while for men it increases by
.080. So beauty has a stronger influence on men compared to female’s
evaluation scores.

``` r
int_m_bty_gen <- lm(score ~ bty_avg * gender, data = evals)
summary(int_m_bty_gen)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg * gender, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8084 -0.3828  0.0903  0.4037  0.9211 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         3.95006    0.11800  33.475   <2e-16 ***
    ## bty_avg             0.03064    0.02400   1.277   0.2024    
    ## gendermale         -0.18351    0.15349  -1.196   0.2325    
    ## bty_avg:gendermale  0.07962    0.03247   2.452   0.0146 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5258 on 459 degrees of freedom
    ## Multiple R-squared:  0.07129,    Adjusted R-squared:  0.06522 
    ## F-statistic: 11.74 on 3 and 459 DF,  p-value: 1.997e-07

\##Exercise 8 the model with gender accounts for more variability in the
scores meaning that gender and beauty explains more than just beauty
alone. Gender adds additional predictive power even if beauty is already
being conseridered.

\##Exercise 9 Yes adding gender changed the slope of beauty in the
model.This probably means taht they are somewhat related.

\##Exercise 10

score = 3.98 + .068(beauty) + -.16(tenure track) + -.13(tenured)

This means that for someone who is just teaching, one unit changes in
beauty scoers increase evaluations scores by .068. For someone who is
tenured, beatuy has the same effect (+.068) they have a lower baseline
comapared to professors who are teaching (-.16). This is the same for
tenured professors (-.13).

``` r
m_bty_rank <- lm(score ~ bty_avg + rank, data = evals)
summary(m_bty_rank)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + rank, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8713 -0.3642  0.1489  0.4103  0.9525 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       3.98155    0.09078  43.860  < 2e-16 ***
    ## bty_avg           0.06783    0.01655   4.098 4.92e-05 ***
    ## ranktenure track -0.16070    0.07395  -2.173   0.0303 *  
    ## ranktenured      -0.12623    0.06266  -2.014   0.0445 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5328 on 459 degrees of freedom
    ## Multiple R-squared:  0.04652,    Adjusted R-squared:  0.04029 
    ## F-statistic: 7.465 on 3 and 459 DF,  p-value: 6.88e-05

\##Exercise 11 I think cls_profs will be the worst predictor of
evaluation scores from that group. My thinking is that whether it is a
one or many professors teaching that same course will not affect a
single professors evaluations.

## Exercise 12

Yeah the R2 seems pretty low.

``` r
m_clsprofs <- lm(score ~cls_profs, data = evals)
summary(m_clsprofs)
```

    ## 
    ## Call:
    ## lm(formula = score ~ cls_profs, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8554 -0.3846  0.1154  0.4154  0.8446 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      4.18464    0.03111 134.493   <2e-16 ***
    ## cls_profssingle -0.02923    0.05343  -0.547    0.585    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5443 on 461 degrees of freedom
    ## Multiple R-squared:  0.0006486,  Adjusted R-squared:  -0.001519 
    ## F-statistic: 0.2992 on 1 and 461 DF,  p-value: 0.5847

\##Exercise 13 If i am including cls_perc_eval and cls_students in the
model then i should not also include cls_did_eval because that is
mathematically the same as the first two which would reduce the adjusted
R2 because it does not add any additional predictive ability.

\##Exercise 14

``` r
m_all <- lm(score ~ rank + ethnicity + gender + language + age + 
             cls_perc_eval + cls_students + cls_level + cls_profs + 
             cls_credits + bty_avg, data = evals)
summary(m_all)
```

    ## 
    ## Call:
    ## lm(formula = score ~ rank + ethnicity + gender + language + age + 
    ##     cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + 
    ##     bty_avg, data = evals)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.84482 -0.31367  0.08559  0.35732  1.10105 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.5305036  0.2408200  14.660  < 2e-16 ***
    ## ranktenure track      -0.1070121  0.0820250  -1.305 0.192687    
    ## ranktenured           -0.0450371  0.0652185  -0.691 0.490199    
    ## ethnicitynot minority  0.1869649  0.0775329   2.411 0.016290 *  
    ## gendermale             0.1786166  0.0515346   3.466 0.000579 ***
    ## languagenon-english   -0.1268254  0.1080358  -1.174 0.241048    
    ## age                   -0.0066498  0.0030830  -2.157 0.031542 *  
    ## cls_perc_eval          0.0056996  0.0015514   3.674 0.000268 ***
    ## cls_students           0.0004455  0.0003585   1.243 0.214596    
    ## cls_levelupper         0.0187105  0.0555833   0.337 0.736560    
    ## cls_profssingle       -0.0085751  0.0513527  -0.167 0.867458    
    ## cls_creditsone credit  0.5087427  0.1170130   4.348  1.7e-05 ***
    ## bty_avg                0.0612651  0.0166755   3.674 0.000268 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.504 on 450 degrees of freedom
    ## Multiple R-squared:  0.1635, Adjusted R-squared:  0.1412 
    ## F-statistic: 7.331 on 12 and 450 DF,  p-value: 2.406e-12

\##Exercise 15 backward

score = 3.51 +.208(ethnicity_notminority) + .15(gender_male) + .58(class
credit) + .08(beauty)

``` r
m_4 <- lm(score ~  ethnicity + gender  + 
             cls_credits + bty_avg, data = evals)
summary(m_4)
```

    ## 
    ## Call:
    ## lm(formula = score ~ ethnicity + gender + cls_credits + bty_avg, 
    ##     data = evals)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.84613 -0.35225  0.08913  0.38913  0.92237 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.51398    0.10406  33.767  < 2e-16 ***
    ## ethnicitynot minority  0.20775    0.07183   2.892  0.00401 ** 
    ## gendermale             0.14687    0.04903   2.996  0.00289 ** 
    ## cls_creditsone credit  0.58015    0.10546   5.501 6.29e-08 ***
    ## bty_avg                0.08214    0.01580   5.199 3.03e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5119 on 458 degrees of freedom
    ## Multiple R-squared:  0.1216, Adjusted R-squared:  0.114 
    ## F-statistic: 15.86 on 4 and 458 DF,  p-value: 3.644e-12

\##Exercise 16 the slope of beauty is .08 which means that for every 1
unit increase in beauty scores, the evaluation score increases by .08.
The slope of gender is .15 which means that when a professor is a male
(as opposed to a female) their evaluation scores are .15 points higher.

\##Exercise 17 Based on this model a person with a high score is likely
to be white, male, very attractive, and teach a 1 credit course.

## Exercise 18

While i think that this finding would likely replicate at another
school, i would not want to generalize too much. Especially because
these results might be different at a smaller school or a school in a
more diverse environment.
