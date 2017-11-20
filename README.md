# HETE - A toolbox for modeling heterogeneous treatment effects

## Usage
```r
library(tidyverse)
library(hete)

data(gotv)

df <- gotv %>%
  filter(treatment %in% c("Control", "Neighbors")) %>%
  mutate(treatment = ifelse(treatment == "Control", 0, 1))

m <- hete_single(voted ~ . | treatment, data = df, est = random_forest)
p <- predict(m, df)
```

## Notes
This package makes heavy use of partials to make all the components fit well
together. There are few standard function signatures used everywhere:

- estimator/base learner: `function(x, y) -> S3`, this roughly corresponds
to models in R. The function should take a design matrix `x`, and an array of
outcomes `y`. The return value
should be an `S3` object which has a `predict` implementation.

- hete estimator: `function(x, y, tmt) -> S3`, similar to the estimator above
but with the addition of the treatment indicator, `tmt`. This interface becomes
important when working with some of the ensemble models or using the
cross-validation tools.

## References

### Talks

1. [The Power of Persuasion Modeling](https://speakerdeck.com/wlattner/the-power-of-persuasion-modeling), Strata + Hadoop World, 2017.

### References

1. Taddy, M., et al. (2015). A nonparametric Bayesian analysis of heterogeneous treatment effects in digital experimentation. arXiv: 1412.8563

2. Siegel, E. (2011). Uplift Modeling: Predictive Analytics: Can't Optimize Marketing Decisions Without It. Precision Impact White Paper.

3. Feller, A. and Holmes, C. (2009). Beyond Toplines: Heterogeneous Treatment Effects in Randomized Experiments.

4. Athey, S. and Imbens, G. (2016). The Econometrics of Randomized Experiments. arXiv: 1607.00698

5. Hill, J. (2010). Bayesian Nonparametric Modeling for Causal Inference. Journal of Computational and Graphical Statistics, 1-24.

6. Grimmer, J., Messing, S., and Westwood, S. (2016). Estimating Heterogeneous Treatment Effects and the Effects of Heterogeneous Treatments with Ensemble Methods.

7. Wager, S. and Athey, S. (2016). Estimation and Inference of Heterogeneous Treatment Effects using Random Forests. arXiv: 1510.04342

8. Athey, S. and Imbens, G. (2016). Recursive partitioning for heterogeneous causal effects. PNAS 113(24):7353-7360.

9. Athey, S., Tibshirani, J. and Wager, S. (2016). Solving Heterogeneous Estimating Equations with Gradient Forests. arXiv: 1610:0127.

10. Imai, K. and Ratkovic, M. (2013). Estimating Treatment Effect Heterogeneity in Randomized Program Evaluation. The Annals of Applied Statistics 7(1): 443-470.

11. Imai, K. and Strauss, A. (2011). Estimation of Heterogeneous Treatment Effects from Randomized Experiments, with Applications to the Optimal Planning of the Get-Out-the-Vote Campaign. Political Analysis 19: 1-19.

12. Qian, M. and Murphy, S. (2011). Performance Guarantees for Individualized Treatment Rules. Ann Stat 39(2): 1180-1210.

13. Muller, J., Reshef, D., Du, G. and Jaakkola, T. (2016). Learning Optimal Interventions. arXiv: 1606.05027.

14. Radcliffe, N. and Surry, P. (2011). Real-World Uplift Modeling with Significance-Based Uplift Trees. Stochastic Solutions White Paper.
