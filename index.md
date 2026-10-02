---
layout: default
title: Central Limit Theorem
description: Sample means converge in distribution to a Gaussian when the population variance is finite.
samwiki: true
---

<div class="sw-lede">
  <div class="sw-lede-copy">
    <h2>Sampling means and the Gaussian limit</h2>
    <p>The classical Lindeberg–Lévy theorem is the reason a sample mean can be treated as approximately normal even when the observations are not. Let X<sub>1</sub>, X<sub>2</sub>, … be independent and identically distributed with mean μ and finite variance σ². Write X̄<sub>n</sub> for the average of the first n of them. Then √n (X̄<sub>n</sub> − μ) / σ converges in distribution to a standard normal random variable. On the scale of the data, X̄<sub>n</sub> is approximately normal with mean μ and variance σ²/n. The population may be skewed, discrete, or bounded. The hypotheses that do the work are independence, a common distribution, and a finite second moment.</p>
    <p>The Shiny explorer is a Monte Carlo picture of that limit. It draws a population of 100,000 values from the family you choose, then repeatedly samples n of those values with replacement and records the sample mean. Sampling with replacement from a large population stands in for independent draws from the same law. The histogram of the recorded means is the sampling distribution of X̄<sub>n</sub>. A normal density, centered at the mean of those simulated means and scaled by their standard deviation, is drawn over the histogram. A normal quantile–quantile plot of the same means asks whether the cloud has actually become Gaussian: points along the reference line support the approximation, and a systematic bend means n is still too small or the variance hypothesis has failed.</p>
    <p>Two sliders set the experiment. Sample size n runs from 1 to 200 and starts at 30. The number of sample means runs from 100 to 5,000 and starts at 1,000. The families are normal, exponential, uniform, Poisson, binomial, gamma, chi-square, Student’s t, and Cauchy, each with its usual parameters. An exponential population with rate λ has mean 1/λ and variance 1/λ², so the standard error of the mean is 1/(λ √n). A uniform law on an interval [a, b] has variance (b − a)²/12. Gamma with shape α and rate β has mean α/β and variance α/β². Poisson(λ) has mean and variance λ. A binomial count with m trials and success probability p has mean mp and variance mp(1 − p). Chi-square with k degrees of freedom has mean k and variance 2k. Whenever σ² is finite, increasing n shrinks the sampling distribution like 1/√n. It also wears down skewness: the skewness of an exponential mean is 2/√n, which is why a badly skewed population can still produce a bell-shaped histogram of means.</p>
    <p>Finite variance is not optional. The Cauchy location-scale family, marked in the app as the case where the theorem does not apply, has neither a mean nor a variance. The average of independent Cauchy draws is Cauchy with the same scale, so the histogram of means stays heavy-tailed and does not tighten as n grows. Student’s t is the neighboring boundary. Its variance equals ν/(ν − 2) only when the degrees of freedom ν exceed 2; at ν = 1 the t law is Cauchy, and at ν = 2 the variance is already infinite. For ν &gt; 2 the classical theorem does apply, but the tails are heavy enough that a moderate sample still leaves the quantile plot curved. Exponential, Poisson, and chi-square are the useful contrast: each is skewed, each has finite variance, and raising n moves the histogram of means onto the normal curve.</p>
    <p>The figure has three panels. The wide top panel is the sampling distribution of the mean, with the normal curve overlaid. The lower left panel is the population that was sampled, so the original skew or discreteness stays visible. The lower right panel is the normal quantile plot of the means. Redraw Plots builds a new population and a new set of means, so two runs with the same settings agree in shape and still differ in the particular sample. The hosted explorer is <a href="https://sdcastillo.shinyapps.io/the_central_limit_theorem/">the Central Limit Theorem app</a>. The sources in this repository are <code>app.R</code>, <code>ui.R</code>, and <code>server.R</code>.</p>
  </div>
  <aside class="sw-find">
    <h2>In the explorer</h2>
    <ul>
      <li><strong>Population</strong> 100,000 simulated values from the family you select.</li>
      <li><strong>Sample size</strong> n from 1 to 200, starting at 30.</li>
      <li><strong>Replications</strong> 100 to 5,000 sample means, starting at 1,000.</li>
      <li><strong>Standard error</strong> The spread of the means estimates σ/√n.</li>
      <li><strong>Cauchy</strong> No finite variance, so the theorem does not apply.</li>
    </ul>
  </aside>
</div>

## What the theorem uses

- **Limit.** √n (X̄<sub>n</sub> − μ) / σ converges in distribution to N(0, 1) when the X<sub>i</sub> are i.i.d. with finite variance σ².
- **Scale of the mean.** X̄<sub>n</sub> is then approximately N(μ, σ²/n). The standard error is σ/√n, not σ.
- **Simulation.** A population of 100,000 draws, then many samples of size n taken with replacement. The histogram of those means is the object the theorem describes.
- **Normal overlay.** The curve uses the mean and standard deviation of the simulated means, which estimates the N(μ, σ²/n) approximation.
- **Cauchy counterexample.** With no mean and no variance, the sample mean stays Cauchy. A larger n does not produce a Gaussian.
- **Student’s t.** Variance is finite only for degrees of freedom greater than 2. Below that, the classical hypothesis fails; above it, convergence in the tails is slow.
- **Skewed laws with finite variance.** Exponential, gamma, Poisson, and chi-square are not normal, and their sample means still approach the overlaid Gaussian as n increases.

<p class="sw-actions">
  <a class="sw-btn sw-btn-live" href="https://sdcastillo.shinyapps.io/the_central_limit_theorem/">Open the Shiny app</a>
  <a class="sw-btn sw-btn-source" href="https://github.com/sdcastillo/Central_Limit_Theorem">Source</a>
</p>
