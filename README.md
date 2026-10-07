# NBA Finals MVP Prediction

An R-based basketball analytics project examining which player statistics best explain and predict the NBA Finals Most Valuable Player award.

The analysis compares an interpretable conditional-logit model with Random Forest, XGBoost, and neural-network models. It evaluates whether defensive rating, broader box-score information, and clutch-time statistics improve on a points-per-game baseline.

## Research questions

- Does defensive rating improve FMVP prediction beyond scoring alone?
- Do broader box-score statistics add predictive value?
- Do clutch-time statistics improve predictions?
- How do results change between leave-one-season-out validation and true forward-chain forecasting?

## Current analysis

- **Full sample:** 492 player-season observations from 43 NBA Finals series, 1984–2026
- **Clutch-era sample:** 350 player-season observations from 30 NBA Finals series, 1997–2026
- **Models:** conditional logistic regression, Random Forest, XGBoost, and neural network
- **Validation:** leave-one-season-out and forward-chain evaluation
- **Metrics:** Top-1 accuracy, Top-2 accuracy, log loss, Brier score, and paired season-level comparisons

## Selected findings

- Under forward-chain validation, the points-plus-defensive-rating model selected 30 of 33 winners (90.9%), compared with 29 of 33 for the scoring-only baseline.
- More complex models did not consistently outperform the simpler specifications on probability-based metrics.
- Adding clutch statistics did not produce a consistent improvement across models. For example, XGBoost Top-1 accuracy increased from 85% to 90%, while its log loss worsened from 0.711 to 0.754.
- The 2004, 2014, 2015, and 2024 Finals were among the hardest seasons for the models, illustrating the role of narrative and contextual factors that box-score data do not capture.

These are historical validation results, not betting recommendations or forecasts of future award outcomes.

## Repository contents

- `FRE497_FMVP_code.R` — data cleaning, feature construction, model fitting, validation, statistical tests, and reporting
- `NBA_Finals_Data_1984_2026.xlsx` — Finals player statistics and FMVP outcomes
- `NBA_Finals_Clutch_Stats_PerGame_1997_2026.xlsx` — Finals clutch-time player statistics

## Reproduce the analysis

Install R 4.1 or later and the required packages:

```r
install.packages(c(
  "readxl", "dplyr", "tidyr", "survival",
  "stringi", "ranger", "xgboost", "nnet"
))
```

Clone the repository, keep the two Excel files in the project root, and run:

```bash
Rscript FRE497_FMVP_code.R
```

The script uses fixed random seeds and relative file paths so it can be run from the repository directory.

## Relationship to the conference paper

An earlier version of this research, coauthored by Tung Wai Chen and Michael Johnson, analyzed 237 player-season observations from the 2006–2025 NBA Finals. That paper combined logistic regression with a cross-validated machine-learning pipeline and found that Random Forest identified 19 of 20 historical winners.

The code in this repository is a subsequent extension. It expands the historical period, adds clutch-time variables, uses season-level choice probabilities, and introduces forward-chain validation. Results in the current script therefore differ from those reported in the paper.

## Data attribution

The original conference-paper dataset was compiled from [Basketball-Reference](https://www.basketball-reference.com/). Sports Reference should be credited when its statistics are reused. The exact upstream source for the clutch-time workbook should be confirmed and added here before the repository is made public.

## Limitations

- FMVP is selected by media voting and includes narrative, matchup, and leadership factors not captured by box-score statistics.
- There is only one winner per season, creating a small and imbalanced prediction problem.
- Clutch metrics are unavailable before 1997.
- Historical validation performance does not guarantee future predictive performance.
