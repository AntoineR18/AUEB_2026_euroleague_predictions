# Predictive models for basketball games

## Objective
In the context of my internship at the Athens University of Economics and
Business (AUEB), I have been asked to predict the results of the Euroleague Final Four.

## Packages used
- **Data analysis workflow**: tidyverse
- **Plot composition**: patchwork
- **Table export**: xtable

## Project structure
```
AUEB_2026_euroleague_predictions/
├── raw_data/
│   ├── po/
│   ├── reg/
│   └── standings/
├── outputs/
│   ├── final_predictions/
│   ├── SF_predictions/
│   ├── SF_predictions_with25/
│   └── validation/
└── scripts/
```

## Data source
[Basketball Reference](https://www.basketball-reference.com/)

## Workflow
1. Data collection from Basketball Reference
2. Data cleaning and preprocessing
3. Model training
4. Prediction generation
5. Validation and output export

## Author
Antoine Rustenholz
