# Crypto News Sentiment Analysis

An interactive R Shiny app for exploring sentiment patterns in cryptocurrency news articles.

## Overview

This app visualizes and analyzes a dataset of crypto news articles using sentiment classification and subjectivity scores. Users can filter by date range and explore univariate or bivariate relationships across key variables.

## Features

- **Date range filter** — narrow analysis to any time window in the dataset
- **Univariate analysis** — bar charts (categorical) and histograms (subjectivity) with customizable color
- **Bivariate analysis** — grouped bar charts and boxplots comparing two variables
- **Descriptive statistics** — counts, proportions, means, and standard deviations for the selected view

## Variables

| Variable | Description |
|---|---|
| `source` | News outlet or publisher |
| `subject` | Topic category of the article |
| `sentiment_class` | Predicted sentiment (e.g. positive, negative, neutral) |
| `subjectivity` | Subjectivity score (0 = objective, 1 = subjective) |

## Dataset

[Crypto News+ by oliviervha](https://www.kaggle.com/datasets/oliviervha/crypto-news) — Kaggle, December 2023.

Place `cryptonews.csv` in the same directory as `crypto_shiny_app.R` before running.

## Requirements

```r
install.packages(c("shiny", "ggplot2", "dplyr", "stringr", "lubridate"))
```

## Running the App

```r
shiny::runApp("crypto_shiny_app.R")
```
