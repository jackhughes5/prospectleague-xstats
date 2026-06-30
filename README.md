# Expected Stats Dashboard

An interactive Shiny dashboard that builds expected batting statistics from TrackMan batted ball data using a random forest model. Built for the [Prospect League](https://prospectleague.com/) with league-specific wOBA weights derived from a custom run expectancy matrix.

**Live App -> (https://jackhughes.shinyapps.io/xstats_app/)**

## What It Does

The model takes every batted ball's exit velocity, launch angle, and spray direction, and predicts the probability of each outcome (out, single, double, triple, home run). Aggregating those probabilities across a batter's plate appearances produces expected stats, metrics that measure **contact quality** rather than outcomes.

A batter who consistently barrels the ball at 100+ mph but hits line drives right at fielders will have a high xBA even if his actual BA is low. That gap between expected and actual is what separates skill from luck, and it's predictive of future performance.

### Dashboard Tabs

- **Leaderboard** — Sortable table of expected stats (xBA, xSLG, xOBP, xwOBA, xISO) with conditional formatting relative to league average. Includes actual BA and wOBA alongside expected values with a diff column showing who's getting lucky or unlucky. Filterable by team and minimum PA threshold. Click any batter to jump to their profile.
- **Batter Profile** — Select by team -> player. Batted ball scatter plot (EV × LA colored by outcome), expected vs actual event rates, exit velo distribution, and a rolling expected stats chart with configurable stat and window size.
- **EV × LA Map** — Heatmap of expected bases across the full exit velocity × launch angle space, with an interactive spray angle slider showing how outcomes shift from pull to oppo.
- **Luck Board** — Horizontal bar chart ranking batters by the gap between expected and actual bases per PA. Positive = unlucky (contact quality exceeds results), negative = lucky (results exceed contact quality).
- **Model Info** — Random forest summary, confusion matrix, variable importance, outcome probabilities by EV bucket, and plain-language explanations of every metric.

## Methodology

### Random Forest Model

- **Features:** Exit velocity, launch angle, spray direction (3 features)
- **Target:** Batted ball outcome (OUT, 1B, 2B, 3B, HR)
- **Architecture:** 500 trees, mtry = 2 (2 of 3 features sampled per split)
- **Train/test split:** 80/20 stratified by outcome class
- **Accuracy:** ~76.7% on held-out test data

Direction is used as raw field angle (not normalized by batter handedness). Testing showed normalization produced negligible accuracy gains while losing the model's ability to learn real field asymmetries (shortstop positioning, outfield wall distances).

### League-Specific wOBA Weights

Standard wOBA uses MLB linear weights, but run environments differ across leagues. I derived Prospect League-specific weights by:

1. Building a **run expectancy matrix** from 2022–2025 PrestoSports XML play-by-play data (~2,100 games)
2. For every PA in the play-by-play, computing `run_value = RE(state_after) + runs_scored - RE(state_before)`
3. Averaging run values by event type to get empirical linear weights

| Event | Prospect League | MLB 2026 | Ratio |
|-------|----------------|----------|-------|
| BB | 0.8246 | 0.70 | 118% |
| 1B | 0.9566 | 0.89 | 107% |
| 2B | 1.2763 | 1.26 | 101% |
| 3B | 1.5350 | 1.60 | 96% |
| HR | 1.8705 | 2.05 | 91% |

- MLB 2026 weights from Fangraphs Guts! page

Walks and singles are worth **more** in the Prospect League (baserunners are scarcer in a wood-bat college league, so each one matters more). Home runs are worth **less** (fewer runners on base when they occur, so each homer drives in fewer runs on average).

### Qualification

Batting title qualification follows the Prospect League standard: **2.7 PA per team game**. League averages and the leaderboard's conditional formatting are computed from qualified batters only.

## Tech Stack

- **R / Shiny** — App framework and data pipeline
- **randomForest** — Classification model
- **shinydashboard** — UI layout
- **plotly** — Interactive charts with hover tooltips
- **DT** — Sortable, filterable data tables with conditional formatting
- **Python** — wOBA weight derivation script (`woba_weights.py`)

## Running Locally

### Prerequisites

```r
install.packages(c("shiny", "shinydashboard", "tidyverse", "randomForest",
                    "caret", "DT", "plotly", "zoo", "shinyjs", "scales"))
```

### Setup

1. Clone this repo
2. Place your TrackMan CSV(s) in a `trackman_data/` folder in the project root (or set `TRACKMAN_DIR` at the top of `app.R`)
3. Run:

```r
shiny::runApp("app.R")
```

The model trains on startup (~5 seconds depending on data size), then the dashboard is live.

### Configuration

| Variable | Default | Description |
|----------|---------|-------------|
| `TRACKMAN_DIR` | `"trackman_data"` | Path to CSV folder |
| `MIN_PA` | `1` | Minimum PA for a batter to have expected stats computed |
| `RF_NTREE` | `500` | Number of trees in the random forest |

The leaderboard's PA threshold is controlled via an in-app slider (default: 30).

## Data

TrackMan data is proprietary and not included in this repo. The app expects CSV files with standard TrackMan columns including: `PitchCall`, `KorBB`, `PlayResult`, `ExitSpeed`, `Angle`, `Direction`, `Batter`, `BatterTeam`, `BatterSide`, `Date`, `GameID`.

## Known Simplifications

- **OBP denominator** uses PA rather than PA minus sacrifice bunts. Effect is ~0.01 for heavy bunters.
- **wOBA is unscaled** — weights are raw run values above out, not normalized to the OBP scale. Rankings are correct; absolute values aren't directly comparable to MLB wOBA.
- **BB and HBP share the same weight** (0.8246). Standard wOBA weights them slightly differently.
- **Triples** have near-zero model sensitivity due to rarity (~1% of batted balls).
- **No park factors** — the model pools all parks together.

## Related Projects

- **Run Expectancy Matrix** — Built from 2022–2025 Prospect League play-by-play data. Used to derive the wOBA weights and as a foundation for the stolen base decision model.
- **Lineup Simulator** — Monte Carlo lineup optimization tool (Python) that uses the RE matrix to simulate run production across different batting orders.
https://github.com/jackhughes5/prospect-league-run-expectancy