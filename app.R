# TrackMan Expected Stats Dashboard
# Shiny app — reads TrackMan CSVs, trains a random forest,
# and serves an interactive dashboard of expected batting stats.

library(shiny)
library(shinydashboard)
library(tidyverse)
library(randomForest)
library(caret)
library(DT)
library(plotly)
library(zoo)
library(shinyjs)

# CONFIG ----
TRACKMAN_DIR <- "."
MIN_PA       <- 1
RF_NTREE     <- 500

# TEAM NAME DICTIONARY ----
TEAM_NAMES <- c(
  "LAF_AVI" = "Lafayette Aviators",
  "CHA_CIT" = "Champion City Half Trax",
  "JOH_MIL" = "Johnstown Mill Rats",
  "KOK_CRE" = "Kokomo Creek Chubs",
  "CHI_PAI" = "Chillicothe Paints",
  "DUB_COU" = "Dubois County Bombers",
  "DAN_DAN1" = "Danville Dans",
  "DEC_BEA" = "Decatur Bean Ballers",
  "SPR_LUC" = "Springfield Lucky Horseshoes",
  "TER_HAU" = "Terre Haute Rex",
  "CLI_LUM1" = "Clinton LumberKings",
  "QUI_DOG" = "Quincy Doggy Paddlers",
  "NOR_COR" = "Normal CornBelters",
  "BUR_BEE1" = "Burlington Bees",
  "ILL_VAL3" = "Illinois Valley Pistol Shrimp",
  "CAP_CAT" = "Cape Catfish",
  "JAC_ROC" = "Jackson Rockabillys",
  "O'F_HOO"   = "O'Fallon Hoots",
  "THR_THR1" = "Thrillville Thrillbillies",
  "ALT_RIV"   = "Alton River Dragons"
)

team_display <- function(code) {
  ifelse(code %in% names(TEAM_NAMES), TEAM_NAMES[code], code)
}

# DATA PREP (runs once on app start) ----
csv_files <- list.files(TRACKMAN_DIR, pattern = "\\.csv$",
                        recursive = TRUE, full.names = TRUE)

raw <- csv_files %>%
  map_dfr(~ read_csv(.x, show_col_types = FALSE))

# Debug: print what we actually loaded
message("Rows loaded: ", nrow(raw))
message("Columns: ", paste(colnames(raw), collapse = ", "))

pa_enders <- raw %>%
  filter(
    PitchCall == "InPlay" |
      KorBB %in% c("Strikeout", "Walk") |
      PitchCall == "HitByPitch"
  )

bip <- pa_enders %>%
  filter(PitchCall == "InPlay",
         !is.na(ExitSpeed),
         !is.na(Angle),
         !is.na(Direction)) %>%
  mutate(
    outcome = case_when(
      PlayResult == "HomeRun"                                    ~ "HR",
      PlayResult == "Triple"                                     ~ "3B",
      PlayResult == "Double"                                     ~ "2B",
      PlayResult == "Single"                                     ~ "1B",
      PlayResult %in% c("Out", "Error", "FieldersChoice",
                        "Sacrifice")                            ~ "OUT",
      TRUE                                                       ~ NA_character_
    )
  ) %>%
  filter(!is.na(outcome))

bip$outcome <- factor(bip$outcome, levels = c("OUT", "1B", "2B", "3B", "HR"))

# TRAIN MODEL ----

set.seed(42)
train_idx <- createDataPartition(bip$outcome, p = 0.8, list = FALSE)
train_set <- bip[train_idx, ]
test_set  <- bip[-train_idx, ]

rf_model <- randomForest(
  outcome ~ ExitSpeed + Angle + Direction,
  data  = train_set,
  ntree = RF_NTREE,
  mtry  = 2,
  importance = TRUE
)

test_preds  <- predict(rf_model, newdata = test_set)
conf_matrix <- confusionMatrix(test_preds, test_set$outcome)

bip_probs <- predict(rf_model, newdata = bip, type = "prob") %>%
  as_tibble()

bip_with_probs <- bip %>%
  select(Batter, BatterTeam, ExitSpeed, Angle, Direction, outcome) %>%
  bind_cols(bip_probs)

# EXPECTED RATES PER BATTER ----

batter_pa <- pa_enders %>%
  mutate(
    pa_type = case_when(
      KorBB == "Strikeout"       ~ "K",
      KorBB == "Walk"            ~ "BB",
      PitchCall == "HitByPitch"  ~ "HBP",
      PitchCall == "InPlay"      ~ "BIP",
      TRUE                       ~ NA_character_
    )
  ) %>%
  filter(!is.na(pa_type)) %>%
  group_by(Batter, BatterTeam) %>%
  summarise(
    total_pa = n(),
    n_k      = sum(pa_type == "K"),
    n_bb     = sum(pa_type == "BB") + sum(pa_type == "HBP"),
    n_bip    = sum(pa_type == "BIP"),
    n_sac    = sum(PlayResult == "Sacrifice", na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(
    k_rate   = n_k  / total_pa,
    bb_rate  = n_bb / total_pa,
    bip_rate = n_bip / total_pa,
    sac_rate = n_sac / total_pa
  )

# Games played per team (for qualification threshold)
team_games <- raw %>%
  distinct(BatterTeam, GameID) %>%
  group_by(BatterTeam) %>%
  summarise(team_games = n(), .groups = "drop")

# Games played per batter
batter_games <- raw %>%
  distinct(Batter, BatterTeam, GameID) %>%
  group_by(Batter, BatterTeam) %>%
  summarise(batter_games = n(), .groups = "drop")

batter_xbip <- bip_with_probs %>%
  group_by(Batter, BatterTeam) %>%
  summarise(
    n_bip_modeled = n(),
    x1B      = mean(`1B`),
    x2B      = mean(`2B`),
    x3B      = mean(`3B`),
    xHR      = mean(HR),
    xOUT_bip = mean(OUT),
    .groups  = "drop"
  )

expected <- batter_pa %>%
  left_join(batter_xbip, by = c("Batter", "BatterTeam")) %>%
  filter(total_pa >= MIN_PA) %>%
  mutate(
    across(c(x1B, x2B, x3B, xHR, xOUT_bip), ~ replace_na(.x, 0)),
    xOUT      = k_rate + bip_rate * xOUT_bip,
    xBB       = bb_rate,
    x1B_final = bip_rate * x1B,
    x2B_final = bip_rate * x2B,
    x3B_final = bip_rate * x3B,
    xHR_final = bip_rate * xHR
  ) %>%
  mutate(
    total = xOUT + xBB + x1B_final + x2B_final + x3B_final + xHR_final,
    xOUT      = xOUT / total,
    xBB       = xBB  / total,
    x1B_final = x1B_final / total,
    x2B_final = x2B_final / total,
    x3B_final = x3B_final / total,
    xHR_final = xHR_final / total
  ) %>%
  mutate(
    # Derived expected stats ----
    xAB  = total_pa - (xBB * total_pa) - n_sac,
    xH   = (x1B_final + x2B_final + x3B_final + xHR_final) * total_pa,
    xTB  = (x1B_final + 2*x2B_final + 3*x3B_final + 4*xHR_final) * total_pa,
    xBA  = ifelse(xAB > 0, xH / xAB, 0),
    xSLG = ifelse(xAB > 0, xTB / xAB, 0),
    xOBP = xBB + x1B_final + x2B_final + x3B_final + xHR_final,
    xOPS = xOBP + xSLG,
    xISO = xSLG - xBA,
    # xwOBA using Prospect League linear weights (empirical from 2022-2025 play-by-play + RE matrix)
    xwOBA = 0.8246*xBB + 0.9566*x1B_final + 1.2763*x2B_final +
      1.5350*x3B_final + 1.8705*xHR_final,
    xBases_per_PA = x1B_final + 2*x2B_final + 3*x3B_final + 4*xHR_final,
    Team = team_display(BatterTeam)
  ) %>%
  left_join(team_games, by = "BatterTeam") %>%
  left_join(batter_games, by = c("Batter", "BatterTeam")) %>%
  mutate(
    pa_threshold = 2.7 * team_games,
    qualified    = total_pa >= pa_threshold
  )

# Actual rates for comparison
actual <- batter_pa %>%
  left_join(
    pa_enders %>%
      filter(PitchCall == "InPlay") %>%
      mutate(
        hit_type = case_when(
          PlayResult == "Single"  ~ "1B",
          PlayResult == "Double"  ~ "2B",
          PlayResult == "Triple"  ~ "3B",
          PlayResult == "HomeRun" ~ "HR",
          TRUE                    ~ "OUT"
        )
      ) %>%
      group_by(Batter, BatterTeam) %>%
      summarise(
        a1B = sum(hit_type == "1B") / n(),
        a2B = sum(hit_type == "2B") / n(),
        a3B = sum(hit_type == "3B") / n(),
        aHR = sum(hit_type == "HR") / n(),
        .groups = "drop"
      ),
    by = c("Batter", "BatterTeam")
  ) %>%
  filter(total_pa >= MIN_PA) %>%
  mutate(across(c(a1B, a2B, a3B, aHR), ~ replace_na(.x, 0)))

# Combine expected + actual for comparison view
comparison <- expected %>%
  select(Batter, BatterTeam, Team, total_pa, bip_rate, sac_rate,
         x1B_final, x2B_final, x3B_final, xHR_final, xBB, xOUT,
         xBA, xSLG, xOBP, xwOBA, xBases_per_PA) %>%
  left_join(
    actual %>% select(Batter, BatterTeam, k_rate, bb_rate,
                      a1B, a2B, a3B, aHR),
    by = c("Batter", "BatterTeam")
  ) %>%
  mutate(
    actual_1B  = bip_rate * a1B,
    actual_2B  = bip_rate * a2B,
    actual_3B  = bip_rate * a3B,
    actual_HR  = bip_rate * aHR,
    actual_BB  = bb_rate,
    actual_OUT = k_rate + bip_rate * (1 - a1B - a2B - a3B - aHR),
    actual_Bases_per_PA = actual_1B + 2*actual_2B + 3*actual_3B + 4*actual_HR,
    luck_gap   = xBases_per_PA - actual_Bases_per_PA,
    actual_hit_rate = actual_1B + actual_2B + actual_3B + actual_HR,
    actual_ab_rate  = 1 - actual_BB - sac_rate,
    BA     = ifelse(actual_ab_rate > 0, actual_hit_rate / actual_ab_rate, 0),
    wOBA   = 0.8246*actual_BB + 0.9566*actual_1B + 1.2763*actual_2B +
      1.5350*actual_3B + 1.8705*actual_HR,
    BA_diff   = xBA - BA,
    wOBA_diff = xwOBA - wOBA
  )

# PA TIMELINE (for rolling charts) ----
# Build a per-PA dataframe with date and expected stat contributions
pa_timeline <- pa_enders %>%
  filter(!is.na(Date)) %>%
  mutate(
    pa_date = as.Date(Date, format = "%m/%d/%y"),
    pa_type = case_when(
      KorBB == "Strikeout"       ~ "K",
      KorBB == "Walk"            ~ "BB",
      PitchCall == "HitByPitch"  ~ "HBP",
      PitchCall == "InPlay"      ~ "BIP",
      TRUE                       ~ NA_character_
    )
  ) %>%
  filter(!is.na(pa_type)) %>%
  select(Batter, BatterTeam, pa_date, pa_type, ExitSpeed, Angle, Direction) %>%
  arrange(Batter, pa_date)

# Get RF probabilities for BIP rows
bip_rows <- pa_timeline %>%
  filter(pa_type == "BIP", !is.na(ExitSpeed), !is.na(Angle), !is.na(Direction))

if (nrow(bip_rows) > 0) {
  bip_rf_probs <- predict(rf_model, newdata = bip_rows, type = "prob") %>%
    unclass() %>%
    as.data.frame()
  bip_rows <- bip_rows %>%
    bind_cols(bip_rf_probs) %>%
    mutate(
      pa_xBA_num   = `1B` + `2B` + `3B` + HR,     # hit prob (numerator for BA)
      pa_is_AB     = 1,                              # BIP counts as AB
      pa_xOBP      = `1B` + `2B` + `3B` + HR,       # on-base prob
      pa_xSLG_num  = `1B` + 2*`2B` + 3*`3B` + 4*HR, # total bases (numerator for SLG)
      pa_xwOBA     = 0.9566*`1B` + 1.2763*`2B` + 1.5350*`3B` + 1.8705*HR,
      pa_xISO_num  = `2B` + 2*`3B` + 3*HR            # extra bases (numerator for ISO)
    )
} else {
  bip_rows <- bip_rows %>%
    mutate(pa_xBA_num = 0, pa_is_AB = 1, pa_xOBP = 0,
           pa_xSLG_num = 0, pa_xwOBA = 0, pa_xISO_num = 0)
}

# Non-BIP PAs
non_bip <- pa_timeline %>%
  filter(pa_type != "BIP" | is.na(ExitSpeed) | is.na(Angle)) %>%
  mutate(
    pa_xBA_num   = 0,
    pa_is_AB     = ifelse(pa_type == "K", 1, 0),     # K = AB, BB/HBP = not AB
    pa_xOBP      = ifelse(pa_type %in% c("BB", "HBP"), 1, 0),
    pa_xSLG_num  = 0,
    pa_xwOBA     = ifelse(pa_type %in% c("BB", "HBP"), 0.8246, 0),
    pa_xISO_num  = 0
  )

# Combine and sort chronologically per batter
pa_timeline_full <- bind_rows(
  bip_rows %>% select(Batter, BatterTeam, pa_date, pa_type,
                      pa_xBA_num, pa_is_AB, pa_xOBP, pa_xSLG_num,
                      pa_xwOBA, pa_xISO_num),
  non_bip %>% select(Batter, BatterTeam, pa_date, pa_type,
                     pa_xBA_num, pa_is_AB, pa_xOBP, pa_xSLG_num,
                     pa_xwOBA, pa_xISO_num)
) %>%
  arrange(Batter, pa_date) %>%
  group_by(Batter) %>%
  mutate(pa_num = row_number()) %>%
  ungroup()

# LEAGUE AVERAGES ----
qualified_batters <- expected %>% filter(qualified == TRUE)
if (nrow(qualified_batters) < 2) qualified_batters <- expected  # fallback for small datasets
league_avg <- list(
  xBA   = round(mean(qualified_batters$xBA), 3),
  xSLG  = round(mean(qualified_batters$xSLG), 3),
  xOBP  = round(mean(qualified_batters$xOBP), 3),
  xwOBA = round(mean(qualified_batters$xwOBA), 3),
  xISO  = round(mean(qualified_batters$xISO), 3),
  avg_ev = round(mean(bip_with_probs$ExitSpeed), 1),
  avg_la = round(mean(bip_with_probs$Angle), 1)
)

all_batters <- bip_with_probs %>%
  distinct(Batter, BatterTeam) %>%
  mutate(Team = team_display(BatterTeam)) %>%
  arrange(Team, Batter)

all_teams <- all_batters %>%
  distinct(BatterTeam, Team) %>%
  arrange(Team)

# STAT COLUMN DEFINITIONS ----
# Maps display names to column info for the leaderboard filter
STAT_CHOICES <- c(
  "xBA"        = "xBA",
  "BA"         = "BA",
  "BA Diff"    = "BA_diff",
  "xSLG"       = "xSLG",
  "xOBP"       = "xOBP",
  "xOPS"       = "xOPS",
  "xwOBA"      = "xwOBA",
  "wOBA"       = "wOBA",
  "wOBA Diff"  = "wOBA_diff",
  "xISO"       = "xISO",
  "xBases/PA"  = "xBases_per_PA",
  "xOUT"       = "xOUT",
  "xBB"        = "xBB",
  "x1B"        = "x1B_final",
  "x2B"        = "x2B_final",
  "x3B"        = "x3B_final",
  "xHR"        = "xHR_final",
  "K%"         = "k_rate",
  "BB%"        = "bb_rate"
)

# UI

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(title = "xStats Dashboard", titleWidth = 250),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem("Leaderboard",    tabName = "leaderboard", icon = icon("ranking-star")),
      menuItem("Batter Profile", tabName = "profile",     icon = icon("user")),
      menuItem("EV \u00D7 LA Map",    tabName = "heatmap",     icon = icon("fire")),
      menuItem("Luck Board",     tabName = "luck",        icon = icon("clover")),
      menuItem("Model Info",     tabName = "model",       icon = icon("gears"))
    ),
    hr(),
    div(style = "padding: 10px; color: #aaa; font-size: 12px;",
        sprintf("%s pitches | %s batted balls",
                format(nrow(raw), big.mark = ","),
                format(nrow(bip), big.mark = ",")),
        br(),
        sprintf("%d qualified batters", sum(expected$qualified)),
        br(), br(),
        div(style = "color: #777; font-size: 11px;",
            "Qualification: 2.7 PA per team game",
            br(),
            "League averages & color scale",
            br(),
            "based on qualified batters only")
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .skin-black .main-header .logo {
        font-weight: bold;
        font-size: 18px;
      }
      .skin-black .main-header .navbar { background-color: #222; }
      .content-wrapper { background-color: #f7f7f7; }
      .small-box { border-radius: 4px; }
      .league-avg-bar {
        background: #2c3e50; color: #ecf0f1; padding: 10px 20px;
        border-radius: 4px; margin-bottom: 15px;
        display: flex; justify-content: space-around; flex-wrap: wrap;
      }
      .league-avg-bar .avg-item {
        text-align: center; padding: 4px 12px;
      }
      .league-avg-bar .avg-label {
        font-size: 11px; color: #95a5a6; text-transform: uppercase;
      }
      .league-avg-bar .avg-value {
        font-size: 18px; font-weight: bold;
      }
      .explanation-box {
        background: #f9f9f9; border-left: 4px solid #3c8dbc;
        padding: 12px 16px; margin-bottom: 15px;
        font-size: 13px; line-height: 1.6; color: #555;
      }
      .explanation-box h4 { margin-top: 0; color: #333; }
      .dataTable tbody tr { cursor: pointer; }
      .dataTable tbody tr:hover { background-color: #eef5fb !important; }
    "))),
    
    shinyjs::useShinyjs(),
    tabItems(
      
      # LEADERBOARD ----
      tabItem(tabName = "leaderboard",
              # League averages bar
              uiOutput("league_avg_bar"),
              fluidRow(
                valueBoxOutput("vb_batters",  width = 3),
                valueBoxOutput("vb_pa",       width = 3),
                valueBoxOutput("vb_bip",      width = 3),
                valueBoxOutput("vb_accuracy", width = 3)
              ),
              fluidRow(
                box(title = "Expected Stats Leaderboard", width = 12,
                    status = "primary", solidHeader = TRUE,
                    fluidRow(
                      column(5,
                             checkboxGroupInput(
                               "stat_filter", "Stats to display:",
                               choices  = names(STAT_CHOICES),
                               selected = c("xBA", "BA", "BA Diff", "xwOBA", "wOBA", "wOBA Diff"),
                               inline   = TRUE
                             )
                      ),
                      column(2,
                             selectInput("sort_stat", "Sort by:",
                                         choices  = names(STAT_CHOICES),
                                         selected = "xwOBA")
                      ),
                      column(3,
                             selectInput("leaderboard_team", "Filter by Team:",
                                         choices  = c("All Teams" = "all",
                                                      setNames(all_teams$BatterTeam, all_teams$Team)),
                                         selected = "all")
                      ),
                      column(2,
                             numericInput("min_pa", "Minimum PA:",
                                          value = 30, min = 1, max = 200, step = 5)
                      )
                    ),
                    DTOutput("leaderboard_table"))
              )
      ),
      
      # BATTER PROFILE ----
      tabItem(tabName = "profile",
              fluidRow(
                box(width = 4,
                    selectInput("team_select", "Select Team",
                                choices  = setNames(all_teams$BatterTeam, all_teams$Team),
                                selected = all_teams$BatterTeam[1]),
                    uiOutput("batter_select_ui"),
                    hr(),
                    tableOutput("batter_summary")
                ),
                box(title = "Batted Ball Outcomes", width = 8,
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("batter_scatter", height = "450px"))
              ),
              fluidRow(
                box(title = "Expected vs Actual Event Rates", width = 6,
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("batter_comparison", height = "350px")),
                box(title = "Exit Velo & Launch Angle Distribution", width = 6,
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("batter_ev_dist", height = "350px"))
              ),
              fluidRow(
                box(title = "Rolling Expected Stats", width = 12,
                    status = "primary", solidHeader = TRUE,
                    fluidRow(
                      column(4,
                             selectInput("rolling_stat", "Stat to display:",
                                         choices = c("xBA", "xwOBA", "xSLG", "xOBP", "xISO"),
                                         selected = "xwOBA")
                      ),
                      column(4,
                             sliderInput("rolling_window", "Rolling window (PAs):",
                                         min = 5, max = 50, value = 15, step = 5)
                      )
                    ),
                    plotlyOutput("rolling_chart", height = "350px"),
                    p(style = "color: #888; margin-top: 6px;",
                      "Rolling average over the selected PA window. Dashed line = league average.
                       Thin line = per-PA value. Bold line = rolling average."))
              )
      ),
      
      # EV x LA HEATMAP ----
      tabItem(tabName = "heatmap",
              fluidRow(
                box(title = "Expected Bases by Exit Velo \u00D7 Launch Angle", width = 12,
                    status = "primary", solidHeader = TRUE,
                    fluidRow(
                      column(6,
                             sliderInput("heatmap_direction", "Direction (Spray Angle):",
                                         min = -45, max = 45, value = 0, step = 5,
                                         post = "\u00B0")
                      ),
                      column(6,
                             p(style = "margin-top: 25px; color: #888;",
                               "Negative = toward 3B (pull for RHH) | 0 = center | Positive = toward 1B (pull for LHH)")
                      )
                    ),
                    plotlyOutput("heatmap_plot", height = "550px"),
                    p(style = "color: #888; margin-top: 8px;",
                      "Predicted expected bases (1B=1, 2B=2, 3B=3, HR=4) from the random forest
                       at the selected spray angle. Brighter = higher expected bases."))
              )
      ),
      
      # LUCK BOARD ----
      tabItem(tabName = "luck",
              fluidRow(
                box(title = "Who's Getting Lucky (and Unlucky)?", width = 12,
                    status = "primary", solidHeader = TRUE,
                    div(class = "explanation-box",
                        h4("How to read this"),
                        "Luck gap = expected bases per PA (from the model) minus actual bases per PA.
                   A positive gap means the hitter is making good contact but not getting results
                  (line drives right at fielders). A negative gap means they're
                   outperforming their contact quality (bloopers falling in).
                   Neither is sustainable long-term, so big gaps in either direction suggest
                   regression is coming."
                    ),
                    plotlyOutput("luck_plot", height = "auto"))
              )
      ),
      
      # MODEL INFO ----
      tabItem(tabName = "model",
              fluidRow(
                box(title = "How the Model Works", width = 12,
                    status = "info", solidHeader = TRUE,
                    div(class = "explanation-box",
                        h4("Random Forest Classifier"),
                        "The model takes three inputs for every batted ball \u2014 exit velocity (how hard
                 it was hit), launch angle (the vertical angle off the bat), and spray direction
                 (where on the field it was hit) \u2014 and predicts the probability of each outcome:
                 out, single, double, triple, or home run. It does this by training 500 decision
                 trees, each one learning slightly different rules from the data, and averaging
                 their votes. The result is a probability distribution for each batted ball, not
                 just a single prediction.",
                        br(), br(),
                        "A batter's expected stats are computed by averaging these probabilities across
                 all their batted balls. If a hitter consistently barrels the ball at 100+ mph
                 and 20-30\u00B0 launch angle, the model assigns high HR/XBH probabilities to those
                 contacts — even if some of them happened to be caught. That's what separates
                 expected stats from actual stats: they measure contact quality, not outcomes."
                    )
                )
              ),
              fluidRow(
                box(title = "Random Forest Summary", width = 6,
                    status = "primary", solidHeader = TRUE,
                    div(class = "explanation-box",
                        "OOB (out-of-bag) estimate: each tree is trained on a bootstrap sample of the
                 data, so ~37% of observations are left out of each tree. The OOB error rate
                 is how often those left-out observations are misclassified. Lower is better."
                    ),
                    verbatimTextOutput("rf_summary")),
                box(title = "Variable Importance", width = 6,
                    status = "primary", solidHeader = TRUE,
                    div(class = "explanation-box",
                        "Mean Decrease Gini = how much each feature helps separate outcomes.
                 Higher is more important."
                    ),
                    plotlyOutput("var_importance", height = "300px"))
              ),
              fluidRow(
                box(title = "Confusion Matrix (Test Set)", width = 6,
                    status = "primary", solidHeader = TRUE,
                    div(class = "explanation-box",
                        "Model performance on 20% held-out test set. Diagonal = correct predictions.
                 Sensitivity = how well it catches each outcome type."
                    ),
                    verbatimTextOutput("conf_matrix_text")),
                box(title = "Outcome Probabilities by EV Bucket", width = 6,
                    status = "primary", solidHeader = TRUE,
                    div(class = "explanation-box",
                        "This breaks batted balls into exit velocity buckets and shows the average
                 predicted probability of each outcome. You should see hit probabilities
                 (especially XBH and HR) climbing sharply above 90 mph."
                    ),
                    plotlyOutput("ev_bucket_plot", height = "350px"))
              )
      )
    )
  )
)

# SERVER

server <- function(input, output, session) {
  
  # LEAGUE AVERAGES BAR ----
  output$league_avg_bar <- renderUI({
    div(class = "league-avg-bar",
        div(class = "avg-item",
            div(class = "avg-label", "Lg xBA"),
            div(class = "avg-value", sprintf("%.3f", league_avg$xBA))),
        div(class = "avg-item",
            div(class = "avg-label", "Lg xSLG"),
            div(class = "avg-value", sprintf("%.3f", league_avg$xSLG))),
        div(class = "avg-item",
            div(class = "avg-label", "Lg xOBP"),
            div(class = "avg-value", sprintf("%.3f", league_avg$xOBP))),
        div(class = "avg-item",
            div(class = "avg-label", "Lg xwOBA"),
            div(class = "avg-value", sprintf("%.3f", league_avg$xwOBA))),
        div(class = "avg-item",
            div(class = "avg-label", "Lg xISO"),
            div(class = "avg-value", sprintf("%.3f", league_avg$xISO))),
        div(class = "avg-item",
            div(class = "avg-label", "Avg EV"),
            div(class = "avg-value", sprintf("%.1f", league_avg$avg_ev))),
        div(class = "avg-item",
            div(class = "avg-label", "Avg LA"),
            div(class = "avg-value", sprintf("%.1f\u00B0", league_avg$avg_la)))
    )
  })
  
  # VALUE BOXES ----
  output$vb_batters <- renderValueBox({
    valueBox(sum(expected$qualified), "Qualified (2.7 PA/G)",
             icon = icon("users"), color = "blue")
  })
  output$vb_pa <- renderValueBox({
    valueBox(format(sum(expected$total_pa), big.mark = ","), "Total PA",
             icon = icon("baseball"), color = "green")
  })
  output$vb_bip <- renderValueBox({
    valueBox(format(nrow(bip), big.mark = ","), "Batted Balls Modeled",
             icon = icon("bullseye"), color = "yellow")
  })
  output$vb_accuracy <- renderValueBox({
    acc <- round(conf_matrix$overall["Accuracy"] * 100, 1)
    valueBox(paste0(acc, "%"), "Model Accuracy",
             icon = icon("check"), color = "red")
  })
  
  # LEADERBOARD TABLE ----
  
  REVERSE_STATS <- c("xOUT", "K%")
  
  output$leaderboard_table <- renderDT({
    full_tbl <- expected %>%
      transmute(
        Batter,
        BatterTeam,
        Team,
        PA         = total_pa,
        xBA        = round(xBA, 3),
        xSLG       = round(xSLG, 3),
        xOBP       = round(xOBP, 3),
        xOPS       = round(xOPS, 3),
        xwOBA      = round(xwOBA, 3),
        xISO       = round(xISO, 3),
        `xBases/PA` = round(xBases_per_PA, 3),
        xOUT       = round(xOUT, 3),
        xBB        = round(xBB, 3),
        x1B        = round(x1B_final, 3),
        x2B        = round(x2B_final, 3),
        x3B        = round(x3B_final, 3),
        xHR        = round(xHR_final, 3),
        `K%`       = round(k_rate, 3),
        `BB%`      = round(bb_rate, 3),
        Qualified  = qualified
      ) %>%
      left_join(
        comparison %>% select(Batter, BatterTeam, BA, wOBA, BA_diff, wOBA_diff),
        by = c("Batter", "BatterTeam")
      ) %>%
      mutate(
        BA        = round(BA, 3),
        wOBA      = round(wOBA, 3),
        `BA Diff`  = round(BA_diff, 3),
        `wOBA Diff` = round(wOBA_diff, 3)
      ) %>%
      select(-BA_diff, -wOBA_diff, -BatterTeam)
    
    league_tbl <- full_tbl %>% filter(Qualified == TRUE)
    if (nrow(league_tbl) < 2) league_tbl <- full_tbl  # fallback if too few qualified
    
    min_pa <- input$min_pa
    if (is.null(min_pa)) min_pa <- 30
    full_tbl <- full_tbl %>% filter(PA >= min_pa)
    
    # Team filter
    team_sel <- input$leaderboard_team
    if (!is.null(team_sel) && team_sel != "all") {
      full_tbl <- full_tbl %>% filter(Team == team_display(team_sel))
    }
    
    selected_stats <- input$stat_filter
    if (is.null(selected_stats)) selected_stats <- "xwOBA"
    cols_to_show <- c("Batter", "Team", "PA", selected_stats)
    tbl <- full_tbl %>% select(all_of(cols_to_show))
    
    sort_col <- input$sort_stat
    if (sort_col %in% colnames(tbl)) {
      sort_idx <- which(colnames(tbl) == sort_col) - 1  # 0-indexed for DT
      tbl <- tbl %>% arrange(desc(.data[[sort_col]]))
    } else {
      sort_idx <- 2  # default to PA
    }
    
    dt <- datatable(tbl,
                    rownames = FALSE,
                    selection = "none",
                    callback = JS("
                      table.on('click', 'tbody tr', function() {
                        var data = table.row(this).data();
                        if (data) {
                          Shiny.setInputValue('leaderboard_click', {
                            batter: data[0],
                            team: data[1]
                          }, {priority: 'event'});
                        }
                      });
                    "),
                    options = list(pageLength = 25, dom = "ftp",
                                   order = list(list(sort_idx, "desc")))) %>%
      formatStyle("Batter", cursor = "pointer", color = "#3c8dbc",
                  fontWeight = "bold")
    
    stat_cols <- setdiff(colnames(tbl), c("Batter", "Team", "PA"))
    
    for (col in stat_cols) {
      lg_vals <- league_tbl[[col]]
      if (is.null(lg_vals) || length(lg_vals) < 2 || all(is.na(lg_vals))) next
      
      col_min <- min(lg_vals, na.rm = TRUE)
      col_max <- max(lg_vals, na.rm = TRUE)
      col_avg <- mean(lg_vals, na.rm = TRUE)
      if (col_min == col_max) next
      
      breaks <- c(
        col_min + (col_avg - col_min) * 0.33,
        col_min + (col_avg - col_min) * 0.66,
        col_avg + (col_max - col_avg) * 0.33,
        col_avg + (col_max - col_avg) * 0.66
      )
      
      if (col %in% REVERSE_STATS) {
        colors <- c("#2d8e47", "#a3d9a5", "#ffffff", "#e8a4a4", "#c23b22")
      } else {
        colors <- c("#c23b22", "#e8a4a4", "#ffffff", "#a3d9a5", "#2d8e47")
      }
      
      dt <- dt %>% formatStyle(col, backgroundColor = styleInterval(breaks, colors))
    }
    
    dt
  })
  
  # LEADERBOARD ROW CLICK → NAVIGATE TO PROFILE ----
  observeEvent(input$leaderboard_click, {
    clicked <- input$leaderboard_click
    if (is.null(clicked)) return()
    
    batter_name <- clicked$batter
    team_name   <- clicked$team
    
    # Look up the BatterTeam code from the display name
    team_code <- all_teams$BatterTeam[all_teams$Team == team_name]
    if (length(team_code) == 0) return()
    team_code <- team_code[1]
    
    # Update team dropdown, then batter dropdown, then switch tab
    updateSelectInput(session, "team_select", selected = team_code)
    
    shinyjs::delay(200, {
      updateSelectInput(session, "batter_select", selected = batter_name)
    })
    
    updateTabItems(session, "tabs", selected = "profile")
  })
  
  # BATTER PROFILE ----
  
  output$batter_select_ui <- renderUI({
    req(input$team_select)
    team_batters <- all_batters %>%
      filter(BatterTeam == input$team_select) %>%
      arrange(Batter)
    
    selectInput("batter_select", "Select Batter",
                choices  = setNames(team_batters$Batter, team_batters$Batter),
                selected = team_batters$Batter[1])
  })
  
  batter_data <- reactive({
    req(input$batter_select)
    bip_with_probs %>% filter(Batter == input$batter_select)
  })
  
  output$batter_summary <- renderTable({
    req(input$batter_select)
    bd <- batter_data()
    if (nrow(bd) == 0) return(data.frame(Stat = "No batted balls", Value = ""))
    
    batter_exp <- expected %>% filter(Batter == input$batter_select)
    
    base_stats <- tibble(
      Stat = c("Batted Balls", "Avg Exit Velo", "Avg Launch Angle",
               "Max Exit Velo", "Hard Hit % (\u226595)"),
      Value = c(
        as.character(nrow(bd)),
        sprintf("%.1f mph", mean(bd$ExitSpeed)),
        sprintf("%.1f\u00B0", mean(bd$Angle)),
        sprintf("%.1f mph", max(bd$ExitSpeed)),
        sprintf("%.1f%%", 100 * mean(bd$ExitSpeed >= 95))
      )
    )
    
    if (nrow(batter_exp) > 0) {
      exp_stats <- tibble(
        Stat = c("xBA", "xSLG", "xwOBA"),
        Value = c(
          sprintf("%.3f", batter_exp$xBA),
          sprintf("%.3f", batter_exp$xSLG),
          sprintf("%.3f", batter_exp$xwOBA)
        )
      )
      base_stats <- bind_rows(base_stats, exp_stats)
    }
    
    base_stats
  }, striped = TRUE, width = "100%")
  
  output$batter_scatter <- renderPlotly({
    req(input$batter_select)
    bd <- batter_data()
    if (nrow(bd) == 0) return(plotly_empty())
    
    colors <- c("OUT" = "#999999", "1B" = "#2196F3",
                "2B" = "#4CAF50", "3B" = "#FF9800", "HR" = "#F44336")
    
    p <- ggplot(bd, aes(x = ExitSpeed, y = Angle, color = outcome,
                        text = paste0("EV: ", round(ExitSpeed, 1),
                                      "\nLA: ", round(Angle, 1),
                                      "\nResult: ", outcome))) +
      geom_point(size = 3, alpha = 0.8) +
      scale_color_manual(values = colors) +
      labs(x = "Exit Velocity (mph)", y = "Launch Angle (\u00B0)", color = "Outcome") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$batter_comparison <- renderPlotly({
    req(input$batter_select)
    sel <- input$batter_select
    comp_row <- comparison %>% filter(Batter == sel)
    if (nrow(comp_row) == 0) return(plotly_empty())
    
    events <- c("1B", "2B", "3B", "HR")
    comp_df <- tibble(
      Event    = rep(events, 2),
      Type     = rep(c("Expected", "Actual"), each = 4),
      Rate     = c(comp_row$x1B_final, comp_row$x2B_final,
                   comp_row$x3B_final, comp_row$xHR_final,
                   comp_row$actual_1B,  comp_row$actual_2B,
                   comp_row$actual_3B,  comp_row$actual_HR)
    )
    comp_df$Event <- factor(comp_df$Event, levels = events)
    
    p <- ggplot(comp_df, aes(x = Event, y = Rate, fill = Type,
                             text = paste0(Type, " ", Event, ": ",
                                           sprintf("%.1f%%", Rate * 100)))) +
      geom_col(position = "dodge", width = 0.6) +
      scale_fill_manual(values = c("Expected" = "#3c8dbc", "Actual" = "#f39c12")) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(x = NULL, y = "Rate per PA", fill = NULL) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  })
  
  output$batter_ev_dist <- renderPlotly({
    req(input$batter_select)
    bd <- batter_data()
    if (nrow(bd) == 0) return(plotly_empty())
    
    p1 <- ggplot(bd, aes(x = ExitSpeed)) +
      geom_histogram(bins = 20, fill = "#3c8dbc", alpha = 0.8) +
      geom_vline(xintercept = mean(bd$ExitSpeed), linetype = "dashed", color = "#e74c3c") +
      labs(x = "Exit Velocity (mph)", y = "Count") +
      theme_minimal(base_size = 12)
    
    ggplotly(p1)
  })
  
  # ROLLING CHART ----
  
  output$rolling_chart <- renderPlotly({
    req(input$batter_select, input$rolling_stat, input$rolling_window)
    
    batter_pa_data <- pa_timeline_full %>%
      filter(Batter == input$batter_select)
    
    if (nrow(batter_pa_data) < 3) return(plotly_empty())
    
    stat <- input$rolling_stat
    window <- input$rolling_window
    
    chart_data <- batter_pa_data %>%
      mutate(
        stat_value = case_when(
          stat == "xBA"   & pa_is_AB == 1 ~ pa_xBA_num,
          stat == "xBA"   & pa_is_AB == 0 ~ NA_real_,
          stat == "xwOBA" ~ pa_xwOBA,
          stat == "xSLG"  & pa_is_AB == 1 ~ pa_xSLG_num,
          stat == "xSLG"  & pa_is_AB == 0 ~ NA_real_,
          stat == "xOBP"  ~ pa_xOBP,
          stat == "xISO"  & pa_is_AB == 1 ~ pa_xISO_num,
          stat == "xISO"  & pa_is_AB == 0 ~ NA_real_,
          TRUE ~ NA_real_
        )
      )
    
    if (stat %in% c("xBA", "xSLG", "xISO")) {
      chart_data <- chart_data %>% filter(!is.na(stat_value))
    }
    
    if (nrow(chart_data) < 3) return(plotly_empty())
    
    chart_data <- chart_data %>%
      mutate(
        pa_seq = row_number(),
        rolling_avg = zoo::rollmean(stat_value, k = min(window, n()), 
                                    fill = NA, align = "right")
      )
    
    lg_avg <- switch(stat,
                     "xBA"   = league_avg$xBA,
                     "xSLG"  = league_avg$xSLG,
                     "xOBP"  = league_avg$xOBP,
                     "xwOBA" = league_avg$xwOBA,
                     "xISO"  = league_avg$xISO,
                     0
    )
    
    date_breaks <- chart_data %>%
      group_by(pa_date) %>%
      summarise(pa_seq = min(pa_seq), .groups = "drop")
    
    line_data <- chart_data %>% filter(!is.na(rolling_avg))
    
    p <- ggplot(chart_data, aes(x = pa_seq)) +
      geom_point(aes(y = stat_value,
                     text = paste0("Date: ", pa_date,
                                   "\nPA #", pa_seq,
                                   "\n", stat, ": ", sprintf("%.3f", stat_value))),
                 alpha = 0.25, size = 1.5, color = "#3c8dbc") +
      geom_line(data = line_data,
                aes(x = pa_seq, y = rolling_avg, group = 1,
                    text = paste0("Date: ", pa_date,
                                  "\n", window, "-PA Rolling ", stat, ": ",
                                  sprintf("%.3f", rolling_avg))),
                color = "#3c8dbc", linewidth = 1.2) +
      geom_hline(yintercept = lg_avg, linetype = "dashed", color = "#e74c3c", linewidth = 0.6) +
      annotate("text", x = 1, y = lg_avg,
               label = paste0("Lg Avg: ", sprintf("%.3f", lg_avg)),
               vjust = -0.5, hjust = 0, size = 3.5, color = "#e74c3c") +
      scale_x_continuous(breaks = date_breaks$pa_seq,
                         labels = format(date_breaks$pa_date, "%b %d")) +
      labs(x = "Date", y = stat) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  # HEATMAP ----
  
  output$heatmap_plot <- renderPlotly({
    dir_val <- input$heatmap_direction
    if (is.null(dir_val)) dir_val <- 0
    
    grid <- expand.grid(
      ExitSpeed = seq(40, 115, by = 1),
      Angle     = seq(-30, 50, by = 1)
    )
    grid$Direction <- dir_val
    
    grid_probs <- predict(rf_model, newdata = grid, type = "prob") %>%
      unclass() %>%
      as.data.frame() %>%
      bind_cols(grid) %>%
      mutate(xBases = 1 * `1B` + 2 * `2B` + 3 * `3B` + 4 * HR)
    
    p <- ggplot(grid_probs, aes(x = ExitSpeed, y = Angle, fill = xBases)) +
      geom_tile() +
      scale_fill_viridis_c(option = "inferno", name = "xBases") +
      labs(x = "Exit Velocity (mph)", y = "Launch Angle (\u00B0)",
           title = sprintf("Spray Angle: %d\u00B0", dir_val)) +
      theme_minimal(base_size = 13)
    
    ggplotly(p)
  })
  
  # LUCK BOARD ----
  
  output$luck_plot <- renderPlotly({
    luck_df <- comparison %>%
      arrange(luck_gap) %>%
      mutate(
        label = paste0(Batter, " (", Team, ")"),
        label = factor(label, levels = label),
        bar_color = ifelse(luck_gap >= 0,
                           "Unlucky (deserves more)",
                           "Lucky (overperforming)")
      )
    
    plot_height <- max(400, nrow(luck_df) * 35)
    
    p <- ggplot(luck_df, aes(x = label, y = luck_gap, fill = bar_color,
                             text = paste0(Batter, " (", Team, ")",
                                           "\nxBases/PA: ",
                                           sprintf("%.3f", xBases_per_PA),
                                           "\nActual: ",
                                           sprintf("%.3f", actual_Bases_per_PA),
                                           "\nGap: ",
                                           sprintf("%+.3f", luck_gap)))) +
      geom_col(width = 0.7) +
      coord_flip() +
      scale_fill_manual(values = c("Unlucky (deserves more)" = "#3c8dbc",
                                   "Lucky (overperforming)"  = "#f39c12")) +
      labs(x = NULL, y = "Luck Gap (xBases/PA \u2212 Actual Bases/PA)", fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(axis.text.y = element_text(size = 11))
    
    ggplotly(p, tooltip = "text", height = plot_height)
  })
  
  # MODEL INFO ----
  
  output$rf_summary <- renderPrint({
    print(rf_model)
  })
  
  output$conf_matrix_text <- renderPrint({
    print(conf_matrix)
  })
  
  output$var_importance <- renderPlotly({
    imp <- importance(rf_model) %>%
      as.data.frame() %>%
      rownames_to_column("Variable")
    
    p <- ggplot(imp, aes(x = reorder(Variable, MeanDecreaseGini),
                         y = MeanDecreaseGini)) +
      geom_col(fill = "#3c8dbc", width = 0.5) +
      coord_flip() +
      labs(x = NULL, y = "Mean Decrease Gini") +
      theme_minimal(base_size = 13)
    
    ggplotly(p)
  })
  
  output$ev_bucket_plot <- renderPlotly({
    buckets <- bip_with_probs %>%
      mutate(ev_bucket = cut(ExitSpeed,
                             breaks = c(0, 60, 70, 80, 90, 100, 120),
                             labels = c("<60", "60-70", "70-80",
                                        "80-90", "90-100", "100+"))) %>%
      group_by(ev_bucket) %>%
      summarise(
        n    = n(),
        `1B` = mean(`1B`),
        `2B` = mean(`2B`),
        `3B` = mean(`3B`),
        HR   = mean(HR),
        OUT  = mean(OUT),
        .groups = "drop"
      ) %>%
      pivot_longer(cols = c(`1B`, `2B`, `3B`, HR, OUT),
                   names_to = "Outcome", values_to = "Prob")
    
    buckets$Outcome <- factor(buckets$Outcome,
                              levels = c("HR", "3B", "2B", "1B", "OUT"))
    
    colors <- c("OUT" = "#999999", "1B" = "#2196F3",
                "2B" = "#4CAF50", "3B" = "#FF9800", "HR" = "#F44336")
    
    p <- ggplot(buckets, aes(x = ev_bucket, y = Prob, fill = Outcome,
                             text = paste0(Outcome, ": ",
                                           sprintf("%.1f%%", Prob * 100),
                                           "\n(n = ", n, ")"))) +
      geom_col() +
      scale_fill_manual(values = colors) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(x = "Exit Velocity Bucket", y = "Probability", fill = NULL) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        