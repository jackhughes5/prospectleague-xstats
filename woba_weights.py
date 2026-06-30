"""
woba_weights.py

Parses PrestoSports XML play-by-play data, computes the run value of each
event type using the run expectancy matrix, and outputs Prospect League-
specific wOBA linear weights.

Uses the same parsing logic as your baseball_sim.py / parse_phase2.py.

Usage:
    python derive_woba_weights.py

Output:
    - Per-event run values
    - wOBA linear weights (run values above out)
    - Ready-to-paste R code for app.R
"""

import xml.etree.ElementTree as ET
import glob
import csv
from collections import defaultdict

# ═══════════════════════════════════════════════════════════
# CONFIG — update these paths for your machine
# ═══════════════════════════════════════════════════════════

# Path to your PrestoSports XML box scores (recursive glob)
DATA_GLOB = "/Users/jackhughes/Desktop/CornbeltersProjects/RE_Matrix/**/*.xml"

# Path to your run expectancy matrix CSV
RE_MATRIX_PATH = "/Users/jackhughes/Desktop/CornbeltersProjects/RE_Matrix/matrices/pl_all.csv"

TERMINAL = (0, 0, 0, 3)


# ═══════════════════════════════════════════════════════════
# XML PARSER (same logic as parse_phase2.py)
# ═══════════════════════════════════════════════════════════

def classify_event(action):
    """Classify a batter action string into an event type."""
    tok = (action or "").strip().split()
    if not tok:
        return None
    t = tok[0]
    if t in ("1B", "2B", "3B", "HR"):
        return t
    if t in ("BB", "IBB", "HP", "HBP"):
        return "BB"
    return "OUT"


def base_out_state(play):
    """The base-out state at the START of this play."""
    outs_str = play.get("outs")
    if outs_str is None:
        return None
    return (
        1 if play.get("first") else 0,
        1 if play.get("second") else 0,
        1 if play.get("third") else 0,
        int(outs_str),
    )


def runs_on_play(play):
    r = sum(1 for rn in play.findall("runner") if rn.get("scored") == "1")
    b = play.find("batter")
    if b is not None and b.get("tobase") == "4":
        r += 1
    return r


def outs_on_play(play):
    o = sum(int(rn.get("out") or 0) for rn in play.findall("runner"))
    b = play.find("batter")
    if b is not None:
        o += int(b.get("out") or 0)
    return o


def is_tiebreaker(batting):
    return any(
        "placed on" in (n.get("text") or "") for n in batting.iter("narrative")
    )


def half_inning_rows(batting):
    """Parse one half-inning into a list of {before, after, runs, event} dicts."""
    groups, cur, outs_made = [], None, 0

    for play in batting.findall("play"):
        if play.find("batter") is None and play.find("runner") is None:
            continue
        if int(play.get("outs") or 0) >= 3:
            continue

        b = play.get("batter")
        state = base_out_state(play)
        if state is None:
            continue
        if cur is None or b != cur["batter"]:
            cur = {
                "batter": b,
                "before": state,
                "runs": 0,
                "event": None,
            }
            groups.append(cur)

        cur["runs"] += runs_on_play(play)
        outs_made += outs_on_play(play)

        be = play.find("batter")
        if be is not None and (be.get("action") or "").strip():
            cur["event"] = classify_event(be.get("action"))

    complete = outs_made >= 3
    rows = []
    for i, g in enumerate(groups):
        if i + 1 < len(groups):
            after = groups[i + 1]["before"]
        elif complete:
            after = TERMINAL
        else:
            break

        if g["event"] is None:
            continue

        rows.append(
            {
                "before": g["before"],
                "after": after,
                "runs": max(0, g["runs"]),
                "event": g["event"],
            }
        )

    return rows


def parse_all_games(data_glob):
    """Parse all XML files and return a list of PA transition dicts."""
    files = sorted(glob.glob(data_glob, recursive=True))
    print(f"Found {len(files)} game files")

    all_rows = []
    skipped = 0
    for f in files:
        try:
            root = ET.parse(f).getroot()
        except ET.ParseError:
            skipped += 1
            continue

        for batting in root.iter("batting"):
            if is_tiebreaker(batting):
                continue
            all_rows.extend(half_inning_rows(batting))

    print(f"Parsed {len(all_rows)} plate appearances ({skipped} files skipped)")
    return all_rows


# ═══════════════════════════════════════════════════════════
# RE MATRIX LOADER
# ═══════════════════════════════════════════════════════════

def load_re_matrix(path):
    """Load RE matrix CSV into a lookup dict: (outs, b1, b2, b3) -> expected_runs."""
    lookup = {}
    with open(path) as f:
        reader = csv.DictReader(f)
        for row in reader:
            outs = int(row["start_outs"])
            base = str(int(float(row["base_state"]))).zfill(3)
            b1, b2, b3 = int(base[0]), int(base[1]), int(base[2])
            lookup[(b1, b2, b3, outs)] = float(row["expected_runs"])
    return lookup


def get_re(state, re_lookup):
    """Look up RE for a state. Terminal (3 outs) = 0."""
    if state[3] >= 3:
        return 0.0
    return re_lookup.get(state, 0.0)


# ═══════════════════════════════════════════════════════════
# WEIGHT DERIVATION
# ═══════════════════════════════════════════════════════════

def derive_weights(rows, re_lookup):
    """Compute average run value per event type."""
    run_values = defaultdict(list)

    for row in rows:
        re_before = get_re(row["before"], re_lookup)
        re_after = get_re(row["after"], re_lookup)
        rv = re_after + row["runs"] - re_before
        run_values[row["event"]].append(rv)

    # Average run value per event type
    avg_rv = {}
    counts = {}
    for event, values in run_values.items():
        avg_rv[event] = sum(values) / len(values)
        counts[event] = len(values)

    return avg_rv, counts


# ═══════════════════════════════════════════════════════════
# MAIN
# ═══════════════════════════════════════════════════════════

if __name__ == "__main__":
    # 1. Load RE matrix
    print("Loading RE matrix...")
    re_lookup = load_re_matrix(RE_MATRIX_PATH)
    print(f"  {len(re_lookup)} states loaded\n")

    # 2. Parse play-by-play
    print("Parsing XML play-by-play...")
    rows = parse_all_games(DATA_GLOB)

    # 3. Derive weights
    avg_rv, counts = derive_weights(rows, re_lookup)

    out_value = avg_rv.get("OUT", 0)

    # 4. Display results
    print("\n" + "=" * 60)
    print("PROSPECT LEAGUE wOBA WEIGHTS (empirical from play-by-play)")
    print("=" * 60)

    print(f"\n{'Event':>8s}  {'Count':>7s}  {'Avg Run Val':>11s}  {'Above Out':>10s}")
    print("-" * 45)

    weights = {}
    for event in ["BB", "1B", "2B", "3B", "HR", "OUT"]:
        if event not in avg_rv:
            continue
        rv = avg_rv[event]
        above = rv - out_value if event != "OUT" else 0
        weights[event] = above
        label = "(baseline)" if event == "OUT" else f"{above:.4f}"
        print(f"{event:>8s}  {counts[event]:>7,d}  {rv:>+11.4f}  {label:>10s}")

    # 5. Compare with estimates and MLB
    mlb = {"BB": 0.69, "1B": 0.89, "2B": 1.27, "3B": 1.62, "HR": 2.10}
    est = {"BB": 0.8663, "1B": 1.0173, "2B": 1.3322, "3B": 1.5820, "HR": 1.9104}

    print(f"\n{'Event':>8s}  {'Empirical':>10s}  {'Estimated':>10s}  {'MLB':>8s}")
    print("-" * 45)
    for e in ["BB", "1B", "2B", "3B", "HR"]:
        emp = weights.get(e, 0)
        print(f"{e:>8s}  {emp:>10.4f}  {est[e]:>10.4f}  {mlb[e]:>8.4f}")

    # 6. Output R code
    print("\n" + "=" * 60)
    print("PASTE INTO app.R (replace the xwOBA lines):")
    print("=" * 60)
    w = weights
    print(
        f"\n# In the 'expected' mutate (line ~188):"
        f"\n# Prospect League wOBA weights (empirical from play-by-play + RE matrix)"
        f"\nxwOBA = {w.get('BB',0):.4f}*xBB + {w.get('1B',0):.4f}*x1B_final + "
        f"{w.get('2B',0):.4f}*x2B_final +"
        f"\n        {w.get('3B',0):.4f}*x3B_final + {w.get('HR',0):.4f}*xHR_final,"
    )
    print(
        f"\n# In pa_timeline BIP xwOBA (line ~277):"
        f"\npa_xwOBA = {w.get('1B',0):.4f}*`1B` + {w.get('2B',0):.4f}*`2B` + "
        f"{w.get('3B',0):.4f}*`3B` + {w.get('HR',0):.4f}*HR,"
    )
    print(
        f"\n# In pa_timeline BB/HBP xwOBA (line ~294):"
        f"\npa_xwOBA = ifelse(pa_type %in% c(\"BB\", \"HBP\"), {w.get('BB',0):.4f}, 0),"
    )