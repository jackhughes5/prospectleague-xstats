"""
Derives Prospect League wOBA weights from play-by-play XML data + run expectancy matrix.

For each PA in the XML data, computes run_value = RE(after) + runs_scored - RE(before),
then averages by event type to get linear weights. Outputs ready-to-paste R code for app.R.

"""

import xml.etree.ElementTree as ET
import glob
import csv
from collections import defaultdict

# paths
DATA_GLOB = "/Users/jackhughes/Desktop/CornbeltersProjects/RE_Matrix/**/*.xml"
RE_MATRIX_PATH = "/Users/jackhughes/Desktop/CornbeltersProjects/RE_Matrix/matrices/pl_all.csv"

TERMINAL = (0, 0, 0, 3)


# parsing ----

def classify_event(action):
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
    """Parse one half-inning into transition dicts with before/after states."""
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

        rows.append({
            "before": g["before"],
            "after": after,
            "runs": max(0, g["runs"]),
            "event": g["event"],
        })

    return rows


def parse_all_games(data_glob):
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


# RE matrix ----

def load_re_matrix(path):
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
    if state[3] >= 3:
        return 0.0
    return re_lookup.get(state, 0.0)


# weight derivation ----

def derive_weights(rows, re_lookup):
    run_values = defaultdict(list)
    for row in rows:
        re_before = get_re(row["before"], re_lookup)
        re_after = get_re(row["after"], re_lookup)
        rv = re_after + row["runs"] - re_before
        run_values[row["event"]].append(rv)

    avg_rv = {}
    counts = {}
    for event, values in run_values.items():
        avg_rv[event] = sum(values) / len(values)
        counts[event] = len(values)

    return avg_rv, counts


if __name__ == "__main__":
    print("Loading RE matrix...")
    re_lookup = load_re_matrix(RE_MATRIX_PATH)
    print(f"  {len(re_lookup)} states loaded\n")

    print("Parsing XML play-by-play...")
    rows = parse_all_games(DATA_GLOB)

    avg_rv, counts = derive_weights(rows, re_lookup)
    out_value = avg_rv.get("OUT", 0)

    # results
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