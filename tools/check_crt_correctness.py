#!/usr/bin/env python3
"""Inspect CRT item correctness table against the answer key using stdlib only.

Reads:
- outputs/sanity/crt_item_correctness.csv
- data/hb_crt_answer_key.json

Prints a summary of which raw responses per item are counted as correct/incorrect
and highlights mismatches relative to the accepted labels/values in the key.
"""

import csv
import json
import sys
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[1]
CSV_PATH = REPO_ROOT / "outputs" / "sanity" / "crt_item_correctness.csv"
KEY_PATH = REPO_ROOT / "data" / "hb_crt_answer_key.json"


def load_answer_key(path):
    key = json.loads(Path(path).read_text())
    accept = {}
    for entry in key:
        if entry.get("scale") != "CRT":
            continue
        item = entry["item"]
        accept[item] = {
            "values": set(str(v) for v in entry.get("accept_value", []) if v != ""),
            "labels": set(str(v).lower() for v in entry.get("accept_label", []) if v != ""),
        }
    return accept


def read_csv_rows(path):
    with path.open(newline="") as f:
        reader = csv.DictReader(f)
        return list(reader)


def main():
    if not CSV_PATH.exists():
        sys.exit(f"Missing {CSV_PATH}; run targets pipeline first.")
    if not KEY_PATH.exists():
        sys.exit(f"Missing {KEY_PATH}; ensure answer key is present.")

    rows = read_csv_rows(CSV_PATH)
    accept = load_answer_key(KEY_PATH)

    # Gather rows by item
    by_item = {}
    for row in rows:
        item = row["item"]
        by_item.setdefault(item, []).append(row)

    issues = []
    for item, subrows in sorted(by_item.items()):
        # map csv item (crt_1_1_num) back to answer key name (CRT_1_1)
        key_item = "CRT_" + item.split("crt_")[1].split("_num")[0].upper()
        allowed = accept.get(key_item, {"values": set(), "labels": set()})
        for row in subrows:
            resp = (row.get("response") or "").strip()
            status = "correct" if int(row.get("correct") or 0) > 0 else "incorrect" if int(row.get("incorrect") or 0) > 0 else "NA"
            match = False
            if resp and resp.lower() in allowed["labels"]:
                match = True
            try:
                if resp not in ("", "nan") and float(resp) in {float(v) for v in allowed["values"]}:
                    match = True
            except ValueError:
                pass
            if status == "correct" and not match:
                issues.append((item, resp, "counted correct but not in key"))
            if status == "incorrect" and match:
                issues.append((item, resp, "counted incorrect but matches key"))

    print("Checked responses against answer key")
    if issues:
        print("Found potential mismatches:")
        for it, resp, msg in issues:
            print(f" - {it}: '{resp}' -> {msg}")
    else:
        print("No mismatches detected; counts align with acceptance rules.")


if __name__ == "__main__":
    main()
