"""Convert Florida Division of Elections precinct-level result files to OpenElections format.

Source: https://dos.fl.gov/elections/data-statistics/elections-data/precinct-level-election-results/
  one zip per election; one tab-delimited file per county (<CC>_PctResults<YYYYMMDD>.txt), no header:
  county code, county name, election no., date, election name, precinct id, precinct location,
  registered (total, REP, DEM, other), contest name, district, contest code, candidate, party,
  candidate voter id, DOE candidate number, votes.
  Some elections add <CC>_PctResults<date>_Recount.txt files holding only the recounted contest(s).

Usage: python dos_precinct_converter.py <unzipped folder> <YYYYMMDD> [--out DIR]
Writes <out>/<date>__fl__general__precinct.csv in the format of OpenElections' 2022 Florida file
(county,precinct,office,district,party,candidate,votes):
  * precinct = "<id>:<location>"; district "NA" when there is none; party "NA" when blank
  * runs of whitespace inside fields collapsed to one space (OE file-format test)
  * one "Registered Voters" row per precinct (office "Registered Voters", candidate "NA")
  * office from the statewide contest code where the code identifies it (below); otherwise the
    state's contest name, so local contests keep their own names
  * candidate names canonicalized per (contest code, DOE candidate number): counties sometimes spell
    the same candidate differently ("Rick Scott" vs "Scott / Lopez-Cantera" in 2014)
  * rows from a county's _Recount file replace the matching precinct x candidate rows (recount files
    have no Over/Under rows, so those stay as originally reported)
"""
import argparse, collections, csv, glob, os, re

COLS = ["cc", "county", "elec_no", "date", "elec_name", "pct", "pct_loc", "reg", "reg_r", "reg_d", "reg_o",
        "contest", "district", "code", "candidate", "party", "cand_id", "doe_no", "votes"]
OFFICE_BY_PREFIX = {"10": "President", "12": "U.S. Senate", "14": "U.S. House",
                    "24": "State Senate", "26": "State House"}
OFFICE_BY_CODE = {"160000": "Governor", "160200": "Attorney General",       # statewide offices
                  "160500": "Chief Financial Officer", "160800": "Commissioner of Agriculture"}
DISTRICTED = {"14", "24", "26"}           # district = digits 3-5 of the contest code


def read(path):
    with open(path, encoding="utf-8", errors="replace", newline="") as fh:
        for r in csv.reader(fh, delimiter="\t", quoting=csv.QUOTE_NONE):
            if len(r) >= len(COLS) and r[18].strip().lstrip("-").isdigit():   # skips header lines
                yield dict(zip(COLS, [re.sub(r"\s+", " ", x).strip() for x in r[:len(COLS)]]))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("folder"); ap.add_argument("date"); ap.add_argument("--out", default=".")
    a = ap.parse_args()
    files = sorted(glob.glob(os.path.join(a.folder, "*_Pct*.txt")))
    main_files = [f for f in files if "recount" not in f.lower()]
    recount_files = [f for f in files if "recount" in f.lower()]

    rows = [r for f in main_files for r in read(f)]
    replaced = []
    for f in recount_files:
        # Replace precinct x candidate rows one for one (2024 recount files have no Over/Under
        # rows, so those stay as originally reported). Recount files may zero-pad the precinct id
        # or prefix it with the county code ("01" vs "1", "SUM109" vs "9"), so match on the
        # normalized id or, failing that, on the polling-location text.
        npct = lambda x, cc: (x[len(cc):] if x.upper().startswith(cc.upper()) else x).lstrip("0") or "0"
        rc_rows = list(read(f))
        by_id = {(r["cc"], r["code"], npct(r["pct"], r["cc"]), r["candidate"]): r for r in rc_rows}
        by_loc = {(r["cc"], r["code"], r["pct_loc"], r["candidate"]): r for r in rc_rows}
        rc = {id(r): r for r in rc_rows}
        n_rep = n_chg = 0
        for i, r in enumerate(rows):
            new = (by_id.get((r["cc"], r["code"], npct(r["pct"], r["cc"]), r["candidate"]))
                   or by_loc.get((r["cc"], r["code"], r["pct_loc"], r["candidate"])))
            if new is not None and id(new) in rc:
                rc.pop(id(new))
                n_rep += 1; n_chg += r["votes"] != new["votes"]
                rows[i] = {**r, "votes": new["votes"]}     # keep the original precinct label
        replaced.append(f"{os.path.basename(f)}: {n_rep} candidate rows replaced ({n_chg} changed), "
                        f"{len(rc)} recount rows unmatched")
        rows.extend(rc.values())

    # canonical names
    # Repeated precinct copies: Palm Beach 2016 lists some precincts twice, once under their own
    # location and once under a combined polling-place label ("1170, 1172, 1173"), with identical
    # results. A location-group of a precinct whose full set of (contest, candidate, votes) equals
    # one already seen for the same precinct is dropped. (Genuine pieces, e.g. Miami-Dade 2016
    # ballot styles, have different numbers and are summed below.)
    groups = collections.OrderedDict()
    for r in rows:
        groups.setdefault((r["cc"], r["pct"], r["pct_loc"]), []).append(r)
    seen_sig, kept, n_dropped_groups = collections.defaultdict(set), [], 0
    for (cc, pct, loc), rs in groups.items():
        sig = frozenset(collections.Counter((r["code"], r["contest"], r["candidate"], r["votes"]) for r in rs).items())
        if sig in seen_sig[(cc, pct)]:
            n_dropped_groups += 1
            continue
        seen_sig[(cc, pct)].add(sig)
        kept.extend(rs)
    rows = kept

    # A county occasionally reuses one contest code for two contests (Indian River 2016: "City of
    # Sebastian 2 Year Term" and "1 Year Term", both 730010); those keep their own contest names.
    names_in_county = collections.defaultdict(set)
    for r in rows:
        names_in_county[(r["cc"], r["code"])].add(r["contest"])
    shared_code = {k for k, v in names_in_county.items() if len(v) > 1}

    # Special rows: statewide DOE numbers 900/901/902 = write-in/over/under, whatever a county calls
    # them ("Times Over Voted", "Number of Under Votes", "Times Blank Voted", "Write-in 30", ...).
    SPECIAL = {"900": "WriteinVotes", "901": "OverVotes", "902": "UnderVotes"}
    def special_name(r):
        if r["doe_no"] in SPECIAL:
            return SPECIAL[r["doe_no"]]
        if not r["party"] and r["doe_no"] in ("", "0"):
            c = r["candidate"].lower()
            if "over" in c: return "OverVotes"
            if "under" in c or "blank" in c: return "UnderVotes"
            if "write" in c: return "WriteinVotes"
        return None

    # Canonical candidate names per (contest code, DOE number), used only when the number names a
    # single choice within every county. Retention questions give "Yes" and "No" the same number
    # (the judge's), so those keys are left alone.
    names = collections.defaultdict(collections.Counter)
    per_county = collections.defaultdict(set)
    contest_names = collections.defaultdict(collections.Counter)
    for r in rows:
        if r["doe_no"] not in ("", "0") and not special_name(r):
            names[(r["code"], r["doe_no"])][r["candidate"]] += 1
            per_county[(r["code"], r["doe_no"], r["cc"])].add(r["candidate"])
        contest_names[r["code"]][r["contest"]] += 1
    ambiguous = {(k[0], k[1]) for k, v in per_county.items() if len(v) > 1}
    canon = {k: c.most_common(1)[0][0] for k, c in names.items() if k not in ambiguous}

    def office_of(r):
        if (r["cc"], r["code"]) in shared_code:
            return r["contest"]
        if r["code"] in OFFICE_BY_CODE:
            return OFFICE_BY_CODE[r["code"]]
        if r["code"][:2] in OFFICE_BY_PREFIX:
            return OFFICE_BY_PREFIX[r["code"][:2]]
        return contest_names[r["code"]].most_common(1)[0][0]

    def district_of(r):
        if r["code"][:2] in DISTRICTED:
            return str(int(r["code"][2:5]))
        d = r["district"]
        m = re.fullmatch(r"District\s+(\d+)", d, flags=re.I)
        return m[1] if m else (d if d else "NA")

    # Different contests that would get the same office+district label in a county (e.g. two seats
    # both called "City of Sebastian 2 Year Term") are told apart by appending the contest code.
    label_codes = collections.defaultdict(set)
    for r in rows:
        label_codes[(r["cc"], office_of(r), district_of(r))].add(r["code"])
    clash = {k for k, v in label_codes.items() if len(v) > 1}

    # Some counties report a precinct in several pieces (e.g. Miami-Dade 2016: one row set per
    # ballot style, each with its own registration count). Pieces are summed into one precinct row;
    # registered voters are summed over the pieces of the first contest/candidate seen.
    agg = collections.OrderedDict()
    reg_key, reg_sum = {}, collections.Counter()
    n_pieces = 0
    for r in rows:
        county, pct = r["county"], f"{r['pct']}:{r['pct_loc']}"
        first = reg_key.setdefault((county, pct), (r["code"], r["candidate"]))
        if (r["code"], r["candidate"]) == first:
            reg_sum[(county, pct)] += int(r["reg"] or 0)
        off, dist = office_of(r), district_of(r)
        if (r["cc"], off, dist) in clash:
            off = f"{off} (contest {r['code']})"
        cand = special_name(r) or canon.get((r["code"], r["doe_no"]), r["candidate"])
        party = "NA" if special_name(r) else (r["party"] or "NA")
        k = (county, pct, off, dist, party, cand)
        if k in agg:
            n_pieces += 1
        agg[k] = agg.get(k, 0) + int(r["votes"] or 0)
    out_rows, done = [], set()
    for (county, pct, off, dist, party, cand), v in agg.items():
        if (county, pct) not in done:
            done.add((county, pct))
            out_rows.append([county, pct, "Registered Voters", "NA", "NA", "NA", reg_sum[(county, pct)]])
        out_rows.append([county, pct, off, dist, party, cand, v])

    os.makedirs(a.out, exist_ok=True)
    fn = os.path.join(a.out, f"{a.date}__fl__general__precinct.csv")
    with open(fn, "w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh, lineterminator="\n")
        w.writerow(["county", "precinct", "office", "district", "party", "candidate", "votes"])
        w.writerows(out_rows)
    print(f"{len(main_files)} county files (+{len(recount_files)} recount), {len(out_rows):,} rows -> {fn}")
    for x in replaced: print("  recount:", x)
    multi = {k: c for k, c in names.items() if len(c) > 1 and k not in ambiguous}
    print(f"  candidates canonicalized (>1 spelling across counties): {len(multi)}; "
          f"Yes/No-style keys left alone: {len(ambiguous)}")
    print(f"  contest labels disambiguated with code: {len(clash)}; shared codes split by name: {len(shared_code)}; "
          f"repeated precinct copies dropped: {n_dropped_groups}; precinct pieces summed: {n_pieces}")


if __name__ == "__main__":
    main()
