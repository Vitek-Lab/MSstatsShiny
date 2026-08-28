# Tracer-constant CSV fixtures

Authored for Phase 0.2 of `plans/tracer-constants-QA.md`. Used by the manual QA
walkthrough (Phases 2–5) and available to automated tests in
`tests/testthat/` via `test_path("data/tracer/<file>")`.

Schema is `GROUP,TracerConstant` (plan §5.A).

## Condition names

These use the **eight conditions of the working turnover dataset**, not the
`0h`/`6h`/`24h` names from the plan's example schema:

    0hr, 1hr, 4hr, 12hrs, 24hrs, 48hrs, 96hrs, 168hrs

Verified that both time parsers agree on all eight (`0, 1, 4, 12, 24, 48, 96,
168`) with no collisions and no `NA`s — so these fixtures reach the specific
trap each one exists for, rather than stopping at the group-match check.

Do not substitute `T0h`/`Time_0h` style names; both parse to `NA` under
`parse_timepoint`'s anchored `^[0-9]+` and yield an empty result set (plan
§0.3). **If the dataset's conditions change, rewrite the `GROUP` column in every
file here to match** — otherwise every fixture fails at the group-match check
and none of the traps below are reachable.

> Note on §0.3: this dataset cannot exercise the `24h`+`1d` collision case. No
> condition name contains `d` or `w`, so `autofill_condition_value` and
> `parse_timepoint` return identical values for all eight. Constructing that
> case needs a deliberately renamed annotation.

## Files

| File | Contents | What it must trap |
|---|---|---|
| `tracer_good.csv` | all eight conditions, `0.98` down to `0.91` | happy path; all values in `[0.01, 1]` |
| `tracer_missing_col.csv` | second column named `Tracer` | missing-column check |
| `tracer_nonnumeric.csv` | `12hrs` = `0.5abc` | Decision B coercion trap |
| `tracer_zero.csv` | `12hrs` = `0` | §0.2 — below the `0.01` floor (and would divide to `Inf`) |
| `tracer_tiny.csv` | `12hrs` = `1e-10` | §0.2 — below the `0.01` floor, must be **rejected** |
| `tracer_floor.csv` | `12hrs` = `0.01` | §0.2 — the inclusive boundary, must be **accepted** |
| `tracer_over.csv` | `12hrs` = `1.5` | range upper bound |
| `tracer_unknown.csv` | all eight conditions **plus** `336hrs` | group match |
| `tracer_partial.csv` | omits `168hrs` | Decision C (all-or-nothing coverage) |
| `tracer_dup.csv` | `0hr` twice, `0.98` then `0.42` | duplicate rows |
| `tracer_dupcol.csv` | `GROUP,TracerConstant,TracerConstant` | A6 |
| `tracer_empty.csv` | 0 bytes | A5 |
| `tracer_headeronly.csv` | header row only | A5 |
| `tracer_quote.csv` | `24"hrs` as a GROUP | Decision J |

## The `0.01` floor

§0.2 was resolved to an **inclusive `0.01` floor**: accepted range is
`[0.01, 1]`. `tracer_tiny.csv` and `tracer_floor.csv` are a matched pair
straddling that boundary — `1e-10` must be rejected, `0.01` must be accepted. A
`>` written where `>=` was meant passes every other fixture here and is caught
only by `tracer_floor.csv`, so run the two together.

## Verified behaviour (data.table `fread`)

Each fixture was checked to actually reproduce its trap, not just to look like
it would:

- **`tracer_nonnumeric.csv`** — `fread` types the column `character`, and
  `all(x > 0 & x <= 1)` over the column returns **`TRUE`** by lexicographic
  comparison. It passes a naive range check, then `as.numeric`
  silently yields `NA`. This is why the range helper must coerce first.
- **`tracer_dupcol.csv`** — `fread` keeps **both** `TracerConstant` columns;
  `dt$TracerConstant` silently returns the **first**. The first column holds
  `0.01`–`0.08` and the second the correct `0.98`–`0.91`, so "took the stale
  column" is visible in the output rather than being a subtle offset.
- **`tracer_empty.csv`** — `fread` **warns** (`has size 0. Returning a NULL
  data.table`) and returns a `0x0` data.table. It does not error, so a
  `tryCatch` around `fread` alone will not catch this.
- **`tracer_headeronly.csv`** — parses cleanly to 0 rows with correct names, and
  `all(logical(0))` is `TRUE`, so a naive range check **passes** on it.
- **`tracer_quote.csv`** — written with a bare `24"h` rather than the RFC 4180
  `"24""h"` form **on purpose**: `fread` does not unescape doubled quotes here
  (it yields the literal `24""hrs`), whereas the bare field parses to exactly
  one `"` character. Confirmed that feeding that group name through the current
  unescaped `paste0("  \"", conditions, "\" = ", ...)` produces a script that
  fails `parse()` with `unexpected symbol`.

  Reaching the script-generation step needs a condition genuinely named
  `24"hrs` in the annotation; against the unmodified annotation this file stops
  at the group-match check instead.

Phase 3 also calls for a **non-CSV file**. That is not checked in — use any
handy binary or text file (e.g. `inst/extdata/evidence.rda`).
