# CLAUDE.md — Bond Ladder Decumulation Thesis (Hutchison & Brodkin)

Project context for future sessions. Read this before continuing work.

## Project overview

Master's/Honours thesis project (UCT, BUS4129H) for supervisor Emlyn Flint's topic **EF3**:
> "Dynamic Retirement Strategies for South Africa: Testing the Inflation-Linked Bond Ladder as a Decumulation Solution"

Authors: Greig Hutchison & Jesse Brodkin.

An R Monte Carlo model that tests a **dynamic bond-ladder + ARVA decumulation strategy**
for South African retirees: buy an immunized bond ladder to secure near-term income, hold
equity for growth, review annually via a funded-ratio decision matrix
(extend / hold / repair / defend), and switch to ARVA drawdown once the ladder runs out.

> **Naming note:** the thesis title says *inflation-linked* bonds, but the code builds the
> ladder from **nominal** SA government bonds (the MTM extract in `Data/Bond Data.xls`).
> `Data/SA_ILB_Ladder_Candidates.xlsx` exists but is not wired in. Inflation is
> bootstrapped from CPI for the actual monthly payout; the flat `inflation_rate <- 0.05`
> survives where a forward-looking expectation is needed (the funded-ratio projection and
> the decision matrix's annual raise).

---

## Current state: mid-migration, and the tree does not run

Read this section before touching anything — it explains a lot of otherwise-confusing code.

The bond-selection layer is being rewritten from **Redington immunization** (match PV,
duration and convexity, then solve for a cash buffer to bridge lumpy coupons) to
**cash-flow matching** (buy the cheapest portfolio whose coupons and redemptions cover
every month's withdrawal). The new file is written; nothing has been migrated onto it yet.

**What is on disk right now:**

- `Functions/Bond Selection.R` — the **new** cash-flow-matching optimizer. Complete,
  self-contained, and **sourced by nothing**. Untracked in git.
- `Functions/Reddington.R` and `Functions/immunisation.R` — the **old** layer.
  **Deleted from the working tree** (`git status` shows `D` for both), still in `HEAD`.
- `Thesis.R`, `Functions/Dynamic_Ladder.R`, `Functions/Sensitivity_Analysis.R` — all still
  written against the **old** Redington API. Both entry scripts open with
  `source("functions/Reddington.R")`.

**So sourcing either entry script fails immediately** — the file it asks for is gone.

**And restoring it from git is not enough.** `git checkout HEAD -- Functions/Reddington.R`
brings back a version whose `size_ladder_cash_buffer()` signature is
`(bond_cf, ladder_years, annual_withdrawal, cash_annual_rate)`. The current `Thesis.R`
(line ~144) and `Dynamic_Ladder.R`'s `rebalance_ladder()` both pass `inflation_rate`, and
`rebalance_ladder()` also passes `inflation_factors` — R errors with *"unused argument"*.
The inflation-aware `Reddington.R` those callers were written against **exists nowhere in
the repository**. The CPI work in `Thesis.R` and `Dynamic_Ladder.R` is committed; the
matching bond file is not.

**Two ways out, pick one deliberately:**

1. **Finish the migration** — port `Dynamic_Ladder.R` and `Thesis.R` onto
   `Bond Selection.R` (API delta below) and drop the Redington references for good. This is
   the intended direction, and it deletes the cash-buffer / convexity / duration-matching
   machinery outright rather than repairing it.
2. **Reconstruct the missing `Reddington.R`** — restore from `HEAD` and re-add the
   `inflation_rate` / `inflation_factors` arguments to `size_ladder_cash_buffer()` (size the
   buffer against escalating rather than level withdrawals). This gets the old model running
   again but leaves `Bond Selection.R` orphaned.

### API delta, old → new

| What `Dynamic_Ladder.R` / `Thesis.R` call today (Redington) | What `Bond Selection.R` offers |
|---|---|
| `optimize_redington_immunization(total_pot_value, withdrawal_rate_pct, ladder_years, convexity_buffer, valuation_date, on_infeasible, bond_metrics_precomputed)` | `optimize_bond_ladder(total_pot_value, withdrawal_rate_pct, ladder_years, valuation_date)` — no `convexity_buffer`, no `bond_metrics_precomputed`, and it **`stop()`s** on infeasibility (no `on_infeasible = "flag"`; callers must `tryCatch`) |
| returns `$total_cost`, `$bond_units`, `$bond_metrics`, `$portfolio_allocation`, `$actuarial_summary`, `$duration_matched`, `$feasible` | returns `$total_cost`, `$cash_at_start`, `$total_outlay`, `$bond_units`, `$bond_amounts`, `$allocation`, `$liability`, `$schedule`, `$min_balance`, `$n_months`, `$ladder_years`, `$eligible_bonds` |
| `bond_cashflow_schedule(portfolio_allocation, bonds_fixed, valuation_date)` → long data.frame with `month_index` / `amount`, fed into `deposits_by_month` | `ladder_cashflow_schedule(bond_units, annual_withdrawal, ladder_years, ...)` → `$schedule` (one row per month, with `balance`) plus `$bond_flows` (the long form, same `month_index` convention) |
| `size_ladder_cash_buffer(...)` → a solved-for `C0` | **nothing to solve.** The only cash the ladder needs is `cash_at_start` = one monthly instalment |
| `bond_ladder_sale_value()` / a material `bond_sale_amount` at ladder end | **not needed** — the new ladder self-liquidates, every bond it holds redeems inside the window |
| `duration_matched` flag, relaxed duration fallback | **gone** — there is no duration matching left to fail |
| `res$bond_metrics$bond_code` used to size the `bond_holdings` matrix | use `bonds_fixed$bond_code` (the full universe) instead |

Other differences worth knowing when porting: `Bond Selection.R` adds a
**`MODEL_START_DATE` check** (stops on source if the MTM extract's cell `C4` is not
31 Jul 2026); it buckets cash flows by **calendar arithmetic** (`month_index_from_date()`,
floored at month 2) where the old file used `round(t_years * 12)`, which credited flows up
to ~2 weeks early; and it reads `Data/Bond Data.xls` with a capital `D` where the old file
used `data/`.

---

## Repository layout

```
Thesis.R                          # MAIN entry point - run in RStudio (currently broken, see above)
Functions/
  moving_block_bootstrap.R        # sourced by both entry scripts
  Reddington.R                    # DELETED from disk, still sourced by both entry scripts
  immunisation.R                  # DELETED from disk, was superseded by Reddington.R
  Bond Selection.R                # NEW cash-flow-matching optimizer - sourced by nothing yet
  ARVA.R                          # sourced - mortality + annuity factor
  Decision_Matrix.R               # sourced - annual extend/hold/repair/defend rule
  Dynamic_Ladder.R                # sourced - THE main simulation engine
  Sensitivity_Analysis.R          # SECOND entry point - run separately, not sourced
  yield_curve_simulation.R        # standalone PCA yield-curve model, not wired in
  bootstrap_returns.R             # LEGACY, unused
  calculate_returns.R             # LEGACY, unused
Data/                             # gitignored - present locally only
sensitivity_plots/                # five PNGs, output from Sensitivity_Analysis.R
```

Two independent scripts are run manually in RStudio: **`Thesis.R`** (base case, full
diagnostics for one parameter set) and **`Functions/Sensitivity_Analysis.R`** (parameter
sweeps → the five PNGs). Neither sources the other; they duplicate their data-loading and
bootstrap blocks, and have drifted apart (limitation 12).

### Data files (`Data/`, gitignored)

| File | Used? | Purpose |
|---|---|---|
| `Bond & Equity Data.xlsx` | **yes** — sheet `LT_DATA` | SA equity & bond total-return index levels → historical monthly returns for the bootstrap |
| `Bond Data.xls` | **yes** — sheet `MTM` | Bond universe: code, coupon, maturity, all-in price, clean price. Cell `C4` holds the valuation date. Also has a `BEASSA Yield Curve` sheet, read only by commented-out code |
| `SAIML98_SAIFL98_Mortality_Table.xlsx` | **yes** | SA insured lives mortality. Only the **male** `qx` column is read |
| `CPI_Index_Long_Format.xlsx` | **yes**, by `Thesis.R` only | Rows 168:558 (Dec 1993 →) → monthly CPI growth ratios, bootstrapped alongside equity and bonds. `Sensitivity_Analysis.R` does not read it |
| `SA Nominal Yield Curves - 1989-2026.xlsx` | no | Stochastic term structure not yet implemented |
| `SA_ILB_Ladder_Candidates.xlsx` | no | ILB universe — not yet wired in |

---

## Analysis workflow (`Thesis.R`, top to bottom)

1. **Source five function files** (`moving_block_bootstrap`, `Reddington`, `ARVA`,
   `Decision_Matrix`, `Dynamic_Ladder`). `Reddington.R` and `ARVA.R` run code at source
   time and create globals the rest of the script depends on: `bonds_fixed`,
   `valuation_date`, `curve_yield()` and `data` (the mortality table). **This is where the
   script currently dies** — `Reddington.R` is not on disk.
2. **Load & clean equity/bond/CPI history.** Read `LT_DATA`, drop the first 407 rows
   (hardcoded), keep columns 1–3, form monthly gross return relatives (`P_t / P_{t-1}`,
   i.e. **gross** factors ≈ 1.0x, not net returns), then merge CPI growth ratios on
   `YearMon` → `returns_matrix`.
3. **Bootstrap.** `set.seed(392)`; `moving_block_bootstrap()` → `sim_paths`, a
   `[360 × 3 series × 1000 sims]` array (equity, bonds, CPI). Equity and CPI are used
   downstream (`equity_monthly_returns`, `inflation_monthly_ratios`); the bond column is
   bootstrapped but unused — the bond sleeve is valued analytically instead.
4. **Set parameters.** `pot = R10m`, `wd = 5%`, starting `ladder_length = 10y`,
   `max_ladder_years = 15`, `extend_by = 1`, `defend_cut = 5%`, `inflation_rate = 5%`
   (expectations only), `bequeathment_pct = 10%`, `cash_annual_rate = 5%`,
   `convexity_buffer = 0.5`, `horizon = 360` months, ages 60 → 90.
5. **Build the initial ladder.** `optimize_redington_immunization()` matches PV, duration
   and convexity of the liability; the script stops if `total_cost` exceeds the pot, then
   prints the actuarial verification table and the allocation.
6. **Size the cash buffer.** `bond_cashflow_schedule()` lays out the ladder's real coupon
   and redemption dates; `size_ladder_cash_buffer()` solves in closed form for the smallest
   starting balance `C0` such that an account taking those deposits and paying `annual/12`
   every month never goes negative. Equity gets `pot − total_cost − C0`.
7. **Run the simulation.** `run_dynamic_ladder_simulation()` — all 1,000 paths ×
   360 months in one pass (see below).
8. **Report & plot.** Terminal-value summary and percentiles, ruin/depletion probabilities,
   a 1,000-path equity fan chart, then single-path diagnostics for `sim_index <- 1`:
   decision log (console + RStudio `View()`), cash-account balance plot, month-by-month cash
   table, and a cash-flow timeline plot. Runtimes are printed for the bootstrap, the
   simulation, and the whole script.

### Ruin definition

`ruin_prob <- mean(EPort < bequeathment_target)` — failing to leave 10% of the starting pot
at month 360. Complete depletion (`EPort == 0`) is reported separately. Note this is
measured on the **equity portfolio only**; any residual cash-account balance is not counted
toward the bequest.

---

## Function reference

### `Functions/moving_block_bootstrap.R`

**`moving_block_bootstrap(returns_matrix, block_size = 6, horizon = 360, n_sims = 1000)`**
Overlapping moving-block bootstrap. For each sim, samples `ceiling(360/6) = 60` random
6-month blocks (with replacement) from the historical matrix, concatenates them, and trims
to `horizon`. Blocks are sampled across all assets jointly, preserving cross-sectional
correlation and short-run autocorrelation. Returns a `[horizon × n_assets × n_sims]` array
with asset names on dim 2.

### `Functions/Bond Selection.R` — the new optimizer (not yet wired in)

Runs on source: reads the `MTM` sheet into **`bonds_fixed`** (bond_code, coupon, redemption
date, all-in price, clean price, years_to_maturity) and **`valuation_date`** from cell `C4`,
then checks that date against **`MODEL_START_DATE`** (31 July 2026) and **stops** if they
differ — bond prices and time zero have to be the same day, so dropping in a newer MTM
extract fails loudly rather than quietly repricing the ladder off a different date. The MTM
sheet's own duration / convexity / PV01 columns are **not read**.

- **`curve_yield(t_years)`** — flat **6% p.a. placeholder**, with the BEASSA read +
  `approx()` interpolation kept commented directly above it. It does **not** drive the
  ladder choice (the optimizer works off actual cash-flow dates and quoted market prices);
  it drives bond PV, the funded ratio, and the ARVA annuity factor. Note both this file and
  the deleted `Reddington.R` define `curve_yield()` — whichever is sourced last wins.
- **`month_index_from_date(dates, valuation_date, min_month = 2L)`** — which simulation
  month a flow falls in, by calendar arithmetic. Month *m* spans
  `[t0 %m+% months(m−1), t0 %m+% months(m))`. **Floored at month 2**: the ladder is bought
  at t0 and the first withdrawal is paid at t0, but the earliest a bond can pay is a full
  month later.
- **`single_bond_cashflows(coupon_rate_nominal, redemption_amount_pct, redemption_date, valuation_date)`**
  — one bond's semi-annual coupon + redemption schedule per R100 par. The coupon cycle is
  anchored **backwards from the bond's own redemption date** in exact 6-month steps (each
  date computed directly from `redemption_date` via `%m-%`, not chained, to avoid month-end
  drift).
- **`calculate_bond_metrics(bonds_fixed, valuation_date)`** — PV and Macaulay duration per
  bond from its own real cash-flow dates at `curve_yield()`. **No convexity column** (the
  Reddington version had one). `market_price` stays the quoted MTM price; `pv` is an
  independent theoretical value, so the two legitimately differ.
- **`calculate_liability_metrics(total_pot_value, withdrawal_rate_pct, ladder_years)`** —
  the retiree's liability as a **monthly annuity-due**: `annual/12` paid at the start of
  each of `round(ladder_years × 12)` months, first instalment at t = 0. `ladder_years` may
  be fractional (a mid-year rebalance), so everything works in whole months.
- **`ladder_cashflow_schedule(bond_units, annual_withdrawal, ladder_years, bonds, valuation_date, starting_cash = annual_withdrawal/12)`**
  — the ladder's month-by-month net cash flow, one row per month: `month`, `month_start`,
  `bond_inflow`, `coupon_in`, `redemption_in`, `withdrawal`, `net_cashflow`, `cum_net`,
  **`balance`**. Also returns `bond_flows`, `overhang` (empty by construction),
  `min_balance` and totals. `bond_units` **must be named**; an unnamed vector or an unknown
  bond code errors immediately rather than silently matching nothing.
- **`bond_units_from_weights(weights, sleeve_value, bonds)`** — sleeve proportions → units
  held (`units_j = weight_j × sleeve_value / market_price_j`), for testing a hand-picked
  ladder against the optimizer's answer.
- **`optimize_bond_ladder(total_pot_value, withdrawal_rate_pct, ladder_years, valuation_date = NULL)`**
  — the core LP (`lpSolve`). Variables are the **rands invested** in each bond:

  > minimise `Σ x_j` subject to `balance_m ≥ 0` for every month, `x_j ≥ 0`

  where the balance constraint expands to `Σ_j x_j · CumIn[m,j] / price_j ≥ (m−1)·W`.
  Month 1 reads `0 ≥ 0` — that is the "month 1 is covered by cash" rule — so the LP only
  carries months 2..n. Rows are scaled by their own right-hand side to keep every
  coefficient near 1, since raw rand amounts (~1e7) against per-rand cash-flow coefficients
  (~1e0) hit lpSolve's default tolerances.
  - **Eligibility**: priced, not already matured, and **redeems inside the ladder window** —
    which is what makes the ladder self-liquidating.
  - **Diagnostics before the solve**: a month no eligible bond can reach is unsatisfiable
    however much is spent, and is by far the most common failure. It is detected and
    reported explicitly (which month, what the earliest available payment is, and how to fix
    it) rather than surfacing as a bare "infeasible" from lpSolve.
  - **It `stop()`s on infeasibility.** Callers that sweep parameters or rebalance per path
    must wrap it in `tryCatch()`.
  - Returns `total_cost`, **`cash_at_start`** (`annual/12`), **`total_outlay`**,
    `bond_units` / `bond_amounts` (named, aligned to the **full** `bonds_fixed` universe
    with 0 for bonds not held), `allocation`, `liability`, `schedule` (rebuilt from scratch
    as a cross-check on the LP matrix), `min_balance` (should be ~0 — a binding constraint
    is what "cheapest" means), `n_months`, `ladder_years`, `eligible_bonds`.
- **`print_ladder_optimization(res, digits = 2)`** — console report: cost breakdown, bonds
  bought with weights, tightest balance, cash left over at ladder end.

#### Why cash-flow matching replaces Redington

The old model matched PV, duration and convexity and then solved separately for a cash
buffer big enough to bridge lumpy semi-annual coupons. Matching **cash flows month by
month** is the stronger requirement: if the money is there when it is needed, the ladder
does its job whatever the curve does. Consequences: no cash buffer to solve for (just the
single day-one instalment); no duration matching, so no `duration_matched` flag and no
relaxed fallback; no `convexity_buffer` in any signature; and the ladder self-liquidates, so
`bond_sale_amount` at ladder end is ~R0 by construction, where the immunized portfolio left
an overhang to sell. The liability is still a **level** monthly income — see limitation 5.

### `Functions/Reddington.R` — deleted from disk, still what everything calls

Present in `HEAD` only. Ran on source: read the `MTM` sheet (from `data/`, lowercase) into
`bonds_fixed` and `valuation_date`, and defined `curve_yield()` (flat 6%).

- `single_bond_cashflows()`, `calculate_bond_metrics()` (PV, Macaulay duration **and
  convexity**), `calculate_liability_metrics()` — same roles as their `Bond Selection.R`
  counterparts.
- **`optimize_redington_immunization(total_pot_value, withdrawal_rate_pct, ladder_years, convexity_buffer = 0.5, valuation_date = NULL, on_infeasible = c("stop","flag"), bond_metrics_precomputed = NULL)`**
  — LP minimising cost subject to `PV_A ≥ PV_L`, `D_A == D_L`, `C_A ≥ C_L + buffer`. Bonds
  with `pv == 0` (already matured, hence `NaN` duration) are filtered out first. If exact
  duration matching is impossible — the liability's duration sits outside the range of
  available bond durations, typical late in a ladder — it falls back to `PV_A == PV_L` plus
  minimum duration mismatch and sets `duration_matched = FALSE`. `on_infeasible = "flag"`
  returns `feasible = FALSE` instead of stopping.
- **`bond_cashflow_schedule(portfolio_allocation, bonds_fixed, valuation_date)`** — long
  data.frame of the bought portfolio's flows, with `month_index = round(t_years * 12)`.
- **`bond_ladder_sale_value(bond_cf, valuation_date, ladder_years)`** — PV at ladder end of
  the flows falling after it (Redington leaves an overhang; cash-flow matching does not).
- **`size_ladder_cash_buffer(bond_cf, ladder_years, annual_withdrawal, cash_annual_rate)`**
  — closed-form smallest `C0`. **The callers on disk pass two further arguments this version
  does not accept** (`inflation_rate`, `inflation_factors`) — see the migration section.

### `Functions/ARVA.R`

Runs on source: reads `Data/SAIML98_SAIFL98_Mortality_Table.xlsx` into a global `data`
holding age and male `qx`.

- **`survival_probability(age_now)`** — `1 − qx` looked up by age.
- **`arva_annuity_factor(age, max_age = 90)`** — `Σ_t tPx / (1+y_t)^t` for
  t = 0..(max_age − age), discounted at the same `curve_yield()` used for the bond ladder,
  so both move together when the real curve lands. Depends only on age, not on portfolio
  value or path.
- **`run_arva_strategy(starting_capital, start_age, annual_returns, max_age = 90)`** —
  standalone single-path ARVA reference implementation (withdraw `portfolio / AF` each
  year). **Not used** by the Monte Carlo; `run_dynamic_ladder_simulation()` implements the
  same mechanic inline. Kept for testing / deterministic checks.

### `Functions/Decision_Matrix.R`

**`decision_matrix(funded_ratio, equity_return, inflation_rate, current_income, defend_cut = 0.05)`**
The annual review rule — a 2×2 on *funded ratio ≥ 1* (surplus) × *trailing 12-month equity
return > 0*:

| | Market up | Market down |
|---|---|---|
| **Surplus** | **Extend & Harvest** — extend the ladder, harvest equity gains, raise income by inflation | **Wait & Hold** — change nothing, don't sell equity |
| **Deficit** | **Repair** — change nothing, let equity recover before extending | **Defend** — cut income by `defend_cut`, no extension |

Returns a list: action, extend_ladder, harvest_equity, inflation_raise, spending_cut,
next-year `withdrawal`, and a notes string.

### `Functions/Dynamic_Ladder.R` — the simulation engine

Still written against the Redington API throughout (`convexity_buffer`, `cash_annual_rate`,
`duration_matched`, `res_redington_initial`, `bond_cf_initial`).

- **`compute_funded_ratio(equity_value, cash_value, bond_value, expected_annual_expense, horizon_years, inflation_rate = 0)`**
  — total assets (equity + cash + mark-to-model bonds) ÷ PV of **all** future expected
  expenses from now to `max_age`, not just the years left on the current ladder. Each future
  year's expense is grown from today's secured income at `inflation_rate` and discounted at
  `curve_yield()`. The `inflation_rate = 0` default exists so the duplicated toggle wrapper
  in `Sensitivity_Analysis.R` keeps working unchanged.
- **`rebalance_ladder(remaining_years, annual_withdrawal, valuation_date_now, current_bond_value, current_cash_value, convexity_buffer, cash_annual_rate, bond_metrics_precomputed = NULL, inflation_rate = 0, inflation_factors = NULL)`**
  — re-immunizes for a new target **as at `valuation_date_now`**, re-sizes the cash buffer,
  and returns `harvest_needed = (new_bond_cost + new_C0) − (current_bond + current_cash)`
  (positive → pull from equity; negative → excess flows back to equity). Calls the optimizer
  with `total_pot_value = annual_withdrawal, withdrawal_rate_pct = 100`, which collapses
  `calculate_liability_metrics()` back to exactly that annual amount. Uses
  `on_infeasible = "flag"` and returns `feasible = FALSE` rather than stopping — a short
  remaining ladder with nothing short-dated left to buy is an expected outcome late in
  retirement, not a bug.
- **`mark_to_model_bond_value(bond_units_held, valuation_date_now, bond_metrics_precomputed = NULL)`**
  — value of current holdings at a later date, using `calculate_bond_metrics()$pv`. There is
  no separate future market-price model, so PV under `curve_yield()` stands in for market
  value. **Known limitation:** with a flat placeholder curve this makes the bond sleeve fully
  deterministic — no bond price risk anywhere in the model.
- **`run_dynamic_ladder_simulation(...)`** — one monthly loop over `horizon` covering all
  paths. Each path carries its own state (phase, ladder target, income, holdings, cash,
  equity) and moves independently:

  1. **Annual check** at months 13, 25, 37… (`m > 12 && (m−1) %% 12 == 0`), only for paths
     still in the ladder phase. `calculate_bond_metrics()` is computed **once per month**,
     not per path — all paths share the same `valuation_date_now`, and recomputing per path
     was a real performance bug in an earlier draft.
  2. **Exit check first, unconditionally.** If `remaining_years <= 1e-9` the ladder is
     liquidated into equity (recorded as `bond_sale_amount`), the path flips to ARVA, and
     `ladder_end_month` is stamped. This must come before `remaining_years` is used anywhere
     else — in R, `1:0` is `c(1, 0)`, not empty, which would have fed the optimizer a bogus
     two-cash-flow liability.
     **Hitting `max_ladder_years` does not end the ladder** — it only caps further extension
     (`min(..., max_ladder_years)`); the path still runs its bond portfolio out to the capped
     target date. The liquidation is logged as either "Cap Reached" or "Ladder Matured".
  3. Frozen paths (an earlier infeasible rebalance) are skipped.
  4. Compute funded ratio and trailing 12-month equity return, call `decision_matrix()`, log
     the decision.
  5. `Wait & Hold` / `Repair` → nothing changes. `Extend & Harvest` / `Defend` → set the new
     ladder target and income, size that rebalance's cash buffer against **this path's own
     realized future CPI** (`inflation_factors_s`, taken from the already-known bootstrapped
     inflation path — observed running ~25% annualized over some paths' final ladder years
     versus the 5% flat assumption), `rebalance_ladder()`, move `harvest_needed` in/out of
     equity, reset cash to the new `C0`, swap bond holdings, and rewrite this path's future
     deposit schedule from month `m` onward. Infeasible → freeze the path
     (`infeasible_flag`), keep existing holdings.
  6. **Monthly cash mechanics, every path, every month.** Ladder phase: coupons/redemptions
     in, then the CPI-indexed monthly income out — `(current_income/12) × (1 + cumulative CPI
     since income_reset_month)`, so the payout steps up smoothly month to month rather than
     jumping once a year, and each path sees its own realized cost-of-living trajectory.
     ARVA phase: on each 12-month anniversary *of that path's own* ladder end, withdraw
     `EPort / arva_annuity_factor(age)` from equity into cash and reset the monthly draw;
     then pay out `income/12`. Cash accrues `cash_monthly_rate` each month. There is **no
     equity top-up** if the cash account runs dry — it simply goes negative (limitation 4).
  7. Equity compounds for everyone every month, both phases.

  Returns: `EPort_history`, `Cash_history`, `Cash_deposit_history`, `Cash_withdrawal_history`,
  `ARVA_withdrawal_history` (a 30-row matrix, one row per possible ARVA year),
  `ladder_end_month`, `ladder_years_final`, `infeasible_flag`, `bond_sale_amount`,
  `duration_matched_flag`, `decision_log` (long data.frame: sim, year, action, funded_ratio,
  equity_return, withdrawal, ladder_years, duration_matched, bonds_bought, horizon_years),
  and terminal `EPort`.

  If `inflation_monthly_ratios` is not supplied, both the monthly payout and the rebalance
  buffer sizing fall back to flat compounding at `inflation_rate` — which is exactly what
  happens in `Sensitivity_Analysis.R`.

  Because every path extends/defends on its own schedule, **there is no single "end of
  ladder" month** — plots in `Thesis.R` show the median across paths, explicitly labelled as
  such.

### `Functions/Sensitivity_Analysis.R` (separate entry point)

Standalone script; re-does the data load and bootstrap with the same `set.seed(392)`. Writes
PNGs to `sensitivity_plots/`. All plots put **P(Ruin) on the x-axis** and the swept parameter
on the y-axis. Assumptions are tagged in-file for easy searching: `[ASSUMPTION: BASE CASE]`,
`[ASSUMPTION: CURVE VALUES]`, `[ASSUMPTION: RANGE]`, `[ASSUMPTION: DECUMULATION]`.

**Important simplification:** graphs 1, 2, 3 and 5 *ignore the actual ladder mechanics* (no
coupons, no cash buffer, no extend/defend). The ladder is used only to set a one-off day-0
cost, so `equity0 = pot − total_cost`; that slug grows untouched through the ladder period
and is then decumulated to month 360. Graph 4 is the one that runs the real simulation.

- **`get_bond_cost(pot, wd, ladder_years, convexity_buffer = 0.5)`** —
  `optimize_redington_immunization()` wrapped in `tryCatch`, returning `feasible = FALSE`
  instead of erroring. Reports `total_cost` only — the day-0 cash buffer is not netted off
  the equity slug in graphs 1/2/3/5.
- **`grow_and_decumulate(equity0, ladder_years, wd, decumulation, ...)`** — grows the equity
  slug through the ladder period, then decumulates annually to the horizon under either
  `"ARVA"` (age-based annuity factor, matching the real ARVA phase) or `"fixed_wd"` (flat
  `wd × pot`, escalated by `inflation_rate` each year). Returns the terminal-value vector
  across sims.
- **`plot_ruin_sensitivity(curve_list, xlab, ylab, main, filename, legend_pos)`** —
  multi-curve line plot; draws to screen and, if `save_plots`, to PNG.
- **`run_dynamic_ladder_simulation_toggle(...)`** — a **second, full copy** of the simulation
  body (~220 lines) with one addition: `use_decision_matrix`. When `FALSE`, the annual check
  always resolves to `Wait & Hold`, i.e. buy the ladder at t = 0 and never touch it — that is
  graph 4's static comparator. This copy predates the CPI work: it takes no
  `inflation_monthly_ratios`, so both arms of graph 4 run on flat inflation. Keeping two
  copies of the engine in sync is a standing hazard (limitation 12).

| Graph | File | y-axis sweep | Curves | Decumulation |
|---|---|---|---|---|
| 1 | `01_withdrawal_rate.png` | wd 2%→10% by 0.1% | ladder length 5/10/15y | ARVA **and** fixed |
| 2 | `02_ladder_length.png` | ladder length 5→15y | wd 4/6/8% | ARVA **and** fixed |
| 3 | `03_bond_cost.png` | bond cost R0→R9.5m in 1%-of-pot steps (set directly, not optimizer-derived) | wd 4/6/8% | ARVA |
| 4 | `04_decision_matrix.png` | wd 2%→10% by 1% (coarse — real sim is expensive) | decision matrix ON vs OFF | full simulation |
| 5 | `05_bequeathment.png` | bequest target 0%→30% by 2% | wd 4/6/8% | ARVA — terminal values simulated once and re-thresholded, since the bequest % only moves the ruin bar |

### Legacy files — not sourced, do not wire back in

- **`bootstrap_returns.R`** — earlier single-path 3-month block bootstrap returning a
  data.frame. Superseded by `moving_block_bootstrap()`.
- **`calculate_returns.R`** — dplyr helper computing net equity/bond returns plus inflation
  from CPI. Superseded by the inline return calc in `Thesis.R`.
- **`immunisation.R`** (deleted) — the Redington prototype; `Reddington.R` was its production
  rewrite.
- **`yield_curve_simulation.R`** — a standalone PCA + AR(1) model of the SA nominal yield
  curve, reading a **CSV** path (`simulate_yield_curves(data_path, ...)`), not the xlsx in
  `Data/`. Not sourced by either entry script, and it defines its own
  `moving_block_bootstrap()`, so sourcing it after `Functions/moving_block_bootstrap.R` would
  shadow the one the thesis actually uses. It is the natural starting point for replacing the
  flat `curve_yield()`.

---

## Known limitations / active placeholders

1. **The tree does not run.** See the migration section at the top — this outranks
   everything below it.
2. **`curve_yield()` is a flat 6%.** It drives bond PV, the funded ratio, the ARVA annuity
   factor, and mark-to-model bond values. Real term structure (`Data/SA Nominal Yield
   Curves`, `Functions/yield_curve_simulation.R`, or the commented BEASSA block) is the
   highest-leverage upgrade — until then the bond sleeve carries no price risk and is
   identical across all paths.
3. **Inflation is bootstrapped for the actual payout, flat 5% for expectations.** `Thesis.R`
   bootstraps CPI jointly with equity and bonds and indexes each path's monthly cash
   withdrawal to its own realized CPI; rebalance cash buffers are sized against that same
   realized path. The flat `inflation_rate` is still used where a forward-looking number is
   genuinely needed: `decision_matrix()`'s annual raise and `compute_funded_ratio()`'s
   liability projection. `Sensitivity_Analysis.R` does **not** bootstrap CPI, so its runs
   fall back to flat compounding throughout.
4. **The cash account can go negative.** Nothing tops it up from equity mid-ladder; the
   buffer is sized once per rebalance and then left to run. `Thesis.R` prints the minimum
   balance reached for the inspected path — worth checking before reading too much into a
   headline ruin number.
5. **The ladder funds a LEVEL income, but the retiree is paid a CPI-indexed one.** The
   liability is a level monthly instalment; the simulation pays that instalment escalated by
   realized CPI. Sizing the ladder on an escalating liability would close the gap, at the
   price of a more expensive ladder.
6. **Nominal bonds, not ILBs** — see the naming note above.
7. **Mortality uses male `qx` only**; the female column is unused. Mortality feeds only the
   ARVA annuity factor — there is no survival-weighting of outcomes and no stochastic death,
   so every path runs the full 360 months.
8. **Cash isn't counted in the ruin metric** — ruin is measured on terminal `EPort` alone.
9. **The bootstrapped bond return series is computed and then never used.**
10. **Fractional bond units** — the LP is continuous, not integer.
11. **No baseline comparator.** The Hulett & Swanepoel (2024) dynamic trigger/haircut living
    annuity named in the proposal is not implemented. Graph 4's "decision matrix OFF" static
    ladder is the only comparator that exists.
12. **The two entry scripts have drifted apart.** `Thesis.R` trims 407 rows, bootstraps three
    series (equity, bonds, CPI) and runs `wd = 5%` / `ladder_length = 10`;
    `Sensitivity_Analysis.R` trims 420, bootstraps two, and uses `BASE_WD = 8%` /
    `BASE_LADDER_YEARS = 6` while claiming to match `Thesis.R`. Same seed, *different* equity
    paths. Graph 4 is therefore not comparable to `Thesis.R`'s headline numbers, and its
    toggle wrapper is a stale copy of the engine.
13. **`Thesis.R` needs RStudio** — it calls `View()`, so it won't run headless under `Rscript`
    as-is.
14. **Path casing is inconsistent.** `source("functions/...")` in both entry scripts, and
    `here("data", ...)` / `read_excel("data/...")` for the xlsx files, against real
    directories `Functions/` and `Data/`; `ARVA.R` and `Bond Selection.R` use `Data/`. Fine
    on Windows and default macOS, would break on a case-sensitive filesystem.

---

## Suggested next steps

1. **Unbreak the tree** — pick one of the two routes in the migration section. Porting
   `Dynamic_Ladder.R` and `Thesis.R` onto `Bond Selection.R` is the intended direction, and
   it dissolves the cash-buffer, convexity and duration-matching machinery entirely.
2. Fit and wire in a real yield curve to replace `curve_yield()`; make it path-dependent so
   bond values vary by simulation.
3. Swap the nominal bond universe for the ILB candidates, matching the thesis title — which
   also dissolves limitation 5, since an ILB ladder funds a real income directly.
4. Build the Hulett & Swanepoel (2024) baseline comparator.
5. Bring `Sensitivity_Analysis.R`'s data load, base case and simulation call back in line with
   `Thesis.R` (limitation 12) — deleting the duplicated toggle wrapper in favour of a
   `use_decision_matrix` argument on `run_dynamic_ladder_simulation()` itself, so the two arms
   cannot drift.

## Key references

- Jonathan Brummer, "The Retirement Income Blueprint" (Substack) — source of the
  decision-matrix concept implemented in `Decision_Matrix.R`
- Waring & Siegel (2015) — ARVA methodology
- Redington (1952) — immunization conditions
- Hulett & Swanepoel (2024) — intended dynamic baseline strategy (not yet implemented)
- ASSA / SAIML98–SAIFL98 mortality tables
