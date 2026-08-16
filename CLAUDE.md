# CLAUDE.md — Bond Ladder Decumulation Thesis (Hutchison & Brodkin)

Project context for future sessions. Read this before continuing work.

## Project overview

Master's/Honours thesis project (UCT, BUS4129H) for supervisor Emlyn Flint's topic **EF3**:
> "Dynamic Retirement Strategies for South Africa: Testing the Inflation-Linked Bond Ladder as a Decumulation Solution"

Authors: Greig Hutchison & Jesse Brodkin.

An R Monte Carlo model that tests a **dynamic bond-ladder + ARVA decumulation strategy**
for South African retirees: buy a Redington-immunized bond ladder to secure near-term
income, hold equity for growth, review annually via a funded-ratio decision matrix
(extend / hold / repair / defend), and switch to ARVA drawdown once the ladder runs out.

> **Naming note:** the thesis title says *inflation-linked* bonds, but the code currently
> builds the ladder from **nominal** SA government bonds (the MTM extract in
> `Data/Bond Data.xls`). `Data/SA_ILB_Ladder_Candidates.xlsx` exists but is not yet wired
> in. Inflation enters only as a flat `inflation_rate <- 0.05` parameter.

## Repository layout

```
Thesis.R                          # MAIN entry point - run this
Functions/
  moving_block_bootstrap.R        # sourced by Thesis.R
  Reddington.R                    # sourced - bond universe, yield curve, immunization LP
  ARVA.R                          # sourced - mortality + annuity factor
  Decision_Matrix.R               # sourced - annual extend/hold/repair/defend rule
  Dynamic_Ladder.R                # sourced - THE main simulation engine
  Sensitivity_Analysis.R          # SECOND entry point - run separately, not sourced
  bootstrap_returns.R             # LEGACY, unused
  calculate_returns.R             # LEGACY, unused
  immunisation.R                  # LEGACY, unused - DO NOT source (see below)
Data/                             # gitignored - present locally only
sensitivity_plots/                # PNG output from Sensitivity_Analysis.R
```

Two independent scripts are run manually in RStudio: **`Thesis.R`** (base case, full
diagnostics for one parameter set) and **`Functions/Sensitivity_Analysis.R`**
(parameter sweeps → the five PNGs in `sensitivity_plots/`). Neither sources the other;
they duplicate their data-loading and bootstrap blocks.

### Data files (`Data/`, gitignored)

| File | Used? | Purpose |
|---|---|---|
| `Bond & Equity Data.xlsx` | **yes** — sheet `LT_DATA` | SA equity & bond total-return index levels → historical monthly returns for the bootstrap |
| `Bond Data.xls` | **yes** — sheet `MTM` | Bond universe: code, coupon, maturity, all-in price, clean price, MTM duration/convexity. Cell `C4` holds the valuation date. Also has a `BEASSA Yield Curve` sheet, currently unused |
| `SAIML98_SAIFL98_Mortality_Table.xlsx` | **yes** | SA insured lives mortality. Only the **male** `qx` column is read |
| `CPI_Index_Long_Format.xlsx` | no | Stochastic inflation not yet implemented |
| `SA Nominal Yield Curves - 1989-2026.xlsx` | no | Stochastic term structure not yet implemented |
| `SA_ILB_Ladder_Candidates.xlsx` | no | ILB universe — not yet wired in |

## Analysis workflow (`Thesis.R`, top to bottom)

1. **Source the five function files.** Note that `Reddington.R` and `ARVA.R` run code at
   source time and create globals the rest of the script depends on:
   `bonds_fixed`, `valuation_date`, `curve_yield()` (from `Reddington.R`) and `data`
   (the mortality table, from `ARVA.R`).
2. **Load & clean equity/bond history.** Read `LT_DATA`, drop the first 420 rows
   (hardcoded), keep columns 1–3, form monthly gross return relatives (`P_t / P_{t-1}`,
   i.e. **gross** factors ≈ 1.0x, not net returns) → `returns_matrix`.
3. **Bootstrap.** `set.seed(392)`; `moving_block_bootstrap()` → `sim_paths`, a
   `[360 × 2 assets × 1000 sims]` array. Only the equity column is actually used
   downstream (`equity_monthly_returns`); the bond column is bootstrapped but unused —
   the bond sleeve is valued analytically instead.
4. **Set parameters.** `pot = R10m`, `wd = 8%`, starting `ladder_length = 6y`,
   `max_ladder_years = 15`, `extend_by = 1`, `defend_cut = 5%`, `inflation_rate = 5%`,
   `bequeathment_pct = 10%`, `horizon = 360` months, ages 60 → 90.
5. **Build the initial ladder.** `optimize_redington_immunization()` finds the cheapest
   bond portfolio immunizing 6 years of `wd × pot` withdrawals. Its cost sets the bond
   share of the pot; script stops if that exceeds the pot.
6. **Size the cash buffer.** `bond_cashflow_schedule()` → `size_ladder_cash_buffer()`
   solves for `C0`, the starting cash needed so lumpy semi-annual coupons can fund a
   level monthly income without the account ever going negative.
   Equity gets the remainder: `pot − bond_cost − C0`.
7. **Run the simulation.** `run_dynamic_ladder_simulation()` — all 1,000 paths ×
   360 months in one pass (see below).
8. **Report & plot.** Terminal-value summary and percentiles, ruin/depletion
   probabilities, a 1,000-path equity fan chart, then single-path diagnostics for
   `sim_index <- 1`: decision log (console + RStudio `View()`), cash-account balance
   plot, month-by-month cash table, and a cash-flow timeline plot. Runtimes are printed
   for the bootstrap, the simulation, and the whole script.

### Ruin definition

`ruin_prob <- mean(EPort < bequeathment_target)` — failing to leave 10% of the starting
pot at month 360. Complete depletion (`EPort == 0`) is reported separately.
Note this is measured on the **equity portfolio only**; any residual cash-account
balance is not counted toward the bequest.

## Function reference

### `Functions/moving_block_bootstrap.R`

**`moving_block_bootstrap(returns_matrix, block_size = 6, horizon = 360, n_sims = 1000)`**
Overlapping moving-block bootstrap. For each sim, samples `ceiling(360/6) = 60` random
6-month blocks (with replacement) from the historical matrix, concatenates them, and
trims to `horizon`. Blocks are sampled across all assets jointly, preserving
cross-sectional correlation and short-run autocorrelation. Returns a
`[horizon × n_assets × n_sims]` array with asset names on dim 2.

### `Functions/Reddington.R`

Runs on source: reads the `MTM` sheet, builds `bonds_market` / **`bonds_fixed`**
(bond_code, coupon, redemption date, all-in price, clean price, MTM duration/convexity,
years_to_maturity) and **`valuation_date`** from cell `C4`. MTM-supplied duration and
convexity are reference-only and never used in the optimizer.

- **`curve_yield(t_years)`** — the discount curve for everything in the model.
  Currently a **flat 6% p.a. placeholder**. The BEASSA read + `approx()` interpolation
  is kept commented directly above it, so swapping in the real curve is a small change.
- **`single_bond_cashflows(coupon_rate_nominal, redemption_amount_pct, redemption_date, valuation_date)`**
  — one bond's semi-annual coupon + redemption schedule per R100 par. The coupon cycle is
  anchored **backwards from the bond's own redemption date** in exact 6-month steps
  (each date computed directly from redemption_date via `%m-%`, not chained, to avoid
  month-end drift). Returns date, exact `t_years`, type, amount.
- **`calculate_bond_metrics(bonds_fixed, valuation_date)`** — for every bond: PV,
  Macaulay duration, and annualized convexity, computed from its *own real* cash-flow
  dates discounted at `curve_yield()`. `market_price` stays the quoted MTM price; PV is
  an independent theoretical value, so the two legitimately differ.
- **`calculate_liability_metrics(total_pot_value, withdrawal_rate_pct, ladder_years)`**
  — the retiree's liability as an annuity-immediate: level annual withdrawals at
  t = 1..N. Returns annual_withdrawal, PV, duration, convexity.
- **`optimize_redington_immunization(total_pot_value, withdrawal_rate_pct, ladder_years, convexity_buffer = 0.5, valuation_date = NULL, on_infeasible = c("stop","flag"), bond_metrics_precomputed = NULL)`**
  — the core LP (`lpSolve`). Minimizes purchase cost subject to the three Redington
  conditions: `PV_A ≥ PV_L`, `D_A == D_L` (linearized as `Σ x_j·PV_j·(D_j − D_L) = 0`),
  and `C_A ≥ C_L + buffer`. Long-only, **continuous** units (fractional holdings allowed).
  - Bonds already matured at `valuation_date` have `pv = 0` and `NaN` duration; they are
    filtered out up front, since a NaN coefficient would silently corrupt the solve.
  - **Fallback:** exact duration matching is impossible when the liability's duration
    falls outside the range of available bond durations (common as a ladder shortens —
    a portfolio duration is a PV-weighted average and can't escape its constituents'
    range). Rather than fail, it re-solves with `PV_A == PV_L` exactly and minimizes
    `Σ x_j·PV_j·D_j`, i.e. gets portfolio duration as close to target as possible.
    Convexity stays a hard constraint. Sets `duration_matched = FALSE`.
  - `on_infeasible = "flag"` returns `list(feasible = FALSE, message = ...)` instead of
    `stop()`ing — used by the per-path rebalancer and the sensitivity sweeps.
  - Returns: feasible, duration_matched, total_cost, liability, bond_metrics, bond_units
    (named vector), `actuarial_summary` (PASSED/FAILED per condition), and
    `portfolio_allocation` (rows with units > 0).
- **`bond_cashflow_schedule(portfolio_allocation, bonds_fixed, valuation_date)`** —
  the ladder's actual cash flows, scaling `single_bond_cashflows()` by units bought.
  `month_index` (which simulation month each flow lands in) is the field used for
  placement; `year` is rounded to the nearest half-year for **display only**.
- **`bond_ladder_sale_value(bond_cf, valuation_date, ladder_years)`** — PV at ladder-end
  of any cash flows falling after ladder-end (immunization matches PV/duration/convexity,
  not exact timing, so overhang is expected). Uses exact date differences, no year
  bucketing. *Defined but not called by the current `Thesis.R` flow* — the simulation
  computes each path's own liquidation via `mark_to_model_bond_value()` instead.
- **`size_ladder_cash_buffer(bond_cf, ladder_years, annual_withdrawal, cash_annual_rate)`**
  — solves for the minimum starting cash `C0`. Because interest is the only
  balance-dependent term, `balance_m = balance_from_zero_m + C0·(1+r)^(m−1)`, so the
  answer is **closed-form**: simulate once from zero, then
  `C0 = max(0, max(−balance_track / growth_factors))`. No search loop.

### `Functions/ARVA.R`

Runs on source: reads `Data/SAIML98_SAIFL98_Mortality_Table.xlsx` into a global `data`
holding age and male `qx`.

- **`survival_probability(age_now)`** — `1 − qx` looked up by age.
- **`arva_annuity_factor(age, max_age = 90)`** — `Σ_t tPx / (1+y_t)^t` for
  t = 0..(max_age − age), discounted at the same `curve_yield()` used for the bond
  ladder, so both move together when the real curve lands. Depends only on age, not on
  portfolio value or path.
- **`run_arva_strategy(starting_capital, start_age, annual_returns, max_age = 90)`** —
  standalone single-path ARVA reference implementation (withdraw `portfolio / AF` each
  year). **Not used** by the Monte Carlo; `run_dynamic_ladder_simulation()` implements the
  same mechanic inline. Kept for testing / deterministic checks.

### `Functions/Decision_Matrix.R`

**`decision_matrix(funded_ratio, equity_return, inflation_rate, current_income, defend_cut = 0.05)`**
The annual review rule — a 2×2 on *funded ratio ≥ 1* (surplus) × *trailing 12-month
equity return > 0*:

| | Market up | Market down |
|---|---|---|
| **Surplus** | **Extend & Harvest** — extend the ladder, harvest equity gains, raise income by inflation | **Wait & Hold** — change nothing, don't sell equity |
| **Deficit** | **Repair** — change nothing, let equity recover before extending | **Defend** — cut income by `defend_cut`, no extension |

Returns a list: action, extend_ladder, harvest_equity, inflation_raise, spending_cut,
next-year `withdrawal`, and a notes string.

### `Functions/Dynamic_Ladder.R` — the simulation engine

- **`compute_funded_ratio(equity_value, cash_value, bond_value, expected_annual_expense, horizon_years)`**
  — total assets (equity + cash + mark-to-model bonds) ÷ PV of **all** future expected
  expenses from now to `max_age`, not just the years left on the current ladder.
  Expenses are treated as flat at today's secured income, discounted at `curve_yield()`.
- **`rebalance_ladder(remaining_years, annual_withdrawal, valuation_date_now, current_bond_value, current_cash_value, convexity_buffer, cash_annual_rate, bond_metrics_precomputed = NULL)`**
  — re-derives ladder *and* cash buffer together for a new target and returns
  `harvest_needed = (new_bond_cost + new_C0) − (current_bond + current_cash)`
  (positive → pull from equity; negative → excess flows back to equity).
  Calls the optimizer with `total_pot_value = annual_withdrawal, withdrawal_rate_pct = 100`,
  which collapses `calculate_liability_metrics()` back to exactly that annual amount.
  Propagates infeasibility rather than stopping.
- **`mark_to_model_bond_value(bond_units_held, valuation_date_now, bond_metrics_precomputed = NULL)`**
  — value of current holdings at a later date, using `calculate_bond_metrics()$pv`.
  There is no separate future market-price model, so PV under `curve_yield()` stands in
  for market value. **Known limitation:** with a flat placeholder curve this makes the
  bond sleeve fully deterministic — no bond price risk anywhere in the model.
- **`run_dynamic_ladder_simulation(...)`** — one monthly loop over `horizon` covering all
  paths. Each path carries its own state (phase, ladder target, income, holdings, cash,
  equity) and moves independently:

  1. **Annual check** at months 13, 25, 37… (`(m−1) %% 12 == 0`), only for paths still in
     the ladder phase. `calculate_bond_metrics()` is computed **once per month**, not per
     path — all paths share the same `valuation_date_now`, and recomputing per path was a
     real performance bug in an earlier draft.
  2. **Exit check first, unconditionally.** If `remaining_years <= 1e-9` the ladder is
     liquidated into equity (recorded as `bond_sale_amount`), the path flips to ARVA, and
     `ladder_end_month` is stamped. This must come before `remaining_years` is used
     anywhere else — in R, `1:0` is `c(1, 0)`, not empty, which would have fed the
     optimizer a bogus two-cash-flow liability.
     **Hitting `max_ladder_years` does not end the ladder** — it only caps further
     extension (`min(..., max_ladder_years)`); the path still runs its immunized
     portfolio out to the capped target date.
  3. Frozen paths (an earlier infeasible rebalance) are skipped.
  4. Compute funded ratio and trailing 12-month equity return, call `decision_matrix()`,
     log the decision.
  5. `Wait & Hold` / `Repair` → nothing changes. `Extend & Harvest` / `Defend` → set the
     new ladder target and income, `rebalance_ladder()`, move `harvest_needed` in/out of
     equity, reset cash to the new `C0`, swap bond holdings, and rewrite this path's
     future deposit schedule from month `m` onward. Infeasible → freeze the path
     (`infeasible_flag`), keep existing holdings.
  6. **Monthly cash mechanics, every path, every month.** Ladder phase: coupons/redemptions
     in, `income/12` out. ARVA phase: on each 12-month anniversary *of that path's own*
     ladder end, withdraw `EPort / arva_annuity_factor(age)` from equity into cash and
     reset the monthly draw; then pay out `income/12`. Cash accrues
     `cash_monthly_rate` each month.
  7. Equity compounds for everyone every month, both phases.

  Returns: `EPort_history`, `Cash_history`, `Cash_deposit_history`,
  `Cash_withdrawal_history`, `ARVA_withdrawal_history`, `ladder_end_month`,
  `ladder_years_final`, `infeasible_flag`, `bond_sale_amount`, `duration_matched_flag`,
  `decision_log` (long data.frame: sim, year, action, funded_ratio, equity_return,
  withdrawal, ladder_years, duration_matched, bonds_bought, horizon_years), and terminal
  `EPort`.

  Because every path extends/defends on its own schedule, **there is no single "end of
  ladder" month** — plots in `Thesis.R` show the median across paths, explicitly labelled
  as such.

### `Functions/Sensitivity_Analysis.R` (separate entry point)

Standalone script; re-does the data load and bootstrap with the same `set.seed(392)` so
every curve is compared against the identical 1,000 equity paths. Writes PNGs to
`sensitivity_plots/`. All plots put **P(Ruin) on the x-axis** and the swept parameter on
the y-axis.

Assumptions are tagged in-file for easy searching: `[ASSUMPTION: BASE CASE]`,
`[ASSUMPTION: CURVE VALUES]`, `[ASSUMPTION: RANGE]`, `[ASSUMPTION: DECUMULATION]`.

**Important simplification:** graphs 1, 2, 3 and 5 *ignore the actual ladder mechanics*
(no coupons, no cash buffer, no extend/defend). The ladder is used only to set a one-off
bond cost, so `equity0 = pot − bond_cost`; that slug grows untouched through the ladder
period and is then decumulated to month 360. Graph 4 is the one that runs the real
simulation.

- **`get_bond_cost(pot, wd, ladder_years, convexity_buffer)`** — Redington cost with
  `tryCatch` + `on_infeasible = "flag"`, returning `feasible = FALSE` instead of erroring.
- **`grow_and_decumulate(equity0, ladder_years, wd, decumulation, ...)`** — grows the
  equity slug through the ladder period, then decumulates annually to the horizon under
  either `"ARVA"` (age-based annuity factor, matching the real ARVA phase) or
  `"fixed_wd"` (flat `wd × pot`, escalated by `inflation_rate` each year). Returns the
  terminal-value vector across sims.
- **`plot_ruin_sensitivity(curve_list, xlab, ylab, main, filename, legend_pos)`** —
  multi-curve line plot; draws to screen and, if `save_plots`, to PNG.
- **`run_dynamic_ladder_simulation_toggle(..., use_decision_matrix = TRUE)`** — a
  **copy of `run_dynamic_ladder_simulation()`** with one added branch: when
  `use_decision_matrix = FALSE`, the annual check always resolves to `Wait & Hold`, i.e.
  buy the ladder at t = 0 and never touch it. Returns a trimmed result list.
  *This is duplicated code and will drift from the original if `Dynamic_Ladder.R` changes.*

| Graph | File | x-axis sweep | Curves | Decumulation |
|---|---|---|---|---|
| 1 | `01_withdrawal_rate.png` | wd 2%→10% by 0.1% | ladder length 5/10/15y | ARVA **and** fixed |
| 2 | `02_ladder_length.png` | ladder length 5→15y | wd 4/6/8% | ARVA **and** fixed |
| 3 | `03_bond_cost.png` | bond cost R0→R9.5m in 1%-of-pot steps (set directly, not Redington-derived) | wd 4/6/8% | ARVA |
| 4 | `04_decision_matrix.png` | wd 2%→10% by 1% (coarse — real sim is expensive) | decision matrix ON vs OFF | full simulation |
| 5 | `05_bequeathment.png` | bequest target 0%→30% by 2% | wd 4/6/8% | ARVA — terminal values simulated once and re-thresholded, since the bequest % only moves the ruin bar |

### Legacy files — not sourced, do not wire back in

- **`bootstrap_returns.R`** — earlier single-path 3-month block bootstrap returning a
  data.frame. Superseded by `moving_block_bootstrap()`.
- **`calculate_returns.R`** — dplyr helper computing net equity/bond returns plus
  inflation from CPI. Superseded by the inline return calc in `Thesis.R`, but it is the
  closest thing to a starting point for wiring CPI in.
- **`immunisation.R`** — the original Redington prototype. Hardcodes a 6-bond universe,
  a flat 9% yield, and semi-annual period counts, and **redefines
  `calculate_bond_metrics()` / `optimize_redington_immunization()` with different
  signatures** while also running an execution block on load. Sourcing it would silently
  break `Reddington.R`. Keep it as a historical reference only.

## Known limitations / active placeholders

1. **`curve_yield()` is a flat 6%.** It drives bond PV, duration, convexity, the funded
   ratio, the ARVA annuity factor, and mark-to-model bond values. Real term structure
   (`Data/SA Nominal Yield Curves`, or the commented BEASSA block in `Reddington.R`) is
   the single highest-leverage upgrade — until then the bond sleeve carries no price risk
   and is identical across all 1,000 paths.
2. **Inflation is a fixed 5%.** `CPI_Index_Long_Format.xlsx` is not read anywhere.
3. **Nominal bonds, not ILBs** — see the naming note above.
4. **Mortality uses male `qx` only**; the female column in the table is unused. Mortality
   feeds only the ARVA annuity factor — there is no survival-weighting of outcomes and
   no stochastic death, so every path runs the full 360 months.
5. **The bootstrapped bond return series is computed and then never used.**
6. **Cash isn't counted in the ruin metric** — ruin is measured on terminal `EPort` alone.
7. **Fractional bond units** — the LP is continuous, not integer.
8. **No baseline comparator.** The Hulett & Swanepoel (2024) dynamic trigger/haircut
   living annuity named in the proposal is still not implemented. Graph 4's
   "decision matrix OFF" static ladder is the only comparator that exists today.
9. **`Thesis.R` needs RStudio** — it calls `View()`, so it won't run headless under
   `Rscript` as-is.
10. **Hardcoded data cleaning** — `Bond_Equity_Data[-1:-420, 1:3]` in both entry scripts;
    also a dead first-pass returns calculation in `Thesis.R` (with a hardcoded
    `rep(1, 377)`) that is immediately overwritten by the correct one.
11. **Path casing** — both scripts `source("functions/...")` and `read_excel("data/...")`
    in lowercase while the real directories are `Functions/` and `Data/`. Fine on Windows
    and default macOS, would break on a case-sensitive filesystem. `ARVA.R` uses `Data/`.
    (`Sensitivity_Analysis.R`'s header note about `ARVA.R` hardcoding an absolute path is
    **stale** — that path is now relative.)
12. **Duplicated simulation body** between `Dynamic_Ladder.R` and the toggle wrapper in
    `Sensitivity_Analysis.R`.

## Suggested next steps

- Fit and wire in a real yield curve to replace `curve_yield()`; make it path-dependent
  so bond values vary by simulation.
- Bootstrap CPI jointly with equity/bonds and replace the flat `inflation_rate`.
- Swap the nominal bond universe for the ILB candidates, matching the thesis title.
- Build the Hulett & Swanepoel (2024) baseline comparator.
- Refactor the toggle wrapper into a `use_decision_matrix` argument on
  `run_dynamic_ladder_simulation()` itself so the two can't diverge.

## Key references

- Jonathan Brummer, "The Retirement Income Blueprint" (Substack) — source of the
  decision-matrix concept now implemented in `Decision_Matrix.R`
- Waring & Siegel (2015) — ARVA methodology
- Redington (1952) — immunization conditions
- Hulett & Swanepoel (2024) — intended dynamic baseline strategy (not yet implemented)
- ASSA / SAIML98–SAIFL98 mortality tables
