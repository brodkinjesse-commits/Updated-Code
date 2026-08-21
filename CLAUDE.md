# CLAUDE.md — Bond Ladder Decumulation Thesis (Hutchison & Brodkin)

Project context for future sessions. Read this before continuing work.

## Project overview

Master's/Honours thesis project (UCT, BUS4129H) for supervisor Emlyn Flint's topic **EF3**:
> "Dynamic Retirement Strategies for South Africa: Testing the Inflation-Linked Bond Ladder as a Decumulation Solution"

Authors: Greig Hutchison & Jesse Brodkin.

An R Monte Carlo model that tests a **dynamic ILB-ladder + ARVA decumulation strategy** for
South African retirees: buy a cash-flow-matched inflation-linked bond ladder to secure
near-term real income, hold equity for growth, review annually via a funded-ratio decision
matrix (extend / hold / repair / defend), and switch to ARVA drawdown once the ladder runs
out.

---

## Current state: the invariant rewrite is done

`Thesis.R` runs end-to-end, headless, under `Rscript`, and every accounting identity in the
model is now checked on every path in every month.

The previous version of this document opened by describing a working ILB migration. The
migration *was* working in the sense that it ran; it was not producing correct numbers. Five
separate defects were all the same mistake — **a REAL rand amount used where a NOMINAL one
belonged** — and every one of them was silent. Nothing crashed, no number looked absurd, and
the model produced a clean-looking distribution that was wrong by up to 1.8×.

All five are fixed, and `Functions/Invariants.R` now exists specifically so that class of
failure cannot recur silently. **Each of the five was re-introduced deliberately and confirmed
to be caught** — see "Proving the layer works" below.

### The five defects

| # | Where | What was wrong |
|---|---|---|
| 1 | `ILB_Indexation.R`, `ILB_Repricing.R` | The **index-ratio double count** — the root cause. Every deposit and every ladder mark was over-stated by the bond's own t0 index ratio (1.138 – 3.188 on this universe). Measured: the base-case ladder marked at **R6,127,443 against a purchase cost of R3,404,649 — 1.80×, at t0 itself**, before a single month of inflation. |
| 2 | `Dynamic_Ladder.R` | `extend_ilb_ladder()`'s `bond_cost` / `cash_top_up` came back in **t0-real** rands and were charged straight to nominal `EPort` and nominal cash. Every extension was cheaper than it really was, by a factor that grew with cumulative inflation (~1.4× by year 10 at 5%). |
| 3 | `Dynamic_Ladder.R` | `compute_funded_ratio()` was handed a **real** income as though it were the current nominal spend, understating the liability and biasing every review toward Extend & Harvest. |
| 4 | `Dynamic_Ladder.R`, `Decision_Matrix.R` | `current_income` **changed denomination mid-run** — see the next section, it is subtler than it looks. |
| 5 | `Thesis.R` | The bequeathment target was **nominal**. R1m at month 360 is R231,377 of today's money at 5% — the metric scored a 77% erosion of the retiree's stated goal as a success. |

Plus two smaller ones surfaced by building the checks: the cash account credited interest
**after** the month's flows while the selector credits it on the opening balance (an extra
month of interest on every flow), and the ARVA branch clamped `EPort` to zero rather than the
withdrawal, silently writing off a negative equity balance.

### Two more, found once the arithmetic was trustworthy

Fixing the units made two **policy** defects visible that the audit could never have caught —
the books balanced perfectly throughout. That division is the point: the invariant layer proves
the arithmetic, not that the strategy is sensible.

| # | Where | What |
|---|---|---|
| 6 | `Thesis.R` | **`inflation_rate` was 5% against a CPI series averaging 5.82%.** The model assumed one number and fed itself another, on the one parameter that sets the Fisher real rate on ladder cash *and* escalates the funded ratio's forward liability. The realised real cash rate was −0.74% against an assumed 0.00%, on 86.5% of paths. Since the cash solve is exactly binding (`min_balance = 0`), that drift alone put **308/1000 paths into overdraft**. Now 5.8%, with `check_inflation_assumption()` re-deriving the realised figure every run. |
| 7 | `ILB_Repricing.R`, `Dynamic_Ladder.R` | **`cash_top_up` was a requirement, not an increment.** `extend_ilb_ladder()` returned `fresh$cash_at_start` — the *full* opening balance its sub-ladder needs on its own start date — and the engine charged it to equity every year. Because the anchor often sits still for several consecutive extensions, the same bridge was funded three or four times. Sim 1 paid R155k, R165k, R179k and R186k of **real** rands across months 12/24/36/48, all solving from the same anchor (R202, month 88); across 25 paths the double-charge totalled R31.8m nominal. Equity was converted into idle cash earning −0.8% real. |

### Defect 4 in full — the one that is not what it looks like

An earlier version of this document recorded, as limitation 1, that "the real income gets a 5%
REAL raise every Extend year, compounding". **That was wrong** and has been removed. Tracing
the mechanic numerically:

`decision_matrix()`'s `current_income × (1 + inflation_rate)` on an Extend cancelled against
`income_reset_month`, which re-based the CPI indexation to 1 on every reset. The bump
*replaced* the year's indexation rather than stacking on it. With an Extend every year and
realized inflation of exactly 5%, the income actually **paid** stayed flat at R600,000 real
even while the logged `current_income` climbed to R804,057 — reading the logged variable
instead of the payment is what produced the false alarm.

The mechanic was only correct by coincidence, and failed in two ways that mattered:

- **Realized ≠ assumed inflation.** The reset substitutes the flat 5% assumption for whatever
  inflation actually happened. At 10% realized inflation the real income eroded from
  R600,000 to **R453,869** over seven years, with no Defend ever taken.
- **A gap between resets.** The reset discards all CPI accrued since the previous one. Hold
  for three years, then a single Extend: real income drops R600,000 → **R518,303**, a 14%
  real cut delivered by the decision labelled *"raise income"*.

The root was that `current_income` was **t0-real before the first decision fired and
reset-date-nominal after it**, while being consumed by two call sites wanting different things
(`extend_ilb_ladder()` wants t0-real; `compute_funded_ratio()` wants current-nominal).

**Both halves were removed together**, since neither makes sense without the other. See "The
income mechanic" below.

---

## The real-vs-nominal boundary — read this first

This is the concept the whole codebase is organised around, and the thing that went wrong five
times.

**The bond ladder is the only thing in real terms. Everything else is nominal.**

- `Functions/Bond Selection ILB.R` works *entirely* in real rands as at `start_date`. Its
  liability is a **level real income**, its prices are `market_price / index_ratio`, its
  cash-flow schedule is real, and its cash account earns a **Fisher-derived real rate**.
- `EPort`, the cash account, ARVA, the funded ratio, and every rand actually paid to the
  retiree are **nominal** throughout.
- The bridge: **a real rand at t0 IS a nominal rand at t0**, so `total_cost`, `cash_at_start`
  and `total_outlay` are used against `pot` with no conversion. Values at *future* dates must
  be translated.

### The conversion factor, and why the obvious one is wrong

`Bond Selection ILB.R` measures money in **ONE UNIT = R100 of START-DATE-INDEXED par**. Under
that convention a bond's quoted real flows (R*c*/2, R100) are **already denominated in t0
rands**, because the par they are struck off has already been grossed up by the t0 index ratio:

```
base par          = units * 100 / index_ratio(t0)
nominal flow at t = base par / 100 * quoted_flow * index_ratio(t)
                  = units * quoted_flow * index_ratio(t) / index_ratio(t0)
                  = units * quoted_flow * CPI(t) / CPI(t0)
```

So the conversion factor for the selector's own money is **`CPI(t)/CPI(t0)` — bond-independent
— and NOT the bond's index ratio `CPI(t)/base_cpi`.** Using the index ratio over-states every
converted amount by that bond's t0 ratio. That is exactly what three separate call sites did.

The index ratio is still the right number for **prices quoted per R100 of BASE par**
(`reprice_universe()`'s `market_price`) and, via the frozen `index_ratio_t0`, for converting
units to original par. **It is never the right number for converting the selector's rand
amounts.** Keep the two jobs apart.

There is now exactly **one** sanctioned crossing: **`real_to_nominal_factor()`** /
**`nominal_factor_matrix()`** in `Functions/ILB_Indexation.R`. Anything that crosses the
boundary any other way is a bug.

### The month convention, stated once

Two indexings coexist, and the relationship is fixed:

| | | |
|---|---|---|
| **elapsed month `k`** | 0-based | the **instant** `t0 %m+% months(k)`. `k = 0` IS t0. This is `ILB_Repricing.R`'s `month`. |
| **simulation month `m`** | 1-based | the **span** `[t0 + (m-1)mo, t0 + m mo)`. This is `Dynamic_Ladder.R`'s loop counter and the selector's `schedule$month`. |

**`k = m - 1`.** A month's valuation instant is its **start**, so everything happening "in
month m" — the annuity-due withdrawal, the annual review, an extension purchase, a coupon
credited — converts at the factor for elapsed month `m-1`. This is why the engine calls
`reprice_ladder(month = k)` and not `month = m`; the old code passed `m`, pricing a month late.

Cash flows are therefore indexed at the *start* of the month they land in, up to a month early.
That is deliberate, and if anything closer to the market than the alternative: SA ILBs index
off a **4-month-lagged** CPI, so the true reference level is earlier still. One convention
applied everywhere beats sub-monthly precision the model does not otherwise have.

One naming trap survives: `real_curve_yield()` (in `ILB_Repricing.R`) is a **generic** clamped
linear interpolator. `ARVA.R` and `compute_funded_ratio()` both call it with *nominal* curves.
The name is a historical accident; the behaviour is correct.

---

## The invariant layer (`Functions/Invariants.R`)

Three layers, cheapest and loudest first.

### 1. Boundary checks — before the run, `stop()` on failure

Called from `Thesis.R` as `run_boundary_checks()`, immediately after the initial
`optimize_ilb_ladder()`. A corrupted run never starts.

- **`assert_cpi_anchor()`** — the bootstrapped CPI path is anchored to the last actual CPI
  print (`cpi_values[length(cpi_values)]`, Jun 2026 = **107.5**); the workbook's index ratios
  are struck off `ILB_REFERENCE_CPI` (**107.5**). `real_to_nominal_factor()` divides one by the
  other, so if a data refresh moves one without the other, *every* conversion is silently
  mis-scaled. Stops rather than warns.
- **`boundary_check_reprice_t0()`** — `reprice_ladder(month = 0)` must equal the selector's
  `total_cost`. Marking the ladder on the day it was bought must return what was paid for it.
  This one line catches any per-unit vs per-base-par slip.
- **`boundary_check_deposits_vs_schedule()`** — with inflation switched off, nominal *is* real,
  so `nominal_deposits_for_holdings()` must reproduce the selector's own `schedule$bond_inflow`
  exactly. The two are built by completely independent code paths from the same contractual
  cash flows.

### 2. The golden path — end to end, `stop()` on failure

**`boundary_check_golden_path()`** runs the *whole engine* on one deterministic path:
realized inflation exactly `inflation_rate`, decisions disabled, flat equity. It then deflates
the engine's nominal cash ledger back to t0 rands and requires it to reproduce the selector's
own real schedule **month for month for the full initial ladder**.

Every conversion, the withdrawal indexation, the deposit schedule and the cash mechanics all
have to be simultaneously right for this to pass. Currently it matches to **R0.000000** across
all 96 months. It is the single check that would have caught all five defects on its own.

It is also what found the interest-ordering mismatch and a one-month deflation offset (0.407%
at 5% — small enough to look like rounding, large enough to be a real error).

### 3. The per-(path, month) audit — collected, reported at the end

`audit_new()` → passed into `run_dynamic_ladder_simulation(audit = ...)` → `audit_report()`.
Nothing stops mid-run: a stray path aborting a 1,000-path job costs more than it saves, and a
breach on one path is usually a breach on all of them. Each check records its count, worst
absolute and relative breach, and the exact `(sim, month)`, so a failure is reproducible on a
single path.

| Check | Identity |
|---|---|
| `cash identity` | `opening × (1+i) + deposits − withdrawals == closing` |
| `deposit conversion` | nominal deposit `==` contractual real flow `× CPI(t)/CPI(t0)`, over the whole grid |
| `extension conserves wealth` | total assets immediately before an extension `==` immediately after |
| `funded ratio matches income paid` | the liability the ratio prices `==` 12 × the payment actually made that month |
| `real income never rises` | the real level can only be cut, by Defend |
| `terminal reconciliation` | `EPort + cash` at the end `==` pot − bond cost + returns + coupons − spending, per path, over all 360 months |
| `cash/equity balance is finite` | no `NaN`/`Inf` leaks |

Tolerance: `1e-6` absolute + `1e-8` relative (10 cents on a R10m portfolio).

`audit_new(enabled = FALSE)` turns every call into an immediate return. The
`extension conserves wealth` check costs a second repricing, so it only runs when the audit is
on.

### Two things the audit deliberately does NOT call failures

Reported as counters in `Thesis.R` instead, because they are accepted model behaviours rather
than broken arithmetic:

- **The cash account may go negative.** Nothing tops it up from equity mid-ladder, and the
  ladder is solved to be exactly solvent under the *assumed* inflation rate.
- **Unaffordable extensions are skipped.** An Extend & Harvest whose purchase exceeds available
  equity is now logged and skipped rather than driving `EPort` negative. Nothing enforced this
  before.

### Proving the layer works

`scratchpad/regression_proof.R` (not in the repo — rebuild if needed) monkey-patches each
original defect back in and confirms the corresponding check fires:

```
[1] deposits converted with index ratio          CAUGHT  ratio 2.72
[2] ladder marked at real_price x index_ratio    CAUGHT  ratio 1.80
[3] extension cost spent as nominal              CAUGHT  'extension conserves wealth' 21/21, worst R498,988 (2.8%)
[4] real income escalated instead of held flat   CAUGHT  'real income never rises' 276/1080
[5] funded ratio priced off the t0-real income   CAUGHT  breach R360,000 (37.5% of the liability)
```

**If you change anything in the real/nominal plumbing, re-run this.**

---

## Repository layout

```
Thesis.R                          # MAIN entry point. Runs headless under Rscript.
Functions/
  moving_block_bootstrap.R        # sourced 1st - equity/bond/CPI bootstrap
  Bond Selection ILB.R            # sourced 2nd - THE ILB selector (~1130 lines)
  ARVA.R                          # sourced 3rd - mortality + nominal annuity factor
  Decision_Matrix.R               # sourced 4th - annual extend/hold/repair/defend rule
  Dynamic_Ladder.R                # sourced 5th - THE main simulation engine
  yield_curve_simulation.R        # sourced 6th - PCA/AR(1) nominal curve
  Real_Yield_Curve.R              # sourced 7th - nominal curve - stochastic breakeven
  ILB_Indexation.R                # sourced 8th - Reference CPI + THE real->nominal bridge
  ILB_Repricing.R                 # sourced 9th - reprice/extend the ladder at future dates
  Invariants.R                    # sourced 10th - identities, boundary checks, audit
  Sensitivity_Analysis.R          # SECOND entry point - BROKEN, not migrated
  bootstrap_returns.R             # LEGACY, unused
Data/                             # gitignored - present locally only
sensitivity_plots/                # five PNGs - STALE, from the old nominal Redington model
Rplots.pdf                        # tracked build artifact; probably should be gitignored
```

Source order matters. `Bond Selection ILB.R` and `ARVA.R` run code at source time and create
globals the rest depends on: **`ilb_fixed`**, **`ILB_MODEL_START_DATE`**,
**`ILB_REFERENCE_CPI`**, and **`data`** (the mortality table).

Directory casing is now consistent — `Functions/` and `Data/`, cased exactly, in both `source()`
and `here()`, and `ILB_FILE` matches the on-disk `Data/ILB Bond data.xlsx`. The project will run
on a case-sensitive filesystem.

### Data files (`Data/`, gitignored)

| File | Used? | Purpose |
|---|---|---|
| `ILB Bond data.xlsx`, sheet `Bond Data` | **yes** — the ILB universe | 10 IGOV bonds: code, real coupon, redemption date, nominal all-in price, base CPI, index ratio |
| `Bond & Equity Data.xlsx`, sheet `LT_DATA` | **yes** | SA equity & bond total-return index levels → monthly returns for the bootstrap |
| `CPI_Index_Long_Format.xlsx` | **yes** | Rows 168:558 (Dec 1993 →) → monthly CPI growth ratios, and the 107.5 anchor for Reference CPI |
| `SA Nominal Yield Curves - 1989-2026.xlsx` | **yes** | Historical nominal curves → PCA/AR(1) simulation |
| `SAIML98_SAIFL98_Mortality_Table.xlsx` | **yes** | SA insured lives mortality. Only the **male** `qx` column is read |
| `Bond Data.xls` | **no longer** | Was the nominal MTM universe. Read by nothing |
| `SA_ILB_Ladder_Candidates.xlsx` | no | Superseded by `ILB Bond data.xlsx` |

---

## The ILB universe (10 bonds, priced 1 Aug 2026)

| Code | Real coupon | Redeems | Index ratio | Real price |
|---|---|---|---|---|
| R210 | 2.600% | 2028-03-31 | 2.743 | 98.41 |
| I2029 | 1.875% | 2029-03-31 | 1.597 | 95.27 |
| I2031 | 4.250% | 2031-01-31 | 1.165 | 100.59 |
| I2033 | 1.875% | 2033-02-28 | 1.702 | 86.60 |
| R202 | 3.450% | 2033-12-07 | 3.188 | 94.60 |
| I2038 | 2.250% | 2038-01-31 | 1.997 | 80.76 |
| I2043 | 5.125% | 2043-01-31 | 1.138 | 108.99 |
| I2046 | 2.500% | 2046-03-31 | 1.882 | 76.35 |
| I2050 | 2.500% | 2050-12-31 | 1.995 | 72.49 |
| I2058 | 5.125% | 2058-01-31 | 1.138 | 116.70 |

**The shape of this grid drives most of the model's behaviour:**

- **Nothing redeems before Mar 2028** → months 1–19 of any ladder can only be funded from
  opening cash. Structural, not a solver failure (see `feasible` below).
- **Nothing between Dec 2033 and Jan 2038** — a 4-year hole, funded by buying more of the bond
  before it.
- **Nothing between Jan 2038 and Jan 2043** — a 5-year hole, which is what makes
  `ladder_locked` fire at the 15-year cap.

Index ratios run 1.14 – 3.19. Dividing them out of `market_price` moves the file into real
space; the *same* spread of ratios is what made defect 1 so large.

---

## Analysis workflow (`Thesis.R`, top to bottom)

1. **Source ten function files** (order matters).
2. **Load** the equity/bond, CPI and yield-curve workbooks.
3. **Clean & build `returns_matrix`** — drop the first 407 rows (hardcoded), form **gross**
   monthly return relatives, merge CPI growth ratios on `YearMon`.
4. **Bootstrap.** `set.seed(392)`; `moving_block_bootstrap()` → `[360 × 3 × 1000]`. Equity and
   CPI are used; the bond column is still computed and never used.
5. **Parameters.** `pot = R10m`, `wd = 6%`, `ladder_length = 8`, `max_ladder_years = 15`,
   `extend_by = 1`, `defend_cut = 5%`, **`inflation_rate = 5.8%`** (expectations only - set to
   the bootstrap's own mean, see defect 6), **`cash_buffer_months = 2`**,
   `bequeathment_pct = 10%` **real**, `cash_annual_rate = 5%`, `horizon = 360`, ages 60 → 90.
6. **Simulate the nominal yield curve** — same horizon/n_sims as the bootstrap, so indices line
   up.
7. **Build the real curve and Reference CPI**, then **`assert_cpi_anchor()`** and
   **`nfac_all <- nominal_factor_matrix(...)`** — the shared real→nominal bridge used by the
   reporting layer.
8. **Build the initial ILB ladder**, `stop()` if `total_outlay > pot`, print the report.
9. **`run_boundary_checks()`** and **`boundary_check_golden_path()`** — both `stop()` on
   failure, both before any simulation runs.
10. **`audit <- audit_new()`**, then **run the simulation** — 1,000 paths × 360 months.
11. **`audit_report()` FIRST**, before any result is read. Then the negative-cash and
    unaffordable-extension counters, and the realized real-income summary.
12. **Report INCOME FIRST**, then wealth. A decumulation strategy is judged first on the income
    it pays and how reliably, not on what is left over.

    This used to be forced on us by a contradiction, which has since been fixed at source. ARVA
    was a pure **spend-down rule** — it amortised the portfolio to nothing by `max_age` — so any
    change that made the strategy more efficient showed up as a *larger income and a smaller
    bequest*, and a genuine improvement scored as a deterioration against the bequeathment
    target. ARVA now amortises down to the bequest instead of to zero, so the two objectives
    point the same way and the ruin figure is no longer fighting the withdrawal rule. Income
    still leads, because it is still the thing the retiree actually lives on.

    The income block reports total real income over the horizon, annual real income pooled over
    every path-year, the **income floor** (each path's single worst year), the share of path-years
    paid below target, the ladder/ARVA split, how much ARVA income came from cash already held
    rather than equity sales, and how often the ARVA income floor bound — i.e. how many years the
    bequest was deliberately sacrificed to keep the retiree eating.
13. **Then report wealth & plot** — terminal-value summary in **both nominal and today's rands**, ruin
    against the **per-path real** bequeathment target, nominal and real fan charts, a realized
    real-income chart, single-path diagnostics for `sim_index <- 1` (`View()` is now guarded by
    `interactive()`), and three yield-curve diagnostic plots.

### Base-case numbers (8y ladder, R10m pot, 6% wd, 1 Aug 2026)

The **bond** purchase is unchanged by the rewrite — the selector was never the thing that was
wrong. Only the cash leg moved, and for two stated reasons: `inflation_rate` at 5.8% rather than
5% lowers the Fisher real rate on cash (R789,128 → R813,942), and `cash_buffer_months = 2` adds
a stated margin on top.

```
ILB PURCHASE COST : R 3,404,649
+ opening cash    : R   913,942   (18.3 months of income; cash is 21.2% of outlay)
    of which solved : R 813,942   (the binding minimum C*)
    of which buffer : R 100,000   (2 months, stated margin over C*)
= TOTAL t0 OUTLAY : R 4,318,591
Equity            : R 5,681,409   (56.81% of pot)
Bonds used        : R210, I2029, I2031, I2033, R202  (5 blocks)
Cash-only months  : 1-19
```

What that bought, measured over 1,000 paths:

```
paths whose cash account ever went negative :   12 / 1000   (was 308 / 1000)
worst path                                  :    3 months under, deepest R-144,588

extensions executed                         : 4,359 across 1,000 paths (median 6)
harvested for bonds, median per path        : R3,804,898
harvested for the cash bridge, median       : R0  -  995 of 1,000 paths needed NONE
```

That last line is the clearest statement of defect 7: once the top-up is sized as a projected
shortfall rather than a re-charged requirement, **995 paths out of 1,000 turn out never to have
needed one at all.** Every rand of it was being harvested from equity and parked in cash.

### Headline results (1,000 paths, all identities passing)

```
Terminal wealth, NOMINAL      median R 5,786,086   mean R 7,764,924   sd R 6,958,773
Terminal wealth, TODAY'S RANDS
     5%       25%       50%       75%       95%
  440,541   713,039 1,079,748 1,756,160 4,012,624      mean R 1,495,860

Terminal split (nominal medians)   equity R 2,063,955 | residual cash R 3,615,996
Paths ending with cash < 0                             0 / 1000

Bequeathment target                R 1,000,000 in today's rands
  per-path nominal target          median R 5,345,487  (R 3.86m - R 8.36m)

PROBABILITY OF RUIN                45.30%      [old equity-only measure: 80.60%]
Probability of complete depletion   0.00%

Real income   start R600,000 | final median R570,000 (one Defend), 5th pct R514,425
              445 / 1000 paths never had to cut income at all
Ladder        739 / 1000 locked; 470 finish at the 15-year cap
```

Runtime is load-sensitive: this run took 28.0 minutes, an identical earlier one 12.7.

**Two things to read carefully before quoting any of this.**

1. **Residual cash was the majority of terminal wealth, and the reason was measured, not
   guessed.** It is *not* that the cash account sits inert after the ladder ends — it compounds.
   ARVA moves a whole year of income into cash in one lump and pays it out at 1/12 a month, so
   half a year of income sits idle earning `cash_annual_rate`, and that interest used to accrete
   rather than be spent. Traced on one path: ARVA deposits and payments were **exactly equal**
   at R214,465,014 each, interest came to R8,059,865, and terminal cash was R8,248,175 — the
   closing balance *was* the accumulated interest. That float is no longer left to ride: ARVA
   now strikes its entitlement on equity **plus** the cash account and spends the cash first, so
   every rand of interest raises the next year's withdrawal and is consumed through it. (An
   earlier revision instead paid the interest out directly each month; sizing ARVA on the full
   portfolio subsumes that, so it was reverted — see the ARVA block in `Dynamic_Ladder.R`.) What
   remains is the genuine ladder-phase residual, decomposed below.
2. **The 45.30% vs 80.60% gap is measurement, not improvement.** It is the same portfolio scored
   two ways. Do not present the fall as an effect of any change to the strategy.

What *did* change is everything downstream. **Results from before this rewrite are not
comparable** — the ladder was marked at 1.80× cost, deposits ran ~1.8× hot, extensions were
under-charged, and ruin was scored against a target worth 23% of its stated value.

### Where the ladder-phase cash residual comes from

Measured by switching off one cause at a time (25 paths, medians, t0 real rands):

| Config | Median cash at that path's own ladder end |
|---|---|
| A base: buffer = 2, defend = 5%, stochastic CPI | R302,801 |
| B buffer = 0, defend = 5%, stochastic CPI | R209,114 |
| C buffer = 0, defend = 0%, stochastic CPI | R133,480 |
| D buffer = 0, defend = 0%, CPI pinned at 5.8% | R175,392 |

```
opening buffer              A - B = R 93,687   (the R100,000, eroded a little)
Defend over-funding         B - C = R 75,634
real-rate drift/dispersion  C - D = R-41,912   (NEGATIVE on the median)
STRUCTURAL, all else off        D = R175,392   (the largest single term)
```

Three things worth knowing:

- **A favourable real cash rate is not a cause.** With `inflation_rate` at 5.8% the assumed
  Fisher real rate is −0.756% and the realised mean is −0.794%, so drift *subtracts* from the
  median. It is what generates the **spread**, though — it is why some paths end at −R22,396.
- **The largest term is structural**, present with no buffer, no cuts, and inflation pinned
  exactly at the assumption. It is the extension design: the engine funds a shortfall but never
  sweeps a surplus, so an account that gets ahead stays ahead. In config D the residual still
  ranges from −R50,719 upward across paths, driven purely by *which years a path extends and
  when its anchor changes* — not by inflation at all.
- **It cuts both ways.** 12/1000 paths spend time below zero, and the minimum ladder-end balance
  in the base case is −R22,396.

### Ruin definition

**Ruin is measured on TOTAL WEALTH: terminal equity PLUS the residual cash balance.**

It used to be measured on `EPort` alone, which understated terminal wealth *systematically*
rather than randomly. In the ARVA phase the annual draw moves money out of equity and into the
cash account, and the annuity factor approaches 1 as age approaches `max_age`, so the last
withdrawals move nearly everything across. On the base case the same portfolio scored **80.60%
ruin on equity alone and 40.00% on equity plus cash** — a 40-point gap that was pure
measurement.

It also closes a loophole. The cash account is allowed to go negative (nothing harvests equity
to cover a shortfall — a deliberate choice, see limitations). On the old metric an overdraft was
**free**, because ruin never looked at the cash balance. It is now netted off terminal wealth,
so a path that finances its income by running the account down is scored on it.

The bond ladder is not a third term: by month 360 every path has long since liquidated it into
equity. The equity-only figure is still printed alongside, so the size of the measurement effect
stays visible.

`ruin_prob <- mean(Wealth < bequeathment_target_path)` where `Wealth = EPort + Cash` and

```r
bequeathment_target_real <- bequeathment_pct * pot                     # R1,000,000, today's rands
bequeathment_target_path <- bequeathment_target_real * nfac_all[horizon + 1, ]
```

i.e. **R1m of today's purchasing power**, inflated to month-360 rands by **each path's own
realized CPI**. A high-inflation path is judged against a target that kept up with its own
inflation, exactly as the withdrawals are.

The headline figure is the **equity-plus-residual-cash** one. The old equity-only number is
still printed beside it so the size of the measurement effect stays visible, and terminal values
are reported in today's rands as well as nominal.

---

## The income mechanic

Stated once, and it does not move:

```
real_income[s]   a T0-REAL annual level. Set at t0 = wd * pot. Changed ONLY by a Defend cut.
payment(m, s)  = real_income[s] / 12 * factor(m, s)
```

`income_reset_month` **is gone**. Indexation always runs from t0, so the real income is flat
under *any* realized inflation path rather than only under one.

**Extend & Harvest does not touch income.** It extends the ladder and harvests equity to pay
for it; purchasing power is already preserved by indexing from t0. `decision_matrix()` returns
`withdrawal = current_income` unchanged on that branch, and `inflation_raise = FALSE`.

**A Defend cut is permanent** — `real_income *= (1 - defend_cut)`, never restored. Ten Defend
years compound to 0.95¹⁰ = 60% of the original standard of living. Deliberate: it is a
ratchet-down guardrail, and keeping it makes results comparable with the existing runs.

`Income_real_history` records the actual monthly payment deflated back to t0 rands. On a path
that never defends it is a flat line at `wd * pot` — the visual statement of "level real
income" the whole strategy is built on.

---

## Function reference

### `Functions/Bond Selection ILB.R` — the ILB selector

**Almost unchanged by the rewrite**, because the selector was never the source of the problem:
it is internally consistent in t0-real rands, and only its *consumers* were mis-reading its
output.

The changes:

- **`cash_buffer_months`** added (default 0, so nothing existing moves). The solve is *exactly*
  binding by construction — `C*` is the smallest opening balance that keeps every closing
  balance non-negative, so `min_balance` is 0 at the binding month and 9 of 96 months sit below
  one month of income. That is fine in the selector's own world, where cash earns the Fisher
  rate by assumption, and fatal once the simulation credits a **fixed nominal** rate against a
  **stochastic** realised CPI. This adds a stated margin of *n* months of real income on top of
  `C*`; the bond purchase is untouched. Returns `solved_cash_at_start` and `cash_buffer`
  alongside `cash_at_start` so the two halves stay legible. Base case at 2 months:
  R813,942 solved + R100,000 buffer, ~1% of the pot diverted from equity, and negative-cash
  paths fall from 308/1000 to near zero.
- `ILB_FILE` casing corrected to the on-disk `Data/ILB Bond data.xlsx`.
- **`ilb_fixed$index_ratio_t0`** added — the t0 index ratio frozen under its own name.
  `index_ratio` is an *as-at-a-date* quantity that `reprice_universe()` overwrites; `units` are
  struck off the **t0** ratio and never re-based, so anything converting units ↔ original par
  must read `index_ratio_t0`. Before this, `base_par` read the live column and silently
  reported par off the wrong date after a repricing.
- `base_par` and the allocation table use it.

Runs on source: reads the workbook into **`ilb_fixed`**, asserts **`ILB_MODEL_START_DATE`**
(1 Aug 2026) and **`ILB_REFERENCE_CPI`** (107.5), and guards prices/ratios/coupons.

**The algorithm — backward dedication, one bond per maturity gap.** Not an LP.

> Cut the ladder into **blocks at the redemption months** of the eligible ILBs. Block *g* opens
> the month its bond redeems and closes the month before the next bond redeems. Walk
> **backwards**. For each block: work out what it still needs (its real withdrawals, less what
> the already-bought later-maturing bonds pay into it as coupons); if that is ≥ 0 it is covered
> by carry, buy nothing; otherwise buy exactly enough of the bond redeeming in its first month
> to close the gap to zero.

Walking backwards makes each block's answer final. Long only; surplus carry flows back to
earlier blocks and is never sold short.

**Why blocks are cut at redemption months** (measured, not assumed): calendar-year buckets with
no carry-back cost R4.56m of t0 outlay on the base case; carry-back onto annual buckets,
R4.39m; cutting on redemption months, **R4.21m**.

**Where `inflation_rate` is and is not used**: **not** to escalate the liability — the real
liability is level by construction. **Only** to derive the real rate credited on idle cash, via
Fisher: `real = (1 + nominal)/(1 + inflation) − 1`. At the base case that is exactly **0%
real**.

**UNITS.** One unit = **R100 of start-date-indexed par**. In that unit real flows are the plain
quoted numbers and the real price is `market_price / index_ratio` — *and*, critically, the
money is denominated in **t0 rands**. This is the fact defect 1 turned on. Base par is
`units × 100 / index_ratio_t0`, reported as `bond_units_base_par`.

Key functions: `fisher_real_rate()`, `months_elapsed_ilb()` / `month_index_from_date_ilb()` /
`year_period_from_date()` (floored at month 2), `single_ilb_real_cashflows()` (coupon cycle
anchored **backwards from the bond's own redemption date** in exact 6-month steps computed
directly, not chained), `ilb_liability()` (level real monthly **annuity-due**, first instalment
at t = 0), `ilb_ladder_real_schedule()`, `optimize_ilb_ladder()`, `print_ilb_ladder()`,
`test_ilb_ladder()` (run by hand; nothing runs at source time except the data load).

`ilb_ladder_real_schedule()`'s within-month order — **this is now load-bearing**, because the
engine matches it exactly:

```
opening(m) = closing(m-1) * (1 + r)      [no interest in month 1]
closing(m) = opening(m) + bond inflows - withdrawal
```

Minimum opening cash is closed-form: `C* = max(0, max_m[−D_m / (1+r)^(m−1)])`. Since no bond
pays in month 1 while a withdrawal is due, `C* ≥ W` always.

#### Two behaviours that look like bugs and are not

**1. `feasible` is ALWAYS `FALSE` on this universe, and that is correct.** It means only "is
the set of cash-only months empty". Nothing redeems before Mar 2028, so months 1–19 are always
cash-only. That is a fact about the market. It emits a `warning()` on every call — which is why
a full run produces "50 or more warnings". **Do not hang a "freeze this path" branch off
`feasible`.**

**2. `tie_break` is nearly inert.** Ties are *same-month* redemption ties, which this universe
never has. `"cheapest"` is the only rule that can move the answer, and it makes selection
price-dependent. Default `"earliest"`.

### `Functions/ILB_Indexation.R` — Reference CPI and the ONE bridge

Rewritten. Turns the shared CPI bootstrap into what the ladder needs, and owns the single
real→nominal crossing.

- **`reference_cpi_path(inflation_monthly_ratios, cpi_anchor)`** — `[horizon × n_sims]`
  absolute CPI levels, `cpi_anchor × cumprod`. Row `k` is the level `k` months after t0.
- **`assert_cpi_anchor(cpi_anchor, cpi_t0)`** — stops if the bootstrap anchor and
  `ILB_REFERENCE_CPI` are not the same price level.
- **`nominal_factor_matrix(reference_cpi, cpi_t0)`** — `[(horizon+1) × n_sims]`, row `k+1` is
  the factor for elapsed month `k`, row 1 a literal `1`. **This replaced `Dynamic_Ladder.R`'s
  `cum_inflation`**, which was a second copy of the same numbers reached by a different route —
  two objects that had to agree, now one that cannot disagree.
- **`real_to_nominal_factor(elapsed_months, sim, reference_cpi, cpi_t0)`** — the accessor.
- **`index_ratio_matrix()` / `index_ratio_for_bond()`** — the *market's* ratio. For prices per
  R100 base par and for par conversion. **Not** for converting the selector's money.
- **`nominal_bond_deposits()` / `nominal_deposits_for_holdings()`** — contractual real flows →
  the nominal rands that actually land in the cash account. **No yield curve involved.**
  `base_cpi` is no longer an argument; that was the bug. Note the two-date signature:
  `purchase_date` controls *which flows exist*, `global_valuation_date` (always
  `ILB_MODEL_START_DATE`) controls *how those dates map to month indices*.
- **`real_bond_deposits()` / `real_deposits_for_holdings()`** — NEW. The same schedule with the
  conversion left off, in t0 rands. Path-independent by construction. Exists so the audit can
  hold the nominal schedule to account and so the reporting layer can deflate without
  re-deriving anything.

### `Functions/ILB_Repricing.R`

Month convention: `month` is the **elapsed-month index `k`**; `k = 0` is t0 and uses
`ilb_fixed`'s static columns with a conversion factor of exactly 1.

- **`real_curve_yield()`** — clamped linear interpolation. Generic despite the name.
- **`reprice_bond_real_price()`** — PV of remaining real flows at each flow's own remaining
  term. Already dirty/all-in. Returns 0 for a redeemed bond. **The result reads two ways and
  they are the same number**: the real price per R100 base par, *and* the t0-real price of one
  unit. Which one you want decides which factor converts it — that ambiguity is the whole
  defect.
- **`reprice_universe()`** — an `ilb_fixed`-shaped frame for that date. `real_price` is the
  t0-real price per unit (what the selector spends, in the same money as
  `annual_real_withdrawal`); `index_ratio` is the market's ratio at that date; `index_ratio_t0`
  is carried through untouched; `market_price` is nominal per R100 base par; and a new
  **`nominal_price_per_unit`** column is what a unit actually costs the nominal engine today.
- **`reprice_ladder()`** — marks held bonds. Now returns `real_price`, **`real_value`**
  (t0 rands), **`factor`**, `index_ratio` (reporting only) and `rand_value = real_value ×
  factor`. At `month = 0` factor is 1 and `sum(rand_value) == total_cost` by construction.
- **`extend_ilb_ladder()`** — the annual additive purchase. Returns `incremental_units`,
  `bond_cost_real` / `bond_cost` / `factor`, and then — deliberately — a *requirement* rather
  than a payment: `cash_required_real`, `solve_start_date` and `solve_start_month`.
  **There is no `cash_top_up` or `harvest_needed` any more.** Those were spendable figures that
  were wrong (defect 7), and the shortfall genuinely cannot be computed here: it depends on what
  the pooled cash account will hold on `solve_start_date`, which is engine state. Removing them
  rather than deprecating them is the point — leaving a wrong number in a return list is what
  lets it be spent. `fresh` is diagnostics only: **do not read `total_cost`/`bond_units` off
  it**, they double-count the anchor's already-held portion, and its money is real.
- **`build_repricing_table()`** — still explicitly **TODO / PLACEHOLDER**. Feeds nothing.

#### How `extend_ilb_ladder()` works, and why it tops up rather than rebuilds

The held bond with the **latest** redemption date (the "anchor") pays its entire remaining
value as **one lump sum** on its redemption date. Buying **more of that same bond today, at
today's price** lets that lump sum stretch further — strictly more capital-efficient than
starting a fresh sub-ladder at the boundary date, which would treat the anchor as
already-spent (wrongly — it is still buyable today) and demand an unnecessary cash top-up.

Two subtleties:

- **`solve_start_date <- anchor_date - 1`.** `optimize_ilb_ladder()`'s eligibility test is
  `redemption_date > start_date` (strict), so using the anchor's own redemption date would make
  it fail its own test. Backing off one day keeps it eligible without pulling in its earlier
  coupons (~6 months back) and — since no ILB here redeems on the 1st — without shifting any
  bond's month bucket.
- **Incremental units are computed only over `fresh$eligible_bonds`.** Every other held bond
  redeems strictly before `solve_start_date`, so it is out of scope, not "reduced to zero".
  Comparing the full held vector would misread that as "sell this back".

It re-solves the whole open block at **this year's** withdrawal level — deliberate, so an
income change is picked up for the anchor's already-open block too. It works across a sequence
of yearly extensions with no lookahead: the anchor is re-derived from current holdings every
call, never tracked as state.

### `Functions/Real_Yield_Curve.R`

`real curve(t, path) = nominal curve(t, path) − breakeven(t, path)`.

- **`simulate_breakeven_inflation(horizon_months, n_sims, theta = 4.5, kappa = 0.15,
  sigma = 1.0, x0 = theta, seed = 4501)`** — monthly Euler discretisation of an OU process, in
  **percentage points** (matching the nominal curve's units).
- **`build_real_yield_curve(yc, breakeven_sim)`** — returns `get_real_curve(month, sim,
  include_residual)`, mirroring `yc$get_curve()` exactly. Validates shape against `yc`.

**`kappa` and `sigma` are explicitly flagged in-file as uncalibrated placeholders**, exposed as
named arguments so they can be swept. `theta = 4.5` is from the brief. Also simplified:
breakeven is a **single scalar per (month, sim) subtracted flat across every tenor**.

### `Functions/ARVA.R`

Reads the mortality table into a global `data` (age + male `qx`) at source time.

- **`survival_probability(age_now)`** — `1 − qx`.
- **`arva_annuity_factor(age, nominal_curve_vec, nominal_tenors_months, max_age = 90)`** —
  `Σ_t tPx / (1+y_t)^t`, discounted at the **NOMINAL** curve. `EPort` is nominal, so `EPort/af`
  only means something if `af` is built off a nominal curve. Supplied per path, per year.
- **`run_arva_strategy(...)`** — standalone single-path reference implementation. **Not used**
  by the Monte Carlo, which implements the same mechanic inline.

### `Functions/Decision_Matrix.R`

**`decision_matrix(funded_ratio, equity_return, inflation_rate, current_income, defend_cut)`** —
a 2×2 on *funded ratio ≥ 1* × *trailing 12-month equity return > 0*.

| | Market up | Market down |
|---|---|---|
| **Surplus** | **Extend & Harvest** — extend the ladder, harvest equity. **Income unchanged.** | **Wait & Hold** — change nothing |
| **Deficit** | **Repair** — change nothing, let equity recover | **Defend** — cut real income by `defend_cut` |

`current_income` is a **t0-real** level. `inflation_rate` is now used only to classify state, not
to escalate anything.

### `Functions/yield_curve_simulation.R`

**`simulate_yield_curves(data, start_date = "1994-01-01", n_pca_components = 3, block_size = 6,
horizon_months = 360, n_sims = 200, seed = 2026)`** — PCA on historical curves, AR(1)-simulated
PC scores, block-bootstrapped residuals via the shared `moving_block_bootstrap()`. Returns
`get_curve(month, sim, include_residual = TRUE)` (named vector, tenor labels like `"120M"`),
plus `mean_curve`, `loadings_k`, `ar1_fits`, `pc_sim`, `resid_sim`, `tenors_months`, `k`,
`var_explained`.

### `Functions/Dynamic_Ladder.R` — the simulation engine

- **`compute_funded_ratio(...)`** — total assets ÷ PV of the retiree's **whole** liability:
  every future expected expense to `max_age` **plus the bequest**. **Every input is nominal**, as
  at this path's current month; `expected_annual_expense` must be `real_income[s] * factor(m, s)`
  and `bequest_nominal_now` must be `bequest_target_real * factor(m, s)`.

  **The bequest leg was added deliberately.** The retiree stated two goals — an income *and*
  something left at the end — so "funded" has to mean able to meet both. Pricing only the income
  made the ratio flatter than the retiree's own objective, and is a large part of why Extend &
  Harvest fired on most reviews even on paths that ended far below target. The bequest is
  projected forward at the flat `inflation_rate` and discounted on the nominal curve — the same
  convention as the income leg, so one calculation never uses two conventions. Returns
  `pv_income` and `pv_bequest` separately so the split is inspectable.
- **`run_dynamic_ladder_simulation(..., cpi_t0, decisions_enabled = TRUE, audit = NULL)`** —
  one monthly loop over `horizon`, every path carrying its own state.

Per-path state: `phase`, `ladder_years_current`, **`real_income`** (t0-real), `bond_sale_amount`,
`ladder_end_month`, `arva_year_index`, `ladder_locked`, `bond_holdings` (sized to the **full**
universe), `cash_account`, `EPort`.

**Two deposit schedules** are carried: `real_deposits` (contractual, t0 rands,
path-independent for the initial ladder so computed once) and `deposits_by_month` (nominal, per
path). The nominal one is **not** derived from the real one — it is built independently from
the same cash flows, so the `deposit conversion` audit genuinely tests
`nominal_bond_deposits()` rather than restating its own arithmetic.

**Annual check** at months 13, 25, 37… (`m > 12 && (m−1) %% 12 == 0`), ladder-phase paths only:

1. **Exit check FIRST, unconditionally.** If `remaining_years <= 1e-9`, liquidate into equity,
   flip to ARVA, stamp `ladder_end_month`. Must precede any other use of `remaining_years` — in
   R, `1:0` is `c(1, 0)`, not empty. **Hitting `max_ladder_years` does not end the ladder** — it
   only caps further extension.
2. **Reprice per path, inside the per-path loop** at `month = k`. The real curve is stochastic
   *per path*; **hoisting this out would silently apply one path's curve to every path**. It is
   also why the run is slow.
3. Funded ratio (on `real_income × factor`) + trailing 12-month equity return →
   `decision_matrix()` → log.
4. **Wait & Hold / Repair** → nothing. **Defend** → cut `real_income`; no holdings change.
   **Extend & Harvest** → if `ladder_locked` or `capped`, fully inert. Otherwise: build the
   increment's deposit schedules, **size the cash top-up as a shortfall** (below), check
   affordability (`harvest_needed > EPort` → skipped and logged), then pull `harvest_needed`
   from equity, add `cash_top_up` to cash, **ADD** (never swap) the incremental units, and add
   the increment's deposits to **both** schedules. Income is not touched.

**Sizing the extension's cash top-up.** `extend_ilb_ladder()` hands back a *requirement* — the
opening balance its sub-ladder needs on `solve_start_date` — not a payment. The engine projects
its own pooled account forward to that month in closed form, through the deposit schedule it
already holds (including the coupons of the bonds being bought right now):

```
opening(m*) = cash * g^(n+1) + SUM_j net_(m+j) * g^(n-j),    g = 1 + i,  n = m* - m
net_mm      = deposits_by_month[mm] + incremental_deposits[mm]
              - real_income/12 * factor(mm)
cash_top_up = max(0, cash_required_real * factor(m*) - opening(m*))
```

and harvests only the gap. In most years the account is already carrying enough and the gap is
zero — which is the whole point, since an anchor that has not moved does not need its bridge
bought a second time. The withdrawal leg assumes today's real income holds, which is the right
forward assumption: a later Defend cut only reduces it.

This is also why `cash_buffer_months` is **not** passed down to the sub-solves. The initial
ladder is bought with a stated margin; each extension is then sized against the margin the
account is projected to still be carrying, rather than stacking a fresh one on top every year.

**Monthly cash mechanics**, every path, every month:

```
opening(m) = closing(m-1) * (1 + i)        [no interest in month 1]
closing(m) = opening(m) + deposits(m) - withdrawal(m)
```

- *Ladder*: `withdrawal = real_income[s]/12 * factor(m, s)`.
- *ARVA*: on each 12-month anniversary **of that path's own** ladder end, strike the entitlement
  on the **whole investment portfolio net of the bequest reserve**, floor it at the secured real
  income, then fund it from the cash already held before touching equity:

  ```
  portfolio  <- EPort[s] + cash_open[s]         # equity AND the bank account
  years_left <- max_age - age_now
  pv_bequest <- bequest_target_real * factor(m,s) * (1+inflation_rate)^years_left *
                (1 + y_T)^(-years_left)         # y_T off the NOMINAL curve
  w_target   <- max(0, (portfolio - pv_bequest) / af)
  w_floor    <- real_income[s] * factor(m, s)   # the income the ladder secured
  w          <- min(max(w_target, min(w_floor, max(0, portfolio))), max(0, portfolio))
  transfer   <- max(0, min(w - cash_open[s], EPort[s]))   # only the shortfall
  EPort[s]   <- EPort[s] - transfer
  current_monthly_withdrawal[s] <- w / 12
  ```

  **Why it amortises to the bequest, not to zero.** A plain ARVA spends the portfolio to nothing
  by `max_age`, which put it in direct conflict with the success criterion: any efficiency gain
  was converted into consumption and scored as a *worse* bequest. Striking the annuity on the
  portfolio net of the PV of the target — the classic "annuity with a reserve" — makes the ladder
  phase and the ARVA phase pursue one goal. `af` itself is untouched: still mortality-weighted,
  still on the nominal curve, still running to `max_age`. Only the amount being amortised changes.

  **The reserve is NOT survival-weighted, and `af` is.** That is a real inconsistency and it is
  the honest one: `af` prices a life annuity, but this model has no stochastic death — every path
  runs all 360 months — and a retiree wants the full bequest left whenever they die, not the
  bequest times the probability of reaching `max_age`.

  **Why there is an income floor.** As `years_left` shrinks the reserve approaches the full
  target, so a portfolio only just above it would otherwise pay almost nothing for the last few
  years — a retiree starving to protect their own estate. The floor is the real income the ladder
  was securing (cut by any Defend, indexed to today). It has a sharp consequence, measured and
  accepted: it converts a *gracefully declining* income into *full income until the money runs
  out, then zero*. At 15 paths the median path's worst year improved (R465,973 → R570,000) while
  the 5th percentile worst year fell to **R0** (from R191,950). The floor protects the middle and
  cliff-edges the tail. `arva_floor_years` counts, per path, the ARVA years where it bound — i.e.
  the years the bequest was deliberately sacrificed to keep eating.

  **Why the base is equity + cash.** The cash account is part of the retiree's wealth, not a
  conduit bolted to it. Pricing the annuity off equity alone both understates what they can
  afford and ignores a balance that is already sitting there — which is precisely how the
  ladder-phase residual used to survive untouched all the way to month 360.

  `cash_open[s]` is the right cash figure: the closing balance of month `m-1` with this month's
  interest already credited. `cash_account[s]` has not been updated for month `m` at that point
  in the loop.

  **Why it spends cash first.** `transfer` is only what the existing balance cannot cover, so
  equity is left invested wherever the bank account can carry the year alone. This is what
  progressively drains the residual instead of letting it ride to the end. It also self-heals an
  overdraft: a negative `cash_open` makes `transfer` larger by exactly the overdraft, repaying it
  out of equity.

  The clamps matter. `min(..., EPort[s])` stops the transfer manufacturing equity that is not
  there; `max(0, ...)` stops a negative equity balance being "sold". A shortfall neither sleeve
  can meet simply drives cash negative, which is the accepted behaviour everywhere else. The
  clamp is on the **withdrawal**, never on `EPort` — the old `max(EPort - w, 0)` wrote off a
  negative equity balance and broke the terminal reconciliation.

  **Interest is not paid out separately, and does not need to be.** It accrues in the account,
  and because the entitlement is struck on equity *plus* cash, every rand of interest raises next
  year's `w` and is spent through that. An earlier revision added the interest directly to each
  month's payment; sizing ARVA on the full portfolio subsumes it, so that was reverted.

  Measured effect of sizing on equity + cash (15 paths, vs the equity-only rule): terminal
  **residual cash collapses from R692,441 to R60,060** nominal, terminal **equity rises**
  R1,883,676 → R2,132,722, total real income is marginally **higher**, and paths ending in
  overdraft go from 2/15 to 0/15. Returned per path as `arva_equity_drawn`.

  Measured effect of adding the bequest reserve on top (15 paths, vs amortising to zero):

  | | ARVA → zero | ARVA → R1m bequest |
  |---|---|---|
  | Terminal equity (nominal median) | R2,132,722 | **R7,490,968** |
  | Terminal wealth, real (median) | — | **R1,333,258** (clears the R1m target) |
  | Ruin | 80.00% | **46.67%** |
  | Total real income (median) | R24,846,466 | **R25,360,058** |
  | ARVA years where the floor bound | — | 69 of 246 (28.0%) |

  **Both income and terminal wealth rose, and that is not a free lunch.** Withdrawing less early
  leaves more invested, and the bootstrap's equity returns comfortably exceed the nominal curve
  the annuity factor discounts at — so amortising at the nominal rate was front-loading
  consumption relative to what the portfolio could sustain. The reserve slows that down. The
  result depends on that spread holding; it would reverse under equity returns below the discount
  curve.

  **A rejected alternative, kept for the writeup.** Disbursing ARVA monthly straight out of equity
  (no float at all) was prototyped and measured: it pays 6.76% more real income than the
  float-payout rule, but raises measured ruin from 52% to 64%, because ARVA is a spend-down rule
  and converts efficiency gains into consumption rather than bequest. That trade is the clearest
  illustration of why income leads the reporting — see below.
- **No equity top-up if cash runs dry** — it simply goes negative, counted and reported.
- Equity compounds for everyone, both phases.

Returns `EPort_history`, `Cash_history`, **`Wealth_history`** (= equity + cash, the series ruin
is measured on), `Cash_deposit_history`, `Cash_withdrawal_history`, **`Cash_interest_history`**,
**`Income_real_history`**, `ARVA_withdrawal_history`, **`deposits_by_month`**,
**`real_deposits`**, **`nominal_factor`**, `ladder_end_month`, `ladder_years_final`,
`bond_sale_amount`, `ladder_locked`, **`real_income_final`**, **`cash_negative_months`**,
**`extension_bond_cost`**, **`extension_cash_topup`**, **`n_extensions`**,
**`arva_equity_drawn`**, **`arva_floor_years`**, **`n_unaffordable`**, `decision_log`, `EPort`,
**`Cash`**, **`Wealth`**.

`Wealth_history` excludes the bond ladder mid-run — the sleeve is not marked monthly, because
repricing is per-path and expensive. It is exact from each path's own liquidation onward, and
therefore exact at month 360, which is where every headline number is taken. The fan charts plot
it and say so on the axis.

`decision_log` is now collected as a list and bound **once** — the old `rbind`-per-row was
quadratic (30,000 reallocations on a full run).

Because every path extends on its own schedule there is **no single "end of ladder" month**;
plots show the median across paths, labelled as such.

#### `ladder_locked` — near-universal at the cap, not rare

`ladder_locked` becomes `TRUE` once a path's anchor has matured with no replacement bought.
Once set, no further purchase is ever attempted.

The in-file comment used to call this rare. **It is not**: with `max_ladder_years = 15` on this
universe it is close to structural. A path that extends steadily hits the cap around year 10;
its anchor by then is **I2038** (redeems month 138); the ladder runs to month 180. There is
nothing between I2038 and I2043 to buy, so the anchor matures ~3.5 years before the ladder ends
and the path locks. The comment in the file now says so.

---

## Known limitations / active placeholders

1. **`Sensitivity_Analysis.R` is broken and unmigrated.** It still opens with
   `source("functions/Reddington.R")` — a file that has not existed for several commits — and
   calls `optimize_redington_immunization()`, `bond_cashflow_schedule()` and
   `size_ladder_cash_buffer()`, none of which exist. Its
   `run_dynamic_ladder_simulation_toggle()` is a stale copy of an engine that has been
   completely rewritten twice since. It fails on line 52. The five PNGs in
   `sensitivity_plots/` are output from the **old nominal Redington model** and describe
   nothing current. **Note that `decisions_enabled` now exists on the real engine**, so the
   duplicated toggle function should be deleted rather than migrated.
2. **The run is slow: ~1.8 s/path → roughly 30 minutes for 1,000.** The cost is the
   per-(path, month) repricing that path-dependent curves *require*. Vectorising
   `reprice_ladder()` across paths is the obvious win and is what would make a 1,000-path
   sensitivity sweep practical.
3. **Breakeven inflation is uncalibrated.** `kappa = 0.15`, `sigma = 1.0` are explicit
   placeholders; the breakeven has no term structure. Both flagged in-file.
4. **The cash account can go negative — deliberately.** Nothing tops it up from equity mid-ladder; the ladder
   is solved to be solvent under the *assumed* inflation rate, so hotter realized inflation
   outspends the buffer. Counted and reported per run, not treated as an audit failure.
5. **The additive-only extension cannot shrink or patch an already-bought block.** A Defend cut
   leaves existing bonds over-funding their committed months. Known, accepted.
6. **A Defend cut is permanent** and compounds (0.95¹⁰ = 60%). Deliberate — see "The income
   mechanic".
7. **Mortality uses male `qx` only.** It feeds only the ARVA annuity factor — no survival
   weighting of outcomes, no stochastic death, so every path runs all 360 months.
8. **Ruin is a terminal-wealth test only.** It asks whether month-360 wealth clears the real
   bequest target; it says nothing about the *path* — a retiree who spent years in overdraft and
   recovered scores identically to one who never did. The income block, not the ruin figure, is
   where path quality shows up.
9. **The bootstrapped bond return series is computed and never used** (`bond_monthly_returns`).
10. **Fractional bond units** — the backward pass is continuous, not integer.
11. **No baseline comparator.** Hulett & Swanepoel (2024), named in the proposal, is not
    implemented. `decisions_enabled = FALSE` now gives a clean static-ladder arm to compare
    against, which is the missing half of that job.
12. **ILBs are treated as exactly real.** SA ILBs index off a 4-month-lagged, daily-interpolated
    CPI, so a flow is fixed to the price level of four months earlier and is not exactly
    constant in real terms. Flagged in-file as second-order — and note that the model's
    start-of-month indexing convention errs in the same direction as the real lag.
13. **The funded ratio flatters.** It discounts a 5%-escalating expense stream at nominal curve
    yields (~9–10%), so `funded_ratio ≥ 1` is common even on paths that end far below target.
    That is a modelling question, not an arithmetic one — the audit confirms the ratio is
    computed on the right money, not that the rule is a good rule.

---

## Suggested next steps

1. **Migrate `Functions/Sensitivity_Analysis.R`** onto the current API, delete its duplicated
   `run_dynamic_ladder_simulation_toggle()` in favour of the engine's own `decisions_enabled`
   argument, and regenerate the five PNGs.
2. **Build the static comparator** off `decisions_enabled = FALSE`, then Hulett & Swanepoel
   (2024).
3. **Speed up the simulation** — vectorising repricing across paths is the obvious win.
4. **Calibrate the breakeven process** against real SA breakeven data.
5. **Revisit `ladder_locked` at the cap** — on this universe a 15-year cap makes locking
   near-inevitable, and a locked path's Extend decisions are silently inert.
6. **Interrogate the funded-ratio rule itself** (limitation 13) now that the arithmetic under it
   is trustworthy. It is the last big unexamined piece: it discounts a 5.8%-escalating expense
   stream at nominal curve yields, so `funded_ratio >= 1` — and therefore Extend & Harvest —
   fires on most reviews even on paths that end far below target.
7. **Sweep surplus cash back into equity during the LADDER phase.** ARVA now consumes the
   residual — it prices the annuity on equity plus cash and spends the cash first — so the
   *terminal* problem is solved. What remains is that the surplus sits idle for the whole ladder
   phase, up to 15 years, earning roughly −0.8% real instead of equity returns. Its largest
   component is structural (R175,392 of the median R302,801, see the decomposition above): each
   extension sub-solve assumes it opens with `fresh$cash_at_start`, and the engine tops up a
   shortfall but **never removes a surplus**, so an account that gets ahead stays ahead. An annual
   sweep of cash above a stated ceiling back into equity would fix that and close the Defend
   over-funding leak in the same move.

## Key references

- Jonathan Brummer, "The Retirement Income Blueprint" (Substack) — source of the decision-matrix
  concept implemented in `Decision_Matrix.R`
- Waring & Siegel (2015) — ARVA methodology
- Redington (1952) — immunization conditions (the *removed* approach; kept for the writeup)
- Hulett & Swanepoel (2024) — intended dynamic baseline strategy (not yet implemented)
- ASSA / SAIML98–SAIFL98 mortality tables
