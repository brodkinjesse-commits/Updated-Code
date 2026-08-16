# CLAUDE.md — ILB Ladder Thesis (Hutchison & Brodkin)

This file preserves project context across sessions. Read this before continuing work.

## Project overview

Master's thesis project for supervisor Emlyn Flint's topic **EF3**:
> "Dynamic Retirement Strategies for South Africa: Testing the Inflation-Linked Bond Ladder as a Decumulation Solution"
(from the department's "All Topics" doc)

Authors: Hutchison & Brodkin. Working with "Jesse" (collaborator/recipient of drafts).

Goal: build an R model comparing an inflation-linked bond (ILB) ladder decumulation strategy against baseline retirement drawdown strategies, using simulated capital markets, for South African retirees.

## Status as of Session 1 (2026-08-03)

Built a first working R skeleton: `ILB_Ladder_Thesis.zip` (sent to Jesse). Runs cleanly end-to-end via Rscript, no package dependencies beyond base R.

### What's implemented
- **Capital markets**: 6-month block bootstrap of 30-year real-return paths (synthetic/simulated capital market assumptions — placeholder, not real data yet)
- **ILB ladder** + an approximate version of Jonathan Brummer's funded-ratio "decision matrix" (extend+raise / extend flat / hold / haircut)
- **ARVA** (Waring & Siegel 2015) applied once the ladder is exhausted
- **Baseline comparator**: simple static-withdrawal all-equity strategy
- **Metrics**: avg/min months to ruin, ruin probability, NPV distribution
- **Diagnostic plots**: equity fan chart, withdrawal fan chart, ruin probability comparison, NPV histograms

### File structure (as of last session)
- `R/00_config.R` — contains `config$decision_matrix` (placeholder thresholds)
- `R/01_capital_market_assumptions.R` — simulated equity/return data (to be replaced with real data)
- (other model files not yet detailed in notes)

## Open items / blockers for next session

1. **Book8.xlsx equity data**: stored in the project as a binary blob; `project_read` cannot extract its contents (blob files return empty). **Need Jesse to either attach the file directly in a chat message, or export the relevant column(s) to CSV**, so the real equity return series can replace the simulated placeholder in `R/01_capital_market_assumptions.R`.

2. **Brummer decision matrix thresholds**: the real matrix is presented as an image in this article: https://jonathanbrummer.substack.com/p/the-retirement-income-blueprint — Claude in Chrome extension was not connected last session, so we couldn't screenshot/extract it. Current thresholds in `config$decision_matrix` (R/00_config.R) are a **placeholder approximation** of the article's described logic (funded ratio vs market performance → raise/hold/cut). Need the real numbers to drop in — try connecting Claude in Chrome next session to capture the image.

3. **Mortality, ILB yields, and inflation** are all simulated (per Jesse's explicit instruction for this first pass). Real ASSA mortality tables and SA ILB yield history are a natural next step, not yet started.

4. **Baseline strategy**: currently the simplest static-withdrawal comparator, **not yet** the Hulett & Swanepoel (2024) dynamic trigger/haircut living annuity described in the proposal as the intended baseline. This needs to be built.

5. **Granularity**: model currently runs at annual granularity; monthly granularity would be needed for a more precise "months to ruin" metric later.

## Next-session priorities (suggested)
- Get real equity data from Jesse (CSV export of Book8.xlsx) and wire it into `R/01_capital_market_assumptions.R`
- Connect Claude in Chrome and extract the real Brummer decision matrix thresholds from the Substack article
- Start scoping the Hulett & Swanepoel (2024) dynamic trigger/haircut baseline
- Consider monthly granularity refactor once other pieces are in place

## Key references
- Jonathan Brummer, "The Retirement Income Blueprint" (Substack) — decision matrix source
- Waring & Siegel (2015) — ARVA methodology
- Hulett & Swanepoel (2024) — intended dynamic baseline strategy (not yet implemented)
- ASSA mortality tables (not yet incorporated)
