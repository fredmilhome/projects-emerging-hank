# Open Work — HANK Emerging Chile

Deadlines: RANK estimation 2026-04-07 · HANK calibration 2026-04-17

## HANK calibration

- [ ] **α (labor income share)**: not yet extracted. Source from BCCh national accounts.
- [ ] **Tlev (G/Y)**: not yet extracted. Source from BCCh (gov_spending / gdp from `calib_series_chile.csv`).
- [ ] **ζ (entrepreneur transition)**: source Chile equivalent of Guvenen, Kaplan & Song (2014); WID Chile top-10% wealth share available.
- [ ] **ι (trans. prob. E→W)**: US value 0.063 from Guvenen, Kaplan, Song (2014). Find Chile equivalent or justify same value.
- [ ] **B/Y (gov. debt/output)**: needed for `q̄_II/Y` row in calibration table. Source from BCCh.

---

## RANK estimation

- [ ] Write `chile-RANK-estimation/main.jl` — full RANK estimation run (currently only `main_noestim.jl` exists).

---

## Open questions

- **STY AR(1) and MPC interpretation.** Storesletten, Telmer & Yaron (2004) estimate a single AR(1) labor income process (Gaussian tails). This limits MPC interpretation — precautionary savings motives understated relative to a fat-tailed process. Need to be explicit about this scope restriction in the paper.
- **Dual income process (García et al. 2024).** Calibrating from this Chilean paper would require a Kronecker product of two Markov chains → ~33 income states (3×11). Need to verify DCT compression accuracy at 33 states and expect slower steady-state computation. Not pursued now; flagged as robustness extension.
- **Inflation / real deflator**: BCCh GDP deflator vs. CPI as proxy — finalize choice.
- **Nonfarm scope for hours and wages**: total-economy proxies accepted for baseline; document explicitly.
