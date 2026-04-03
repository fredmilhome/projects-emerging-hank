# BBL (2024, AER) RANK Model: Complete Data Requirements

## Reference
Bayer, Born, and Luetticke (2024), "Shocks, Frictions, and Inequality in US Business Cycles," American Economic Review 114(5). The RANK model is the representative-agent analogue of their HANK model. It shares the same firm sector, government sector, and shock structure, but replaces heterogeneous households with a single representative agent. The codebase is BASEtoolbox.jl (Julia), available at https://github.com/BASEforHANK.

**Key structural differences RANK vs HANK (from direct review of the paper):**
- The paper states (p. 1226): "the representative household model only changes equilibrium conditions in replacing the Bellman equation and the capital and bonds demand equations but leaves the entire other model structure unchanged." All firm-sector equations (production, price Phillips curve, capital goods producers, wage Phillips curve), government-sector equations (Taylor rule eq. 32, deficit rule eq. 33, tax rule eq. 34), and market clearing are identical.
- In RANK, the household has two Euler equations: one for bonds (eq. 29, includes the risk premium shock A_t) and one for capital (eq. 31). There is no portfolio adjustment friction (λ), no profit shares as separate tradable assets, and no no-arbitrage condition between bonds and profit shares (eq. 37 drops out).
- The RANK calibration values differ from HANK in three parameters because the different household structure requires different values to hit the same (or similar) targets: β = 0.996 (vs 0.983), τ̄_L = 0.250 (vs 0.180), and τ̄_P = 0.120 (vs 0.102). The RANK K/Y target is 11.44 (vs HANK's 11.22). The appendix text says "all other parameters are externally chosen and equal to the parameterization of the HANK model," referring to the non-targeted parameters (ξ, γ, α, δ₀, η̄, ζ̄, R̄_b, π̄).
- ι_Π (profit share maturity) is NOT estimated in RANK — Table 2 shows "—" in the RANK column.

---

## 1. CALIBRATION: Steady-State Parameters

These parameters are fixed before estimation. They do NOT vary during MCMC. They split into two categories: (a) parameters set to match country-specific data moments (targeted), and (b) parameters taken directly from the literature or set by normalization (externally calibrated).

### 1A. Targeted to country-specific data moments

Each parameter below is calibrated to match a specific long-run average data ratio. The calibration is joint in principle, but each parameter is primarily informed by one target moment.

| Parameter | BBL US value | Target moment | Description |
|-----------|-------------|---------------|-------------|
| β (discount factor) | 0.996 | K/Y = 11.44 (quarterly) | Average ratio of fixed assets to quarterly GDP (excluding net exports), sample average |
| τ̄_L (tax rate level) | 0.250 | G/Y = 0.2 | Average ratio of government consumption expenditure to GDP; τ̄_L is set so the government budget balances at this spending level |
| α (labor share) | 0.680 | Average labor income share = 62% | Share of labor in production; pinned down by average labor income share given the steady-state markup η̄ |
| δ₀ (depreciation rate) | 0.018 | Average annual depreciation ≈ 7.0% | Average depreciation rate on the capital stock (including buildings), converted to quarterly |
| τ̄_P (tax progressivity) | 0.120 | Average tax progressivity, 1954–2019 | Time-series average of the tax progressivity exponent P = (AMTR − ATR)/(1 − ATR), where AMTR is the average marginal tax rate and ATR is the average tax rate. For a log-linear tax system of the form y_net = (1 − τ_L) · y_gross^{1−τ_P}, this P equals the curvature parameter τ_P. In the RANK model this is set to 0.12 (Table A.1), vs the HANK value of 0.102 (Table 1). The BBL construction follows Ferriere and Navarro (2023), extending Mertens and Montiel Olea (2018) estimates through 2017. The AMTR is decomposed into an average marginal individual income tax rate (AMIITR, from SOI tax tables) and an average marginal payroll tax rate (AMPRT, from SSA data). The ATR uses total tax liabilities from NIPA Table 3.2 plus federal social insurance contributions from NIPA Table 3.6, divided by the Piketty-Saez income series. This is a complex construction; for a non-US country, one would need analogous tax micro-simulation or published estimates of AMTR and ATR. |

**Note on GDP definition used by BBL:** Output Y_t is defined as GDP excluding net exports. Specifically: Y_t = gross private domestic investment + personal consumption expenditures (nondurables + durables + services) + government consumption expenditures and gross investment. This definition is used both for the calibration ratios and for the estimation observable.

### 1B. Externally calibrated (from literature / normalization)

These do NOT require country-specific data. They are taken from the literature or set by normalization. In BBL's RANK specification they are set equal to the HANK values.

| Parameter | Value | Source / Rationale |
|-----------|-------|--------------------|
| ξ (relative risk aversion) | 4.000 | Kaplan et al. (2018); standard in incomplete markets literature |
| γ (inverse Frisch elasticity) | 2.000 | Chetty et al. (2011); implies Frisch elasticity of 0.5 |
| η̄ (goods elasticity of substitution) | 11.000 | Born and Pfeifer (2014); implies 10% steady-state price markup |
| ζ̄ (labor elasticity of substitution) | 11.000 | Born and Pfeifer (2014); implies 10% steady-state wage markup |
| R̄_b (gross nominal interest rate) | 1.000 | Normalization: growth ≈ interest rate |
| π̄ (gross steady-state inflation) | 1.000 | Normalization: indexation to steady-state inflation, without loss of generality |

**RANK-specific household equations (for reference):**

The RANK household block consists of three equations replacing the full HANK Bellman + distribution system:

1. **Bond Euler equation (eq. 29):** u_c(x_t) = β · E_t [A_t · R^b_t / π_{t+1} · u_c(x_{t+1})]
2. **Capital Euler equation (eq. 31):** u_c(x_t) = β · E_t [(q_{t+1} + r_{t+1}) / q_t · u_c(x_{t+1})]
3. **Budget constraint (eq. 30):** q_t · K_{t+1} + B_{t+1} = (R^b_t / π_t) · B_t + (q_t + r_t) · K_t + (1 − τ_t) · [net_labor_income + Π^U_t + Π^F_t] − x_t

where x_t = c_t − G(n_t) is the composite consumption-leisure good (GHH preferences), q_t is the price of capital, r_t is the net dividend/rental rate, and Π^U_t and Π^F_t are union and firm profits. In RANK all profits go to the single representative agent. The GHH preference structure means G(n_t) = n_t^{1+γ}/(1+γ) and the felicity function is u(x) = x^{1−ξ}/(1−ξ).

---

## 2. ESTIMATION: Bayesian Inference via MCMC

### 2A. Observable time series (7 series, quarterly frequency)

BBL use quarterly US data from 1954:III to 2019:IV. The model is stationary, so all growth rates are de-meaned (sample mean subtracted). The level variables (hours, nominal rate) are also expressed as deviations from their sample means.

| # | Observable | Model variable | Transformation |
|---|-----------|----------------|----------------|
| 1 | Real per capita output growth | Δlog(Y_t) | Log-difference of real per capita GDP (excl. net exports), de-meaned |
| 2 | Real per capita consumption growth | Δlog(C_t) | Log-difference of real per capita consumption (nondurables + durables + services), de-meaned |
| 3 | Real per capita investment growth | Δlog(I_t) | Log-difference of real per capita gross private domestic investment, de-meaned |
| 4 | Real wage growth | Δlog(w^F_t) | Log-difference of hourly compensation in nonfarm business sector deflated by GDP deflator, de-meaned |
| 5 | Per capita hours worked (level) | log(N_t) | Log of nonfarm business hours divided by civilian noninstitutional population, de-meaned |
| 6 | Inflation | log(π_t) | Log-difference of GDP deflator, de-meaned |
| 7 | Nominal interest rate | log(R^b_t) | Quarterly average of the effective federal funds rate (or shadow rate at ZLB), de-meaned |

**Critical construction details:**
- All real variables are deflated by the GDP deflator (not CPI).
- All per capita variables are divided by civilian noninstitutional population.
- Output excludes net exports by construction.
- Consumption includes durables (unlike some DSGE specifications).
- The wage is compensation per hour in the nonfarm business sector, not average hourly earnings.
- At the zero lower bound, BBL use the Wu-Xia shadow federal funds rate.

### 2B. Government sector equations (identical across RANK/HANK/HANK-X)

These are the policy rules whose parameters are estimated. They are written here in the log-linearized form used in estimation.

**Taylor rule (eq. 32):**
R^b_{t+1} / R̄_b = (R^b_t / R̄_b)^{ρ_R} · (π_t / π̄)^{(1-ρ_R)·θ_π} · (Y_t / Y_{t-1})^{(1-ρ_R)·θ_Y} · ε^R_t

**Deficit / government debt rule (eq. 33):**
B_{t+1} / B_t = (B_t / B̄)^{-γ_B} · (π_t / π̄)^{γ_π} · (Y_t / Y_{t-1})^{γ_Y} · D_t
where D_t = D_{t-1}^{ρ_D} · ε^D_t is a persistent structural deficit shock.

**Tax level rule (eq. 34):**
τ_t / τ̄ = (τ_{t-1} / τ̄)^{ρ_τ} · (B_t / B_{t-1})^{(1-ρ_τ)·γ^τ_B} · (Y_t / Y_{t-1})^{(1-ρ_τ)·γ^τ_Y}

The tax level parameter τ^L_t adjusts endogenously so the average tax rate on income equals the target τ_t from the rule above (eq. 35). Government spending G_t is then determined residually from the government budget constraint: G_t = B_{t+1} + T_t − (R^b_t / π_t) · B_t.

### 2C. Structural shocks (7 shocks, one per observable)

The system is exactly identified: 7 observables, 7 shocks. Six shocks follow log-AR(1) processes (5 exogenous states + the deficit process D_t with its own persistence ρ_D). The monetary policy shock ε^R_t is i.i.d., but enters a Taylor rule with interest-rate smoothing ρ_R, so the policy rate itself is persistent.

| Shock | Exogenous state | Process parameters | Drives primarily |
|-------|----------------|-------------------|-----------------|
| TFP | Z_t | ρ_A, σ_A | Aggregate productivity level |
| Investment-specific technology | Ψ_t | ρ_Z, σ_Z | Marginal efficiency of investment (à la Justiniano, Primiceri, Tambalotti 2011) |
| Price markup | μ^Y_t | ρ_Ψ, σ_Ψ | Target price markup; enters the price Phillips curve |
| Wage markup | μ^W_t | ρ_µ, σ_µ | Target wage markup; enters the wage Phillips curve |
| Risk premium | A_t | ρ_µw, σ_µw | Intermediation efficiency shifter; enters the bond Euler equation (eq. 29 in RANK) |
| Monetary policy | (i.i.d.) | σ_R | Taylor rule innovation ε^R_t; the policy rate R^b_t has persistence via smoothing parameter ρ_R |
| Structural deficit | D_t | ρ_D, σ_D | Persistent shock to the government's structural deficit (eq. 33: D_t = D_{t-1}^{ρ_D} · ε^D_t) |

**WARNING on notation in Table A.2:** The parameter subscripts in Table A.2 do NOT match the shock labels intuitively. The subscript ordering in Table A.2 is: A = TFP, Z = investment-specific technology, Ψ = price markup, µ = wage markup, µw = risk premium. This is the ordering used in the BASEtoolbox.jl codebase. The mapping above follows from cross-referencing Table A.2 posteriors with the variance decomposition results and the model equations.

**How the risk premium shock works in RANK:** The risk premium shock A_t enters the RANK bond Euler equation (29) as: u_c(x_t) = β E_t [A_t · R^b_t / π_{t+1} · u_c(x_{t+1})]. This is the same "risk premium" or "intermediation efficiency" wedge as in Smets and Wouters (2007). In HANK, A_t instead multiplies the return on the liquid asset portfolio (eq. 26), which includes both government bonds and profit shares. In RANK, there are no profit shares — the representative household holds capital directly via a separate capital Euler equation (31).

### 2D. Estimated parameters

All parameters below are estimated via Bayesian MCMC (random walk Metropolis-Hastings). Their prior distributions are listed.

**Frictions (4 parameters):**

| Parameter | Prior | Mean | SD | Description |
|-----------|-------|------|----|-------------|
| δ_s (= δ₂/δ₁) | Gamma | 5.00 | 2.00 | Elasticity of marginal depreciation w.r.t. capacity utilization |
| ϕ | Gamma | 4.00 | 2.00 | Investment adjustment cost |
| κ | Gamma | 0.10 | 0.03 | Slope of price Phillips curve |
| κ_w | Gamma | 0.10 | 0.03 | Slope of wage Phillips curve |

**Monetary policy (4 parameters):**

| Parameter | Prior | Mean | SD | Description |
|-----------|-------|------|----|-------------|
| ρ_R | Beta | 0.50 | 0.20 | Interest rate smoothing |
| σ_R | Inv-Gamma | 0.10 | 2.00 | Std. dev. of monetary policy shock (×100 in tables; RANK posterior: 0.238) |
| θ_π | Normal | 1.70 | 0.30 | Taylor rule coefficient on inflation |
| θ_Y | Normal | 0.13 | 0.05 | Taylor rule coefficient on output growth |

**Fiscal policy — deficit/spending rule (4 parameters):**

| Parameter | Prior | Mean | SD | Description |
|-----------|-------|------|----|-------------|
| ρ_D | Beta | 0.50 | 0.20 | Persistence of deficit process |
| γ_B | Gamma | 0.10 | 0.08 | Debt feedback in deficit rule |
| γ_π | Normal | 0.00 | 1.00 | Inflation feedback in deficit rule |
| γ_Y | Normal | 0.00 | 1.00 | Output growth feedback in deficit rule |

**Fiscal policy — tax level rule (3 parameters):**

| Parameter | Prior | Mean | SD | Description |
|-----------|-------|------|----|-------------|
| ρ_τ | Beta | 0.50 | 0.20 | Persistence of tax rate process |
| γ^τ_B | Normal | 0.00 | 1.00 | Debt feedback in tax rule |
| γ^τ_Y | Normal | 0.00 | 1.00 | Output growth feedback in tax rule |

**Shock processes (from Table A.2 — 11 parameters total):**

The five exogenous AR(1) states (TFP, inv.-spec. tech., price markup, wage markup, risk premium) each contribute a persistence ρ and volatility σ = 10 parameters. The deficit shock D_t contributes only its volatility σ_D here (its persistence ρ_D is listed in the fiscal policy block of Table 2 above). The monetary policy shock contributes σ_R and ρ_R, both listed in the monetary policy block of Table 2. All prior means and SDs below follow BBL's Table A.2 convention where standard deviations are multiplied by 100 for readability.

| Parameter | Prior | Mean | SD | Notes |
|-----------|-------|------|----|-------|
| ρ_A (TFP persistence) | Beta | 0.50 | 0.20 | RANK posterior: 0.943 |
| σ_A (TFP volatility) | Inv-Gamma | 0.10 | 2.00 | RANK posterior: 0.222 (×100) |
| ρ_Z (inv.-spec. tech. persistence) | Beta | 0.50 | 0.20 | RANK posterior: 0.996 |
| σ_Z (inv.-spec. tech. volatility) | Inv-Gamma | 0.10 | 2.00 | RANK posterior: 0.576 (×100) |
| ρ_Ψ (price markup persistence) | Beta | 0.50 | 0.20 | RANK posterior: 0.721 |
| σ_Ψ (price markup volatility) | Inv-Gamma | 0.10 | 2.00 | RANK posterior: 16.723 (×100) |
| ρ_µ (wage markup persistence) | Beta | 0.50 | 0.20 | RANK posterior: 0.964 |
| σ_µ (wage markup volatility) | Inv-Gamma | 0.10 | 2.00 | RANK posterior: 1.276 (×100) |
| ρ_µw (risk premium persistence) | Beta | 0.50 | 0.20 | RANK posterior: 0.888 |
| σ_µw (risk premium volatility) | Inv-Gamma | 0.10 | 2.00 | RANK posterior: 3.663 (×100) |
| σ_D (deficit shock volatility) | Inv-Gamma | 0.10 | 2.00 | RANK posterior: 0.541 (×100) |

**Total estimated parameters in RANK: 4 (frictions) + 4 (monetary policy) + 4 (fiscal deficit rule) + 3 (fiscal tax rule) + 11 (shock processes from Table A.2) = 26 parameters.**

**RANK posterior reference values (Table 2 + Table A.2, for sanity-checking your estimation):**
- δ_s = 0.697, ϕ = 7.875, κ = 0.119, κ_w = 0.282
- ρ_R = 0.794, σ_R = 0.238, θ_π = 2.162, θ_Y = 0.254
- ρ_D = 0.960, γ_B = 0.089, γ_π = −2.756, γ_Y = −0.734
- ρ_τ = 0.492, γ^τ_B = 3.442, γ^τ_Y = −1.621
- Log marginal data density (RANK, aggregate data only) = 6,590

---

## 3. What is NOT needed for RANK

The following data and parameters are required ONLY for HANK or HANK-X. They can be completely ignored when estimating the RANK model.

### NOT needed — HANK-specific calibration targets:
- Liquid-to-illiquid asset ratio (from household balance sheet survey, e.g., SCF)
- Share of borrowers in the population (from household survey)
- Top 10% wealth share long-run average (from WID, used as calibration target for entrepreneur transition probability ζ)
- Government debt-to-GDP ratio (used to pin down profit share value q̄Π/Y in HANK; in RANK this channel doesn't exist)
- Idiosyncratic income process parameters ρ_h, σ_h (from panel earnings data)
- Entrepreneur-to-worker transition probability ι (from top-1% exit rates)
- Portfolio adjustment probability λ (governs capital market participation frequency)
- Borrowing penalty R̄ (spread on household borrowing)

### NOT needed — HANK-specific estimated parameter:
- ι_Π (profit share maturity/duration) — only appears in HANK and HANK-X

### NOT needed — HANK-X additional observables:
- Top 10% wealth share time series (annual, WID)
- Top 10% income share time series (annual, WID)
- Tax progressivity time series (annual, Ferriere-Navarro construction)
- Idiosyncratic income risk time series (quarterly, from panel survey)

### NOT needed — HANK-X additional estimated parameters:
- Tax progressivity shock process (ρ_P, σ_P)
- Income risk process (ρ_s, σ_s, Σ_Y)
- Measurement errors on top-10 shares (σ^me_{I10}, σ^me_{W10})

---

## 4. RANK Estimation Summary

**To estimate the BBL RANK model for a country, you need:**

**For calibration (5 country-specific targets + 6 externally set parameters):**
1. Average capital-to-output ratio (quarterly) → β
2. Average government spending share of GDP → τ̄_L
3. Average labor income share → α
4. Average depreciation rate on capital → δ₀
5. Average tax progressivity → τ̄_P
6. Set ξ = 4, γ = 2, η̄ = 11, ζ̄ = 11, R̄_b = 1, π̄ = 1 (no data needed)

**For estimation (7 quarterly time series):**
1. Real per capita GDP growth (excl. net exports)
2. Real per capita consumption growth (including durables)
3. Real per capita investment growth
4. Real wage growth (compensation per hour, nonfarm, deflated by GDP deflator)
5. Per capita hours worked (nonfarm, level in logs)
6. Inflation (GDP deflator)
7. Nominal policy interest rate (or shadow rate at ZLB)

**All growth rates are de-meaned. All level variables are expressed as deviations from sample means.**
