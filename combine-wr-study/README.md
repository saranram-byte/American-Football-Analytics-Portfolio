# WR Combine & Draft Capital Study
### "Is paying for WR talent becoming obsolete when the draft
can find you the same production for pennies?"

---

## Core Investigation
With WR salaries exploding, teams may increasingly need to find
receiver talent through the draft. But does the combine create
hype that distorts draft decisions? And can combine profiles
historically predict not just where WRs get drafted — but how
good they actually become?

---

## Series Structure

| Part | Question | Status |
|---|---|---|
| 1 — Historical EDA | Do combine metrics correlate with draft position? | 🔄 In Progress |
| 2a — Model Validation | How well did combine data predict the 2025 draft? | 📋 Planned |
| 2b — 2026 Projections | Where does the model project the 2026 WR class? | 📋 Planned |
| 3 — Outcome Prediction | Can combine profiles predict NFL outcome tiers? | 📋 Planned |
| 4 — Scouting Profiles | What does the full picture say about key 2026 prospects? | 📋 Planned |
| 5 — Mid-Season Check-in | Are early trends matching predictions? (November 2026) | 📋 Planned |
| 6 — Full Retrospective | Combine profiles vs actual outcomes (February 2027) | 📋 Planned |

---

## Outcome Tier Framework
WRs from 2010-2021 combine classes categorized as:
*(2022-2024 classes excluded — insufficient NFL sample size)*

- **Hit** — Made a real NFL impact (starter/Pro Bowl caliber)
- **Serviceable** — Stuck around, contributed in a rotational role
- **Miss** — Draft capital wasted or never stuck on a roster
- **Undrafted Success** — UDFA or Day 3 who outperformed draft
status (tracked separately)

---

## Data Sources
- NFL combine data via `nflreadr`
- NFL production data via `nflfastR`
- College production via `cfbfastR`

---

## Key Metrics
- 40 yard dash, vertical, broad jump, cone, shuttle
- Speed Score `(wt * 200) / (forty ^ 4)`
- Explosion Index `vertical + broad_jump`
- BMI `(wt / ht_inches^2) * 703`

---

## Tools
R · nflfastR · nflreadr · cfbfastR · tidyverse · ggplot2 · tidymodels
