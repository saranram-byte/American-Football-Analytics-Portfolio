# WR Combine & Draft Capital Study
### "Is paying for WR talent becoming obsolete when the draft can find you the same production for pennies?"
#### And does the combine actually help teams find that talent — or create hype that obscures it?

---

## Core Investigation
With WR salaries exploding, teams may increasingly need to find
receiver talent through the draft. But does the combine create
hype that distorts draft decisions? And can combine profiles
historically predict not just where WRs get drafted — but how
good they actually become?

The 2026 NFL Draft brought this question into sharp focus:
the 49ers publicly credited AI for drafting De'Zhaun Stribling
33rd overall while the model's #1 ranked WR athletically —
Jeff Caldwell — went completely undrafted and signed with
the Kansas City Chiefs.

---

## Key Findings (Parts 1 & 2)
- The 40 yard dash is the **weakest** predictor of WR draft position among all combine metrics (R = 0.279)
- Speed Score (size-adjusted speed) is the strongest single predictor (R = -0.253)
- A 3-feature combine model explains only **11.2%** of WR draft position variance — the other 88.8% lives in film rooms
- In 2026, the model's #1 ranked WR (Jeff Caldwell) went undrafted — immediately signed by the Chiefs as a UDFA
- **8 of 8** athletically undervalued WRs in 2026 signed UDFA deals immediately after the draft
- The same pattern held in 2025 — model's #1 ranked WR (Joshua Lane) also went undrafted
- The 49ers used AI to justify drafting Stribling 33rd — the model ranked him 4th athletically in the class

---

## Series Structure

| Part | Question | Status |
|---|---|---|
| 1 — Historical EDA | Do combine metrics correlate with draft position? | ✅ Complete |
| 2a — Model Validation | How well did combine data predict the 2025 draft? | ✅ Complete |
| 2b — 2026 Projections | Where does the model project the 2026 WR class? | ✅ Complete |
| 2c — 2025 Retrospective | How did the model perform on the 2025 WR class? | ✅ Complete |
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
- 2025 & 2026 draft results via `nflreadr::load_draft_picks()`

---

## Key Metrics
- 40 yard dash, vertical, broad jump, cone, shuttle
- Speed Score `(wt * 200) / (forty ^ 4)`
- Explosion Index `vertical + broad_jump`
- Explosion Adjusted `(vertical + broad_jump) / sqrt(wt)`
- BMI `(wt / ht_inches^2) * 703`

---

## Tools
R · nflfastR · nflreadr · cfbfastR · tidyverse · ggplot2 · tidymodels · gt
