# Data audit

Written 2026-10-03, before any outcome model was fit. Every number here comes from `scripts/01_data_audit.R` (saved to `reports/tables/data_audit.json`).

## Sources and licenses

| Data | nflverse release | Seasons | License and attribution |
|---|---|---|---|
| Play-by-play | `pbp` | 2016–2025 | CC BY 4.0 (nflverse-data repository) |
| Participation (personnel on the field) | `pbp_participation` | 2016–2025 | CC BY-SA 4.0. "NFL NextGenStats via nflverse" through 2022; "FTN Data via nflverse" from 2023 |
| FTN charting | `ftn_charting` | 2022–2025 | CC BY-SA 4.0, "FTN Data via nflverse" |
| Schedule | `schedules` | 2016–2025 | nflverse (Lee Sharpe's nfldata) |

Because two inputs are share-alike, everything this project derives from them (tables, figures, the app's data) is released under CC BY-SA 4.0. Raw files are not committed. `scripts/00_fetch_data.R` downloads them and checks them against `data/manifest.csv`:
- **Play-by-play, participation and charting files:** pinned by SHA-256, since nflverse updates release files in place.
- **Schedule:** pinned by a fingerprint of the completed 2016–2025 games, since the file changes daily during the season.

The 2026 season is in progress and is not used.

## Completeness

- **Games:** all 2,761 completed games on the schedule, regular season and playoffs (267–285 a season), have play-by-play. Play-by-play has no games missing from the schedule.
- **Participation:** joins one-to-one on game and play ID, with no duplicate keys. Personnel is missing on 17 of the 342,254 analysis plays.
- **FTN charting:** joins one-to-one for 2022–2025.
  - Motion is never missing.
  - Backfield count is missing on 1,059 plays, and starting hash on 115.
  - QB alignment is uncoded ("0" or blank) on 485.

## Analysis rows and the outcome

| Step | Rows removed | Rows left |
|---|---|---|
| All play-by-play rows, 2016–2025 | | 484,254 |
| Not a run or pass play (kickoffs, punts, field goals, extra points, kneels, spikes, timeouts, penalty-nullified "no play" rows) | 139,434 | 344,820 |
| No down (two-point tries) | 1,276 | 343,544 |
| Aborted snap (the intended call is unknown) | 1,045 | 342,499 |
| Fake punt or field goal (description says "Punt formation" or "Field Goal formation") | 245 | **342,254** |

- **Outcome definition:** the outcome is the called play. `pass = 1` when the QB dropped back (`qb_dropback`), which counts 13,121 sacks and 9,293 scrambles as passes. Designed runs are 0.
- **Pass rate:** 60.4%–62.2% by season, with no missing values.
- **Penalty-nullified plays:** excluded. The call was made, but the play-by-play flags on "no play" rows are not reliable enough to label run or pass.

## Which fields are pre-snap

The question is what an opponent can know before the snap, so only these are inputs.

| Block | Fields | Notes |
|---|---|---|
| Game state | down, distance, yard line, goal-to-go, quarter, seconds left in half and game, score margin (offense view), both teams' timeouts, home | Raw state only. nflfastR's `ep` and `wp` are excluded: they are model outputs fit on outcomes from many seasons, including the test seasons, and the raw state already contains their inputs. |
| Alignment and tempo | shotgun, no-huddle | From the play description, consistent across all seasons |
| Personnel | number of RB (including FB), TE and WR; 6+ offensive linemen | Harmonized across the 2023 source change (below) |
| FTN pre-snap (2022+) | motion, backfield count, pistol, starting hash | FTN defines motion as "before or at the time of the snap" and backfield count as "at the snap" |

**Excluded as post-snap**, and a test enforces it:
- EPA and success
- scramble and sack flags
- air yards, pass location and length, run location and gap
- receiver
- FTN's play action, screen and RPO flags
- time to throw, pressure, routes, coverage type and pass rushers

**Defensive alignment** (defenders in the box, defensive personnel) is also excluded. It is the defense's choice, made after seeing the offense, so it measures what the defense expects rather than what the offense gives away.

**Betting columns:** play-by-play is read with an explicit column list that contains none. The schedule file has betting columns (`spread_line`, `total_line`, moneylines and odds), but only game ID, season and scores are read from it. A test checks that no column matching a betting pattern reaches the analysis table.

## Source change in 2023 and how it is handled

Participation data comes from NFL Next Gen Stats through 2022 and from FTN from 2023, and the two sources code formation and personnel differently.

- **Formation labels are not comparable.** NGS uses SHOTGUN, SINGLEBACK, I_FORM, PISTOL, EMPTY, JUMBO and WILDCAT. FTN uses SHOTGUN, UNDER CENTER and PISTOL. A model trained on one set would meet unseen labels in 2023+ test seasons, so **formation labels are not used.** QB alignment comes from the play-by-play shotgun flag instead. It is consistent across seasons, and it agrees with FTN's charted alignment on 99.3% of 139,203 plays. Pistol is counted as shotgun 99.6% of the time.
- **Personnel strings differ in format.** NGS writes "1 RB, 1 TE, 3 WR" and lists linemen only when there are not five ("6 OL, ..."). FTN lists every player ("1 C, 2 G, 1 QB, 1 RB, 2 T, 1 TE, 3 WR"). Both are parsed into the same counts. The harmonized mix shows no break at the source change:

| Season | 11 personnel | 12 | 21 | 6+ OL |
|---|---|---|---|---|
| 2021 | 59.7% | 20.1% | 6.7% | 3.9% |
| 2022 (NGS) | 62.1% | 18.6% | 7.9% | 3.0% |
| 2023 (FTN) | 63.2% | 19.4% | 7.4% | 2.6% |
| 2024 | 61.9% | 22.1% | 6.2% | 3.0% |
| 2025 | 57.2% | 21.9% | 6.3% | 5.5% |

The 2025 rise in 6+ OL plays (5.5%, against 2.6–4.2% from 2017 to 2024; 2016 was 5.7%) may be a real trend or a coding change. Either way, it lands in a test season and is reported, not adjusted.

Personnel uses roster positions, so a lineman reporting as a tight end counts as a lineman. In 0.2–1.2% of plays a season, the counts do not add up to 11 players.

## Ranges and sign conventions

- **Down and distance:** down is 1–4. Distance and yard line are positive.
- **Field position:** `yardline_100` is the distance to the opponent's goal line. On every goal-to-go play, distance equals yard line.
- **Score margin:** `score_differential` equals the offense's score minus the defense's score on every 2024 play checked.
- **Timeouts and clock:** timeouts are 0–3, and seconds remaining are non-negative.
- **Teams:** 32 franchises. Relocations are mapped to one code (OAK→LV, SD→LAC, STL→LA). A team-season has 860–1,330 analysis plays.

## FTN pre-snap fields over time

| Season | Motion rate | Mean backfield count | Middle-hash rate |
|---|---|---|---|
| 2022 | 37.4% | 1.10 | 10.2% |
| 2023 | 45.8% | 1.09 | 9.9% |
| 2024 | 49.4% | 1.10 | 10.0% |
| 2025 | 55.2% | 1.13 | 9.5% |

Motion use rose every season. That is a known league trend, and it means a model trained on earlier seasons sees a shifting input.

## Leakage risks carried into the analysis plan

1. **Post-snap fields:** they must never be inputs. This is enforced in code and tested.
2. **Random splits:** they would put plays from the same game, drive and team-season on both sides. Validation is temporal (train on earlier seasons, test on a later one). Team-tendency models predict a week using only earlier weeks.
3. **Model-derived features fit on future seasons:** `ep` and `wp` are excluded for this reason.
4. **The source change in 2023:** formation labels are dropped and personnel is harmonized (above).
