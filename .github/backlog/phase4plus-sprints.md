# Post-Phase-4 Long-Term Sprint Backlog

This backlog translates the long-term roadmap (P4B-P8) into 2-week sprint execution units.

## Sprint Calendar

| Sprint | Dates (2026-2027) | Milestone Focus | Planned Issues |
|---|---|---|---|
| S01 | 2026-03-09 to 2026-03-20 | P4B | P4B-01, P4B-02 |
| S02 | 2026-03-23 to 2026-04-03 | P4B | P4B-03, P4B-04 |
| S03 | 2026-04-13 to 2026-04-24 | P5 | P5-01, P5-02 |
| S04 | 2026-05-04 to 2026-05-15 | P5 | P5-03, P5-04 |
| S05 | 2026-06-01 to 2026-06-12 | P6 | P6-01, P6-02 |
| S06 | 2026-06-15 to 2026-06-26 | P6 | P6-03, P6-04 |
| S07 | 2026-08-03 to 2026-08-14 | P7 | P7-01, P7-02 |
| S08 | 2026-08-17 to 2026-08-28 | P7 | P7-03, P7-04 |
| S09 | 2026-11-02 to 2026-11-13 | P8 | P8-01, P8-02 |
| S10 | 2026-11-16 to 2026-11-27 | P8 | P8-03, P8-04 |

## Delivery Rules

1. No issue is marked `Done` without all acceptance tests passing.
2. Any schema-impacting change must include snapshot/contract test updates.
3. Every sprint must produce one demo artifact:
   - benchmark report, reproducibility report, or end-to-end vignette update.

## GitHub Project Field Suggestions

Use these fields in GitHub Projects:

- `Status`: `Backlog`, `Ready`, `In Progress`, `Blocked`, `Review`, `Done`
- `Phase`: `P4B`, `P5`, `P6`, `P7`, `P8`
- `Sprint`: `S01` ... `S10`
- `Priority`: `High`, `Medium`, `Low`
- `Estimate`: story points
- `Risk`: `Low`, `Medium`, `High`

## Tracking Queries

Use these issue filters:

- `is:open label:priority-high label:reliability`
- `is:open label:phase-5 label:graph`
- `is:open label:phase-6 label:interop`
- `is:open label:phase-7 label:cloud`
- `is:open label:phase-8`
