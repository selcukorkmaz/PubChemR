# Post-Phase-4 Backlog Pack

This folder contains a GitHub-Projects-ready backlog for the long-term roadmap (P4B-P8).

## Files

- `phase4plus-milestones.json`: milestone definitions and due dates
- `phase4plus-labels.json`: label catalog used by roadmap issues
- `phase4plus-issues.json`: issue definitions with objective/tasks/acceptance tests
- `phase4plus-sprints.md`: sprint-by-sprint execution calendar

## Create Backlog In GitHub

Use:

```powershell
pwsh ./scripts/github/create_phase4plus_backlog.ps1
```

Optional parameters:

```powershell
pwsh ./scripts/github/create_phase4plus_backlog.ps1 `
  -Repo "owner/repo" `
  -ProjectOwner "owner" `
  -ProjectNumber 3
```

Dry run:

```powershell
pwsh ./scripts/github/create_phase4plus_backlog.ps1 -DryRun
```

## Requirements

1. `gh` CLI installed and authenticated (`gh auth status`).
2. Permission to create labels, milestones, issues (and project items if used).

## Notes

1. The script is idempotent for issue creation by roadmap ID in title (for example `[P4B-01]`).
2. Existing milestones and issues are detected and skipped.
