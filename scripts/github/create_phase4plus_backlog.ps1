param(
  [string]$Repo = "",
  [string]$ProjectOwner = "",
  [int]$ProjectNumber = 0,
  [string]$MilestonesPath = ".github/backlog/phase4plus-milestones.json",
  [string]$LabelsPath = ".github/backlog/phase4plus-labels.json",
  [string]$IssuesPath = ".github/backlog/phase4plus-issues.json",
  [switch]$DryRun
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

function Require-Gh {
  if (-not (Get-Command gh -ErrorAction SilentlyContinue)) {
    throw "GitHub CLI ('gh') is required but was not found in PATH."
  }
}

function Read-JsonFile {
  param([string]$Path)
  if (-not (Test-Path $Path)) {
    throw "Required file not found: $Path"
  }
  Get-Content -Path $Path -Raw | ConvertFrom-Json
}

function Get-Repo {
  param([string]$CurrentRepo)
  if ([string]::IsNullOrWhiteSpace($CurrentRepo)) {
    return (gh repo view --json nameWithOwner -q .nameWithOwner).Trim()
  }
  return $CurrentRepo
}

function Ensure-Labels {
  param(
    [string]$TargetRepo,
    [object[]]$Labels,
    [switch]$WhatIf
  )

  foreach ($label in $Labels) {
    $name = [string]$label.name
    $color = [string]$label.color
    $desc = [string]$label.description
    if ($WhatIf) {
      Write-Host "[dry-run] label ensure: $name"
      continue
    }
    gh label create $name `
      --repo $TargetRepo `
      --color $color `
      --description $desc `
      --force | Out-Null
    Write-Host "label ensured: $name"
  }
}

function Get-MilestoneMap {
  param([string]$TargetRepo)
  $raw = gh api "repos/$TargetRepo/milestones?state=all&per_page=100"
  $items = $raw | ConvertFrom-Json
  $map = @{}
  foreach ($m in $items) {
    $map[[string]$m.title] = $m
  }
  return $map
}

function Ensure-Milestones {
  param(
    [string]$TargetRepo,
    [object[]]$Milestones,
    [switch]$WhatIf
  )

  $existing = Get-MilestoneMap -TargetRepo $TargetRepo

  foreach ($m in $Milestones) {
    $title = [string]$m.title
    $due = [string]$m.due_on
    $desc = [string]$m.description

    if ($existing.ContainsKey($title)) {
      Write-Host "milestone exists: $title"
      continue
    }

    if ($WhatIf) {
      Write-Host "[dry-run] milestone create: $title"
      continue
    }

    gh api --method POST "repos/$TargetRepo/milestones" `
      -f title="$title" `
      -f due_on="$due" `
      -f description="$desc" | Out-Null
    Write-Host "milestone created: $title"
  }
}

function Test-IssueExists {
  param(
    [string]$TargetRepo,
    [string]$IssueId,
    [string]$IssueTitle
  )
  $raw = gh issue list --repo $TargetRepo --search "$IssueId in:title" --state all --limit 100 --json title
  $items = $raw | ConvertFrom-Json
  foreach ($it in $items) {
    if ([string]$it.title -eq $IssueTitle) {
      return $true
    }
  }
  return $false
}

function Build-IssueBody {
  param([object]$Issue)

  $deps = if ($Issue.depends_on.Count -gt 0) {
    ($Issue.depends_on | ForEach-Object { "- $_" }) -join "`n"
  } else {
    "- None"
  }

  $tasks = ($Issue.tasks | ForEach-Object { "- [ ] $_" }) -join "`n"
  $acc = ($Issue.acceptance_tests | ForEach-Object { "- [ ] $_" }) -join "`n"

  @"
## Objective
$($Issue.objective)

## Sprint
$($Issue.sprint)

## Estimate
$($Issue.estimate_points) points

## Dependencies
$deps

## Tasks
$tasks

## Acceptance Tests
$acc
"@
}

function Create-Issues {
  param(
    [string]$TargetRepo,
    [string]$Owner,
    [int]$ProjectNo,
    [object[]]$Issues,
    [switch]$WhatIf
  )

  foreach ($i in $Issues) {
    $id = [string]$i.id
    $title = [string]$i.title

    if (Test-IssueExists -TargetRepo $TargetRepo -IssueId $id -IssueTitle $title) {
      Write-Host "issue exists: $title"
      continue
    }

    $body = Build-IssueBody -Issue $i

    if ($WhatIf) {
      Write-Host "[dry-run] issue create: $title"
      continue
    }

    $args = @(
      "issue", "create",
      "--repo", $TargetRepo,
      "--title", $title,
      "--body", $body,
      "--milestone", [string]$i.milestone
    )

    foreach ($label in $i.labels) {
      $args += @("--label", [string]$label)
    }

    $issueUrl = (& gh @args).Trim()
    Write-Host "issue created: $title"

    if ($ProjectNo -gt 0 -and -not [string]::IsNullOrWhiteSpace($Owner)) {
      gh project item-add $ProjectNo --owner $Owner --url $issueUrl | Out-Null
      Write-Host "  added to project #$ProjectNo: $issueUrl"
    }
  }
}

Require-Gh
$Repo = Get-Repo -CurrentRepo $Repo

if ([string]::IsNullOrWhiteSpace($ProjectOwner)) {
  $ProjectOwner = ($Repo.Split("/"))[0]
}

$milestonesData = Read-JsonFile -Path $MilestonesPath
$labelsData = Read-JsonFile -Path $LabelsPath
$issuesData = Read-JsonFile -Path $IssuesPath

Write-Host "Target repo: $Repo"
if ($ProjectNumber -gt 0) {
  Write-Host "Target project: owner=$ProjectOwner number=$ProjectNumber"
}

Ensure-Labels -TargetRepo $Repo -Labels $labelsData.labels -WhatIf:$DryRun
Ensure-Milestones -TargetRepo $Repo -Milestones $milestonesData.milestones -WhatIf:$DryRun
Create-Issues -TargetRepo $Repo -Owner $ProjectOwner -ProjectNo $ProjectNumber -Issues $issuesData.issues -WhatIf:$DryRun

Write-Host "Backlog sync completed."
