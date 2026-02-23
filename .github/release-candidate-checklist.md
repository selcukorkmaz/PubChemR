# Release Candidate Checklist (Phase 3)

Use this checklist before merging a Phase 3 PR.

## 1. Scope lock

Review scope should be limited to Phase 3 additions/refinements:

- `R/pc_phase3.R`
- `man/pc_phase3.Rd`
- `tests/testthat/test-pc-phase3.R`
- `vignettes/pubchemr-workflow.Rmd`
- `NAMESPACE`
- `README.Rmd`
- `README.md`
- `NEWS.md`

Quick scope command:

```bash
git diff --name-only origin/master...HEAD
```

## 2. Local RC gate (tarball-based)

Build from source (vignettes enabled):

```bash
R CMD build .
```

Run check on the built tarball:

```bash
R CMD check --no-manual PubChemR_*.tar.gz
```

Expected result:

- `Status: OK`

## 3. CI RC gate

Confirm `Release Candidate Gate` workflow passes for:

- `ubuntu-latest`
- `windows-latest`
- `macos-latest`

Gate policy:

- Checks run from a built tarball.
- Vignettes are built/validated.
- Warnings fail the workflow (`error_on = "warning"`).

## 4. Regression safety

Run full local tests:

```bash
Rscript -e "testthat::test_local(stop_on_failure = TRUE)"
```

Expected result:

- `FAIL 0`
- `WARN 0`

## 5. Merge readiness

Merge only if all are true:

- Phase 3 scope is respected.
- Local tarball check is `Status: OK`.
- RC CI matrix is fully green.
- No unresolved TODOs in newly added Phase 3 files.
