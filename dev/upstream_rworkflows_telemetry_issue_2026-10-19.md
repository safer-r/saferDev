# Issue report for upstream `neurogenomics/rworkflows`
# Target repo: https://github.com/neurogenomics/rworkflows/issues/new
# Subject/title to use:
# "CI broken: catchpoint/workflow-telemetry-action no longer resolvable (repo 404) - job fails at 'Set up job' even with run_telemetry: false"
#
# How to post it:
# 1. Open https://github.com/neurogenomics/rworkflows/issues/new
# 2. Paste the title above, then everything BELOW this line into the body.
# 3. Fill in nothing else; the text is ready. Your account posts it, not the agent.
# ---------------------------------------------------------------------------

## Bug description

The `catchpoint/workflow-telemetry-action` repository is no longer
resolvable on GitHub (both `https://github.com/catchpoint/workflow-telemetry-action`
and the REST API return 404 as of 2026-10-19). Because the first step of the
rworkflows composite action references it:

```yaml
- name: 🔭 Collect Workflow Telemetry 
  if: inputs.run_telemetry == 'true'
  uses: catchpoint/workflow-telemetry-action@v2
```

every workflow that uses `neurogenomics/rworkflows@master` (and recent tags,
v1.0.10 through v1.0.12) now fails during job setup, before any step runs:

```
Getting action download info
Error: Unable to resolve action `catchpoint/workflow-telemetry-action`, not found
```

All three OS jobs (ubuntu-latest, macOS-latest, windows-latest) fail at the
"Set up job" step.

Important: setting the `run_telemetry` input to `false` does NOT work around
this. The GitHub Actions runner resolves/downloads **all** `uses:` references
of a composite action at job-setup time, including steps whose `if:` condition
evaluates to false. So the job dies at "Set up job" even with
`run_telemetry: false` (verified on our side: run failed identically with the
input disabled).

This is the same class of breakage as issue #133
(runforesight/workflow-telemetry-action -> repository not found), which was
fixed in 1.0.2 by switching runforesight -> catchpoint. The action has now
moved again (or been deleted/made private), and there is no live repository
under either the old or the new name that we can find.

## Expected behaviour

Workflows using `neurogenomics/rworkflows@master` should not fail at job
setup. Suggested fix: remove the telemetry step entirely (or make it truly
optional, which the current `if: inputs.run_telemetry == 'true'` cannot
achieve given the runner's upfront resolution of `uses:` references).

## Reproducible example

Any repository with a workflow calling
`uses: neurogenomics/rworkflows@master` and pushing to a triggering branch.
Example from our repo (safer-r/saferDev, run #945, 2026-10-19 13:36 UTC,
commit 27300bf, which had `run_telemetry: ${{ false }}` set):

https://github.com/safer-r/saferDev/actions/runs/39261535225

All three matrix jobs failed at "Set up job" with the annotation:

```
Unable to resolve action `catchpoint/workflow-telemetry-action`, not found
```

The same failure occurred on the previous run without the
`run_telemetry: false` input, i.e. it appeared between two pushes with no
workflow-file change on our side - the breakage is external and came from the
upstream action repository disappearing.

## Session info

- rworkflows action ref: `master` (last upstream commit 6aaf493, 2026-05-06, v1.0.12)
- Runner: GitHub-hosted ubuntu-latest / macOS-latest / windows-latest
- Failure occurs before R is ever started, so no R session info is relevant.
