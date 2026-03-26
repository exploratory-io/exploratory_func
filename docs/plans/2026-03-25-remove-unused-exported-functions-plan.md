# Remove Unused Exported Functions — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove all exported functions from `exploratory_func` that are not called by any of the three known consumers (tam, datablog, scheduler), reducing the public API from 543 exports to ~175.

**Architecture:** Script-assisted diff to produce authoritative DELETE and KEEP-AS-INTERNAL lists, then systematic removal from R source files + NAMESPACE regeneration via `devtools::document()`.

**Tech Stack:** R, roxygen2, devtools, bash

---

## Background

- `NAMESPACE` has 543 `export(...)` entries (plus S3methods).
- Consumer repos use `exploratory::<fn>` syntax exclusively.
- Internal dependencies (kept functions calling candidate-removal functions) must be preserved as unexported helpers.
- Design doc: `docs/plans/2026-03-25-remove-unused-exported-functions.md`

---

### Task 1: Build the used-function list from all consumer repos

**Files:**
- Create: `/tmp/used_functions.txt` (scratch — not committed)

**Step 1: Extract all `exploratory::` references from tam**

```bash
grep -roh "exploratory::[A-Za-z0-9_.]*" \
  ~/Work/gitrepo/tam/src \
  --include="*.js" --include="*.ts" 2>/dev/null \
  | sed 's/exploratory:://' | sort -u
```

**Step 2: Same for datablog**

```bash
grep -roh "exploratory::[A-Za-z0-9_.]*" \
  ~/Work/gitrepo/datablog/src \
  --include="*.js" --include="*.ts" 2>/dev/null \
  | sed 's/exploratory:://' | sort -u
```

**Step 3: Same for scheduler**

```bash
grep -roh "exploratory::[A-Za-z0-9_.]*" \
  ~/Work/gitrepo/scheduler \
  --include="*.js" --include="*.ts" --include="*.R" 2>/dev/null \
  | sed 's/exploratory:://' | sort -u
```

**Step 4: Combine and deduplicate into one file**

```bash
{ \
  grep -roh "exploratory::[A-Za-z0-9_.]*" ~/Work/gitrepo/tam/src --include="*.js" --include="*.ts" 2>/dev/null; \
  grep -roh "exploratory::[A-Za-z0-9_.]*" ~/Work/gitrepo/datablog/src --include="*.js" --include="*.ts" 2>/dev/null; \
  grep -roh "exploratory::[A-Za-z0-9_.]*" ~/Work/gitrepo/scheduler --include="*.js" --include="*.ts" --include="*.R" 2>/dev/null; \
} | sed 's/exploratory:://' | sort -u > /tmp/used_functions.txt

wc -l /tmp/used_functions.txt
```

Expected: ~170–180 lines.

---

### Task 2: Build the candidate removal list

**Files:**
- Create: `/tmp/exported_functions.txt` (scratch)
- Create: `/tmp/candidate_removal.txt` (scratch)

**Step 1: Extract exported function names from NAMESPACE**

```bash
grep "^export(" /Users/hidekoji/Work/gitrepo/exploratory_func/NAMESPACE \
  | sed 's/export(\(.*\))/\1/' \
  | sort > /tmp/exported_functions.txt

wc -l /tmp/exported_functions.txt
```

Expected: 543 lines.

**Step 2: Compute the difference (exported but not used)**

```bash
comm -23 /tmp/exported_functions.txt /tmp/used_functions.txt > /tmp/candidate_removal.txt
wc -l /tmp/candidate_removal.txt
```

Expected: ~360–380 lines.

**Step 3: Sanity-check — confirm used functions are in the exported set**

```bash
# These should ALL appear in exported_functions.txt
comm -13 /tmp/exported_functions.txt /tmp/used_functions.txt
```

Expected: empty output (or a handful of operator/re-exported names like `%>%`). If non-empty, investigate — these are consumer calls to functions not currently exported (possible bug or search miss).

---

### Task 3: Check internal dependencies

**Files:**
- Create: `/tmp/internal_deps.txt` (candidates that are called internally — must keep as unexported helpers)

**Step 1: For each candidate, grep R/ source for calls to it**

```bash
cd /Users/hidekoji/Work/gitrepo/exploratory_func

while IFS= read -r func; do
  # Match function call pattern: funcname( or funcname (
  # Exclude lines that define the function itself
  matches=$(grep -rn "\b${func}\b\s*(" R/ --include="*.R" \
    | grep -v "^\(.*\)${func}\s*<-\s*function" \
    | grep -v "^\(.*\)${func}\s*=\s*function" \
    | grep -v "^#" \
    | head -3)
  if [ -n "$matches" ]; then
    echo "=== $func ==="
    echo "$matches"
  fi
done < /tmp/candidate_removal.txt > /tmp/internal_deps_raw.txt

cat /tmp/internal_deps_raw.txt
```

**Step 2: From the raw output, identify true internal dependencies**

Review `/tmp/internal_deps_raw.txt`. For each function that appears to be called from another R function (not just from its own test or definition), add it to the internal deps list:

```bash
# Manually reviewed list — run after inspecting internal_deps_raw.txt
# Example (populate after review):
# echo "my_helper_fn" >> /tmp/internal_deps.txt
```

**Step 3: Produce final DELETE list (candidates minus internal deps)**

```bash
comm -23 \
  <(sort /tmp/candidate_removal.txt) \
  <(sort /tmp/internal_deps.txt) \
  > /tmp/delete_list.txt

wc -l /tmp/delete_list.txt
cat /tmp/delete_list.txt
```

**Step 4: Produce KEEP-AS-INTERNAL list**

```bash
cat /tmp/internal_deps.txt | sort > /tmp/keep_as_internal.txt
```

---

### Task 4: For each R source file, identify which functions to delete

**Files:**
- Read: All `R/*.R` files (as needed)

**Step 1: Map each DELETE function to its source file**

```bash
cd /Users/hidekoji/Work/gitrepo/exploratory_func

while IFS= read -r func; do
  file=$(grep -rl "^${func}\s*<-\s*function\|^${func}\s*=\s*function\|^#' @rdname ${func}" R/ --include="*.R" | head -1)
  if [ -n "$file" ]; then
    echo "$file|$func"
  else
    echo "NOT_FOUND|$func"
  fi
done < /tmp/delete_list.txt | sort > /tmp/func_to_file_map.txt

# Review any NOT_FOUND entries
grep "^NOT_FOUND" /tmp/func_to_file_map.txt
```

**Step 2: Group by file**

```bash
awk -F'|' '{print $1}' /tmp/func_to_file_map.txt | sort -u
```

This shows which files need editing. Process them in the next task.

---

### Task 5: Remove DELETE functions from R source files (repeat per file)

For each file identified in Task 4, perform the following. This task is repeated once per affected file.

**Files:**
- Modify: `R/<filename>.R`

**Step 1: Read the file and identify the roxygen + function blocks to remove**

For each function on the delete list in this file, locate:
- The opening `#'` roxygen comment block
- The `function_name <- function(...)` definition
- The closing `}` of the function body

**Step 2: Remove those blocks**

Use the Edit tool to remove each identified block. Be careful to:
- Remove the complete roxygen comment block above the function
- Remove from the first `#'` of that function's docs to the closing `}` of the function body
- Leave a blank line between retained functions
- Do NOT remove functions that are in the keep list

**Step 3: If the file becomes empty (all functions removed), delete the file**

```bash
# Check if only whitespace/comments remain
wc -l R/<filename>.R
```

If effectively empty, delete it:
```bash
git rm R/<filename>.R
```

**Step 4: Commit this file's removals**

```bash
git add R/<filename>.R
git commit -m "refactor: remove unused exported functions from <filename>.R"
```

---

### Task 6: Convert KEEP-AS-INTERNAL functions (strip @export)

**Files:**
- Modify: relevant `R/*.R` files (those containing KEEP-AS-INTERNAL functions)

**Step 1: For each function in `/tmp/keep_as_internal.txt`, find its file**

```bash
while IFS= read -r func; do
  grep -rn "@export" R/ --include="*.R" -l | xargs grep -l "\b${func}\b"
done < /tmp/keep_as_internal.txt
```

**Step 2: In each file, remove the `@export` line from the roxygen block**

Use the Edit tool: find the line `#' @export` immediately above or near the function definition and remove it.

**Step 3: Commit**

```bash
git add R/*.R
git commit -m "refactor: convert internal-dep functions to unexported helpers"
```

---

### Task 7: Rebuild NAMESPACE and verify export count

**Step 1: Regenerate NAMESPACE**

```bash
cd /Users/hidekoji/Work/gitrepo/exploratory_func
Rscript -e "devtools::document()"
```

Expected: no errors; NAMESPACE file updated.

**Step 2: Count remaining exports**

```bash
grep "^export(" NAMESPACE | wc -l
```

Expected: ~175 (±adjustment for internal deps).

**Step 3: Confirm no DELETE-list function appears in NAMESPACE**

```bash
while IFS= read -r func; do
  if grep -q "^export(${func})$" NAMESPACE; then
    echo "STILL EXPORTED: $func"
  fi
done < /tmp/delete_list.txt
```

Expected: no output.

**Step 4: Commit updated NAMESPACE**

```bash
git add NAMESPACE man/
git commit -m "refactor: regenerate NAMESPACE after removing unused exports"
```

---

### Task 8: Run tests and fix any failures

**Step 1: Run the full test suite**

```bash
cd /Users/hidekoji/Work/gitrepo/exploratory_func
Rscript -e "devtools::test()"
```

**Step 2: If tests fail because they reference a removed function**

Identify the test file and the test case. If the test only exercises a now-removed function, delete the test. If the test exercises a retained function that incidentally called a removed helper, investigate whether the helper needs to be kept as internal.

```bash
# For each failing test file:
git rm tests/testthat/test_<removed_feature>.R
git commit -m "test: remove tests for deleted functions"
```

**Step 3: Re-run until green**

```bash
Rscript -e "devtools::test()"
```

Expected: all tests pass, 0 failures.

---

### Task 9: Run R CMD check

**Step 1: Full package check**

```bash
cd /Users/hidekoji/Work/gitrepo/exploratory_func
Rscript -e "devtools::check()"
```

Expected: 0 ERRORs, 0 WARNINGs (NOTEs about package size are acceptable).

**Step 2: If check fails due to missing documentation for removed functions**

```bash
# Re-run document() if needed
Rscript -e "devtools::document()"
git add man/
git commit -m "docs: remove man pages for deleted functions"
```

---

### Task 10: Final commit and summary

**Step 1: Confirm clean state**

```bash
git status
git diff --stat HEAD~10..HEAD
```

**Step 2: Count before/after**

```bash
echo "Exports now: $(grep '^export(' NAMESPACE | wc -l)"
echo "Exports before: 543"
```

**Step 3: Update the design doc status**

Edit `docs/plans/2026-03-25-remove-unused-exported-functions.md`:
- Change `**Status:** Planning` to `**Status:** Implemented`
- Add a section: `## Actual Results` with the final export count and any deviations from the plan.

**Step 4: Commit doc update**

```bash
git add docs/plans/
git commit -m "docs: mark removal of unused exports as implemented"
```

---

## Notes for Implementer

- **S3 methods** (`S3method(...)` in NAMESPACE) are separate from `export()` entries — do not remove S3method registrations unless you are also removing the generic or the class it applies to.
- **Re-exported tidyverse operators** (`%>%`, `%in%`, etc.) are used implicitly by tam/datablog even without `exploratory::` prefix — keep them.
- If a function is a roxygen `@rdname` alias, both the primary function and the alias must be removed together.
- When in doubt about a borderline function, keep it as internal rather than delete.
