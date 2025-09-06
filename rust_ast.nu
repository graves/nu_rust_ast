# =============================================================================
# rust-ast toolkit — Harvest, analyze, and pretty-print Rust symbols & calls
# =============================================================================
#
# OVERVIEW
# --------
# CLI-oriented Nushell helpers that scan a Rust crate (or given paths) with
# ast-grep and produce:
#   1) A FLAT TABLE of symbols with rich, normalized metadata (`rust-ast`)
#   2) A NESTED SYMBOL TREE for pretty printing (`rust-tree` -> `print-symbol-tree`)
#   3) A CALL GRAPH (both callees-of and callers-of) with cycle/dup handling
#      (`print-call-graph`, internal walkers)
#   4) DEPENDENCY USAGE TREES per external crate, including grouped uses,
#      alias resolution, and optional JSON-like nested records (`print-dep-usage`)
#
# The tooling is designed so you can compose commands in pipelines, or call the
# printers directly. All printers accept piped input where noted.
#
# =============================================================================
# 1) SYMBOL HARVESTING (FLAT TABLE)
# =============================================================================
#
#   rust-ast [...paths]
#
# Scans Rust sources and emits a single, flat table of symbols:
#   kind ∈ { fn | extern_fn | struct | enum | type | trait | impl
#            | mod | macro_rules | const | static | use }
#
# For nodes with bodies (fns, struct-with-fields, traits, impls, inline mods),
# `body_text` is captured verbatim. Tuple/unit structs, trait item decls, and
# `mod foo;` have `body_text = null`. File modules (src/foo.rs, src/foo/mod.rs)
# get a synthesized `mod` row covering the full file, with inner file docs.
#
# IMPORTANT INVARIANTS
# --------------------
# - Line spans (span.start_line / end_line) are 1-based and inclusive (ast-grep).
# - Byte offsets are inclusive start / exclusive end (ast-grep).
# - UFCS fqpaths are computed for impl methods:
#     crate::<MyTy as my::Trait>::method   (trait impl)
#     crate::MyTy::method                  (inherent impl)
# - We attach leading rustdoc/#[doc] blocks to `doc`. For file modules we also
#   capture top-of-file inner docs (//! or /*! ... */).
#
# OUTPUT SCHEMA (per row)
# -----------------------
# kind, name, crate, module_path[], fqpath, visibility,
# file, span{start_line,end_line,start_byte,end_byte},
# attrs[], signature, has_body, async|unsafe|const (bool flags),
# abi|generics|where (optional strings),
# doc?, impl_of?{trait_path?,type_path?}, trait_items[], reexports[],
# body_text?, synthetic?, doc_tokens?, body_tokens?
#
# TOKEN COUNTING
# --------------
# Token counts are heuristic and configurable via $env.RUST_AST_TOKENIZER:
#   - "words"   (default): rough word-based tokens
#   - "chars"   : chars/4
#   - "tiktoken": use tiktoken model (falls back to "words" if unavailable)
#
# EXTERNAL-CRATE DETECTION
# ------------------------
# We parse the nearest Cargo.toml to identify *external* crates from:
#   [dependencies], [dev-dependencies], [build-dependencies], and
#   [target.*.dependencies], ignoring path/workspace deps.
#
# =============================================================================
# 2) NESTED SYMBOL TREE & PRINTER
# =============================================================================
#
#   rust-tree [...paths] [--include-use]
#
# Produces a *minimal* nested tree of records for the crate rooted at "crate":
#   { kind, name, fqpath, children: [...] }
#
#   rust-tree | print-symbol-tree [--fq-branches] [--tokens]
#
# Pretty-prints the nested tree with columns:
#   - Name:    tree branches + symbol name (colorized by kind)
#   - Kind:    aligned, colorized kind
#   - FQ Path: shown for leaves; with --fq-branches also for branch nodes
#   - Tokens:  optional rightmost column ("Body Tokens, Doc Tokens") when
#              --tokens is supplied; widths are aligned per column.
#
# Notes:
# - Color is via `ansi`; alignment uses visible-length (`ansi strip`) so spacing
#   stays correct with or without color.
# - FQ paths are printed as plain text (no brackets).
#
# Examples:
#   rust-tree | print-symbol-tree
#   rust-tree | print-symbol-tree --fq-branches
#   rust-tree | print-symbol-tree --tokens
#
# =============================================================================
# 3) CALL GRAPH (CALLEES / CALLERS)
# =============================================================================
#
# Internals build a canonicalized adjacency (per-fqpath with generics/whitespace
# stripped segment-wise) and a map from canonical → real fqpaths so display shows
# real names even when multiple canonical variants exist.
#
#   print-call-graph <pattern>
#     [--max-depth <N>=3] [--reverse] [--show-roots]
#
# - <pattern> can be an exact fqpath ("crate::api::ask"), a module tail
#   ("::ask"), or a bare name ("ask"); multiple matches will render sequentially.
# - Forward (default): "→ callees of" graph, rooted at seed(s)
# - Reverse (--reverse): "← callers of" graph, rooted at seed(s)
# - --show-roots prints a one-line header for each root
# - Leaf column shows the *name* (last path segment) and the full fqpath in
#   brackets, e.g.:
#       ask  [crate::api::ask]
#
# CYCLES & DUPLICATES
# -------------------
# Walkers track a visited set and annotate loops (⟲ cycle). Repeated nodes are
# printed once per path and not expanded again, preventing combinatorial blowups.
#
# =============================================================================
# 4) DEPENDENCY USAGE (EXTERNAL CRATES)
# =============================================================================
#
#   print-dep-usage [<crate>] [--max-depth <N>=4] [--include-maybe] [--records]
#
# Scans functions to find references to external crates, using:
#   - explicit paths (e.g., `serde::Serialize::serialize`)
#   - module-local aliases introduced by `use` (e.g., `use serde_json as sj;`)
#   - optional glob imports (`use serde::*;`) gated via `--include-maybe`
#
# Output modes:
#   (default) Pretty text per crate with callers-of trees for each seed function:
#       Dependency usage: serde
#       direct references
#       serialize  [crate::my_mod::serialize] uses: Serialize
#       ├─ ...
#       └─ ...
#
#   --records  Emits *nested* records instead of text. For each crate:
#       [
#         {
#           kind, name, fqpath, children: [...]
#           (leaf nodes merged with { dep, ref_type: "real"|"maybe", uses: [...] })
#         }
#       ]
#     If you pass a specific <crate>, the command returns just that crate's tree.
#
# Filtering & heuristics:
#   - "direct references" are concrete symbol paths resolved to that crate
#   - With --include-maybe, possible references via glob imports are included,
#     but *only* when they lie on a callers-path that reaches a direct reference.
#
# DISPLAY DETAILS
# ---------------
# - For all call/dependency trees the first column prints the *leaf name*
#   (short `name`), and the full fqpath is shown in brackets for clarity.
# - Parents/continuation branches use ASCII/Unicode lines; repeated ancestors
#   are marked with ⟲ and not expanded again.
#
# =============================================================================
# 5) RUST `use` HANDLING
# =============================================================================
#
#   rust-use-records
#
# Emits one row per `use` statement with normalized `signature` and `fqpath`.
# Handles:
#   - grouped imports:  use foo::{bar, baz::Qux as Alias, *};
#   - aliasing:         use serde_json as sj;
#   - base resolution:  `crate` is normalized against the module path
#   - grouped leaves:   a synthetic '*' name is used where appropriate
#
# Auxiliary expanders:
#   - `_expand-grouped-use`, `_expand-group-item` split grouped uses into leaves
#   - `_alias-map-by-module` maps visible alias/bindings → external crate names
#   - `_ext_globs_by_module` records external crates imported via `::*` per module
#
# =============================================================================
# 6) COMPOSITION & PIPING
# =============================================================================
#
# - Most printers accept piped rows from `rust-ast` to avoid re-scanning:
#     rust-ast | print-dep-usage serde
#     rust-ast | print-dep-usage --records | to json
#
# - `print-symbol-tree` expects the *nested* shape from `rust-tree`:
#     rust-tree | print-symbol-tree --tokens
#
# =============================================================================
# 7) QUICK RECIPES
# =============================================================================
#
# - Show a colorized, aligned tree of your crate (no uses):
#     rust-tree | print-symbol-tree
#
# - Same, with fqpath on every node and token counts aligned to the right:
#     rust-tree | print-symbol-tree --fq-branches --tokens
#
# - Who calls `crate::api::prepare_messages` (up to 5 levels)?
#     print-call-graph crate::api::prepare_messages --reverse --max-depth 5 --show-roots
#
# - Where do we reference `serde`? (pretty)
#     print-dep-usage serde --max-depth 5 --include-maybe
#
# - Where do we reference `serde`? (structured)
#     rust-ast | print-dep-usage serde --records | to json
#
# =============================================================================
# IMPLEMENTATION NOTES (highlights)
# =============================================================================
#
# - Canonicalization: `_fq_canon` strips generics/whitespace per segment so
#   adjacencies remain stable across monomorphizations; display uses `canon2real`
#   to recover a representative real path.
#
# - Name vs FQ display: Tree/graph printers show the short `name` (leaf) in the
#   first column and the full `fqpath` in brackets. This improves scanability.
#
# - Cycle/dup guarding: Callers/callees renderers track a `seen` set; previously
#   visited nodes are shown once with a ⟲ marker and not expanded again.
#
# - Performance: Most builders accept piped input to avoid re-running `rust-ast`.
#   `print-dep-usage --records` shares the row index with the tree builder to
#   prevent re-computation.
#
# =============================================================================
export def rust-ast [...paths:string] {
  [
    (rust-fn-records ...$paths)
    (rust-extern-fn-records ...$paths)
    (rust-struct-records ...$paths)
    (rust-enum-records ...$paths)
    (rust-type-records ...$paths)
    (rust-trait-records ...$paths)
    (rust-impl-records ...$paths)
    (rust-mod-records ...$paths)
    (rust-file-mod-records ...$paths)
    (rust-macro-records ...$paths)
    (rust-const-records ...$paths)
    (rust-static-records ...$paths)
    (rust-use-records ...$paths)
  ]
  | flatten
  | _attach_impl_to_fns
  | _uniq-records
  | _attach_callers
}

# # Nested structure of symbols — MINIMAL payload (kind, name, fqpath, children).
# This is exactly what `print-symbol-tree` expects to render/align/paint columns.
export def rust-tree [
  ...paths:string
  --include-use
] {
  let rows_all  = (rust-ast ...$paths)
  let rows_base = if $include_use { $rows_all } else { $rows_all | where kind != 'use' }

  # Build parent→children adjacency and a minimal index
  let edges = (_build-symbol-edges $rows_base)
  let idx   = (_rows-index $rows_base)

  # Build minimal children under 'crate'
  let root_kids_fq = (_children-for $edges 'crate')
  let root_kids    = (
    $root_kids_fq
    | each {|cfq| _build-subtree $idx $edges $cfq }
    | where {|x| (($x | describe) =~ '^record<') }
  )

  # Minimal root record for output (only one list-typed column: `children`)
  let root_rec = {
    kind: 'mod'
    name: 'crate'
    fqpath: 'crate'
    children: $root_kids
  }

  [ $root_rec ]
}

# =============================================================================
# print-symbol-tree — Pretty-print a nested Rust symbol tree with columns
# =============================================================================
# Works with the nested output from `rust-tree`.
#
# Columns:
#   - Name (tree/ASCII branches + symbol name; name text is colorized by kind)
#   - Kind (colorized and padded; e.g., fn, struct, enum, impl, mod, …)
#   - FQ Path (shown for leaves; with `--fq-branches` also shown on branches)
#   - Tokens (optional; with `--tokens`, shows "Body Tokens: N, Doc Tokens: M")
#
# Color:
#   Names and kinds are colorized via `_paint-kind`, which uses `ansi`.
#   Column widths are computed using `_vlen`, which strips ANSI codes so
#   alignment remains correct even when color is enabled.
#
# FQ Path formatting:
#   FQ paths are printed as plain text (no brackets).
#
# Usage:
#   rust-tree | print-symbol-tree
#   rust-tree | print-symbol-tree --fq-branches
#   rust-tree | print-symbol-tree --tokens
#   rust-tree | print-symbol-tree --fq-branches --tokens
#
# Options:
#   --fq-branches    Show fqpath on branch nodes too (defaults to leaves only).
#   --tokens         Add a rightmost column with body/doc token counts.
#
# Notes:
#   - This command expects a *nested* node (or list of nodes) from `rust-tree`.
#     It tolerates a single record, a list/table of records, or a JSON string.
#   - If your terminal or platform doesn't support ANSI, you'll still get
#     correct spacing (we measure visible length with `ansi strip`).
# =============================================================================
export def print-symbol-tree [
  --fq-branches
  --tokens                     # <-- new flag
] {
  let input = $in
  let roots = (_roots-of $input)
  if ($roots | is-empty) {
    error make { msg: "print-symbol-tree: input contains no records" }
  }

  let rows = (
    $roots
    | each {|r| _collect-rows $r [] true }
    | flatten
  )

  let tok_idx = if $tokens { _build-token-index } else { null }

  _print-with-columns $rows ($fq_branches | default false) $tok_idx
}


# ---------- helpers -----------------------------------------------------------

# Normalize CLI paths: empty => ["."], else pass-through.
def _target-list [...paths:string] {
  if ($paths | is-empty) { [ "." ] } else { $paths }
}

# Read Cargo.toml package.name (best effort). Falls back to "crate".
def _cargo-crate-name [] {
  try {
    # open --raw Cargo.toml | from toml | get package.name
    open Cargo.toml | get package.name     # if you prefer implicit parse
  } catch { "crate" }
}

# Find the nearest Cargo.toml by walking up from pwd
def _find-cargo-root [] {
  mut cur = (pwd)
  loop {
    let cand = ([$cur "Cargo.toml"] | path join)
    let typ  = (try { $cand | path type } catch { null })

    if $typ == 'file' {
      return { root: $cur, cargo: $cand }
    }

    let parent = ($cur | path dirname)
    if $parent == $cur { break }   # reached fs root
    $cur = $parent
  }
  null
}

# Read Cargo.toml (nearest), return {} if none
def _read-cargo-toml [] {
  let loc = (_find-cargo-root)
  if $loc == null { {} } else {
    try {
      # EITHER explicit:
      open --raw $loc.cargo | from toml
      # OR implicit (no from toml):
      # open $loc.cargo
    } catch {|e|
      {}
    }
  }
}

# --- replace your _external-crate-set with this ---
def _external-crate-set [] {
  let toml = (_read-cargo-toml)

  def _g [rec key] { $rec | get -i $key | default {} }
  def _keys [x] { if (($x | describe) =~ '^record<') { $x | columns } else { [] } }

  def _is-ext [v] {
    let t = ($v | describe)
    if $t == 'string' {
      true
    } else if ($t | str starts-with 'record<') {
      let has_path = (try { $v | get -i path } catch { null }) != null
      let has_ws   = (try { $v | get -i workspace } catch { null }) != null
      (not $has_path) and (not $has_ws)
    } else { false }
  }

  def _dep-keys [t] {
    let sections = [
      (_g $t dependencies)
      (_g $t 'dev-dependencies')
      (_g $t 'build-dependencies')
      ( (_g $t target | values | each {|sec| _g $sec "dependencies"} ) | flatten | default {} )
    ]
    $sections
    | each {|rec|
        _keys $rec
        | where {|k| _is-ext (try { $rec | get $k } catch { null }) }
      }
    | flatten
  }

  _dep-keys $toml | uniq | sort
}

# O(1) membership
def _external-crate-map [] {
  _external-crate-set
  | reduce -f {} {|name, acc| $acc | upsert $name true }
}

# Convert file path to "module path" (Vec<String>) rooted at src/.
# - src/lib.rs and src/main.rs => [] (crate root modules)
# - src/foo.rs       => ["foo"]
# - src/foo/mod.rs   => ["foo"]
# - src/a/b.rs       => ["a","b"]
# - src/a/b/mod.rs   => ["a","b"]
def _module-path-from-file [file:string] {
  let p = ($file | path expand)
  let parts = ($p | path split)

  # locate "src" segment; if missing, return []
  let src_idx = (
    $parts
    | enumerate
    | where item == "src"
    | get index
    | get 0?
    | default (-1)
  )

  if $src_idx == -1 { [] } else {
    # take tail after src/
    let tail = ($parts | skip ($src_idx + 1))
    if ($tail | is-empty) { [] } else {
      let filename = ($tail | last)
      # crate root files carry no module path components
      if $filename in ["lib.rs", "main.rs"] { [] } else {
        # mod.rs => drop the filename
        if ($filename == "mod.rs") { $tail | drop 1 } else {
          # foo.rs => strip .rs; keep intermediate dirs
          $tail
          | each {|s|
              if ($s | str ends-with ".rs") { $s | str replace -r '\.rs$' '' } else { $s }
            }
        }
      }
    }
  }
}

# Best-effort visibility classifier based on the *signature* text.
def _visibility-of [sig:string] {
  let s = ($sig | into string)
  if ($s | str starts-with 'pub(crate)') {
    'pub(crate)'
  } else if ($s | str starts-with 'pub(super)') {
    'pub(super)'
  } else if ($s | str starts-with 'pub(') {
    'pub(in …)'
  } else if ($s | str starts-with 'pub ') {
    'pub'
  } else { 'private' }
}

# Crude "has body" check: we only set true if there's a '{' and not a trailing ';').
def _has-body [text: string] {
  let t = ($text | default '' | into string)
  if ($t | str ends-with ';') {
    false
  } else {
    $t | str contains '{'
  }
}

# Normalize the first-line "signature" of a snippet (trim whitespace/comments after '{' or ';').
def _sigline [text: string] {
  let t = ($text | default '' | into string | str trim)
  if ($t | str contains '{') {
    $t | split row '{' | get 0 | str trim | str replace -ra '\s+' ' '
  } else if ($t | str contains ';') {
    $t | split row ';' | get 0 | str trim | str replace -ra '\s+' ' '
  } else {
    $t | lines | get 0 | str trim | str replace -ra '\s+' ' '
  }
}

# ---- sg I/O helpers ----------------------------------------------------------

# Run ast-grep safely (returns error if neither `sg` nor `ast-grep`) works.
def _run_sg [...args:string] {
  try {
    ^sg ...$args
  } catch {
    try {
      ^ast-grep ...$args
    } catch {
      error make -u { msg: "ast-grep (`sg`/`ast-grep`) not found or failed", label: { text: (['sg' 'ast-grep'] | str join ' / ') } }
    }
  }
}

# Parse ast-grep --json=stream output into a flat list of records.
def _parse_sg_json [] {
  let v = ($in | default "")
  let t = ($v | describe)

  if $t == 'nothing' {
    []
  } else if $t == 'string' {
    $v
    | lines
    | where {|l| ($l | str length) > 0 }
    | each {|l| (try { $l | from json } catch { null }) }
    | where {|x| $x != null }
  } else if ($t | str starts-with 'list<string>') {
    $v
    | each {|l| (try { $l | from json } catch { null }) }
    | where {|x| $x != null }
  } else if ($t | str starts-with 'record<') {
    [ $v ]
  } else if ($t | str starts-with 'list<record') {
    $v
  } else {
    []
  }
}

# Convenience wrappers for sg that always return parsed records.
def _sg_json [pattern:string, ...paths:string] {
  let target = (_target-list ...$paths)
  (_run_sg 'run' '-l' 'rust' '-p' $pattern '--json=stream' '--heading=never' '--color=never' ...$target)
  | _parse_sg_json
}

def _sg_rewrite [pattern:string, rewrite:string, ...paths:string] {
  let target = (_target-list ...$paths)
  (_run_sg 'run' '-l' 'rust' '-p' $pattern '-r' $rewrite '--json=stream' '--heading=never' '--color=never' ...$target)
  | _parse_sg_json
  | get replacement
  | default []
}

def _sg_text [pattern:string, ...paths:string] {
  let target = (_target-list ...$paths)
  (_run_sg 'run' '-l' 'rust' '-p' $pattern '--json=stream' '--heading=never' '--color=never' ...$target)
  | _parse_sg_json
  | get text
  | default []
}

# Versions that accept an already-expanded list of file targets.
def _sg_json_on [pattern:string, targets:list<string>] {
  (_run_sg 'run' '-l' 'rust' '-p' $pattern '--json=stream' '--heading=never' '--color=never' ...$targets)
  | _parse_sg_json
}

def _sg_rewrite_on [pattern:string, rewrite:string, targets:list<string>] {
  (_run_sg 'run' '-l' 'rust' '-p' $pattern '-r' $rewrite '--json=stream' '--heading=never' '--color=never' ...$targets)
  | _parse_sg_json
}

def _sg_text_on [pattern:string, targets:list<string>] {
  (_run_sg 'run' '-l' 'rust' '-p' $pattern '--json=stream' '--heading=never' '--color=never' ...$targets)
  | _parse_sg_json
}

# Map many (pattern,rewrite) pairs through sg -r and flatten unique results.
def _rewrite-many [pairs:list<record<p: string, r: string>>, ...paths:string] {
  $pairs
  | each {|it| _sg_rewrite $it.p $it.r ...$paths }
  | flatten
  | uniq
  | sort
}

# --- Token count helpers ------------------------------------------------------

def _tok_wordish [s?: string] {
  let t = ($s | default "" | into string | str trim)
  if $t == "" { 0 } else { ($t | split row -r '\s+' | length) }
}

def _token-count [s?: string, model?: string] {
  let mode = ($env.RUST_AST_TOKENIZER | default "words")

  if $mode == "tiktoken" {
    let exact = (_token-count-via-tiktoken $s ($model | default "cl100k_base"))
    if $exact != null { $exact } else { _tok_wordish $s }
  } else if $mode == "chars" {
    let t = ($s | default "" | into string)
    if ($t == "") { 0 } else { ((($t | str length) + 3) / 4 | into int) }
  } else {
    _tok_wordish $s
  }
}

# Item (outer) rustdoc just above a node: return the exact lines verbatim.
def _extract-rustdoc [raw: record] {
  let file       = ($raw.file | into string)
  let start_line = ($raw.range.start.line | default 1)
  if $start_line <= 1 { return "" }

  let lines = (try { open $file | into string | lines } catch { [] })
  if ($lines | is-empty) { return "" }

  mut i   = ($start_line - 2)
  mut acc = []

  while $i >= 0 {
    let raw_line = ($lines | get $i)
    let t = ($raw_line | str trim)

    if $t == "" { break }

    if ($t | str starts-with "///") {
      $acc = ([$raw_line] | append $acc)
      $i = ($i - 1)
      continue
    }

    if (($t | str starts-with "#[") and ($t | str contains "doc")) {
      $acc = ([$raw_line] | append $acc)
      $i = ($i - 1)
      continue
    }

    if (($t | str ends-with "*/") and ($t | str contains "/*")) {
      mut j = $i
      mut block = []
      loop {
        if $j < 0 { break }
        let l2 = ($lines | get $j)
        $block = ([$l2] | append $block)
        if ((($l2 | str trim) | str starts-with "/**")) { break }
        $j = ($j - 1)
      }
      $acc = ($block | append $acc)
      $i = ($j - 1)
      continue
    }

    break
  }

  ($acc | str join "\n")
}

# Crate/file inner docs at the top of a file (//! or /*! ... */), verbatim.
def _extract-file-mod-doc [file: string] {
  let lines = (try { open $file | into string | lines } catch { [] })
  if ($lines | is-empty) { return "" }

  mut i = 0
  mut acc = []

  if ((($lines | get 0 | default "" ) | str starts-with "#!")) { $i = 1 }

  loop {
    if $i >= ($lines | length) { break }
    let raw_line = ($lines | get $i)
    let t = ($raw_line | str trim)

    if $t == "" { break }

    if ($t | str starts-with "//!") {
      $acc = ($acc | append $raw_line)
      $i = ($i + 1)
      continue
    }

    if ($t | str starts-with "/*!") {
      mut j = $i
      loop {
        if $j >= ($lines | length) { break }
        let l2 = ($lines | get $j)
        $acc = ($acc | append $l2)
        if ((($l2 | str trim) | str ends-with "*/")) { break }
        $j = ($j + 1)
      }
      $i = ($j + 1)
      continue
    }

    break
  }

  ($acc | str join "\n")
}

# Given the flat rows, annotate fn rows with enclosing impl info (if any) and
# compute a better fqpath that disambiguates trait impl methods (via UFCS).
def _attach_impl_to_fns [rows?: list<record>] {
  let rows = if ($rows | is-empty) { $in } else { $rows }

  let impls  = ($rows | where kind == 'impl')
  let fns    = ($rows | where kind == 'fn')
  let others = ($rows | where {|r| $r.kind != 'fn' and $r.kind != 'impl' })

  let annotated_fns = (
    $fns | each {|f|
      let encl = (
        $impls
        | where file == $f.file
        | where {|i|
            (($i.span.start_byte | default 0) <= ($f.span.start_byte | default 0)) and (($i.span.end_byte   | default 0) >= ($f.span.end_byte   | default 0))
          }
        | sort-by {|i| ($i.span.end_byte | default 0) - ($i.span.start_byte | default 0) }
        | get 0?
      )

      if $encl == null {
        $f
      } else {
        let trait_path = ($encl.impl_of.trait_path | default null)
        let type_path  = ($encl.impl_of.type_path  | default null)
        let modp       = ($f.module_path | default [])
        let modp_str   = (if ($modp | is-empty) { "" } else { ($modp | str join '::') })

        let fq = if $trait_path != null and ($trait_path | str length) > 0 and $type_path != null and ($type_path | str length) > 0 {
          if ($modp | is-empty) {
            $"crate::<($type_path) as ($trait_path)>::($f.name)"
          } else {
            $"crate::($modp_str)::<($type_path) as ($trait_path)>::($f.name)"
          }
        } else if $type_path != null and ($type_path | str length) > 0 {
          if ($modp | is-empty) {
            $"crate::($type_path)::($f.name)"
          } else {
            $"crate::($modp_str)::($type_path)::($f.name)"
          }
        } else {
          $f.fqpath
        }

        $f
        | upsert impl_of $encl.impl_of
        | upsert fqpath $fq
      }
    }
  )
  [$others, $impls, $annotated_fns] | flatten
}

# Call sites with qualifiers captured when present
def _rust-call-sites-on [targets:list<string>] {
  let files = ($targets | where {|f| ($f | default null) != null } | uniq)

  let pats = [
    '$N($$$A)'
    '$Q::$N($$$A)'
    '$RECV.$N($$$A)'
  ]

  mut out = []
  for p in $pats {
    let rows = (
      _sg_json_on $p $files
      | each {|raw|
          let s = ($raw.metaVariables.single? | default {})

          let n = ($s | get -i N | default {} | get -i text | default null)
          if $n == null { null } else {
            let has_q    = (($s | get -i Q | default null)    != null)
            let has_recv = (($s | get -i RECV | default null) != null)
            let qual_val = if $has_q { ($s | get -i Q    | get -i text | default '') } else if $has_recv { ($s | get -i RECV | get -i text | default '') } else ''

            {
              callee: $n
              qual: $qual_val
              kind: (if $has_q { 'qualified' } else if $has_recv { 'method' } else { 'plain' })
              file: ($raw.file | into string)
              span: {
                start_byte: ($raw.range.byteOffset.start | default 0)
                end_byte:   ($raw.range.byteOffset.end   | default 0)
              }
            }
          }
        }
      | where {|x| $x != null }
    )
    $out = ($out | append $rows)
  }

  $out | reduce -f [] {|it, acc| $acc | append $it }
}

# rows: the full table you already produce
# Accept rows from arg or pipeline and attach a disambiguated 'callers' list
def _attach_callers [rows?: list<record>] {
  let rows0 = if ($rows | is-empty) { $in } else { $rows }
  let rows = ($rows0 | where {|r| ($r | describe) =~ '^record<' })

  let fns = ($rows | where {|r| ($r | get -i kind | default '') == 'fn' })

  let files = (
    $rows
    | each {|r| ($r | get -i file | default null) }
    | where {|f| $f != null }
    | uniq
  )

  let calls    = (_rust-call-sites-on $files)
  let fn_index = (_index-fns-by-file $fns)
  let idx      = (_build-fn-indexes $fns)

  let pairs = (
    $calls
    | each {|c|
        let caller = (_enclosing-fn $fn_index $c.file ($c.span.start_byte | default 0) ($c.span.end_byte | default 0))
        if $caller == null { null } else {
          let target = (_resolve-call $idx $fns $c $caller)
          if $target == null { null } else {
            { callee_fq: ($target | get -i fqpath | default '')
            , caller_fq: ($caller | get -i fqpath | default '') }
          }
        }
      }
    | where {|x| $x != null }
  )

  let callee_to_callers = (
    $pairs
    | group-by callee_fq
    | transpose fq callers
    | each {|g|
        { fq: $g.fq
        , callers: ($g.callers | get caller_fq | where {|v| ($v | default '') != '' } | uniq | sort) }
      }
  )

  $rows
  | each {|r|
      let t = ($r | describe)
      if ($t =~ '^record<') {
        let kind = ($r | get -i kind | default '')
        if $kind != 'fn' {
          $r
        } else {
          let fq  = ($r | get -i fqpath | default '')
          let ent = ($callee_to_callers | where fq == $fq | get 0?)
          if ( ($ent | describe) =~ '^record<' ) {
            ($r | upsert callers ($ent.callers | default []))
          } else {
            ($r | upsert callers [])
          }
        }
      } else {
        null
      }
    }
  | where {|x| $x != null }
}

# Find smallest enclosing fn for a call site (same file; span containment)
def _enclosing-fn [
  fn_index:list<record<file: string, items: list>>,
  file:string,
  s:int,
  e:int
] {
  let matches = ($fn_index | where file == $file)
  if ($matches | is-empty) {
    null
  } else {
    let bucket = ($matches | get 0 | get items | default [])
    $bucket
    | where {|r|
        (($r.span.start_byte | default 0) <= $s) and (($r.span.end_byte   | default 0) >= $e)
      }
    | sort-by {|r| ($r.span.end_byte - $r.span.start_byte)}
    | get 0?
  }
}

# Given FN rows, return an index { file -> [fn rows sorted by span size asc] }
def _index-fns-by-file [fns:list<record>] {
  $fns
  | group-by file
  | transpose file items
  | each {|it|
      { file: $it.file, items: ($it.items | sort-by {|r| ($r.span.end_byte - $r.span.start_byte) }) }
    }
}

# Group functions by quick keys we'll use for resolution
def _build-fn-indexes [fns:list<record>] {
  let by_fqpath = ($fns | group-by fqpath | transpose key vals)

  let impl_methods = (
    $fns
    | where {|r| ($r | get -i impl_of | default {} | get -i type_path | default '') != '' }
    | each {|r| { key: { ty: ($r.impl_of | get -i type_path), name: $r.name }, row: $r } }
    | group-by {|x| $"($x.key.ty)::($x.key.name)" }
    | transpose key vals
  )

  let free_fns = (
    $fns
    | where {|r| ($r | get -i impl_of | default null) == null }
    | each {|r| { key: { mod: ($r.module_path | default [] | str join '::'), name: $r.name }, row: $r } }
    | group-by {|x| $"($x.key.mod)::($x.key.name)" }
    | transpose key vals
  )

  { by_fqpath: $by_fqpath, impl_methods: $impl_methods, free_fns: $free_fns }
}

# Resolve a callee to *one* function row (best-effort heuristics)
def _resolve-call [
  idx: record,
  fns: list<record>,
  call: record,
  caller_fn?: record
] {
  let name = $call.callee
  let qual = ($call.qual | default '')
  let kind = ($call.kind | default 'plain')

  let caller_impl_ty = (
    if ($caller_fn | describe) =~ '^nothing' {
      ''
    } else {
      $caller_fn | get -i impl_of | default {} | get -i type_path | default ''
    }
  )
  let caller_mod = (
    if ($caller_fn | describe) =~ '^nothing' {
      ''
    } else {
      $caller_fn | get -i module_path | default [] | str join '::'
    }
  )

  if ($qual | str starts-with 'crate::') {
    let tail = $"($qual)::($name)"
    let exact = ($idx.by_fqpath | where key == $tail | get 0? | get -i vals | default [])
    if (not ($exact | is-empty)) { return ($exact | get 0) }
  }

  if ($kind == 'qualified' and ($qual | str contains '::')) {
    let tail = $"($qual)::($name)"
    let cand1 = ($fns | where {|r| ($r.fqpath | default '' | str ends-with $tail) })
    if (not ($cand1 | is-empty)) { return ($cand1 | get 0) }
  } else if ($kind == 'qualified' and (not ($qual | str contains '::'))) {
    let key = $"($qual)::($name)"
    let cand2 = ($idx.impl_methods | where key == $key | get 0? | get -i vals | default [])
    if (not ($cand2 | is-empty)) { return ($cand2 | get 0).row }
  }

  if ($kind == 'method' and $caller_impl_ty != '') {
    let key = $"($caller_impl_ty)::($name)"
    let cand3 = ($idx.impl_methods | where key == $key | get 0? | get -i vals | default [])
    if (not ($cand3 | is-empty)) { return ($cand3 | get 0).row }
  }

  let key4 = $"($caller_mod)::($name)"
  let cand4 = ($idx.free_fns | where key == $key4 | get 0? | get -i vals | default [])
  if (not ($cand4 | is-empty)) { return ($cand4 | get 0).row }

  let cand_mod = (
    $fns
    | where name == $name
    | where {|r| ($r.module_path | default []) == $caller_fn.module_path }
  )
  if ($cand_mod | length) == 1 {
    return ($cand_mod | get 0)
  }

  let cand5 = ($fns | where name == $name)
  if ($cand5 | length) == 1 {
    return ($cand5 | get 0)
  } else {
    null
  }

  null
}

# ---- record builder / deduper -----------------------------------------------

def _mk-record [
  kind:string,
  raw: record,
  want_body: bool,
  name_from?: string
] {
  let crate = (_cargo-crate-name)
  let file  = ($raw.file | default '')
  let text  = ($raw.text | default '')
  let modp  = (_module-path-from-file $file)
  let hasb  = (_has-body $text)
  let sig   = (_sigline $text)
  let vis   = (_visibility-of $sig)

  let single = ($raw.metaVariables.single? | default {})
  let nmeta  = ($single | get -i N | default {} | get -i text | default '')
  let name   = if ($name_from | default '' | str length) > 0 { $name_from } else { $nmeta }

  let abi   = ($single | get -i ABI | default {} | get -i text | default null)
  let gens  = ($single | get -i G   | default {} | get -i text | default null)
  let where_txt = ($single | get -i W | default {} | get -i text | default null)

  let fq = if ($name | is-empty) { '' } else {
    if ($modp | is-empty) { $"crate::($name)" } else { $"crate::($modp | str join '::')::($name)" }
  }

  let doc_txt   = (_extract-rustdoc $raw)
  let body_txt  = (if $want_body { _extract-src $raw } else { null })
  let doc_tok   = (_token-count $doc_txt)
  let body_tok  = (_token-count $body_txt)

  {
    kind: $kind
    name: $name
    crate: $crate
    module_path: $modp
    fqpath: $fq
    visibility: $vis
    file: $file
    span: {
      start_line: ($raw.range.start.line | default null)
      end_line:   ($raw.range.end.line   | default null)
      start_byte: ($raw.range.byteOffset.start | default null)
      end_byte:   ($raw.range.byteOffset.end   | default null)
    }
    attrs: []
    signature: $sig
    has_body: $hasb
    async:  ( ($sig | str starts-with 'async ')  or ($sig | str contains ' async ') )
    unsafe: ( ($sig | str starts-with 'unsafe ') or ($sig | str contains ' unsafe ') )
    const:  ( ($sig | str starts-with 'const ')  or ($sig | str contains ' const ') )
    abi:     $abi
    generics: $gens
    where:    $where_txt
    doc:        $doc_txt
    doc_tokens: $doc_tok
    impl_of: null
    trait_items: []
    reexports: []
    body_text:   $body_txt
    body_tokens: $body_tok
  }
}

# Create a synthetic `mod` row for a file module (src/foo.rs or src/foo/mod.rs).
def _mk-synthetic-mod [file:string] {
  let crate = (_cargo-crate-name)
  let modp  = (_module-path-from-file $file)
  if ($modp | is-empty) { return null }

  let name = ($modp | last)
  let fq   = if ($modp | is-empty) { $"crate::($name)" } else { $"crate::($modp | str join '::')" }

  let content    = (try { open $file | into string } catch { "" })
  let line_count = ($content | lines | length)
  let byte_len   = ($content | into binary | length)

  let doc_txt  = (_extract-file-mod-doc $file)
  let doc_tok  = (_token-count $doc_txt)
  let body_tok = (_token-count $content)

  {
    kind: 'mod'
    name: $name
    crate: $crate
    module_path: $modp
    fqpath: $fq
    visibility: 'private'
    file: $file
    span: { start_line: 1, end_line: $line_count, start_byte: 0, end_byte: $byte_len }
    attrs: []
    signature: $"mod ($name) {{ ... }}"
    has_body: true
    async: false
    unsafe: false
    const: false
    abi: null
    generics: null
    where: null
    doc: $doc_txt
    doc_tokens: $doc_tok
    impl_of: null
    trait_items: []
    reexports: []
    body_text: $content
    body_tokens: $body_tok
    synthetic: true
  }
}

# Deduplicate rows by (kind, file, byte span).
def _uniq-records [rows?: list<record>] {
  let r = if ($rows | is-empty) { $in } else { $rows }
  $r
  | group-by {|x| [$x.kind $x.file $x.span.start_byte $x.span.end_byte] | to json }
  | values
  | each {|g| $g.0 }
  | sort-by file span.start_line
}

# Extract exact source for a matched node.
def _extract-src [raw: record] {
  let from_raw = ($raw.text | default '' | into string)
  if ($from_raw | str length) > 0 { $from_raw } else {
    let file   = ($raw.file | into string)
    let sline0 = ( ($raw.range.start.line | default 1) - 1 )
    let eline0 =   ($raw.range.end.line   | default 1)

    try {
      open $file
      | into string
      | lines
      | skip $sline0
      | take ( ($eline0 - $sline0) | into int )
      | str join "\n"
    } catch { "" }
  }
}

# Split a comma list at top-level only, respecting brace nesting depth.
def _split-top-commas [s:string] {
  mut depth = 0
  mut cur = ""
  mut parts = []
  for ch in ($s | split chars) {
    if $ch == '{' {
      $depth = $depth + 1; $cur = $cur + $ch
    } else if $ch == '}' {
      $depth = $depth - 1; $cur = $cur + $ch
    } else if ($ch == ',' and $depth == 0) {
      let piece = ($cur | str trim)
      if ($piece | str length) > 0 { $parts = ($parts | append $piece) }
      $cur = ""
    } else { $cur = $cur + $ch }
  }
  let tail = ($cur | str trim)
  if ($tail | str length) > 0 { $parts = ($parts | append $tail) }
  $parts
}

# Expand a grouped use leaf (recursively handles nested groups).
def _expand-group-item [base:string, item:string] {
  let t = ($item | str trim | str replace -ra '^\s*::' '')

  if ($t | str contains '{') {
    let prefix = ($t | str replace -ra '\{.*$' '' | str trim | str replace -ra '\s+' '')
    let inside = ($t | str replace -ra '^[^{]*\{' '' | str replace -ra '\}\s*$' '')

    let new_base = if ($prefix | str length) > 0 { $"($base)::($prefix)" } else { $base }
    _split-top-commas $inside
    | each {|leaf| _expand-group-item $new_base $leaf }
    | flatten
  } else {
    let parts = ($t | split row ' as ')
    let path  = ($parts | get 0 | str replace -ra '\s+' '')

    let resolved = if $path == 'self' {
      $base
    } else if $path == 'super' or $path == 'crate' {
      $path
    } else {
      if ($path | str starts-with 'crate::') { $path } else { $"($base)::($path)" }
    }

    let leaf_name0 = (
      if ($parts | length) > 1 { $parts | get 1 | str trim } else {
        $resolved | split row '::' | last
      }
    )

    [{ name: $leaf_name0, fqpath: $resolved }]
  }
}

# Expand a single grouped-use statement string into leaf entries (name, fqpath).
def _expand-grouped-use [src_text:string] {
  let s = ($src_text | str replace -ra '(?s)^\s*' '' | str replace -ra '(?s)\s*$' '')

  let base0  = ($s | str replace -ra '(?s)^.*?\buse\s+' '' | str replace -ra '(?s)\{.*$' '' | str replace -ra '\s+' '')
  let base   = ($base0 | str replace -ra '::$' '')
  let inside = ($s | str replace -ra '(?s)^.*?\{' '' | str replace -ra '(?s)\}.*$' '')

  let base_final = if ($base | str length) > 0 { $base } else { 'crate' }

  _split-top-commas $inside
  | each {|leaf| _expand-group-item $base_final $leaf }
  | flatten
}

# Expand provided paths to a list of *.rs files.
def _list-rust-files [...paths:string] {
  let targets = (_target-list ...$paths)

  $targets
  | each {|t|
      let p = ($t | path expand)
      let typ = (try { $p | path type } catch { null })

      if $typ == 'file' {
        if ($p | str ends-with '.rs') { [$p] } else { [] }
      } else if $typ == 'dir' or $typ == null {
        try { glob $"($p)/**\/*.rs" } catch { [] }
      } else { [] }
    }
  | flatten
  | sort
  | uniq
}

# ---------- collectors per kind ---------------------------------------------

export def rust-fn-records [...paths:string] {
  let targets = (_target-list ...$paths)

  let pats = [
    'fn $N($$$P) { $$$B }'
    'pub fn $N($$$P) { $$$B }'
    'async fn $N($$$P) { $$$B }'
    'pub async fn $N($$$P) { $$$B }'
    'unsafe fn $N($$$P) { $$$B }'
    'pub unsafe fn $N($$$P) { $$$B }'
    'const fn $N($$$P) { $$$B }'
    'pub const fn $N($$$P) { $$$B }'

    'fn $N($$$P) -> $R { $$$B }'
    'pub fn $N($$$P) -> $R { $$$B }'
    'async fn $N($$$P) -> $R { $$$B }'
    'pub async fn $N($$$P) -> $R { $$$B }'
    'unsafe fn $N($$$P) -> $R { $$$B }'
    'pub unsafe fn $N($$$P) -> $R { $$$B }'
    'const fn $N($$$P) -> $R { $$$B }'
    'pub const fn $N($$$P) -> $R { $$$B }'

    'fn $N($$$P);'
    'pub fn $N($$$P);'
    'async fn $N($$$P);'
    'pub async fn $N($$$P);'
    'unsafe fn $N($$$P);'
    'pub unsafe fn $N($$$P);'
    'const fn $N($$$P);'
    'pub const fn $N($$$P);'

    'fn $N($$$P) -> $R;'
    'pub fn $N($$$P) -> $R;'
    'async fn $N($$$P) -> $R;'
    'pub async fn $N($$$P) -> $R;'
    'unsafe fn $N($$$P) -> $R;'
    'pub unsafe fn $N($$$P) -> $R;'
    'const fn $N($$$P) -> $R;'
    'pub const fn $N($$$P) -> $R;'

    'fn $N($$$P) where $W { $$$B }'
    'pub fn $N($$$P) where $W { $$$B }'
    'fn $N($$$P) -> $R where $W { $$$B }'
    'pub fn $N($$$P) -> $R where $W { $$$B }'

    'pub async fn $N<$G>($$$P) -> $R { $$$B }'
    'pub async fn $N<$G>($$$P) -> $R where $W { $$$B }'
    'pub async fn $N<$G>($$$P) { $$$B }'
    'async fn $N<$G>($$$P) -> $R { $$$B }'
    'async fn $N<$G>($$$P) -> $R where $W { $$$B }'

    "impl $TY { fn $N<$G>($$$P) -> $R { $$$B } }"
    "impl $TY { fn $N<$G>($$$P) -> $R where $W { $$$B } }"
    "impl $TY { fn $N<$G>($$$P) { $$$B } }"

    "impl $TR for $TY { fn $N<$G>($$$P) -> $R { $$$B } }"
    "impl $TR for $TY { fn $N<$G>($$$P) -> $R where $W { $$$B } }"
    "impl $TR for $TY where $W { fn $N<$G>($$$P) -> $R { $$$B } }"
    "impl $TR for $TY where $W { fn $N<$G>($$$P) -> $R where $W2 { $$$B } }"

    "impl<$G> $TR for $TY { fn $N<$G2>($$$P) -> $R { $$$B } }"
    "impl<$G> $TR for $TY where $W { fn $N<$G2>($$$P) -> $R where $W2 { $$$B } }"

    "trait $TR { fn $N<$G>($$$P) -> $R; }"
    "trait $TR { fn $N<$G>($$$P) -> $R where $W; }"

    'fn $N<$G>($$$P) { $$$B }'
    'pub fn $N<$G>($$$P) { $$$B }'
    'async fn $N<$G>($$$P) { $$$B }'
    'pub async fn $N<$G>($$$P) { $$$B }'
    'unsafe fn $N<$G>($$$P) { $$$B }'
    'pub unsafe fn $N<$G>($$$P) { $$$B }'
    'const fn $N<$G>($$$P) { $$$B }'
    'pub const fn $N<$G>($$$P) { $$$B }'

    'fn $N<$G>($$$P) -> $R { $$$B }'
    'pub fn $N<$G>($$$P) -> $R { $$$B }'
    'async fn $N<$G>($$$P) -> $R { $$$B }'
    'pub async fn $N<$G>($$$P) -> $R { $$$B }'
    'unsafe fn $N<$G>($$$P) -> $R { $$$B }'
    'pub unsafe fn $N<$G>($$$P) -> $R { $$$B }'
    'const fn $N<$G>($$$P) -> $R { $$$B }'
    'pub const fn $N<$G>($$$P) -> $R { $$$B }'

    'fn $N<$G>($$$P) where $W { $$$B }'
    'pub fn $N<$G>($$$P) where $W { $$$B }'
    'fn $N<$G>($$$P) -> $R where $W { $$$B }'
    'pub fn $N<$G>($$$P) -> $R where $W { $$$B }'

    'fn $N<$G>($$$P);'
    'pub fn $N<$G>($$$P);'
    'fn $N<$G>($$$P) -> $R;'
    'pub fn $N<$G>($$$P) -> $R;'
  ]

  mut out = []
  for p in $pats {
    let want_body = ($p | str contains '{ $$$B }')
    let rows = (_sg_json_on $p $targets | each {|raw| _mk-record 'fn' $raw $want_body })
    $out = ($out | append $rows)
  }

  $out | reduce -f [] {|batch, acc| $acc | append $batch } | _uniq-records
}

# extern "ABI" functions
export def rust-extern-fn-records [...paths:string] {
  let targets = (_target-list ...$paths)
  let pats = [
    'pub unsafe extern $ABI fn $N$G?($$$P) -> $R { $$$B }'
    'pub unsafe extern $ABI fn $N$G?($$$P) { $$$B }'
    'unsafe extern $ABI fn $N$G?($$$P) -> $R { $$$B }'
    'unsafe extern $ABI fn $N$G?($$$P) { $$$B }'
    'pub extern $ABI fn $N$G?($$$P) -> $R { $$$B }'
    'pub extern $ABI fn $N$G?($$$P) { $$$B }'
    'extern $ABI fn $N$G?($$$P) -> $R { $$$B }'
    'extern $ABI fn $N$G?($$$P) { $$$B }'
    'pub unsafe extern $ABI fn $N$G?($$$P) -> $R;'
    'pub unsafe extern $ABI fn $N$G?($$$P);'
    'unsafe extern $ABI fn $N$G?($$$P) -> $R;'
    'unsafe extern $ABI fn $N$G?($$$P);'
    'pub extern $ABI fn $N$G?($$$P) -> $R;'
    'pub extern $ABI fn $N$G?($$$P);'
    'extern $ABI fn $N$G?($$$P) -> $R;'
    'extern $ABI fn $N$G?($$$P);'
  ]
  mut out = []
  for p in $pats {
    let rows = (_sg_json_on $p $targets | each {|raw| _mk-record 'extern_fn' $raw ($p | str contains '{ $$$B }') })
    $out = ($out | append $rows)
  }
  $out | reduce -f [] {|batch, acc| $acc | append $batch } | _uniq-records
}

# Structs
export def rust-struct-records [...paths:string] {
  let targets = (_target-list ...$paths)
  let pats = [
    'pub struct $N { $$$F }'
    'struct $N { $$$F }'
    'pub struct $N<$G>? { $$$B }'
    'struct $N<$G>? { $$$B }'
    'pub struct $N<$G>?($$$F);'
    'struct $N<$G>?($$$F);'
    'pub struct $N<$G>?;'
    'struct $N<$G>?;'
  ]
  mut out = []
  for p in $pats {
    let want_body = ($p | str contains '{')
    let rows = (_sg_json_on $p $targets | each {|raw| _mk-record 'struct' $raw $want_body })
    $out = ($out | append $rows)
  }
  $out | reduce -f [] {|batch, acc| $acc | append $batch } | _uniq-records
}

# Enums
export def rust-enum-records [...paths:string] {
  let targets = (_target-list ...$paths)
  let pats = [
    'pub enum $N$G? { $$$V }'
    'enum $N$G? { $$$V }'
  ]
  mut out = []
  for p in $pats {
    let rows = (_sg_json_on $p $targets | each {|raw| _mk-record 'enum' $raw true })
    $out = ($out | append $rows)
  }
  $out | reduce -f [] {|batch, acc| $acc | append $batch } | _uniq-records
}

# Type aliases
export def rust-type-records [...paths:string] {
  let targets = (_target-list ...$paths)
  let pats = [
    'pub type $N$G? = $$$T;'
    'type $N$G? = $$$T;'
  ]
  mut out = []
  for p in $pats {
    let rows = (_sg_json_on $p $targets | each {|raw| _mk-record 'type' $raw true })
    $out = ($out | append $rows)
  }
  $out | reduce -f [] {|batch, acc| $acc | append $batch } | _uniq-records
}

# Traits
export def rust-trait-records [...paths:string] {
  let targets = (_target-list ...$paths)
  let pats = [
    'pub unsafe trait $N { $$$B }'
    'pub trait $N { $$$B }'
    'unsafe trait $N { $$$B }'
    'trait $N { $$$B }'
  ]
  mut out = []
  for p in $pats {
    let rows = (_sg_json_on $p $targets | each {|raw| _mk-record 'trait' $raw false })
    $out = ($out | append $rows)
  }
  $out | reduce -f [] {|batch, acc| $acc | append $batch } | _uniq-records
}

# impl blocks
export def rust-impl-records [...paths:string] {
  let targets = (_target-list ...$paths)
  let pats = [
    'unsafe impl $TR for $TY { $$$B }'
    'impl $TR for $TY { $$$B }'
    'unsafe impl<$G> $TR for $TY { $$$B }'
    'impl<$G> $TR for $TY { $$$B }'
    'unsafe impl $TR for $TY where $W { $$$B }'
    'impl $TR for $TY where $W { $$$B }'
    'unsafe impl<$G> $TR for $TY where $W { $$$B }'
    'impl<$G> $TR for $TY where $W { $$$B }'

    'unsafe impl $TY { $$$B }'
    'impl $TY { $$$B }'
    'unsafe impl<$G> $TY { $$$B }'
    'impl<$G> $TY { $$$B }'
    'unsafe impl $TY where $W { $$$B }'
    'impl $TY where $W { $$$B }'
    'unsafe impl<$G> $TY where $W { $$$B }'
    'impl<$G> $TY where $W { $$$B }'
  ]

  mut out = []
  for p in $pats {
    let rows = (
      _sg_json_on $p $targets
      | each {|raw|
          let rec = (_mk-record 'impl' $raw true)

          let single     = ($raw.metaVariables.single? | default {})
          let trait_path = ($single | get -i TR | default {} | get -i text | default null)
          let type_path1 = ($single | get -i TY | default {} | get -i text | default null)
          let type_path2 = ($single | get -i T  | default {} | get -i text | default null)
          let type_path  = (if $type_path1 != null { $type_path1 } else { $type_path2 })

          let impl_name = if ($trait_path | default '' | str length) > 0 and ($type_path | default '' | str length) > 0 {
            $"<($type_path) as ($trait_path)>"
          } else if ($type_path | default '' | str length) > 0 {
            $type_path
          } else {
            'impl'
          }

          let modp     = ($rec.module_path | default [])
          let modp_str = (if ($modp | is-empty) { "" } else { ($modp | str join '::') })
          let fq = if ($modp | is-empty) { $"crate::($impl_name)" } else { $"crate::($modp_str)::($impl_name)" }

          $rec
          | upsert impl_of { trait_path: $trait_path, type_path: $type_path }
          | upsert name    $impl_name
          | upsert fqpath  $fq
        }
      )
    $out = ($out | append $rows)
  }

  $out | reduce -f [] {|batch, acc| $acc | append $batch } | _uniq-records
}

# Module syntax (inline and declarations)
export def rust-mod-records [...paths:string] {
  let targets = (_target-list ...$paths)

  let pats_with_body = [
    'pub mod $N { $$$B }'
    'mod $N { $$$B }'
  ]

  let pats_decl = [
    'pub mod $N;'
    'mod $N;'
  ]

  mut out = []

  for p in $pats_with_body {
    let rows = (_sg_json_on $p $targets | each {|raw| _mk-record 'mod' $raw true })
    $out = ($out | append $rows)
  }

  for p in $pats_decl {
    let rows = (_sg_json_on $p $targets | each {|raw| _mk-record 'mod' $raw false })
    $out = ($out | append $rows)
  }

  $out | reduce -f [] {|batch, acc| $acc | append $batch } | _uniq-records
}

# File modules synthesized from filesystem layout
export def rust-file-mod-records [...paths:string] {
  _list-rust-files ...$paths
  | each {|f| _mk-synthetic-mod $f }
  | where {|x| $x != null }
  | _uniq-records
}

# macro_rules!
export def rust-macro-records [...paths:string] {
  let targets = (_target-list ...$paths)
  let pats = [ 'macro_rules! $N { $$$B }' ]
  mut out = []
  for p in $pats {
    let rows = (_sg_json_on $p $targets | each {|raw| _mk-record 'macro_rules' $raw false })
    $out = ($out | append $rows)
  }
  $out | reduce -f [] {|batch, acc| $acc | append $batch } | _uniq-records
}

# const items
export def rust-const-records [...paths:string] {
  let targets = (_target-list ...$paths)
  let pats = [
    'pub const $N: $$$T = $$$V;'
    'const $N: $$$T = $$$V;'
  ]
  mut out = []
  for p in $pats {
    let rows = (_sg_json_on $p $targets | each {|raw| _mk-record 'const' $raw false })
    $out = ($out | append $rows)
  }
  $out | reduce -f [] {|batch, acc| $acc | append $batch } | _uniq-records
}

# static items
export def rust-static-records [...paths:string] {
  let targets = (_target-list ...$paths)
  let pats = [
    'pub static $N: $$$T = $$$V;'
    'static $N: $$$T = $$$V;'
  ]
  mut out = []
  for p in $pats {
    let rows = (_sg_json_on $p $targets | each {|raw| _mk-record 'static' $raw false })
    $out = ($out | append $rows)
  }
  $out | reduce -f [] {|batch, acc| $acc | append $batch } | _uniq-records
}

# use/import statements (one row per statement)
export def rust-use-records [...paths:string] {
  let targets = (_target-list ...$paths)
  let pats = ['pub use $$$I;' 'use $$$I;']
  mut out = []

  for p in $pats {
    let rows = (
      _sg_json_on $p $targets
      | each {|raw|
          let file  = ($raw.file | into string)
          let sline = ( ($raw.range.start.line | default 1) - 1 )
          let eline =   ($raw.range.end.line   | default 1)

          let src_block = (
            try {
              open $file
              | into string
              | lines
              | skip $sline
              | take ( ($eline - $sline + 1) | into int )
              | str join "\n"
            } catch { ($raw.text | into string) }
          )

          let from_use = (
            $src_block
            | str replace -ra '^(?s)\s*(#\[[^\]]*\]\s*)*' ''
            | str replace -ra '^(?m:\s*//[^\n]*\n)+' ''
            | str replace -ra '^(?s)\s*/\*.*?\*/\s*' ''
            | str replace -ra '^(?s).*?\b(use\b.*)$' '$1'
          )

          let upto_semi = (
            $from_use
            | str replace -ra '^(?s)(.*?;).*?$' '$1'
            | str trim
          )

          if ($upto_semi | is-empty) or (not ($upto_semi | str contains 'use')) {
            null
          } else {
            let stmt = ($upto_semi | str replace -ra '\s+' ' ' | str trim)

            if ($stmt | str starts-with '///') or ($stmt | str starts-with '#[') {
              null
            } else {
              let body0 = (
                $stmt
                | str replace -ra '^\s*(pub\s+)?use\s+' ''
                | str replace -ra '\s*;\s*$' ''
                | str trim
              )

              if ($body0 == '' or $body0 == '}') { null } else {
                let is_grouped = ($body0 | str contains '{')
                let modp       = (_module-path-from-file $file)
                let crate_base = if ($modp | is-empty) { 'crate' } else { $"crate::($modp | str join '::')" }

                if $is_grouped {
                  let prefix0 = (
                    $body0
                    | str replace -ra '(?s)\{.*$' ''
                    | str replace -ra '\s+' ''
                    | str replace -ra '::$' ''
                  )
                  let base = if $prefix0 == 'crate' { $crate_base } else { $prefix0 }

                  (_mk-record 'use' $raw false '*')
                  | upsert signature $stmt
                  | upsert fqpath $"($base)::*"
                } else {
                  let body_norm   = ($body0 | str replace -ra '\s+' ' ' | str trim)
                  let alias_parts = ($body_norm | split row ' as ')
                  let alias       = (if ($alias_parts | length) > 1 { $alias_parts | get 1 | str trim } else { null })
                  let path0       = ($alias_parts | get 0 | str replace -ra '\s+' '')

                  let path     = if $path0 == 'crate' { $crate_base } else { $path0 }
                  let is_star  = ($path | str ends-with '::*')
                  let base_nm  = if $is_star { '*' } else { ($path | split row '::' | last) }
                  let name     = (if ($alias | default '' | str length) > 0 { $alias } else { $base_nm })

                  if ($name | is-empty) or $name == '}' { null } else {
                    (_mk-record 'use' $raw false $name)
                    | upsert signature $stmt
                    | upsert fqpath $path
                  }
                }
              }
            }
          }
        }
      )
      | where {|x| $x != null }
    $out = ($out | append $rows)
  }

  $out | reduce -f [] {|batch, acc| $acc | append $batch } | _uniq-records
}

# ---------- nesting helpers (for rust-tree) ----------------------------------

def _index-by-fq [rows:list<record>] {
  $rows
  | where {|r| ($r | get -i fqpath | default '') != '' }
  | group-by fqpath
  | transpose fq rows
}

# Build parent→children edges (each as fq strings)
def _build-symbol-edges [rows:list<record>] {
  let keyed = ($rows | where {|r| ($r | get -i fqpath | default '') != '' })

  let parent_of = {|fq|
    if $fq == 'crate' { null } else {
      let parts = ($fq | split row '::')
      let len = ($parts | length)
      if $len <= 1 { null } else { ($parts | take ($len - 1) | str join '::') }
    }
  }

  $keyed
  | each {|r|
      let fq = ($r | get -i fqpath | default '')
      let p  = (do $parent_of $fq)
      if $p == null or $p == $fq { null } else { { parent: $p, child: $fq } }
    }
  | where {|e| $e != null }
  | group-by parent
  | transpose parent children
  | each {|g| { parent: $g.parent, children: ($g.children | get child | uniq | sort) } }
}

def _paint-kind [kind:string, text:string] {
  let t = ($text | default "")
  match $kind {
    "mod"        => $"(ansi blue)($t)(ansi reset)"
    "fn"         => $"(ansi green)($t)(ansi reset)"
    "extern_fn"  => $"(ansi light_green)($t)(ansi reset)"
    "struct"     => $"(ansi magenta)($t)(ansi reset)"
    "enum"       => $"(ansi purple)($t)(ansi reset)"
    "trait"      => $"(ansi cyan)($t)(ansi reset)"  # or 'purple' (alias)
    "impl"       => $"(ansi yellow)($t)(ansi reset)"
    "const"      => $"(ansi light_red)($t)(ansi reset)"
    "static"     => $"(ansi light_red)($t)(ansi reset)"
    "macro_rules" => $"(ansi dark_gray)($t)(ansi reset)"  # or purple
    "use"        => $"(ansi white_dimmed)($t)(ansi reset)"  # “dim” style
    _            => $t
  }
}

# Build a nested node: take the canonical row for `fq` and attach `children: [...]`
def _nest-node [
  rows:list<record>,
  edges:list<record<parent: string, children: list<string>>>,
  index:list<record<fq: string, rows: list<record>>>,
  fq:string
] {
  let r0 = (
    $index
    | where fq == $fq
    | get 0?
    | get -i rows
    | get 0?
    | default null
  )

  let kids = (
    $edges
    | where parent == $fq
    | get 0?
    | get -i children
    | default []
  )

  let children = $kids
    | each {|cfq| _nest-node $rows $edges $index $cfq }

  if $r0 == null {
    { kind: 'node', name: ($fq | split row '::' | last), fqpath: $fq, children: $children }
  } else {
    $r0 | upsert children $children
  }
}

# --- helpers for nested build (no flatten) ------------------------------------

# Build an index of MINIMAL rows (only fields the printer needs).
def _rows-index [rows: list<record>] {
  $rows
  | reduce -f {} {|r, acc|
      let fq = ($r.fqpath | default '')
      if $fq == '' {
        $acc
      } else {
        # store minimal payload; avoid other list-typed columns
        let minimal = {
          kind:   ($r.kind   | default '')
          name:   ($r.name   | default '')
          fqpath: $fq
          children: []  # placeholder; we'll fill this when we build the tree
        }
        $acc | upsert $fq $minimal
      }
    }
}

# Safely get child fq list for a parent from edges structure
def _children-for [edges: list<record<parent: string, children: list<string>>>, parent_fq: string] {
  $edges | where parent == $parent_fq | get 0? | get -i children | default []
}

# Recursive builder: construct a fresh record (avoid upsert-on-record issues)
def _build-subtree [idx: record, edges: list<record>, fq: string] {
  let self = (try { $idx | get $fq } catch { null })
  if $self == null {
    null
  } else {
    let kids_fq = (_children-for $edges $fq)
    let kids = (
      $kids_fq
      | each {|cfq| _build-subtree $idx $edges $cfq }
      | where {|x| (($x | describe) =~ '^record<') }
    )
  {
    kind:   ($self.kind   | default '')
    name:   ($self.name   | default '')
    fqpath: ($self.fqpath | default '')
    children: (if (($kids | describe) =~ '^(list|table)') { $kids } else { [] })
  }
  }
}

# Build { fqpath -> { body_tokens:int, doc_tokens:int } }
def _build-token-index [] {
  rust-ast
  | reduce -f {} {|r, acc|
      let fq = ($r.fqpath | default '')
      if $fq == '' {
        $acc
      } else {
        $acc | upsert $fq {
          body_tokens: ($r.body_tokens | default 0)
          doc_tokens:  ($r.doc_tokens  | default 0)
        }
      }
    }
}

def _spaces [n: int] {
  if $n <= 0 { "" } else { (0..<$n | each { " " } | str join "") }
}

def _display-name [r: record] {
  let fq = ($r.fqpath | default '')
  if $fq == '' { ($r.name | default "") } else { $fq | split row '::' | last }
}

def _kind-width [rows: list<record>] {
  $rows
  | each {|r| (_vlen ($r.kind | default '')) }
  | math max
  | default 0
}

# Robustly pull a single root record out of whatever came in
def _roots-of [x: any] {
  let t = ($x | describe)

  if ($t =~ '^record<') {
    [ $x ]                           # single root -> list of 1
  } else if ($t =~ '^(list|table)') {
    $x                                # keep top-level items only
    | where {|it| (($it | describe) =~ '^record<') }  # only records
  } else if $t == 'string' {
    let parsed = (try { $x | from json } catch { null })
    if $parsed == null {
      error make { msg: "print-symbol-tree: got a string that isn't JSON" }
    } else {
      _roots-of $parsed
    }
  } else {
    error make { msg: $"print-symbol-tree: unsupported input type: ($t)" }
  }
}

# ---------- tree walking (first pass: collect rows) ---------------------------

def _collect-rows [
  node: record,
  ancestors_last: list<bool> = [],
  is_last: bool = true
] {
  # Coerce `children` → always a list of records
  let kids0 = (try { $node | get -i children } catch { [] })
  let kids = (
    [ (try { $node | get -i children } catch { [] }) ]
    | flatten
    | where {|x| (($x | describe) =~ '^record<') }
  )
  let n     = ($kids | length)

  let prefix_parts = ($ancestors_last | each {|last| if $last { "   " } else { "|  " } })
  let tee          = (if ($ancestors_last | length) == 0 { "" } else { if $is_last { "`- " } else { "|- " } })
  let prefix = ($prefix_parts | str join "")
  let line_prefix = ( $prefix + $tee )

  let row = {
    line_prefix: $line_prefix
    depth: ($ancestors_last | length)
    is_last: $is_last
    is_leaf: ($n == 0)
    name: (_display-name $node)
    kind: ($node.kind | default '')
    fqpath: ($node.fqpath | default '')
  }

  let children_rows = (
    0..<( $n )
    | each {|i|
        let child = ($kids | get $i)
        let lastf = ($i == ($n - 1))
        _collect-rows $child ($ancestors_last | append $is_last) $lastf
      }
    | flatten
  )

  [$row] | append $children_rows
}

# ---------- second pass: compute widths & print -------------------------------

def _print-with-columns [
  rows: list<record>,
  show_fq_on_branches: bool = false,
  token_idx?: record
] {
  if ($rows | is-empty) { return }

  let tok_enabled = ( ($token_idx | describe) =~ '^record<' )

  # Pipe position uses PAINTED names so spacing matches the visible output
  let target_pipe_col = (
    $rows
    | each {|r| (_vlen $r.line_prefix) + (_vlen (_paint-kind ($r.kind | default '') ($r.name | default ''))) }
    | math max
    | default 20
  ) + 1

  # Kind column width (painted)
  let kind_w = (
    $rows
    | each {|r| (_vlen (_paint-kind ($r.kind | default '') ($r.kind | default ''))) }
    | math max
    | default 0
  )

  # fqpath width (only where shown)
  let fq_w = (
    $rows
    | each {|r|
        let show_fq = ($r.is_leaf or $show_fq_on_branches)
        if $show_fq { (_vlen ($r.fqpath | default '')) } else { 0 }
      }
    | math max
    | default 0
  )

  # -------- token sub-column widths (right-align numbers) --------
  let body_w = if $tok_enabled {
    $rows
    | each {|r| (try { $token_idx | get $r.fqpath | get body_tokens } catch { null }) | default 0 | into string | str length }
    | math max
    | default 1
  } else { 0 }

  let doc_w = if $tok_enabled {
    $rows
    | each {|r| (try { $token_idx | get $r.fqpath | get doc_tokens } catch { null }) | default 0 | into string | str length }
    | math max
    | default 1
  } else { 0 }

  # total width of the tokens column once numbers are padded
  let tok_w = if $tok_enabled {
    (_vlen "Body Tokens: ") + $body_w + (_vlen ", Doc Tokens: ") + $doc_w
  } else { 0 }

  for r in $rows {
    # Name (painted) + left padding to the first pipe
    let name_raw = ($r.name | default '')
    let name_col = (_paint-kind ($r.kind | default '') $name_raw)
    let pre_len  = (_vlen $r.line_prefix)
    let name_len = (_vlen $name_col)
    let pad      = $target_pipe_col - ($pre_len + $name_len)
    let pad      = if $pad < 1 { 1 } else { $pad }

    # Kind (painted + padded)
    let kind_raw = ($r.kind | default '')
    let kind_txt = (_paint-kind $kind_raw $kind_raw)
    let kind_pad = $kind_w - (_vlen $kind_txt)
    let kind_pad = if $kind_pad < 0 { 0 } else { $kind_pad }
    let kind_col = ($kind_txt + (_spaces $kind_pad))

    # fqpath (no brackets)
    let show_fq = ($r.is_leaf or $show_fq_on_branches)
    let fq_txt  = if $show_fq { ($r.fqpath | default '') } else { '' }
    let fq_pad  = $fq_w - (_vlen $fq_txt)
    let fq_pad  = if $fq_pad < 0 { 0 } else { $fq_pad }
    let fq_col  = ($fq_txt + (_spaces $fq_pad))

    # tokens (optional column, with per-number alignment)
    let tok_txt = if $tok_enabled {
      let info  = (try { $token_idx | get $r.fqpath } catch { null })
      if $info == null {
        # produce a blank cell of the correct width so the column stays aligned
        (_spaces $tok_w)
      } else {
        let btxt = (($info.body_tokens | default 0) | into string)
        let dtxt = (($info.doc_tokens  | default 0) | into string)
        let bpad = $body_w - (_vlen $btxt)
        let dpad = $doc_w  - (_vlen $dtxt)
        let bfmt = (_spaces (if $bpad < 0 { 0 } else { $bpad })) + $btxt
        let dfmt = (_spaces (if $dpad < 0 { 0 } else { $dpad })) + $dtxt
        $"Body Tokens: ($bfmt), Doc Tokens: ($dfmt)"
      }
    } else { "" }

    # Assemble line
    mut parts = [
      $r.line_prefix,
      $name_col,              # painted name
      (_spaces $pad),
      "| ",
      $kind_col,
      " | ",
      $fq_col
    ]
    if $tok_enabled { $parts = ($parts | append " | " | append $tok_txt) }

    print ($parts | str join "")
  }
}

def _vlen [s: any] {
  ($s | into string | ansi strip | str length)
}

# =============================================================================
# call graph (edges + printer)
# =============================================================================

# Build unique edges with *both* real and canonical fqpaths
def _build-call-edges [rows:list<record>] {
  let fns = ($rows | where {|r| ($r | get -i kind | default '') == 'fn' })

  let files = (
    $rows
    | each {|r| ($r | get -i file | default null) }
    | where {|f| $f != null }
    | uniq
  )

  let calls    = (_rust-call-sites-on $files)
  let fn_index = (_index-fns-by-file $fns)
  let idx      = (_build-fn-indexes $fns)

  $calls
  | each {|c|
      let caller = (_enclosing-fn $fn_index $c.file ($c.span.start_byte | default 0) ($c.span.end_byte | default 0))
      if $caller == null { null } else {
        let target = (_resolve-call $idx $fns $c $caller)
        if $target == null { null } else {
          let caller_fq = ($caller.fqpath | default '')
          let callee_fq = ($target.fqpath | default '')
          if $caller_fq == '' or $callee_fq == '' { null } else {
            {
              caller: $caller_fq,
              callee: $callee_fq,
              caller_c: (_fq_canon $caller_fq),
              callee_c: (_fq_canon $callee_fq),
            }
          }
        }
      }
    }
  | where {|e| $e != null }
  | uniq
}

# Map a free-form pattern → seed fqpaths to start the graph from
# - Accepts: exact fqpath ("crate::api::ask"), ends-with module path ("::ask"),
#   or bare name ("ask"). If multiple matches, returns them all.
def _lookup-fn-seeds [fns:list<record>, pattern:string] {
  let pat = ($pattern | into string | str trim)
  if ($pat | str starts-with 'crate::') {
    $fns | where {|r| ($r.fqpath | default '') == $pat } | get -i fqpath
  } else if ($pat | str contains '::') {
    $fns | where {|r| ($r.fqpath | default '') | str ends-with $pat } | get -i fqpath
  } else {
    let by_name = ($fns | where name == $pat | get -i fqpath)
    if (not ($by_name | is-empty)) { $by_name } else {
      $fns | where {|r| ($r.fqpath | default '') | str ends-with $"::($pat)" } | get -i fqpath
    }
  }
}

def _walk-fq-tree [
  adj: record,                 # canonical -> [canonical]
  canon2real: record,          # canonical -> [real fq]
  node_c: string,              # canonical seed
  max_depth: int,
  ancestors_last: list<bool> = [],
  visited: list<string> = [],  # canonical
] {
  let is_cycle = ($visited | any {|v| $v == $node_c })
  let indent   = ($ancestors_last | each {|last| if $last { "   " } else { "|  " } } | str join "")
  let tee      = (if ($ancestors_last | length) == 0 { "" } else { "|- " })
  let face_fq  = (try { $canon2real | get $node_c | get 0 } catch { $node_c })
  let face_nm  = (_leaf-name $face_fq)

  mut out = [ $"($indent)($tee)(ansi white)($face_nm)(ansi reset)  (ansi dark_gray)[($face_fq)](ansi reset)" ]

  if $is_cycle or $max_depth <= 0 { 
    if $is_cycle { let out = ($out | append $"($indent)   (ansi red)⟲ cycle(ansi reset)") }
    return $out
  }

  let kids = (try { $adj | get $node_c } catch { [] }) | default [] | uniq | sort
  let n = ($kids | length)
  for i in 0..<( $n ) {
    let ch = ($kids | get $i)
    let sub = _walk-fq-tree $adj $canon2real $ch ($max_depth - 1) ($ancestors_last | append ($i == ($n - 1))) ($visited | append $node_c)
    $out = ($out | append $sub)
  }
  $out
}

# Build canonical adjacency and a mapping from canonical -> set of real fqpaths
def _adjacency-from-edges [
  edges:list<record<caller:string, callee:string, caller_c:string, callee_c:string>>,
  reverse: bool = false
] {
  let adj = (
    $edges
    | reduce -f {} {|e, acc|
        let from_c = (if $reverse { $e.callee_c } else { $e.caller_c })
        let to_c   = (if $reverse { $e.caller_c } else { $e.callee_c })
        let cur    = (try { $acc | get $from_c } catch { [] })
        let nxt    = ($cur | append $to_c | uniq | sort)
        $acc | upsert $from_c $nxt
      }
  )
  let canon2real = (
    $edges
    | reduce -f {} {|e, acc|
        let a1 = ($acc | upsert $e.caller_c ((try { $acc | get $e.caller_c } catch { [] }) | append $e.caller | uniq | sort))
        let a2 = ($a1   | upsert $e.callee_c ((try { $a1  | get $e.callee_c } catch { [] }) | append $e.callee | uniq | sort))
        $a2
      }
  )
  { adj: $adj, canon2real: $canon2real }
}

def _leaf [fq:string] { $fq | split row '::' | last }

# Walk a *canonical* adjacency but print the *real* fqpaths.
def _walk-call-graph [
  adj: record,               # canonical -> [canonical]
  canon2real: record,        # canonical -> [real fqpaths]
  node_c: string,            # canonical fqpath of current node
  max_depth: int,
  ancestors_last: list<bool> = [],
  visited: list<string> = [] # canonical seen
] {
  let is_cycle = ($visited | any {|v| $v == $node_c })
  let prefix   = ($ancestors_last | each {|last| if $last { "   " } else { "|  " } } | str join "")
  let tee      = (if ($ancestors_last | length) == 0 { "" } else { "|- " })
  let real0    = (try { $canon2real | get $node_c | get 0 } catch { $node_c })
  let face_nm  = (_leaf-name $real0)

  mut out = [ $"($prefix)($tee)(ansi white)($face_nm)(ansi reset)  (ansi dark_gray)[($real0)](ansi reset)" ]

  if $is_cycle or $max_depth <= 0 {
    if $is_cycle { let out = ($out | append $"($prefix)   (ansi red)⟲ cycle detected(ansi reset)") }
    return $out
  }

  let children_c = (try { $adj | get $node_c } catch { [] }) | default [] | uniq | sort
  let n = ($children_c | length)
  for i in 0..<( $n ) {
    let ch_c = ($children_c | get $i)
    let sub = _walk-call-graph $adj $canon2real $ch_c ($max_depth - 1) ($ancestors_last | append ($i == ($n - 1))) ($visited | append $node_c)
    $out = ($out | append $sub)
  }
  $out
}

export def print-call-graph [
  pattern:string,
  --max-depth:int = 3,
  --reverse,       # callers-of when set; else callees-of
  --show-roots,
] {
  let rows = (rust-ast)              # already includes callers via _attach_callers
  let fns  = ($rows | where kind == 'fn')

  let seeds_real = (_lookup-fn-seeds $fns $pattern)
  if ($seeds_real | is-empty) {
    error make { msg: $"print-call-graph: no function matched: '($pattern)'" }
  }
  let seeds_c = ($seeds_real | each {|fq| _fq_canon $fq } | uniq)

  let built = (_adj_from_rows $rows)
  let adj   = (if ($reverse | default false) { $built.callers_of } else { $built.callees_of })
  let map   = $built.canon2real

  for root_c in $seeds_c {
    # Prefer a real fq from canon→real; fall back to the matching seed
    let root_real = (try { $map | get $root_c | get 0 } catch { ($seeds_real | where {|fq| (_fq_canon $fq) == $root_c } | get 0) })
    if ($show_roots | default false) {
      let dir = (if ($reverse | default false) { "← callers of" } else { "→ callees of" })
      print $"(ansi cyan)Call graph depth: ($max_depth) (ansi reset)(ansi dark_gray)($dir)(ansi reset) (ansi white)($root_real)(ansi reset)"
    }
    let lines = _walk-fq-tree $adj $map $root_c $max_depth [] []
    for ln in $lines { print $ln }
    if ($seeds_c | length) > 1 { print "" }
  }
}

# Split an fqpath into (module_chain, leaf_name)
# "crate::a::b::c" -> (["crate","crate::a","crate::a::b"], "c")
def _fq_split [fq:string] {
  let parts = ($fq | split row '::')
  if ($parts | is-empty) { return [[], $fq] }
  let leaf  = ($parts | last)
  let mods  = (
    0..<( ($parts | length) - 1 )
    | each {|i| ($parts | take ($i + 1) | str join '::') }
  )
  [ $mods, $leaf ]
}

# Paint a module name (folder-like) and a function name (leaf-like)
def _paint-node-line [fq:string, kind:string] {
  let name = ($fq | split row '::' | last)
  match $kind {
    "mod" => $"(ansi blue)($name)(ansi reset)  (ansi dark_gray)[($fq)](ansi reset)"
    "fn"  => $"(ansi white)($name)(ansi reset)  (ansi dark_gray)[($fq)](ansi reset)"
    _     => $"(ansi white)($name)(ansi reset)  (ansi dark_gray)[($fq)](ansi reset)"
  }
}

# Decide if an fqpath is a known function
def _is_fn_fq [fn_index:list<record<fqpath: string>> , fq:string] {
  ($fn_index | where fqpath == $fq | length) > 0
}

# Map: module_path -> { alias_or_leaf -> external_crate_name }
def _alias-map-by-module [rows:list<record>] {
  let exts = (_external-crate-map)
  $rows
  | where kind == 'use'
  | where {|u| not (($u.fqpath | default '') | str ends-with '::*') }  # skip globs here
  | each {|u|
      let mp    = ($u.module_path | default [] | str join '::')
      let path  = ($u.fqpath | default '')
      if ($path == '' or $path == 'crate') { null } else {
        let segs = ($path | split row '::')
        let first = (if ($segs | is-empty) { '' } else { $segs | get 0 })
        # Only care if the 'first' segment is an external crate
        let is_ext = (try { $exts | get $first } catch { null }) == true
        if (not $is_ext) { null } else {
          # Determine binding name visible in this module:
          # - if signature had "as Alias", rust-use-records put that in `name`
          # - else leaf of the path
          let bind = (try { $u.name } catch { null }) | default ( $segs | last )
          { mod: $mp, bind: $bind, crate: $first }
        }
      }
    }
  | where {|x| $x != null }
  | group-by mod
  | transpose mod items
  | reduce -f {} {|it, acc|
      let pairs = ($it.items | each {|r| { ($r.bind): $r.crate } } )
      let merged = ($pairs | reduce -f {} {|p, a| $a | merge $p })
      $acc | upsert $it.mod $merged
    }
}

# Canonicalize an fqpath by removing generic/lifetime args in each segment.
# Accepts null/empty and returns "" in that case.
def _fq_canon [fq?: string] {
  let s = ($fq | default "" | into string)
  if $s == "" { "" } else {
    ($s
     | split row '::'
     | each {|seg|
         $seg
         | str replace --regex --all '<[^>]*>' ''   # strip <...>
         | str replace --regex --all '\s+' ''       # strip spaces
       }
     | str join '::')
  }
}

# Return table: { fqpath, uses: list<string>, maybe: list<string> }
def _scan-ext-refs-on-fns [rows:list<record>] {
  let ex_crates = (_external-crate-set)
  let ex_set    = ($ex_crates | reduce -f {} {|c, acc| $acc | upsert $c true })
  let globs     = (_ext_globs_by_module $rows)
  let aliases   = (_alias-map-by-module $rows)

  let fns = ($rows | where kind == 'fn')

  # split into “pathish” tokens that contain '::'
  def _path_tokens [s:string] {
    $s
    | split row -r '[^A-Za-z0-9_:]+'
    | where {|t| $t | str contains '::'}
  }

  $fns
  | each {|f|
      let mp     = ($f.module_path | default [] | str join '::')
      let glb    = (try { $globs | get $mp } catch { [] }) | default []
      let a_map  = (try { $aliases | get $mp } catch { {} })

      let sig  = ($f.signature | default '')
      let body = ($f.body_text | default '')
      let txt  = $"($sig)\n($body)"

      # 1) gather full path tokens
      let paths = (_path_tokens $txt)

      # 2) map token → (dep, sym) by resolving first segment via external set or alias map
      let details = (
        $paths
        | each {|p|
            let segs = ($p | split row '::')
            if ($segs | is-empty) { null } else {
              let first = ($segs | get 0)
              let dep0  = (if ((try { $ex_set | get $first } catch { false }) == true) {
                            $first
                          } else {
                            (try { $a_map | get $first } catch { null })
                          })
              if ($dep0 == null) { null } else {
                let sym = ($segs | skip 1 | str join '::')  # what’s used *within* the dep
                { dep: ($dep0 | str downcase), sym: $sym }
              }
            }
          }
        | where {|x| $x != null }
      )

      # 3) coarse “uses” set (deps only) for compatibility
      let direct_deps = ($details | get dep | uniq | sort | default [])

      # 4) maybe (glob) heuristic
      let maybe_from_glob = if ($glb | is-empty) {
        []
      } else {
        if ($txt | str contains '(') { $glb } else { [] }
      }

      {
        fqpath: $f.fqpath,
        uses:   $direct_deps,
        maybe:  ($maybe_from_glob | uniq | sort),
        uses_detail: (
          $details
          | group-by dep
          | transpose dep items
          | each {|g| { dep: $g.dep, syms: ($g.items | get sym | where {|s| ($s | default '' | str length) > 0 } | uniq | sort) } }
        )
      }
    }
}

# Build canonical adjacency and canon→real map using rows that already have `callers: [...]`
def _adj_from_rows [
  rows: list<record>,   # output of rust-ast (already has callers on fn rows)
] {
  let fns = ($rows | where kind == 'fn')

  # canon→real display names (prefer de-duped, sorted)
  let canon2real = (
    $fns
    | reduce -f {} {|r, acc|
        let fq  = ($r.fqpath | default '')
        if $fq == '' { $acc } else {
          let c = (_fq_canon $fq)
          let cur = (try { $acc | get $c } catch { [] })
          $acc | upsert $c ($cur | append $fq | uniq | sort)
        }
      }
  )

  # Two directed adjacencies from the same data:
  #  - callers_of:   callee_canon -> [caller_canon]
  #  - callees_of:   caller_canon -> [callee_canon]
  let callers_of = (
    $fns
    | reduce -f {} {|r, acc|
        let callee_fq = ($r.fqpath | default '')
        if $callee_fq == '' { $acc } else {
          let callee_c = (_fq_canon $callee_fq)
          let callers  = ($r.callers | default [] | where {|x| ($x | default '') != '' })
          let caller_cs = ($callers | each {|cfq| _fq_canon $cfq })
          let cur = (try { $acc | get $callee_c } catch { [] })
          $acc | upsert $callee_c ($cur | append $caller_cs | flatten | uniq | sort)
        }
      }
  )

  let callees_of = (
    $fns
    | reduce -f {} {|r, acc|
        let callee_fq = ($r.fqpath | default '')
        let callers   = ($r.callers | default [])
        if ($callee_fq == '' or ($callers | is-empty)) { $acc } else {
          let callee_c = (_fq_canon $callee_fq)
          $callers
          | each {|caller_fq|
              let caller_c = (_fq_canon $caller_fq)
              let cur = (try { $acc | get $caller_c } catch { [] })
              $acc | upsert $caller_c ($cur | append $callee_c | uniq | sort)
            }
          | reduce -f $acc {|_, a| $a }  # passthrough
        }
      }
  )

  { callers_of: $callers_of, callees_of: $callees_of, canon2real: $canon2real }
}

# Map: module_path (joined by ::) -> set<external_crate_with_glob_import>
def _ext_globs_by_module [rows:list<record>] {
  let exts = (_external-crate-map)
  $rows
  | where kind == 'use'
  | where {|u| ($u.fqpath | default '') | str ends-with '::*' }
  | each {|u|
      let mp  = ($u.module_path | default [] | str join '::')
      let fst = ($u.fqpath | split row '::' | get 0)
      let is_ext = (try { $exts | get $fst } catch { null }) == true
      if $is_ext { { mod: $mp, ext: $fst } } else { null }
    }
  | where {|x| $x != null }
  | group-by mod
  | transpose mod items
  | reduce -f {} {|it, acc| $acc | upsert $it.mod ($it.items | get ext | uniq | sort) }
}

# Walk callers-of adjacency to roots, printing a tree (internal-only nodes).
def _walk-callers-tree [
  callers_of: record,           # canon -> [canon callers]
  canon2real: record,           # canon -> [real fqs]
  node_c: string,
  max_depth:int,
  ancestors_last:list<bool> = [],
  visited:list<string> = []
] {
  let cyc = ($visited | any {|v| $v == $node_c })
  let prefix = ($ancestors_last | each {|last| if $last { "   " } else { "|  " } } | str join "")
  let tee    = (if ($ancestors_last | length) == 0 { "" } else { "|- " })
  let real0  = (try { $canon2real | get $node_c | get 0 } catch { $node_c })
  mut out = [ $"($prefix)($tee)(ansi white)($real0 | split row '::' | last)(ansi reset)  (ansi dark_gray)[($real0)](ansi reset)" ]

  if $cyc or $max_depth <= 0 {
    if $cyc { let out = ($out | append $"($prefix)   (ansi red)⟲ cycle(ansi reset)") }
    return $out
  }

  let parents = (try { $callers_of | get $node_c } catch { [] }) | default [] | uniq | sort
  let n = ($parents | length)
  for i in 0..<( $n ) {
    let p = ($parents | get $i)
    let sub = _walk-callers-tree $callers_of $canon2real $p ($max_depth - 1) ($ancestors_last | append ($i == ($n - 1))) ($visited | append $node_c)
    let out = ($out | append $sub)
  }
  $out
}

# Renders a callers tree (up to max depth) for a canon name.
# callers: map<canon -> list<canon>>
# canon2real: map<canon -> list<fqpath>>
def _render_callers_tree [root maxd callers canon2real root_label?: string] {

  let C_hdr = (ansi cyan)
  let C_fn  = (ansi white)
  let C_fq  = (ansi dark_gray)
  let C_br  = (ansi dark_gray)
  let R     = (ansi reset)

  def _fq_of [canon canon2real] {
    let v = ($canon2real | get -i $canon | default [])
    if ($v | length) > 0 { $v | get 0 } else { $canon }
  }

  # --- add: seen set to avoid repeats (see §2) ---
  def _go [node prefix depth_left callers canon2real seen:list<string>] {
    if $depth_left <= 0 { return [] }

    let parents = ($callers | get -i $node | default [] | enumerate)
    if ($parents | is-empty) { return [] }

    let last_idx = (($parents | length) - 1)
    mut out = []
    for it in $parents {
      let p = $it.item
      let i = $it.index
      let is_last = ($i == $last_idx)
      let branch  = (if $is_last { "└─ " } else { "├─ " })
      let cont    = (if $is_last { "   " } else { "│  " })

      let fq    = (_fq_of $p $canon2real)
      let short = (_leaf $fq)

      # if we've already shown this node, mark and don’t expand it again
      if ($seen | any {|x| $x == $p }) {
        let line = $"($C_br)($prefix)($branch)($R)($C_fn)($short)($R)  ($C_fq)[($fq)] (ansi red)⟲(ansi reset)"
        $out = ($out | append $line)
        continue
      }

      let line = $"($C_br)($prefix)($branch)($R)($C_fn)($short)($R)  ($C_fq)[($fq)]($R)"
      $out = ($out | append $line)
      $out = ($out | append (_go $p $"($prefix)($cont)" ($depth_left - 1) $callers $canon2real ($seen | append $p)))
    }
    $out
  }

  let header = $"($C_hdr)Call graph depth: ($maxd) ← callers of (_fq_of $root $canon2real)($R)"

  let first  = if ($root_label | default '' | str length) > 0 {
    $root_label
  } else {
    let fq0   = (_fq_of $root $canon2real)
    let short = (_leaf $fq0)
    $"($C_fn)($short)($R)  ($C_fq)[($fq0)]($R)"
  }

  [ $header, $first ] | append (_go $root "" $maxd $callers $canon2real [])
}

def _leaf-name [fq:string] {
  $fq | split row '::' | last
}

# --- helper: build a nested tree from a set of fqpaths (and optionally leaf annotations)
# Accept an optional prebuilt rows index to avoid recomputing rust-ast during tree build.
# Build a nested tree from a set of fqpaths (and optionally leaf annotations).
# fq_list can be any type; we'll sanitize it to a list<string>.
def _tree_from_fqpaths [
  fq_list:any,                      # tolerant input
  leaf_info?: record,               # optional: { <fq> -> {...} }
  rows_idx?: record                 # optional: { <fq> -> minimal row }
] {
  # 0) sanitize inputs to a clean, unique, sorted list<string>
  let fqs = (
    [ $fq_list ] | flatten
    | where {|x| ($x | describe) == 'string' }
    | where {|x| ($x | str length) > 0 }
    | uniq | sort
  )

  if ($fqs | is-empty) {
    return [ { kind: "mod", name: "crate", fqpath: "crate", children: [] } ]
  }

  # 1) collect all intermediate nodes from crate to each leaf
  def _chains_of [fq:string] {
    let parts = ($fq | split row '::')
    0..<( $parts | length )
    | each {|i| ($parts | take ($i + 1) | str join '::') }
  }

  let all_nodes = (
    $fqs
    | reduce -f (["crate"]) {|fq, acc| $acc | append (_chains_of $fq) }
    | flatten | uniq | sort
  )

  # 2) parent → children table (query via a list to avoid 'nothing' inputs)
  let edges = (
    $all_nodes
    | each {|fq|
        if $fq == "crate" { null } else {
          let parent = ($fq | split row '::' | drop 1 | str join '::' | default "crate")
          { parent: (if $parent == "" { "crate" } else { $parent }), child: $fq }
        }
      }
    | where {|x| $x != null }
    | group-by parent
    | transpose parent rows
    | each {|g| { parent: $g.parent, children: ($g.rows | get child | uniq | sort) } }
  )

  # 3) minimal row lookup (prefer provided index, else fall back)
  let idx = (
    if (($rows_idx | describe) =~ '^record<') { $rows_idx } else { _rows-index (rust-ast) }
  )

  def _mk_node [fq:string] {
    let base = (try { $idx | get $fq } catch { null })
    if $base != null { $base } else {
      { kind: "mod", name: ($fq | split row '::' | last), fqpath: $fq, children: [] }
    }
  }

  def _nest [fq:string] {
    let base = (_mk_node $fq)
    let kids_fq = (
      [ $edges ] | flatten
      | where parent == $fq
      | get 0?
      | get -i children
      | default []
    )
    let kids = (
      $kids_fq
      | each {|c| _nest $c }
      | where {|x| (($x | describe) =~ '^record<') }
    )

    let node0 = ($base | upsert children $kids)

    if (($leaf_info | describe) =~ '^record<') and ($kids | is-empty) {
      let ann = (try { $leaf_info | get $fq } catch { null })
      if $ann == null { $node0 } else { $node0 | merge $ann }
    } else {
      $node0
    }
  }

  [ (_nest "crate") ]
}

# --- modify: print-dep-usage to support --records output and piped rows (no nested rust-ast calls)
export def print-dep-usage [
  dep?: string
  --max-depth:int = 4
  --include-maybe
  --records                 # emit nested records instead of pretty text
] {
  # Prefer piped rows if present
  let piped = $in
  let rows = (
    if (( $piped | describe ) =~ '^(list|table)') and (not ($piped | is-empty)) and (($piped | first | describe) =~ '^record<') {
      $piped
    } else {
      rust-ast
    }
  )

  let ext_set = (_external-crate-set)
  if ($ext_set | is-empty) {
    error make { msg: "print-dep-usage: no external deps found in Cargo.toml" }
  }

  let scanned    = (_scan-ext-refs-on-fns $rows)
  let built      = (_adj_from_rows $rows)
  let callers    = $built.callers_of
  let canon2real = $built.canon2real
  let rows_idx   = (_rows-index $rows)   # build once, pass into tree builder

  # Build per-dep index like before (real/maybe)
  mut dep_index = {}
  for row in $scanned {
    let fq       = ($row.fqpath | default '')
    let uses_det = ($row.uses_detail | default [])
    let maybes   = ($row.maybe | default [])

    for d in $uses_det {
      let key = ($d.dep | str downcase)
      if ($ext_set | any {|e| ($e | str downcase) == $key }) {
        let cur = ($dep_index | get -i $key | default { real: {}, maybe: {} })
        let cur_syms = ($cur.real | get -i $fq | default [])
        let next_syms = ($cur_syms | append $d.syms | flatten | uniq | sort)
        let nxt = ($cur | upsert real ($cur.real | upsert $fq $next_syms))
        $dep_index = ($dep_index | upsert $key $nxt)
      }
    }

    if $include_maybe {
      for m in $maybes {
        let key = ($m | str downcase)
        if ($ext_set | any {|e| ($e | str downcase) == $key }) {
          let cur = ($dep_index | get -i $key | default { real: {}, maybe: {} })
          let cur_syms = ($cur.maybe | get -i $fq | default [])
          let nxt = ($cur | upsert maybe ($cur.maybe | upsert $fq $cur_syms))
          $dep_index = ($dep_index | upsert $key $nxt)
        }
      }
    }
  }

  let wanted = if ($dep | default '' | str length) > 0 {
    let key = ($dep | str downcase)
    if ($dep_index | columns | any {|k| $k == $key }) { [ $key ] } else { [] }
  } else {
    ($dep_index | columns | sort)
  }

  # If --records is set: build nested trees (one per dep if dep not specified; else just the tree)
  if $records {
    mut out = []
    for crate_name in $wanted {
      let info = ($dep_index | get $crate_name | default { real: {}, maybe: {} })
      let seeds_real_map   = ($info.real  | default {})
      let seeds_maybe_map  = ($info.maybe | default {})

      # limit maybe-seeds to callers-path of real seeds
      def _collect_ancestors [start_canon: string, maxd: int] {
        mut seen = [$start_canon]
        mut frontier = [$start_canon]
        mut depth = 0
        while ( $depth < $maxd ) {     # tightened from <= to <
          mut nxt = []
          for n in $frontier {
            let parents = ($callers | get -i $n | default [])
            for p in $parents {
              if not ($seen | any {|x| $x == $p }) {
                $seen = ($seen | append $p)
                $nxt  = ($nxt  | append $p)
              }
            }
          }
          if ($nxt | is-empty) { break }
          $frontier = $nxt
          $depth = ($depth + 1)
        }
        $seen
      }

      let seeds_real  = ($seeds_real_map  | columns | sort)
      let seeds_maybe_all = ($seeds_maybe_map | columns | sort)

      mut on_path = {}
      for s in $seeds_real {
        let c = (_fq_canon $s)
        let anc = (_collect_ancestors $c $max_depth)
        for a in $anc { $on_path = ($on_path | upsert $a true) }
      }
      let seeds_maybe = (
        $seeds_maybe_all
        | where {|fq| ($on_path | get -i (_fq_canon $fq) | default false) }
      )

      let keep_fqs = ($seeds_real | append $seeds_maybe)

      if (not ($keep_fqs | is-empty)) {
        # annotate leaves: { fq -> {dep, ref_type, uses} }
        let leaf_info = (
          $keep_fqs
          | reduce -f {} {|fq, acc|
              let uses = ($seeds_real_map | get -i $fq | default [])
              let ref_type = (if (not ($uses | is-empty)) { "real" } else { "maybe" })
              $acc | upsert $fq {
                dep: $crate_name,
                ref_type: $ref_type,
                uses: $uses
              }
            }
        )

        # IMPORTANT: pass rows_idx so _tree_from_fqpaths doesn't call rust-ast
        let tree = (_tree_from_fqpaths $keep_fqs $leaf_info $rows_idx)

        if ($wanted | length) == 1 {
          return $tree
        } else {
          $out = ($out | append { dep: $crate_name, tree: $tree })
        }
      }
    }
    return $out
  }

  # ---------- original pretty printing path (unchanged except tighter BFS) ----------
  for crate_name in $wanted {
    let info = ($dep_index | get $crate_name | default { real: {}, maybe: {} })
    let seeds_real_map   = ($info.real  | default {})
    let seeds_maybe_map  = ($info.maybe | default {})
    let seeds_real       = ($seeds_real_map  | columns | sort)
    let seeds_maybe_all  = ($seeds_maybe_map | columns | sort)

    if (($seeds_real | is-empty) and ($seeds_maybe_all | is-empty)) { continue }

    print $"(ansi green)Dependency usage: (ansi red)($crate_name)(ansi reset)"

    def _collect_ancestors [start_canon: string, maxd: int] {
      mut seen = [$start_canon]
      mut frontier = [$start_canon]
      mut depth = 0
      while ( $depth < $maxd ) {       # tightened from <= to <
        mut nxt = []
        for n in $frontier {
          let parents = ($callers | get -i $n | default [])
          for p in $parents {
            if not ($seen | any {|x| $x == $p }) {
              $seen = ($seen | append $p)
              $nxt  = ($nxt  | append $p)
            }
          }
        }
        if ($nxt | is-empty) { break }
        $frontier = $nxt
        $depth = ($depth + 1)
      }
      $seen
    }

    mut on_path = {}
    for s in $seeds_real {
      let c = (_fq_canon $s)
      let anc = (_collect_ancestors $c $max_depth)
      for a in $anc { $on_path = ($on_path | upsert $a true) }
    }
    let seeds_maybe = (
      $seeds_maybe_all
      | where {|fq| ($on_path | get -i (_fq_canon $fq) | default false) }
    )

    if (not ($seeds_real | is-empty)) {
      print $"(ansi dark_gray)direct references(ansi reset)"
      for s in $seeds_real {
        let c = (_fq_canon $s)
        let sym_list = ($seeds_real_map | get -i $s | default [] | uniq | sort)
        let sym_suffix = if ($sym_list | is-empty) { "" } else {
          $" (ansi dark_gray)uses:(ansi reset) (ansi light_yellow)($sym_list | str join ', ')(ansi reset)"
        }
        let leaf = (_leaf $s)
        let root_lbl = $"(ansi white)($leaf)(ansi reset)  (ansi dark_gray)[($s)](ansi reset)($sym_suffix)"
        let lines = (_render_callers_tree $c $max_depth $callers $canon2real $root_lbl)
        for ln in $lines { print $ln }
        print ""
      }
    }

    if $include_maybe and (not ($seeds_maybe | is-empty)) {
      print $"(ansi dark_gray)[?] from glob imports(ansi reset)"
      for s in $seeds_maybe {
        let c = (_fq_canon $s)
        let leaf = (_leaf $s)
        let root_lbl = $"(ansi white)($leaf)(ansi reset)  (ansi dark_gray)[($s)](ansi reset)"
        let lines = (_render_callers_tree $c $max_depth $callers $canon2real $root_lbl)
        for ln in $lines { print $ln }
        print ""
      }
    }
  }
}
