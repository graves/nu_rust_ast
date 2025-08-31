
# =============================================================================
# rust_ast.nu — Rust symbol harvesting with ast-grep (CLI-only, --json=stream)
# =============================================================================
#
# WHAT THIS DOES
# --------------
# Scans a Rust crate (or paths you pass) and emits a single, flat table of
# symbols: fn/extern_fn/struct/enum/type/trait/impl/mod/macro_rules/const/static/use
# Each row has normalized metadata (file, byte/line spans, visibility, fqpath,
# module path, etc.). For nodes with bodies (fns, structs with fields, inline
# mods, …) the exact source text is included in body_text when feasible.
#
# IMPORTANT INVARIANTS / CONVENTIONS
# ----------------------------------
# - ast-grep line numbers (range.start.line / range.end.line) are 1-based and
#   *inclusive*. We preserve those in span.start_line / span.end_line.
# - Byte offsets are inclusive start / exclusive end, copied from ast-grep.
# - For symbols matched with braces (`{ … }`) we set want_body=true and fill
#   body_text via _extract-src. This includes fn, struct-with-fields, trait,
#   impl, and inline mod definitions.
# - Tuple/unit structs, trait item declarations, and `mod foo;` declarations
#   have body_text=null.
# - For file modules (e.g., src/foo.rs, src/foo/mod.rs), we *synthesize* a
#   `mod` row that covers the entire file; body_text is the whole file.
# - Each row also carries a `doc` string containing leading Rustdoc comments:
#   contiguous `///`, `#[doc=…]`, or `/** … */` blocks immediately above the
#   item are preserved verbatim. For file modules we additionally capture
#   crate/module-level inner docs (`//!` or `/*! … */`).
# - For fn inside impls we compute a richer fqpath using UFCS where needed:
#   e.g., crate::<MyTy as my::Trait>::method, or crate::MyTy::method for
#   inherent methods. Free functions keep their original fqpath.
#
# OUTPUT SCHEMA (each row)
# ------------------------
# kind:         string   # 'fn'|'extern_fn'|'struct'|'enum'|'type'|'trait'|'impl'|'mod'|'macro_rules'|'const'|'static'|'use'
# name:         string   # best-effort symbol name ('*' for grouped-use leaf, file-mod name for synthetic mods)
# crate:        string   # from Cargo.toml package.name; 'crate' fallback
# module_path:  list     # e.g. ['foo','bar'] computed from file path under src/
# fqpath:       string   # canonicalized path; e.g. 'crate::foo::bar::Baz' or UFCS for impl fns
# visibility:   string   # 'pub'|'pub(crate)'|'pub(super)'|'pub(in …)'|'private'
# file:         string   # absolute/expanded file path
# span:         record   # { start_line:int, end_line:int, start_byte:int, end_byte:int }
# attrs:        list     # reserved ([])
# signature:    string   # normalized single-line signature/preamble (no body)
# has_body:     bool     # syntax has a body ( { ... } ) or not
# async|unsafe|const: bool   # best-effort flags parsed from signature text
# abi|generics|where: string?  # captured meta (when present)
# doc:          string?  # contiguous Rustdoc/#[doc] comments or inner-file docs (preserved verbatim)
# impl_of:      record?  # for 'impl' and 'fn' inside impls: { trait_path?, type_path? }
# trait_items:  list     # reserved
# reexports:    list     # reserved
# body_text:    string?  # exact matched text (want_body=true) or whole file for synthetic mods
# synthetic:    bool?    # only for file-synthesized 'mod' rows
# doc_tokens:   int?     # token estimate for `doc` (0 if empty)
# body_tokens:  int?     # token estimate for `body_text` (0 if empty)
#
# HOW PATTERN VARIABLES MAP IN AST-GREP
# -------------------------------------
# $N    name identifier
# $G    generics/lifetime list node (no angle brackets in the capture itself)
# $P    parameter list (fn)
# $R    return type
# $B    body chunks
# $W    where-clause
# $$$X  multi-capture (sequence) — tolerant of commas/docs/trailing commas
# $X?   optional capture
#
# =============================================================================

# ---------- helpers -----------------------------------------------------------

# Normalize CLI paths: empty => ["."], else pass-through.
def _target-list [...paths:string] {
  if ($paths | is-empty) { [ "." ] } else { $paths }
}

# Read Cargo.toml package.name (best effort). Falls back to "crate".
def _cargo-crate-name [] {
  try {
    open Cargo.toml | from toml | get package.name
  } catch { "crate" }
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

# Run ast-grep safely (returns error if neither `sg` nor `ast-grep` works).
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
# Call sites with qualifiers (when present) without piping through 'nothing'
def _rust-call-sites-on [targets:list<string>] {
  let files = ($targets | where {|f| ($f | default null) != null } | uniq)

  let pats = [
    '$N($$$A)'        # plain: foo(...)
    '$Q::$N($$$A)'    # qualified/assoc/UFCS: path::to::foo(...), Type::new(...)
    '$RECV.$N($$$A)'  # method: recv.foo(...)
  ]

  mut out = []
  for p in $pats {
    let rows = (
      _sg_json_on $p $files
      | each {|raw|
          let s = ($raw.metaVariables.single? | default {})

          let n = ($s | get -i N | default {} | get -i text | default null)
          if $n == null { null } else {
            # Build 'qual' via if/else, never piping from 'nothing'
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
# Accept rows from either an explicit arg or the pipeline
# Accept rows from arg or pipeline and attach a disambiguated 'callers' list
def _attach_callers [rows?: list<record>] {
  let rows = if ($rows | is-empty) { $in } else { $rows }

  let fns   = ($rows | where kind == 'fn')
  let files = ($rows | get file | where {|f| $f != null } | uniq)

  let calls    = (_rust-call-sites-on $files)
  let fn_index = (_index-fns-by-file $fns)
  let idx      = (_build-fn-indexes $fns)

  let pairs = (
    $calls
    | each {|c|
        let caller = (_enclosing-fn $fn_index $c.file ($c.span.start_byte | default 0) ($c.span.end_byte | default 0))
        if $caller == null { null } else {
          let target = (_resolve-call $idx $fns $c $caller)
          if $target == null { null } else { { callee_fq: $target.fqpath, caller_fq: $caller.fqpath } }
        }
      }
    | where {|x| $x != null }
  )

  let callee_to_callers = (
    $pairs
    | group-by callee_fq
    | transpose fq callers
    | each {|g| { fq: $g.fq, callers: ($g.callers | get caller_fq | uniq | sort) } }
  )

  $rows
  | each {|r|
      if $r.kind != 'fn' { $r } else {
        let ent = ($callee_to_callers | where fq == $r.fqpath | get 0?)
        ($r | upsert callers (if $ent == null { [] } else { $ent.callers }))
      }
    }
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

# Group functions by quick keys we’ll use for resolution
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
  fns: list<record>,   # pass only fn rows (not all rows)
  call: record,
  caller_fn?: record
] {
  let name = $call.callee
  let qual = ($call.qual | default '')
  let kind = ($call.kind | default 'plain')

  # Safely read optional fields from caller_fn
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

  # 0) Exact-like: call already fully qualified to crate::...::name
  if ($qual | str starts-with 'crate::') {
    let tail = $"($qual)::($name)"
    let exact = ($idx.by_fqpath | where key == $tail | get 0? | get -i vals | default [])
    if (not ($exact | is-empty)) { return ($exact | get 0) }
  }

  # 1) Qualified (assoc/UFCS) like Type::name or path::to::Type::name
  if ($kind == 'qualified' and ($qual | str contains '::')) {
    let tail = $"($qual)::($name)"
    let cand1 = ($fns | where {|r| ($r.fqpath | default '' | str ends-with $tail) })
    if (not ($cand1 | is-empty)) { return ($cand1 | get 0) }
  } else if ($kind == 'qualified' and (not ($qual | str contains '::'))) {
    let key = $"($qual)::($name)"
    let cand2 = ($idx.impl_methods | where key == $key | get 0? | get -i vals | default [])
    if (not ($cand2 | is-empty)) { return ($cand2 | get 0).row }
  }

  # 2) Method call: prefer same-impl method
  if ($kind == 'method' and $caller_impl_ty != '') {
    let key = $"($caller_impl_ty)::($name)"
    let cand3 = ($idx.impl_methods | where key == $key | get 0? | get -i vals | default [])
    if (not ($cand3 | is-empty)) { return ($cand3 | get 0).row }
  }

  # 3) Same-module free fn
  let key4 = $"($caller_mod)::($name)"
  let cand4 = ($idx.free_fns | where key == $key4 | get 0? | get -i vals | default [])
  if (not ($cand4 | is-empty)) { return ($cand4 | get 0).row }

    # 3b) Same-module free fn by name (only if unique in that module)
  let cand_mod = (
    $fns
    | where name == $name
    | where {|r| ($r.module_path | default []) == $caller_fn.module_path }
  )
  if ($cand_mod | length) == 1 {
    return ($cand_mod | get 0)
  }

  # 4) Fallback by name (only if unique crate-wide)
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

# ---------- Aggregator --------------------------------------------------------
export def rust-ast-records [...paths:string] {
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
  | _attach_callers         # ← add this
}

# Public entry point.
export def rust-ast [...paths:string] {
  rust-ast-records ...$paths
}
