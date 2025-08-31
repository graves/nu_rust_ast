# Rust Symbol Harvesting with Nushell 🌾
**A CLI Tool for Structured Symbol Analysis of Rust Projects**

```
                   ,_  .--.
             , ,   _)\/    ;--.
     . ' .    \_\-'   |  .'    \
    -= * =-   (.-,   /  /       |
     ' .\'    ).  ))/ .'   _/\ /
         \_   \_  /( /     \ /(
         /_\ .--'   `-.    //  \
         ||\/        , '._//    |
         ||/ /`(_ (_,;`-._/     /
         \_.'   )   /`\       .'
              .' .  |  ;.   /`
             /      |\(  `.(
            |   |/  | `    `
            |   |  /
            |   |.'
         __/'  /
     _ .'  _.-`
  _.` `.-;`/
 /_.-'` / /
       | /
jgs   ( /
     /_/
```

---

## 🧑🏽‍🌾 *What This Does*
  
This script is your trusty sidekick for **harvesting symbols** from Rust projects into structured Nushell records. It uses `ast-grep` to:  
- Extract **all Rust identifiers** (`fn`, `struct`, `enum`, `trait`, `mod`, etc.)  
- Normalize metadata (file, span, visibility, fully qualified paths)  
- Capture **Rustdoc comments** and **full source code bodies** (when applicable).  
- Estimate token count of source code and documentation blocks.

Think of it as a Nushell first, *Rust AST explorer*. It’s your go-to tool for reverse-engineering codebases, analyzing symbol usage, and generating documentation from raw source files.  

---

## 🧠 *Core Features*  
### 1. **Structured Symbol Tables**  
Each row represents a Rust symbol with:  
| Field             | Description                                                                                     |
|------------------|------------------------------------------------------------------------------------------------|
| `kind`           | Symbol type (e.g., `'fn'`, `'struct'`, `'mod'`, etc.)                                         |
| `name`           | Best-effort symbol name (e.g., `'*'` for grouped-use leaves, or file/module name for synthetic `mod`s) |
| `crate`          | Package name from `Cargo.toml` or `"crate"` as fallback                                         |
| `module_path`    | List of strings (e.g., `[A, B]`) representing module hierarchy under `src/`                    |
| `fqpath`         | Canonicalized path (e.g., `"crate::A::B"`) or UFCS for trait/impl methods                       |
| `visibility`     | Visibility (`pub`, `private`, or nested: e.g., `"pub(crate)"`)                                 |
| `file`           | Absolute/expanded file path                                                                    |
| `span`           | Line/column spans (inclusive 1-based)                                                            |
| `attrs`          | Reserved list (empty by default)                                                               |
| `signature`      | Normalized signature/preamble (no body, e.g., `"pub fn foo()`)                                 |
| `has_body`       | Boolean: true if the symbol has a body (`{ ... }`)                                             |
| `async/unsafe/const` | Best-effort flags parsed from signature text (e.g., `"async"`, `"unsafe"`).                |
| `abi/generics/where` | Captured metadata (e.g., generics, where clauses) if present                                  |
| `doc`            | Verbatim Rustdoc comments or inner-file docs (`//!`, `/*! ... */`)                             |
| `impl_of`        | For `'impl'` and `'fn'` inside impls: contains `trait_path?`, `type_path?`.                    |
| `trait_items`    | Reserved list (empty by default)                                                               |
| `reexports`      | Reserved list (empty by default)                                                               |
| `body_text`      | Exact source text of the symbol if `want_body=true`.                                           |
| `synthetic`      | Boolean: true for synthetic `'mod'` rows covering entire files (`src/foo.rs`, etc.)             |
| `doc_tokens`     | Token estimate for the `doc` field (0 if empty).                                               |
| `body_tokens`    | Token estimate for the `body_text` field (0 if empty).                                         |
---

### 2. **Ast-grep Integration**
 
- Uses `ast-grep` with patterns and modifiers to parse Rust code.  
- Utilizes `ast-grep`'s json output (`--json=stream`).  

### 3. **Optimized for Large Projects**

- Synthesizes "file mod" rows (for `src/mod.rs`) and includes full file text.  
- Normalizes module paths from source layouts (`src/foo.rs` → `["foo"]`).  

### 4. **Rustdoc & Token Counts**
  
- Extracts leading `///`/`#`[`...`] comments.  
- Estimates token counts for `doc` and `body_text`.  

### 5. **Call Site Analysis**
  
Identifies *call sites* with qualifiers (e.g., `Type::new(...)`) and attaches them to function definitions.  

---

## 🧪 *How to Use*
  
### 1. **Install Dependencies**
  
```nu
brew install ast-grep
cd $"($nu.data-dir)/scripts"
wget https://raw.github.com/graves/nu_rust_ast/tree/main/nu_rust_ast.nu
```

### 2. **Export the functions to your Nushell environment**

Add the following to your Nushell configuration file `$nu.config-path`:

```nu
use $"($nu.data-dir)/scripts/rust_ast.nu" *
```

Reload your shell.

### 3. Call the functions
  
```nu
λ rust-ast --help
Public entry point.

Usage:
  > rust-ast ...(paths)

Flags:
  -h, --help: Display the help message for this command

Parameters:
  ...paths <string>

Input/output types:
  ╭───┬───────┬────────╮
  │ # │ input │ output │
  ├───┼───────┼────────┤
  │ 0 │ any   │ any    │
  ╰───┴───────┴────────╯
```

```nu
λ rust-ast
| where kind == 'fn'
| select name fqpath callers
╭───┬────────────────────┬───────────────────────────┬──────────────────────────────╮
│ # │        name        │          fqpath           │           callers            │
├───┼────────────────────┼───────────────────────────┼──────────────────────────────┤
│ 0 │ main               │ crate::main               │ [list 0 items]               │
│ 1 │ process_files      │ crate::process_files      │ ╭───┬─────────────╮          │
│   │                    │                           │ │ 0 │ crate::main │          │
│   │                    │                           │ ╰───┴─────────────╯          │
│ 2 │ write_row_to_file  │ crate::write_row_to_file  │ ╭───┬──────────────────────╮ │
│   │                    │                           │ │ 0 │ crate::process_files │ │
│   │                    │                           │ ╰───┴──────────────────────╯ │
│ 3 │ fetch_with_backoff │ crate::fetch_with_backoff │ ╭───┬──────────────────────╮ │
│   │                    │                           │ │ 0 │ crate::process_files │ │
│   │                    │                           │ ╰───┴──────────────────────╯ │
╰───┴────────────────────┴───────────────────────────┴──────────────────────────────╯
```

```nu
λ rust-ast
| where kind == 'fn' and name == 'process_files'
| select doc doc_tokens body_text body_tokens
╭─────┬──────────────────────────────────────────────────────────────────────────────────┬─────────────┬─────────────────────────────────────────────────────────────────────────────────────────────────────┬──────╮
│   # │                                       doc                                        │ doc_tokens  │                                              body_text                                              │ ...  │
├─────┼──────────────────────────────────────────────────────────────────────────────────┼─────────────┼─────────────────────────────────────────────────────────────────────────────────────────────────────┼──────┤
│   0 │ /// Process `.txt` files under the given directory and sanitize their contents.  │         136 │ async fn process_files(                                                                             │  ... │
│     │ ///                                                                              │             │     input_dir: &PathBuf,                                                                            │      │
│     │ /// - Splits each file into ~500-token chunks.                                   │             │     output_dir_path: &str,                                                                          │      │
│     │ /// - Submits each chunk to the model using [`fetch_with_backoff`].              │             │     config: AwfulJadeConfig,                                                                        │      │
│     │ /// - Appends sanitized chunks to a YAML file named after the input file.        │             │ ) -> Result<(), String> {                                                                           │      │
│     │ ///                                                                              │             │     // Initialize tokenizer for tokenization                                                        │      │
│     │ /// # Parameters                                                                 │             │     let tokenizer = cl100k_base().map_err(|e| e.to_string())?;                                      │      │
│     │ /// - `input_dir`: Path to directory containing `.txt` files.                    │             │     let max_tokens = 500;                                                                           │      │
│     │ /// - `output_dir_path`: Path where YAML files are written.                      │             │                                                                                                     │      │
│     │ /// - `config`: Configuration for model endpoint.                                │             │     // Configure text splitter to chunk content                                                     │      │
│     │ ///                                                                              │             │     let splitter = TextSplitter::new(ChunkConfig::new(max_tokens).with_sizer(tokenizer));           │      │
│     │ /// # Errors                                                                     │             │                                                                                                     │      │
│     │ /// Returns `Err(String)` on filesystem, config, or API errors. Errors for       │             │     // Load template for sanitization                                                               │      │
│     │ /// individual files/chunks are logged and do not abort other files.             │             │     let template = template::load_template("book_txt_sanitizer")                                    │      │
│     │ ///                                                                              │             │         .await                                                                                      │      │
│     │ /// # Example                                                                    │             │         .map_err(|e| format!("Template load error: {e}"))?;                                         │      │
│     │ /// ```no_run                                                                    │             │                                                                                                     │      │
│     │ /// # async fn demo(cfg: awful_aj::config::AwfulJadeConfig) {                    │             │     // Process each file in the input directory                                                     │      │
│     │ /// let res = process_files(&"/tmp/books".into(), "/tmp/out", cfg).await;        │             │     for entry in fs::read_dir(input_dir).map_err(|e| e.to_string())? {                              │      │
│     │ /// if let Err(err) = res {                                                      │             │         let entry = entry.map_err(|e| e.to_string())?;                                              │      │
│     │ ///     eprintln!("Sanitization failed: {err}");                                 │             │         let path = &entry.path();                                                                   │      │
│     │ /// }                                                                            │             │                                                                                                     │      │
│     │ /// # }                                                                          │             │         // Check if the file is a `.txt` text file                                                  │      │
│     │                                                                                  │             │         if path.is_file() && path.extension().and_then(|s| s.to_str()) == Some("txt") {             │      │
│     │                                                                                  │             │             let filename = path.file_name().unwrap().to_string_lossy();                             │      │
│     │                                                                                  │             │             let mut yaml_path = format!("{}/{}.yaml", output_dir_path, filename);                   │      │
│     │                                                                                  │             │                                                                                                     │      │
│     │                                                                                  │             │             // Open YAML file for writing                                                           │      │
│     │                                                                                  │             │             let mut file = fs::OpenOptions::new()                                                   │      │
│     │                                                                                  │             │                 .create(true)                                                                       │      │
│     │                                                                                  │             │                 .append(true)                                                                       │      │
│     │                                                                                  │             │                 .open(&yaml_path)                                                                   │      │
│     │                                                                                  │             │                 .map_err(|e| e.to_string())?;                                                       │      │
│     │                                                                                  │             │                                                                                                     │      │
│     │                                                                                  │             │             // Write YAML header                                                                    │      │
│     │                                                                                  │             │             writeln!(file, "chunks:").map_err(|e| e.to_string())?;                                  │      │
│     │                                                                                  │             │                                                                                                     │      │
│     │                                                                                  │             │             // Read and process the text content                                                    │      │
│     │                                                                                  │             │             let contents = fs::read_to_string(&path).map_err(|e| e.to_string())?;                   │      │
│     │                                                                                  │             │             let chunks: Vec<_> = splitter.chunks(&contents).collect();                              │      │
│     │                                                                                  │             │                                                                                                     │      │
│     │                                                                                  │             │             // Process each chunk                                                                   │      │
│     │                                                                                  │             │             for chunk in chunks {                                                                   │      │
│     │                                                                                  │             │                 let book_chunk = fetch_with_backoff(&config, &chunk, &template)                     │      │
│     │                                                                                  │             │                     .await                                                                          │      │
│     │                                                                                  │             │                     .map_err(|e| e.to_string())?;                                                   │      │
│     │                                                                                  │             │                                                                                                     │      │
│     │                                                                                  │             │                 if let Some(sanitized_text) = book_chunk {                                          │      │
│     │                                                                                  │             │                     // Write sanitized content to YAML                                              │      │
│     │                                                                                  │             │                     write_row_to_file(sanitized_text, &mut yaml_path).map_err(|e| e.to_string())?;  │      │
│     │                                                                                  │             │                 }                                                                                   │      │
│     │                                                                                  │             │             }                                                                                       │      │
│     │                                                                                  │             │         }                                                                                           │      │
│     │                                                                                  │             │     }                                                                                               │      │
│     │                                                                                  │             │                                                                                                     │      │
│     │                                                                                  │             │     Ok(())                                                                                          │      │
│     │                                                                                  │             │ }                                                                                                   │      │
╰─────┴──────────────────────────────────────────────────────────────────────────────────┴─────────────┴─────────────────────────────────────────────────────────────────────────────────────────────────────┴──────╯
```

```nu
λ rust-ast
| where kind == 'fn' and name == 'process_files'
| select signature body_tokens
╭───┬──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┬─────────────╮
│ # │                                                      signature                                                       │ body_tokens │
├───┼──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┼─────────────┤
│ 0 │ async fn process_files( input_dir: &PathBuf, output_dir_path: &str, config: AwfulJadeConfig, ) -> Result<(), String> │         186 │
╰───┴──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┴─────────────╯
```

---

## 🙋🏻‍♀️ *Why This Matters*
  
This tool is a game-changer for developers who want to:  
- **Debug** complex symbol relationships (e.g., trait impls).  
- **Generate docs** from raw source files.  
- **Analyze code structure** for refactoring or performance tuning.  

It’s a *slightly-hacky, basically reliable* tool that can answer the questions:

> "What is this?", "Where did it come from?", "What does it do?","How good is it?", "Is it documented?", "What's it related to?"

with actionable metadata. 

---

## 🧩 *Limitations & Tips*

- **Performance**: For large crates, consider filtering patterns to reduce output.  
- **Module Paths**: Synthesized `mod` rows are generated based on file structure, not actual imports.  
- **Ast-grep**: Requires `ast-grep` to be installed and accessible in your PATH.  

---

## 🎯 *Example Use Case*
  
```nu
# Analyze all public functions in a crate
rust-ast ./my_crate |
  where kind = 'fn' and visibility = 'public' |
  sort -by fqpath
```

---

## 🚀 *Bonus: Call Site Tracking*

The script automatically maps function definitions to their *call sites*, including qualified paths (e.g., `Type::method(...)`). This is especially useful for tracking how *traits* or *static methods* are used across your codebase.  

---

## 📚 *Further Reading*

- [Ast-grep Documentation](https://ast-grep.github.io/reference/cli.html)  
- [Nushell Language Reference](https://www.nushell.sh/commands/)  

---

## 📦 *License*
  
This script is provided under the **Creative Commons Zero v1.0 Universal**. Use it wisely, meaning (document your code with this, LLMs are good at that).  

--- 

**Contributing?** Feel free to open pull requests or file issues on the repository.  
**Questions?** Send me an email.  

---  
*Written by [Thomas Gentry](https://awfulsec.com) – a real human bean.* 🫛
