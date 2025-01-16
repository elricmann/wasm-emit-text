## wasm-emit-text

Lightweight library for **programmatically** generating WebAssembly Text Format (WAT) code. This library would be useful when implementing a custom language and targeting WebAssembly since it offers an ergonomic builder-pattern API for constructing WAT modules, functions, and instructions.

### Features

- [x] Builder pattern API for creating WAT modules
- [x] Support for functions, parameters, locals, and control flow
- [x] Full support for WAT numeric instructions and binary operations
- [x] Memory and global variable support
- [x] Import/export functionality
- [x] Pretty-printing of generated WAT code
- [ ] Complete WebAssembly specification

### Installation

Add the dependency below to your `Cargo.toml`:

```toml
[dependencies]
wasm-emit-text = "0.1.0"
```

### Quick Example

Let's generate a simple factorial function:

```rust
use wasm_emit_text::{Module, Function, ValueType, Instruction, Const, BinaryOp, Local, WatEmitter};

let factorial: Function<_> = Function::new()
    .with_name("factorial")
    .add_param("n", ValueType::I32)
    .add_result(ValueType::I32)
    .add_local(Local {
        name: "o".to_string(),
        typ: ValueType::I32,
    })
    .add_instructions(vec![
        // o = 1
        Instruction::SetLocal {
            name: "o".to_string(),
            value: Box::new(Instruction::Const(Const::I32(1))),
        },
        // block $exit
        Instruction::Block {
            label: "exit".to_string(),
            instructions: vec![
                // loop $factorial_loop
                Instruction::Loop {
                    label: Some("factorial_loop".to_string()),
                    body: Box::new(vec![
                        // br_if $exit (i32.eqz (local.get $n))
                        Instruction::BrIf {
                            label: "exit".to_string(),
                            condition: Box::new(Instruction::Eqz {
                                ty: ValueType::I32,
                                value: Box::new(Instruction::GetLocal("n".to_string())),
                            }),
                        },
                        // o = o * n
                        Instruction::SetLocal {
                            name: "o".to_string(),
                            value: Box::new(Instruction::BinOp {
                                op: BinaryOp::Mul,
                                ty: ValueType::I32,
                                lhs: Box::new(Instruction::GetLocal("o".to_string())),
                                rhs: Box::new(Instruction::GetLocal("n".to_string())),
                            }),
                        },
                        // n = n - 1
                        Instruction::SetLocal {
                            name: "n".to_string(),
                            value: Box::new(Instruction::BinOp {
                                op: BinaryOp::Sub,
                                ty: ValueType::I32,
                                lhs: Box::new(Instruction::GetLocal("n".to_string())),
                                rhs: Box::new(Instruction::Const(Const::I32(1))),
                            }),
                        },
                        // br $factorial_loop
                        Instruction::Br("factorial_loop".to_string()),
                    ]),
                },
            ],
        },
        // return o
        Instruction::GetLocal("o".to_string()),
    ]);

let module = Module::new()
    .with_name("factorial")
    .add_function(factorial);

// either:
// 1. write to file
write_wat_to_file(&module, "factorial.wat").unwrap();

// 2. or access the buffer
let mut output = Vec::new();
WatEmitter::emit_wat(&module, &mut output)?;
let wat_string = String::from_utf8(output).unwrap();
```

This generates a `factorial.wat` file containing:

```wat
(module
  ;; name: factorial
  (func $factorial (param $n i32) (result i32)
    (local $o i32)
    (local.set $o
      (i32.const 1))
    (block $exit
      (loop $factorial_loop
        (br_if $exit
          (i32.eqz
            (local.get $n)))
        (local.set $o
          (i32.mul
            (local.get $o)
            (local.get $n)))
        (local.set $n
          (i32.sub
            (local.get $n)
            (i32.const 1)))
        (br $factorial_loop)))
    (local.get $o)))
```

Refer to the test in [`src/lib.rs`](https://github.com/elricmann/wasm-emit-text/blob/main/src/lib.rs) for more cases.

### License

Copyright © 2025 Elric Neumann. MIT License.
