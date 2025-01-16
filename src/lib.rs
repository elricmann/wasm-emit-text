use std::io::{self, Write};
use std::marker::PhantomData;

pub trait WatEmitter {
    fn emit_wat<W: Write>(&self, writer: &mut W) -> io::Result<()>;
}

#[derive(Clone)]
pub struct Module<T> {
    name: Option<String>,
    functions: Vec<Function<T>>,
    imports: Vec<Import>,
    exports: Vec<Export>,
    memory: Option<Memory>,
    globals: Vec<Global>,
}

#[derive(Clone)]
pub struct Function<T> {
    name: Option<String>,
    params: Vec<Parameter>,
    results: Vec<ValueType>,
    locals: Vec<Local>,
    instructions: Vec<Instruction>,
    _phantom: PhantomData<T>,
}

#[derive(Clone)]
pub struct Parameter {
    name: String,
    typ: ValueType,
}

#[derive(Clone)]
pub struct Global {
    name: String,
    typ: ValueType,
    mutable: bool,
    init: Instruction,
}

#[derive(Clone)]
pub struct Import {
    module: String,
    name: String,
    kind: ImportKind,
}

#[derive(Clone)]
pub struct Export {
    name: String,
    kind: ExportKind,
}

#[derive(Clone)]
pub struct Memory {
    name: Option<String>,
    initial: u32,
    maximum: Option<u32>,
}

#[derive(Clone)]
pub struct Local {
    name: String,
    typ: ValueType,
}

#[derive(Clone)]
pub enum ValueType {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Clone)]
pub enum ImportKind {
    Function(Vec<Parameter>, Vec<ValueType>),
    Memory(u32, Option<u32>),
    Global(ValueType),
}

#[derive(Clone)]
pub enum ExportKind {
    Function(String),
    Memory(String),
    Global(String),
}

#[derive(Clone)]
pub enum Instruction {
    GetLocal(String),
    SetLocal {
        name: String,
        value: Box<Instruction>,
    },
    GetGlobal(String),
    SetGlobal {
        name: String,
        value: Box<Instruction>,
    },
    Const(Const),
    Call(String),
    // BinOp(BinaryOp),
    BinOp {
        op: BinaryOp,
        ty: ValueType,
        lhs: Box<Instruction>,
        rhs: Box<Instruction>,
    },
    If {
        condition: Box<Vec<Instruction>>,
        then_branch: Box<Vec<Instruction>>,
        else_branch: Option<Box<Vec<Instruction>>>,
    },
    Loop {
        label: Option<String>,
        body: Box<Vec<Instruction>>,
    },
    BrIf {
        label: String,
        condition: Box<Instruction>,
    },
    Block {
        label: String,
        instructions: Vec<Instruction>,
    },
    Br(String),
    Eqz {
        ty: ValueType,
        value: Box<Instruction>,
    },
}

#[derive(Clone)]
pub enum Const {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

#[derive(Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Xor,
    Shl,
    ShrS,
    ShrU,
}

impl<T> Module<T> {
    pub fn new() -> Self {
        Module {
            name: None,
            functions: Vec::new(),
            imports: Vec::new(),
            exports: Vec::new(),
            memory: None,
            globals: Vec::new(),
        }
    }

    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }

    pub fn add_function(mut self, function: Function<T>) -> Self {
        self.functions.push(function);
        self
    }

    pub fn add_global(mut self, global: Global) -> Self {
        self.globals.push(global);
        self
    }
}

impl<T> Function<T> {
    pub fn new() -> Self {
        Function {
            name: None,
            params: Vec::new(),
            results: Vec::new(),
            locals: Vec::new(),
            instructions: Vec::new(),
            _phantom: PhantomData,
        }
    }

    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }

    pub fn add_param(mut self, name: impl Into<String>, typ: ValueType) -> Self {
        self.params.push(Parameter {
            name: name.into(),
            typ,
        });

        self
    }

    pub fn add_result(mut self, typ: ValueType) -> Self {
        self.results.push(typ);
        self
    }

    pub fn add_local(mut self, local: Local) -> Self {
        self.locals.push(local);
        self
    }

    pub fn add_instruction(mut self, instruction: Instruction) -> Self {
        self.instructions.push(instruction);
        self
    }

    pub fn add_instructions(mut self, instructions: Vec<Instruction>) -> Self {
        self.instructions.extend(instructions);
        self
    }
}

impl<T: WatEmitter> WatEmitter for Module<T> {
    fn emit_wat<W: Write>(&self, writer: &mut W) -> io::Result<()> {
        writeln!(writer, "(module")?;

        if let Some(name) = &self.name {
            writeln!(writer, "  ;; name: {}", name)?;
        }

        // emit imports
        for import in &self.imports {
            write!(
                writer,
                "  (import \"{}\" \"{}\" ",
                import.module, import.name
            )?;

            match &import.kind {
                ImportKind::Function(params, results) => {
                    write!(writer, "(func")?;
                    for param in params {
                        write!(writer, " (param ${} {})", param.name, param.typ.to_string())?;
                    }

                    for result in results {
                        write!(writer, " (result {})", result.to_string())?;
                    }

                    writeln!(writer, "))")?;
                }

                ImportKind::Memory(initial, maximum) => {
                    write!(writer, "(memory {} ", initial)?;

                    if let Some(max) = maximum {
                        write!(writer, "{}", max)?;
                    }

                    writeln!(writer, "))")?;
                }

                ImportKind::Global(typ) => {
                    writeln!(writer, "(global {}))", typ.to_string())?;
                }
            }
        }

        // emit memory
        if let Some(memory) = &self.memory {
            write!(writer, "  (memory")?;

            if let Some(name) = &memory.name {
                write!(writer, " ${}", name)?;
            }

            write!(writer, " {}", memory.initial)?;

            if let Some(max) = memory.maximum {
                write!(writer, " {}", max)?;
            }

            writeln!(writer, ")")?;
        }

        // emit globals
        for global in &self.globals {
            write!(writer, "  (global ${} ", global.name)?;

            if global.mutable {
                write!(writer, "(mut {})", global.typ.to_string())?;
            } else {
                write!(writer, "{}", global.typ.to_string())?;
            }

            write!(writer, " ")?;
            emit_instruction(writer, &global.init, 2)?;
            writeln!(writer, ")")?;
        }

        // emit functions
        for function in &self.functions {
            write!(writer, "  (func")?;
            if let Some(name) = &function.name {
                write!(writer, " ${}", name)?;
            }

            // emit named parameters
            for param in &function.params {
                write!(writer, " (param ${} {})", param.name, param.typ.to_string())?;
            }

            // emit results
            for result in &function.results {
                write!(writer, " (result {})", result.to_string())?;
            }

            writeln!(writer)?;

            // emit locals
            for local in &function.locals {
                writeln!(
                    writer,
                    "    (local ${} {})",
                    local.name,
                    local.typ.to_string()
                )?;
            }

            // function body with S-expression folding
            // emit_instructions(writer, &function.instructions, 4)?;

            // emit exports
            for export in &self.exports {
                write!(writer, "  (export \"{}\" ", export.name)?;
                match &export.kind {
                    ExportKind::Function(name) => writeln!(writer, "(func ${}))", name)?,
                    ExportKind::Memory(name) => writeln!(writer, "(memory ${}))", name)?,
                    ExportKind::Global(name) => writeln!(writer, "(global ${}))", name)?,
                }
            }

            // emit fn instructions
            emit_instructions(writer, &function.instructions, 4)?;

            writeln!(writer, "  )")?;
        }

        writeln!(writer, ")")
    }
}

fn emit_instructions<W: Write>(
    writer: &mut W,
    instructions: &[Instruction],
    indent: usize,
) -> io::Result<()> {
    for instruction in instructions {
        emit_instruction(writer, instruction, indent)?;
    }

    Ok(())
}

fn emit_instruction<W: Write>(
    writer: &mut W,
    instruction: &Instruction,
    indent: usize,
) -> io::Result<()> {
    let indent_str = " ".repeat(indent);
    match instruction {
        Instruction::GetLocal(name) => writeln!(writer, "{}(local.get ${})", indent_str, name)?,

        Instruction::SetLocal { name, value } => {
            writeln!(writer, "{}(local.set ${}", indent_str, name)?;
            emit_instructions(writer, &[*value.clone()], indent + 2)?;
            writeln!(writer, "{})", indent_str)?;
        }

        Instruction::GetGlobal(name) => writeln!(writer, "{}(global.get ${})", indent_str, name)?,
        Instruction::SetGlobal { name, value } => {
            writeln!(writer, "{}(global.set ${}", indent_str, name)?;
            emit_instructions(writer, &[*value.clone()], indent + 2)?;
            writeln!(writer, "{})", indent_str)?;
        }

        Instruction::Const(c) => match c {
            Const::I32(v) => writeln!(writer, "{}(i32.const {})", indent_str, v)?,
            Const::I64(v) => writeln!(writer, "{}(i64.const {})", indent_str, v)?,
            Const::F32(v) => writeln!(writer, "{}(f32.const {})", indent_str, v)?,
            Const::F64(v) => writeln!(writer, "{}(f64.const {})", indent_str, v)?,
        },

        Instruction::Call(name) => writeln!(writer, "{}(call ${})", indent_str, name)?,

        Instruction::BinOp { op, ty, lhs, rhs } => {
            let op_str = match op {
                BinaryOp::Add => "add",
                BinaryOp::Sub => "sub",
                BinaryOp::Mul => "mul",
                BinaryOp::Div => "div",
                BinaryOp::And => "and",
                BinaryOp::Or => "or",
                BinaryOp::Xor => "xor",
                BinaryOp::Shl => "shl",
                BinaryOp::ShrS => "shr_s",
                BinaryOp::ShrU => "shr_u",
            };

            writeln!(writer, "{}({}.{}", indent_str, ty.to_string(), op_str)?;
            emit_instructions(writer, &[*lhs.clone()], indent + 2)?;
            emit_instructions(writer, &[*rhs.clone()], indent + 2)?;
            writeln!(writer, "{})", indent_str)?;
        }

        Instruction::If {
            condition,
            then_branch,
            else_branch,
        } => {
            writeln!(writer, "{}(if", indent_str)?;
            emit_instructions(writer, &condition, indent + 2)?;
            writeln!(writer, "{}  (then", indent_str)?;
            emit_instructions(writer, &then_branch, indent + 4)?;
            writeln!(writer, "{}  )", indent_str)?;

            if let Some(else_branch) = else_branch {
                writeln!(writer, "{}  (else", indent_str)?;
                emit_instructions(writer, &else_branch, indent + 4)?;
                writeln!(writer, "{}  )", indent_str)?;
            }

            writeln!(writer, "{})", indent_str)?;
        }

        Instruction::Loop { label, body } => {
            write!(writer, "{}(loop", indent_str)?;

            if let Some(label) = label {
                write!(writer, " ${}", label)?;
            }

            writeln!(writer)?;
            emit_instructions(writer, &body, indent + 2)?;
            writeln!(writer, "{})", indent_str)?;
        }

        Instruction::BrIf { label, condition } => {
            writeln!(writer, "{}(br_if ${}", indent_str, label)?;
            emit_instructions(writer, &[*condition.clone()], indent + 2)?;
            writeln!(writer, "{})", indent_str)?;
        }

        Instruction::Block {
            label,
            instructions,
        } => {
            writeln!(writer, "{}(block ${}", indent_str, label)?;
            emit_instructions(writer, instructions, indent + 2)?;
            writeln!(writer, "{})", indent_str)?;
        }

        Instruction::Br(label) => {
            writeln!(writer, "{}(br ${})", indent_str, label)?;
        }

        Instruction::Eqz { ty, value } => {
            writeln!(writer, "{}({}.eqz", indent_str, ty.to_string())?;
            emit_instructions(writer, &[*value.clone()], indent + 2)?;
            writeln!(writer, "{})", indent_str)?;
        }
    }
    Ok(())
}

impl ValueType {
    fn to_string(&self) -> &'static str {
        match self {
            ValueType::I32 => "i32",
            ValueType::I64 => "i64",
            ValueType::F32 => "f32",
            ValueType::F64 => "f64",
        }
    }
}

pub fn write_wat_to_file<T: WatEmitter>(
    emitter: &T,
    path: impl AsRef<std::path::Path>,
) -> io::Result<()> {
    let mut file = std::fs::File::create(path)?;
    emitter.emit_wat(&mut file)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_factorial() -> io::Result<()> {
        struct ModuleContext;
        impl WatEmitter for ModuleContext {
            fn emit_wat<W: Write>(&self, _writer: &mut W) -> io::Result<()> {
                Ok(())
            }
        }

        let factorial: Function<_> = Function::<ModuleContext>::new()
            .with_name("factorial")
            .add_param("n", ValueType::I32)
            .add_result(ValueType::I32)
            .add_local(Local {
                name: "o".to_string(),
                typ: ValueType::I32,
            })
            .add_instructions(vec![
                // local.set $o (i32.const 1)
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
                                // local.set $o (i32.mul (local.get $o) (local.get $n))
                                Instruction::SetLocal {
                                    name: "o".to_string(),
                                    value: Box::new(Instruction::BinOp {
                                        op: BinaryOp::Mul,
                                        ty: ValueType::I32,
                                        lhs: Box::new(Instruction::GetLocal("o".to_string())),
                                        rhs: Box::new(Instruction::GetLocal("n".to_string())),
                                    }),
                                },
                                // local.set $n (i32.sub (local.get $n) (i32.const 1))
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
                // local.get $o
                Instruction::GetLocal("o".to_string()),
            ]);

        let module: Module<_> = Module::new().with_name("factorial").add_function(factorial);

        let mut output = Vec::new();
        WatEmitter::emit_wat(&module, &mut output)?;
        let wat_string = String::from_utf8(output).unwrap();

        // dbg!(wat_string);

        assert!(wat_string.contains("(func $factorial"));
        assert!(wat_string.contains("(param $n i32)"));
        assert!(wat_string.contains("(local $o i32)"));
        assert!(wat_string.contains("(block $exit"));
        assert!(wat_string.contains("(loop $factorial_loop"));

        write_wat_to_file(&module, "test/factorial.wat").unwrap();

        Ok(())
    }
}
