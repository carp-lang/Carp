# Dynamic Evaluator VM

Carp's dynamic evaluator is VM-backed.

This document is both a high-level architecture overview and a maintainer guide for future VM work.

## Scope

The dynamic evaluator is the compile-time execution engine used for:

1. dynamic code (`defndynamic`, commands, primitives),
2. macro expansion and macro execution,
3. evaluator-time forms such as `let`, `if`, `while`, `set!`, and function calls.

`src/Eval.hs` is the public API surface (`evalDynamic`, `evalStatic`, `eval`) and delegates execution to the VM path.

## Pipeline

The evaluator pipeline is:

1. `XObj` -> `EvalIR` lowering (`lowerExpr` in `src/EvalIR.hs`).
2. `EvalIR` -> `EvalCode` bytecode compilation (`compileEvalIR` in `src/EvalVM.hs`).
3. Bytecode execution (`runEvalCode` in `src/EvalVM.hs`).

Callables may be:

1. eagerly compiled (`VMPrecompiled`),
2. compile-on-first-call (`VMCompileOnCall`) with cached compiled code.

## Module map

- `src/Eval.hs`: public evaluator entry points and integration with expansion.
- `src/EvalIR.hs`: evaluator IR, lowering, and raising (`raiseExpr`).
- `src/EvalCode.hs`: bytecode instruction and resolver-handle definitions.
- `src/EvalVM.hs`: compile pipeline, lookup logic, dispatch, opcode loop, caches.
- `src/EvalVMCore.hs`: low-level frame/code-store execution for registered callable code.
- `src/EvalSlotLowering.hs`: function-local slot lowering for fast local variable access.
- `src/EvalBound.hs`, `src/EvalBind.hs`: bound reference representation/helpers.
- `src/EvalTypes.hs`: evaluator lookup and execution-mode types.

## Execution modes and lookup preference

Evaluator behavior is mode-driven via `LookupPreference`:

1. `PreferDynamic`
2. `PreferGlobal`
3. `PreferLocal ... ExecFunction|ExecDynamic|ExecMacro`

Mode controls lookup and callable compilation policy:

1. Function mode expects local slots to exist and fails fast when missing.
2. Dynamic and macro modes keep dynamic-friendly lookup behavior.

## Bytecode model

`EvalCode` is a list plus array form of instructions. Important instruction classes:

1. stack/value ops: `IPushConst`, `IMakeArray`, `IMakeStaticArray`, `IDrop`,
2. control flow: `IJumpIfFalseRel`, `IJumpRel`, `IHalt`, `ITrap`,
3. symbol/call dispatch: `IResolveSymbol`, `IExecCallSymbol`, `IExecCall`,
4. special-form execution ops: `IExecLet`, `IExecFn`, `IExecWhile`, `IExecWith`, `IExecSet`.

`IExecCallSymbol` and `IExecCall` carry both raw argument IR and precompiled argument code, so non-macro argument evaluation avoids recursive IR re-entry in hot paths.

## Callable representation and transparency

The VM introduces `VMClosure` for executable callable payloads, but language-level semantics should remain transparent.

Current contract:

1. dynamic/macro definitions keep legacy outer shape `(dynamic|macro name params body)`,
2. executable body may be a VM-backed closure internally,
3. user-visible macro and dynamic behavior should match pre-VM semantics unless explicitly changed.

## Caching

There are three relevant caches:

1. IR-to-bytecode cache keyed by positive `Info.infoIdentifier` (`evalIRCacheKey`),
2. symbol resolution cache in opcode loop keyed by `(contextBindingEpoch, symbolId)`,
3. callable resolution cache (same keying strategy) for call dispatch.

`contextBindingEpoch` is used to invalidate cached bindings when environment state changes.

## Symbol resolution model

Compilation assigns each symbol a `ResolverHandle`:

1. `RHLocalSlot`
2. `RHGlobal`
3. `RHDynamic`
4. `RHQualified`
5. `RHUnqualified`

The opcode loop resolves by handle shape first and only falls back to broader lookup where required by semantics.

## Slot lowering invariants

Function mode relies on slot lowering:

1. function parameters and local references are lowered to slot refs,
2. unresolved local refs after slot lowering are treated as an error,
3. `set!` updates sync back into local slot state to keep reads coherent after mutation.

If you change binding/lowering behavior, keep these invariants intact or update tests and docs together.

## How to make VM changes safely

When changing evaluator behavior:

1. decide if this is implementation-only or intended language semantics,
2. if implementation-only, preserve outer forms and error surface,
3. update/extend evaluator tests (`test/TestEvalIR.hs`, `test/TestEvalSlotLowering.hs`, `test/TestEvalVM.hs`, `test/TestEvalVMCore.hs`),
4. run macro-heavy and dynamic-closure tests (`test/macros.carp`, `test/dynamic-closures.carp`),
5. benchmark (`./bench/run-evaluator-bench.sh`) and compare medians, not single runs.

## Performance workflow

Primary benchmark script:

1. `./bench/run-evaluator-bench.sh`

Use medians from repeated runs and compare at least:

1. baseline startup,
2. evaluator benchmark (`bench/evaluator.carp`),
3. real-world macro workload (`test/macros.carp`).

## Current non-goal

The evaluator VM is internal compiler infrastructure. It is not a user runtime VM and does not change Carp's compiled-code execution model.
