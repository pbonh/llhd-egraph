# LLHD Egglog CFG Skeleton

This document describes a two-layer egglog representation of LLHD that keeps pure
dataflow in the existing DFG sorts while modeling control flow and side effects
with a CFG skeleton. The skeleton mirrors LLHD semantics for functions, processes,
and entities while supporting non-entity units.

## Two-layer model

- DFG layer: pure expressions and value construction. This uses the existing
  `LLHDDFG` datatype and `LLHDValue` ids as e-class references.
- Skeleton layer: ordered statements and block terminators. These operations are
  side-effecting or control-flow sensitive and therefore anchored in a CFG.

The skeleton references DFG values by `LLHDValue` ids. Control-flow joins are
represented with block arguments rather than phi nodes in the DFG.

## Skeleton sorts and bindings

The skeleton layer is defined in `resources/egglog/llhd_cfg_skeleton.egg`. It
adds ordered statement lists and block terminators while reusing existing DFG
sorts from `resources/egglog/llhd_dfg_sort.egg`.

```egglog
; Unit identity and kind.
(datatype LLHDUnit (Unit i64))
(function unit_kind (LLHDUnit) LLHDUnitKind)

; CFG structure.
(function unit_blocks (LLHDUnit) LLHDVecBlock)
(function block_params (LLHDBlock) LLHDVecValue)
(function block_stmts (LLHDBlock) LLHDVecSkelStmt)
(function block_term (LLHDBlock) LLHDTerminator)
```

### Ordered statements

Statements are ordered within each block to preserve LLHD's sequential semantics
inside a block. The skeleton anchors operations that are stateful or have side
effects. Each constructor references DFG values by `LLHDValue` id.

```egglog
(datatype LLHDSkelStmt
    ; Signal and timing interactions.
    (SkSig LLHDValue LLHDValue)
    (SkPrb LLHDValue LLHDValue)
    (SkDrv LLHDValue LLHDValue LLHDValue)
    (SkDrvCond LLHDValue LLHDValue LLHDValue LLHDValue)
    (SkCon LLHDValue LLHDValue)
    (SkDel LLHDValue LLHDValue LLHDValue)

    ; Stateful operations.
    (SkReg LLHDValue LLHDVecValue LLHDVecRegMode)
    (SkVar LLHDValue LLHDValue)
    (SkLd LLHDValue LLHDValue)
    (SkSt LLHDValue LLHDValue)

    ; Calls and instantiations.
    (SkCall LLHDVecValue LLHDExtUnit i64 LLHDVecValue)
    (SkInst LLHDVecValue LLHDExtUnit i64 LLHDVecValue))
```

The first `LLHDValue` argument in statements is the statement result value id
when the LLHD operation produces a value. For statements that are purely
side-effecting, the value id can be a conventional placeholder (e.g. a void or
ignored value id chosen by the importer).

### Terminators

Terminators express control flow. They also carry the edge arguments for block
parameters, encoding phi semantics via block arguments.

```egglog
(datatype LLHDTerminator
    (SkBr LLHDBlock LLHDVecValue)
    (SkBrCond LLHDValue LLHDBlock LLHDVecValue LLHDBlock LLHDVecValue)
    (SkWait LLHDBlock LLHDVecValue)
    (SkWaitTime LLHDBlock LLHDValue LLHDVecValue)
    (SkRet)
    (SkRetValue LLHDValue)
    (SkHalt))
```

## Block arguments as phi

Block arguments encode control-dependent merges. Instead of a phi node, each
branch passes its chosen value as an edge argument.

```egglog
; Block parameters for a join block.
(set (block_params (Block 3)) (vec-of 1))

; Conditional branch passes a value to the join block on each edge.
(set (block_term (Block 1))
  (SkBrCond (Value 10)
    (Block 2) (vec-of 11)
    (Block 3) (vec-of 12)))
```

## Example: process with wait

This models a simple toggling process that probes a signal, drives it with a
delay, then waits for a change.

```egglog
; Unit and block setup.
(set (unit_kind (Unit 0)) Process)
(set (unit_blocks (Unit 0)) (vec-of (Block 0)))
(set (block_params (Block 0)) (vec-of))

; Ordered statements: prb, drv.
(set (block_stmts (Block 0)) (vec-of
  (SkPrb (Value 1) (Value 2))
  (SkDrv (Value 3) (Value 1) (Value 4))))

; Terminator: wait on a sensitivity list.
(set (block_term (Block 0)) (SkWait (Block 0) (vec-of 2)))
```

Here `(Value 1)` is the `prb` result, `(Value 4)` is a DFG time value, and the
`wait` sensitivity list references the probed signal.

## Example: conditional branch with block args

```egglog
(set (unit_kind (Unit 1)) Function)
(set (unit_blocks (Unit 1)) (vec-of (Block 1) (Block 2) (Block 3)))
(set (block_params (Block 3)) (vec-of 20))

; Block 1: evaluate condition, branch.
(set (block_stmts (Block 1)) (vec-of))
(set (block_term (Block 1))
  (SkBrCond (Value 10)
    (Block 2) (vec-of 11)
    (Block 3) (vec-of 12)))

; Block 2: fallthrough to join with an argument.
(set (block_stmts (Block 2)) (vec-of))
(set (block_term (Block 2)) (SkBr (Block 3) (vec-of 13)))

; Block 3: return the incoming argument.
(set (block_stmts (Block 3)) (vec-of))
(set (block_term (Block 3)) (SkRetValue (Value 20)))
```

Block 3 has one parameter `(Value 20)`. The two incoming edges pass different
arguments, which correspond to the values selected by a phi in SSA form.

## Example: entity with side effects anchored

Even entities can be represented as a single-block skeleton to anchor structural
operations like `sig`, `inst`, `con`, and `drv` in a deterministic order.

```egglog
(set (unit_kind (Unit 2)) Entity)
(set (unit_blocks (Unit 2)) (vec-of (Block 0)))
(set (block_params (Block 0)) (vec-of))

(set (block_stmts (Block 0)) (vec-of
  (SkSig (Value 1) (Value 2))
  (SkInst (vec-of 3) (ExtUnit 0) 0 (vec-of 1 2))
  (SkCon (Value 3) (Value 4))
  (SkDrv (Value 5) (Value 3) (Value 6))))

(set (block_term (Block 0)) (SkRet))
```

## Extraction intuition

Extraction starts from skeleton statements and terminators. For each referenced
`LLHDValue`, the extractor picks a representative DFG expression and materializes
its dependencies. Dominating statements can be reused for subsequent references.
Control flow is preserved by the skeleton, while the DFG provides optimization
opportunities for pure computation.
