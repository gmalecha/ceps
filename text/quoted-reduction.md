- Title: Modular Reduction Specifications

- Drivers: Your name/contact

----

# Summary

This CEP proposes a mechanism for specification of reduction using markers that delimit where reduction should occur rather than a white- or black-list of symbols to reduce. This makes is much more natural to specify reductions in a modular way that is amenable to composition.

# Motivation

One pain point in Coq is specifying reduction white or black-lists that are safe to use in larger contexts. Specification of these lists especially occurs when writing reflective tactics and places that use large amounts of computation intermingled with user-written terms that should not be reduced at all. This commonly occurs inside of reflective or partially reflective automation.

## Example
Taking a simple example from reflective automation. It is common to write reflective lemmas of the following form:

```coq
Lemma auto_ok : forall (p q : expr),
  auto p = q ->
  [[ q ]] ->
  [[ p ]].
```

where `expr` is some representation of terms, e.g. using deBruijn indicies. The first equation premise is normally solved by something like `vm_compute; reflexivity` and then the denotation in the second line (i.e. `[[ q ]]`) is reduced via something akin to `cbv beta iota zeta delta [ denote ... ]` where the `...` represents all of the transitive dependenceis of `denote`, which can be significant (note also that constructing this list can be painful).

While this phase separation is common, it isn't always desireable. For example, you can not embed semantic values (i.e. from the semantic domain) within your term because `vm_compute` will reduce them completely, which is usually not intended. It also requires that users include the entirety of `q` within the proof term.

Under this proposal, the above lemma could be re-written (more naturally) as

```coq
Lemma auto_ok : forall p, [[ auto p ]] -> [[ p ]].
```

Which avoids the need to embed `q` inside the term.

**Other Examples**

This sort of pattern also shows up in custom proof modes such as IPM where computation is used to resolve hypothesis names but reduction within the type of the hypothesis must be avoided.

# High-level Design

The proposal is to incorporate ideas from multi-stage programming to make a multi-stage aware reduction function. Ideally, this functionality could be inserted into all reduction mechanisms but for reflective purposes `vm_compute` would be the most important.

Suppose that Coq terms were extended with a quote mechanism, for simplicity in the high-level presentation, we adapt the notation of MetaOcaml. `.< t >.` represents the value `t` that should not be reduced. In MetaOcaml, the type of this term is `t code` (rendered in Gallina as `code t`).

The rules for quote-aware reduction are that the body of a `.< _ >.` should not be reduced (only substituted in). To see how it works, consider the following example,

```coq
Fixpoint add_tree (n : nat) : nat :=
  match n with
  | 0 => .< 1 >.
  | S n =>
    let r := add_tree n in
    .< r + r >.
  end.

Eval vm_compute_ms in add_tree (1 + 1).
(* (1 + 1) + (1 + 1) *)
```

Note that the top-level `1 + 1` is reduced because it does not occur under a `quote` while the `quote (r + r)` inside the function is not reduced because it occurs syntactically under a `quote`.

Another example is a denotation function with terms.

```coq
Inductive monoid (T : Type) : Type :=
| Inj (_ : T)
| Unit
| Join (_ _ : monoid T).

Fixpoint denote {t} (m : monoid t) : t :=
  match m with
  | Inj t => t
  | Unit => .< 0 >.
  | Join a b =>
    let aa := denote a in
    let bb := denote b in
    .< aa + bb >.
  end.

Eval ms_compute in denote (Join (Inj (quote (denote (Inj (1 + 1))))) Emp).
(* denote (Inj (1 + 1)) + 0 *)
```

Note that in this example, quotations are embedded inside of the term itself to prevent reduction. In the more standard environment-based representation, values placed in the environment are quoted.


# Detailed design

There are several ways to incorporate this feature into Coq with different degrees of invasiveness.

## Full Generality

In full generality, Coq would implement full quoting and escaping in the same sense as MetaOcaml. This would involve the following additional constructs:

```coq
code t : Type (* the type of quoted values of type t *)
.< t >.       (* quote t *)
.~ t          (* splice t *)
.! t          (* run t *)
```

with the semantics described in the [MetaOcaml paper](). Unfortunately, but not surprsingly, this has deep semantics impact throughout the proof assistant because it requires type checking to track the quoting level of terms.

## Minimal Design

A less invasive design would be to introduce a special identity function marked as `quote` that blocks reduction in subterms.

```coq
(* Coq.Reduction.Quote *)
Set Universe Polymorphism.

Definition quote {T : Type} (v : T) : T := v.
```

When an evaluation mechanism, e.g. `cbv`, comes across a term `quote t`, it reduces the term to `subst σ t` where `σ` is the current term substition, i.e. it treats all symbols as opaque and does not perform `beta`, `iota`, or `zeta` reductions.

It is important to note that `quote t` is not the same as `(fun f => f t) quote`. That is, when `quote` is not fully applied, it should be treated as the identity function. This may seem counter-intuitive, but it is important for implementing `vm_compute` and `native_compute` because the compilation scheme for terms under `quote` is different than the compilation scheme for terms not under `quote`.

## Middle-ground Design



# Drawbacks

Is the proposed change affecting any other component of the system? How?

# Alternatives

Currently, the only viable alternative is to copy all "standard" functions that are used, e.g. `nth`, `+`, `++`, etc, and make private copies of them which are definitionally equivalent but have different names. The names of all of these functions are then used to create a whitelist of functions to reduce that is fed to `cbv` or `lazy`. Even this, however, doesn't completely address this problem (though it does ameliorate it to a large extext, because `beta`, `iota`, and *most painfully* sometimes `zeta` reductions are still performed.

With the exception of the `zeta` problem (for which there is no known solution except to avoid the use of `let` in your reductions), the above workaround could be automated using a library such as [MetaCoq](https://github.com/metacoq/metacoq).

# Unresolved questions

Questions about the design that could not find an answer.
