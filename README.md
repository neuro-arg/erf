# Erf

Erf is a simple ML-like language designed for for running on GPUs.

## Implementation details

(You don't have to understand any of this for learning and using the
language, but the language isn't done so there's no tutorial)

Erf's goal isn't being a shading language in itself, but working in
combination with a cooperating host that can pass it the data it asks
for. This takes the burden of choosing struct offsets off the programmer
and onto the host application.

Some of the design goals:

- Be easy to learn
- Run on GPUs
- Have decent polymorphism (at least good enough for multiplying a
  vector by a scalar)
- Be well suited for image processing, but audio processing could be
  interesting as well, or just raw data processing.

With that in mind:

- No GC on the GPUs obviously, but we can be clever with closures! If we
  pass closures by value and forbid references, we get a lot of
  expressive power.
- At first glance, no GC means we can't have lists. But they can
  actually be implemented as generators (i.e. opaque iterators with
  `head`, `tail`, `empty?` operators), since these functions are trivial
  to implement in terms of closures.
- This poses some problems, however. For example, listing the digits of
  a number is trivial if you have `cons`, but it's not obvious how
  `cons` can be implemented. Instead, we'd have to "flip" the number
  (additionally counting trailing zeroes) while dealing with it as a
  number, and only then we can trivially iterate over its digits.
- Similarly, arbitrary precision integers may be useful, and at least
  some operations can be implemented on them using generators. This
  needs further exploration.
- To some extent, we can support `cons` by completely detaching control
  flow from its original structure, employing continuation-passing style
  in the HIR. However, this is hard to build and hard to translate to
  SPIR-V. I'd love to do it if it's at all possible!
- Strong typing is 100% necessary for running on the GPU. We also have
  to monomorphize as much as possible, because GPUs don't have unions,
  so they don't have tagged unions either.
- Each function is a separate type (like in Rust), because there are no
  function pointers. It doesn't *always* has to be monomorphized (we can
  simulate function pointers with dispatch by tag), but it's obviously
  preferred, especially on GPUs where branching is expensive.
- If a function ends with a call to another function, that function is
  to be inlined (tail call), if this results in recursion it should be
  converted into a loop
- Aggressive monomorphization and inlining *will* blow up the resulting
  program size, but the idea is to have the programs be small enough for
  that to not matter. If it will matter, that's a problem for later (and
  if it works for Rust surely it will work here, right?...)
- Note that infinite closures are impossible (like in Rust), this should
  be a proper error message. This means recursive generators (generators
  that capture previous state while also adding new state) are also
  impossible. This shouldn't cause major problems in practice. Also, if
  non-GPU targets are added, it's possible to allow that with
  allocation.
- Subtyping with full type inference is used. This is both easy to use
  and easier to learn compared to HM-like systems. However, this poses
  some performance problems and makes ad-hoc polymorphism much harder. I
  hope to solve this, and I think I know how to.
- I thought of using significant whitespace, but frankly I don't think
  it's that much easier, in fact it can be a source of problems,
  semicolons may be easy to forget for new programmers but at least the
  compiler can point it out.
- One may argue currying is somewhat confusing and results in weird
  error messages. I agree, really, and I'll switch up the syntax later
  (currying is better for testing the language's limits and it's an
  easier way of expressing certain concepts).

## Phases

- AST
- HIR (basically typed AST, but with resolved patterns and ad-hoc
  polymorphism)
- MIR (basic blocks, suitable for translation into lower-level IR)
- SPIR-V
