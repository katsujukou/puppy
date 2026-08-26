# Conflicts

A conflict is the parser reaching a point where the grammar allows two things
and does not say which. Puppy refuses to write a parser with one left
unsettled, and explains each in terms of the grammar rather than the automaton.

## Reading a report

```plain
shift/reduce conflict in state 4, on `PLUS`.

  after reading   INT PLUS INT
  and seeing      PLUS

  the parser can shift it, continuing
      expr -> expr . PLUS expr
  or reduce, having read the whole of
      expr -> expr PLUS expr

  Nothing in the grammar says which to prefer, so the parser will shift the token.
  `%shift` on the production says to prefer the shift, which is what will
  happen anyway -- writing it makes that the grammar's decision rather than a
  rule of thumb. Declaring `%left`, `%right` or `%nonassoc` for the token, or
  `%prec` on the production, would settle it by precedence instead.
```

The state number is there for reference; the two lines under it are the part to
read. `after reading` is a real input that gets the parser to this point — a
short one, built by taking a shortest path through the automaton and spelling
each nonterminal on it out as the shortest input that matches. `and seeing` is
the token that then arrives.

The dot in `expr -> expr . PLUS expr` is where the parser is. Shifting means
carrying on through that alternative; reducing means the other alternative is
finished and its value is about to be built.

## Shift/reduce

Almost always precedence, and almost always meant. `1 + 2 + 3` reaches the
conflict above: having read `1 + 2`, and seeing another `+`, the parser can
either finish the addition or carry on. Both readings are valid arithmetic;
which one you want is a fact about `+` and not about the grammar's shape.

Say it with a precedence declaration:

```plain
%left PLUS
```

Now the conflict is settled deliberately and Puppy stops mentioning it. See
[precedence](grammar.md#left-right-nonassoc) for how ranking and
associativity work.

Where a conflict is not about an operator, `%prec` moves one alternative to a
different rank, and [`%inline`](grammar.md#inline)
folds away a rule whose reduction has to happen too early.

And sometimes shifting is simply right, with no ranking behind it. The dangling
`else` is the standard case: having read `if c then s` and seeing an `else`,
the parser can finish the shorter alternative or carry on into the longer one,
and attaching the `else` to the nearer `if` is what was meant every time.
[`%shift`](grammar.md#shift) on the shorter alternative says so:

```plain
stmt:
  | IF c = expr THEN a = stmt %shift        { If c a }
  | IF c = expr THEN a = stmt ELSE b = stmt { IfElse c a b }
```

The parser does the same thing it was going to do. What changes is that the
conflict is now one the grammar decided, so it stops standing between you and a
build -- and a conflict that turns up later, in a part of the grammar nobody
meant to make ambiguous, is not buried among a hundred deliberate ones.

## Reduce/reduce

```plain
reduce/reduce conflict in state 1, on `end of input`.

  after reading   E
  and seeing      end of input

  the parser could reduce either
      a -> E
  or
      b -> E

  Nothing in the grammar says which to prefer, so the parser will reduce by `a -> E`.
  Precedence cannot settle a reduce/reduce conflict. Two rules matching the
  same input usually means the grammar says something other than what was
  meant.
```

Precedence has nothing to say here: both alternatives are complete, and neither
is waiting for a token whose rank could decide anything. `%shift` has nothing
to say either — there is no shift in this cell to prefer, so marking one of
them changes nothing and hides nothing. Two rules that match the same input are
usually two names for one idea, and the fix is in the grammar — merge them, or
give one of them something the other has not got.

## What Puppy does not report

Puppy builds its automaton with Pager's algorithm, which merges two states only
when merging cannot invent a conflict. That matters because the obvious
cheap construction, LALR, merges every pair with the same shape and reports
conflicts the grammar does not have.

This grammar is the standard example:

```plain
s:
  | A x = e C { x }
  | A x = f D { x }
  | B x = f C { x }
  | B x = e D { x }

e: | v = E { v }
f: | v = E { v }
```

After `A E`, seeing `C` means `e`, and seeing `D` means `f`. After `B E` it is
the other way round. An LALR generator merges those two states and then cannot
tell `e` from `f` on either token — a reduce/reduce conflict, in a grammar that
has none. Puppy generates it without complaint.

## Getting on with it

`--allow-conflicts` writes the parser anyway. The conflicts are still reported,
and the resolutions are the usual ones: a shift beats a reduce, and among
reduces the alternative written first wins.

This is worth doing while you are still shaping a grammar. It is worth
examining before it becomes permanent, because an unsettled conflict is the
parser choosing on your behalf, and it will choose the same way in the cases
you thought about and the ones you did not.

`--emit-explain` puts the reports in a `.puppy-explain` file beside the grammar
instead of on standard error. Settling the conflicts removes the file again, so
what is there always describes the grammar as it stands.
