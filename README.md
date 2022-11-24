`ary` - The array literal of the gods.
=====

Rust provides two syntaxes for array literals: `[a, b, c]`, and `[x; count]`.
When building complex `const` arrays, having only these two forms can be
quite limiting.

This library provides the `ary![]` macro to improve this situation.

# Array Splatting

First, we allow an element expression to begin with `in`, which indicates
it should be flattened into the overall array. The expression must be a
`const` array or slice. For example:

```rs
const MACRO: &[u8] = b"ary![]";

assert_eq!(&ary![0x3a, 0x3a, in MACRO, 0x3b], b"::ary![];");
assert_eq!(&ary![in b"ary![]"; 3], b"ary![]ary![]ary![]");
```

Note that because this may be *any* `const` expression, we can easily build
varying-size arrays.

```rs
const fn make_slice() -> &'static [u8] {
  // ...
}

const WORDS: &[u8] = &ary![in make_slice(); 3];
assert_eq!(WORDS.len() % 3, 0);
```

# Range Modifiers

After all of the elements, *modifiers* for ranges of the constructed array
can be specified by placing range-value pairs after a `=>` token.

```rs
const ARRAY: &[i32] = &ary![0; 32 =>
  5..10: |i| !(i as i32),  // Closure is called once for each index.
  10.._: [1, 2, 3],        // Upper bound of range can be inferred.
  14..=17: [-1; _],        // Length of a Rust array literal can be inferred.
  29..: [-2, -2, -2],      // Unbounded ranges work too.
  
  31: 42,       // Single elements can be set directly.
  (1 + 1): 2,   // Complex index expressions must be wrapped in parens to
                // avoid a syntax ambiguity.                  
];

assert_eq!(ARRAY, &[
  0, 0, 2, 0, 0, -6, -7, -8, -9, -10, 1, 2, 3, 0, -1, -1,
  -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -2, -2, 42,
]);

// You can use .. to replace the contents of the whole array.
const RANGE: &[i32] = &ary![0; 9 => ..: |i| i as i32 + 1];
assert_eq!(RANGE, &[1, 2, 3, 4, 5, 6, 7, 8, 9]);
```

## Inferring Array Length from Ranges

It is possible to skip the element expressions and proceed to using range
modifiers; in this case, the size of the array will be inferred from the
upper bounds of the given modifiers.

```rs
const EVENS: [usize; 8] = ary![=> ..8: |i| i * 2];
assert_eq!(EVENS, [0, 2, 4, 6, 8, 10, 12, 14]);

// Unbounded ranges do not contribute to the inferred size.
const EMPTY: [i32; 0] = ary![=> ..: |_| unreachable!()];

// Lower bounds of unbounded-above ranges *do* contribute, though.
const NONEMPTY: [i32; 10] = ary![=>
  ..: [42; _],
  10..: [-1; _],
];
```