//! The array literal of the gods.
//!
//! Rust provides two syntaxes for array literals: `[a, b, c]`, and `[x; count]`.
//! When building complex `const` arrays, having only these two forms can be
//! quite limiting.
//! 
//! This library provides the [`ary![]`][ary] macro to improve this situation.
//! 
//! # Array Splatting
//! 
//! First, we allow an element expression to begin with `in`, which indicates
//! it should be flattened into the overall array. The expression must be a
//! `const` array or slice. For example:
//! 
//! ```
//! # use ary::ary;
//! const MACRO: &[u8] = b"ary![]";
//! 
//! assert_eq!(&ary![0x3a, 0x3a, in MACRO, 0x3b], b"::ary![];");
//! assert_eq!(&ary![in b"ary![]"; 3], b"ary![]ary![]ary![]");
//! ```
//! 
//! Note that because this may be *any* `const` expression, we can easily build
//! varying-size arrays.
//! 
//! ```
//! # use ary::ary;
//! const fn make_slice() -> &'static [u8] {
//!   // ...
//! # b"ary![]"
//! }
//! 
//! const WORDS: &[u8] = &ary![in make_slice(); 3];
//! assert_eq!(WORDS.len() % 3, 0);
//! ```
//! 
//! # Range Modifiers
//! 
//! After all of the elements, *modifiers* for ranges of the constructed array
//! can be specified by placing range-value pairs after a `=>` token.
//! 
//! ```
//! # use ary::ary;
//! const ARRAY: &[i32] = &ary![0; 32 =>
//!   5..10: |i| !(i as i32),  // Closure is called once for each index.
//!   10.._: [1, 2, 3],        // Upper bound of range can be inferred.
//!   14..=17: [-1; _],        // Length of a Rust array literal can be inferred.
//!   29..: [-2, -2, -2],      // Unbounded ranges work too.
//!   
//!   31: 42,       // Single elements can be set directly.
//!   (1 + 1): 2,   // Complex index expressions must be wrapped in parens to
//!                 // avoid a syntax ambiguity.                  
//! ];
//! 
//! assert_eq!(ARRAY, &[
//!   0, 0, 2, 0, 0, -6, -7, -8, -9, -10, 1, 2, 3, 0, -1, -1,
//!   -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -2, -2, 42,
//! ]);
//! 
//! // You can use .. to replace the contents of the whole array.
//! const RANGE: &[i32] = &ary![0; 9 => ..: |i| i as i32 + 1];
//! assert_eq!(RANGE, &[1, 2, 3, 4, 5, 6, 7, 8, 9]);
//! ```
//! 
//! It is possible to skip the element expressions and proceed to using range
//! modifiers; in this case, the size of the array will be inferred from the
//! upper bounds of the given modifiers.
//! 
//! ```
//! # use ary::ary;
//! const EVENS: [usize; 8] = ary![=> ..8: |i| i * 2];
//! assert_eq!(EVENS, [0, 2, 4, 6, 8, 10, 12, 14]);
//! 
//! // Unbounded ranges do not contribute to the inferred size.
//! const EMPTY: [i32; 0] = ary![=> ..: |_| unreachable!()];
//! 
//! // Lower bounds of unbounded-above ranges *do* contribute, though. 
//! const NONEMPTY: [i32; 10] = ary![=>
//!   ..: [42; _],
//!   10..: [-1; _],
//! ];
//! ```
//! 
//! In this case, the ranges must completely cover all indices of the array.
//! 
//! ```compile_fail
//! # use ary::ary;
//! const GAP: &[i32; 10] = &ary![=>
//!   0..5: [1; 5],
//!   // Element 5 is missing.
//!   6..10: [2; 4],
//! ];
//! ```
//! 
//! # Caveats
//! 
//! These macros are intended for creating complex constants baked into a
//! binary, so only [`Copy`] arguments are supported. You can use [`ary![]`][ary]
//! in a `let` binding in non-`const` code, but until `const {}` blocks are
//! stabilized, performance can't be guaranteed, especially in unoptimized
//! builds.

#![no_std]

/// Constructs a super-duper `const` array.
///
/// See the [crate docs](self) for complete syntax documentation.
#[macro_export]
macro_rules! ary {
  ($($tt:tt)*) => {{
    const LEN: (usize, bool) = {
      let mut total: usize = 0;
      let mut has_preinit = false;
      total = total;  // Silence "unnecessary mut" warning without attributes.
      has_preinit = has_preinit;
      let _ = (total, has_preinit);
      $crate::__compute_len!(@total, has_preinit => $($tt)*);
      (total, has_preinit)
    };

    extern crate core;
    const BITS: usize = usize::BITS as usize;
    let mut uninit_arr = [core::mem::MaybeUninit::uninit(); LEN.0];
    let mut is_init = [0usize; LEN.0 / BITS + 1];
    uninit_arr = uninit_arr;
    is_init = is_init;
    let mut idx: usize = 0;
    idx = idx;
    $crate::__init!(uninit_arr, is_init, idx => $($tt)*);
    let _ = idx;

    if LEN.1 {
      let mut i = 0;
      while i < uninit_arr.len() {
        assert!(
          is_init[i / BITS] >> (i % BITS) & 1 == 1,
          "failed to initialize at least one array element",
        );
        i += 1;
      }
    }

    let arr = unsafe { core::mem::transmute(uninit_arr) };
    $crate::__type_check(&uninit_arr, &arr);
    arr
  }};
}

#[doc(hidden)]
pub const fn __type_check<T: Copy + 'static, const N: usize>(
  _: &[core::mem::MaybeUninit<T>; N],
  _: &[T; N],
) {
}

#[doc(hidden)]
pub const fn __max(a: usize, b: usize) -> usize {
  if a > b { a } else { b }
}

#[doc(hidden)]
#[macro_export]
macro_rules! __compute_len {
  (@ $total:ident, $has_preinit:ident => => $($tt:tt)*) => {
    $has_preinit = true;
    $crate::__compute_dynamic_len!($total => $($tt)*);
  };
  (@ $total:ident, $has_preinit:ident => $e:expr; $n:expr $(=> $($tt:tt)*)?) => {
    $total += $n;
  };
  (@ $total:ident, $has_preinit:ident => in $e:expr; $n:expr $(=> $($tt:tt)*)?) => {
    $total += $n * $e.len();
  };
  ($(@)? $total:ident, $has_preinit:ident => $(,)? $(=> $($tt:tt)*)?) => {};
  ($(@)? $total:ident, $has_preinit:ident => in $e:expr $(, $($tt:tt)*)?) => {
    $total += $e.len();
    $crate::__compute_len!($total, $has_preinit => $($($tt)*)?);
  };
  ($(@)? $total:ident, $has_preinit:ident => in $e:expr $(=> $($tt:tt)*)?) => {
    $total += $e.len();
  };
  ($(@)? $total:ident, $has_preinit:ident => $e:expr $(, $($tt:tt)*)?) => {
    $total += 1;
    $crate::__compute_len!($total, $has_preinit => $($($tt)*)?);
  };
  ($(@)? $total:ident, $has_preinit:ident => $e:expr $(=> $($tt:tt)*)?) => {
    $total += 1;
  };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __init {
  ($arr:ident, $is_init:ident, $idx:ident => $e:expr; $n:expr $(=> $($tt:tt)*)?) => {{
    let e = $e;
    while $idx < $arr.len() {
      extern crate core;
      $arr[$idx] = core::mem::MaybeUninit::new(e);
      $idx += 1;
    }
    $crate::__parse_key!($arr, $is_init => $($($tt)*)?);
  }};
  ($arr:ident, $is_init:ident, $idx:ident => in $e:expr; $n:expr $(=> $($tt:tt)*)?) => {{
    let e = $e;
    let n: usize = $n;
    let mut i = 0;
    while i < n {
      let mut j = 0;
      while j < e.len() {
        extern crate core;
        $arr[$idx] = core::mem::MaybeUninit::new(e[j]);
        $idx += 1;
        j += 1;
      }
      i += 1;
    }
    $crate::__parse_key!($arr, $is_init => $($($tt)*)?);
  }};
  ($arr:ident, $is_init:ident, $idx:ident => $(,)? $(=> $($tt:tt)*)?) => {{
    $crate::__parse_key!($arr, $is_init => $($($tt)*)?);
  }};
  ($arr:ident, $is_init:ident, $idx:ident => in $e:expr $(, $($tt:tt)*)?) => {{
    let e = $e;
    let mut i = 0;
    while i < $e.len() {
      extern crate core;
      $arr[$idx + i] = core::mem::MaybeUninit::new(e[i]);
      i += 1;
    }
    $idx += i;
    $crate::__init!($arr, $is_init, $idx => $($($tt)*)?);
  }};
  ($arr:ident, $is_init:ident, $idx:ident => in $e:expr $(=> $($tt:tt)*)?) => {{
    $crate::__init!($arr, $is_init, $idx => in $e);
    $crate::__parse_key!($arr, $is_init => $($($tt)*)?);
  }};
  ($arr:ident, $is_init:ident, $idx:ident => $e:expr $(, $($tt:tt)*)?) => {{
    extern crate core;
    $arr[$idx] = core::mem::MaybeUninit::new($e);
    $idx += 1;
    $crate::__init!($arr, $is_init, $idx => $($($tt)*)?);
  }};
  ($arr:ident, $is_init:ident, $idx:ident => $e:expr $(=> $($tt:tt)*)?) => {{
    $crate::__init!($arr, $is_init, $idx => $e);
    $crate::__parse_key!($arr, $is_init => $($($tt)*)?);
  }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! __insert {
  ($arr:ident, $is_init:ident, $start:expr, $end:expr, ($cmp:tt), |$i:tt| $v:expr) => {{
    let start: Option<usize> = $start;
    let start = match start {
        Some(x) => x,
        None => 0,
    };

    let end: Option<usize> = $end;
    let end = match end {
        Some(x) => x,
        None => $arr.len(),
    };

    let mut i = start;
    while i $cmp end {
      extern crate core;
      // Closure execution is not allowed in const, so we make do.
      $arr[i] = core::mem::MaybeUninit::new({ let $i = i; $v });
      $is_init[i / (usize::BITS as usize)] |= (1 << (i % usize::BITS as usize));
      i += 1;
    }
  }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! __compute_dynamic_len {
  ($total:ident =>) => {};

  ($total:ident => $a:tt.._: $v:expr $(, $($tt:tt)*)?) => {{
    $total = $crate::__max($total, $v.len() + $a);
    $crate::__compute_dynamic_len!($total => $($($tt)*)?);
  }};

  ($total:ident => .._: $v:expr $(, $($tt:tt)*)?) => {{
    $total = $crate::__max($total, $v.len());
    $crate::__compute_dynamic_len!($total => $($($tt)*)?);
  }};

  ($total:ident => $a:tt..$b:tt: $v:expr $(, $($tt:tt)*)?) => {{
    $total = $crate::__max($total, $b);
    $crate::__compute_dynamic_len!($total => $($($tt)*)?);
  }};

  ($total:ident => $a:tt..=$b:tt: $v:expr $(, $($tt:tt)*)?) => {{
    $total = $crate::__max($total, $b+1);
    $crate::__compute_dynamic_len!($total => $($($tt)*)?);
  }};

  ($total:ident => $a:tt..: $v:expr $(, $($tt:tt)*)?) => {{
    $total = $crate::__max($total, $a);
    $crate::__compute_dynamic_len!($total => $($($tt)*)?);
  }};
  ($total:ident => ..$b:tt: $v:expr $(, $($tt:tt)*)?) => {{
    $total = $crate::__max($total, $b);
    $crate::__compute_dynamic_len!($total => $($($tt)*)?);
  }};

  ($total:ident => ..=$b:tt: $v:expr $(, $($tt:tt)*)?) => {{
    $total = $crate::__max($total, $b);
    $crate::__compute_dynamic_len!($total => $($($tt)*)?);
  }};

  ($total:ident => ..: $v:expr $(, $($tt:tt)*)?) => {{
    $crate::__compute_dynamic_len!($total => $($($tt)*)?);
  }};

  ($total:ident => $a:tt: $v:expr $(, $($tt:tt)*)?) => {{
    $total = $crate::__max($total, $a + 1);
    $crate::__compute_dynamic_len!($total => $($($tt)*)?);
  }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! __parse_key {
  ($arr:ident, $is_init:ident =>) => {};

  ($arr:ident, $is_init:ident => $a:tt.._: $v:expr $(, $($tt:tt)*)?) => {{
    let v = $v;
    let start: usize = $a;
    let len: usize = v.len();
    let end = start + len;
    $crate::__insert!($arr, $is_init, Some(start), Some(end), (<), |i| $v[i - start]);
    $crate::__parse_key!($arr, $is_init => $($($tt)*)?);
  }};

  ($arr:ident, $is_init:ident => .._: $v:expr $(, $($tt:tt)*)?) => {{
    let v = $v;
    let start: usize = 0;
    let len: usize = v.len();
    let end = start + len;
    $crate::__insert!($arr, $is_init, Some(start), Some(end), (<), |i| $v[i - start]);
    $crate::__parse_key!($arr, $is_init => $($($tt)*)?);
  }};

  ($arr:ident, $is_init:ident => $a:tt..$b:tt: $($tt:tt)*) => {{
    $crate::__parse_value!($arr, $is_init, Some($a), Some($b), (<) => $($tt)*);
  }};

  ($arr:ident, $is_init:ident => $a:tt..=$b:tt: $($tt:tt)*) => {{
    $crate::__parse_value!($arr, $is_init, Some($a), Some($b), (<=) => $($tt)*);
  }};

  ($arr:ident, $is_init:ident => ..$b:tt: $($tt:tt)*) => {{
    $crate::__parse_value!($arr, $is_init, None, Some($b), (<) => $($tt)*);
  }};

  ($arr:ident, $is_init:ident => ..=$b:tt: $($tt:tt)*) => {{
    $crate::__parse_value!($arr, $is_init, None, Some($b), (<=) => $($tt)*);
  }};

  ($arr:ident, $is_init:ident => $a:tt..: $($tt:tt)*) => {{
    $crate::__parse_value!($arr, $is_init, Some($a), None, (<) => $($tt)*);
  }};

  ($arr:ident, $is_init:ident => ..: $($tt:tt)*) => {{
    $crate::__parse_value!($arr, $is_init, None, None, (<) => $($tt)*);
  }};

  ($arr:ident, $is_init:ident => $a:tt: $v:expr $(, $($tt:tt)*)?) => {{
    extern crate core;
    $arr[$a] = core::mem::MaybeUninit::new($v);
    $crate::__parse_key!($arr, $is_init => $($($tt)*)?);
  }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! __parse_value {
  ($arr:ident, $is_init:ident, $start:expr, $end:expr, ($cmp:tt) => |$i:tt| $v:expr $(, $($tt:tt)*)?) => {{
    $crate::__insert!($arr, $is_init, $start, $end, ($cmp), |$i| $v);
    $crate::__parse_key!($arr, $is_init => $($($tt)*)?);
  }};

  ($arr:ident, $is_init:ident, $start:expr, $end:expr, ($cmp:tt) => [$e:expr; _] $(, $($tt:tt)*)?) => {{
    let e = $e;
    $crate::__insert!($arr, $is_init, $start, $end, ($cmp), |_| e);
    $crate::__parse_key!($arr, $is_init => $($($tt)*)?);
  }};

  ($arr:ident, $is_init:ident, $start:expr, $end:expr, ($cmp:tt) => $v:expr $(, $($tt:tt)*)?) => {{
    let v = $v;
    let start: usize = match $start {
      Some(x) => x,
      _ => 0,
    };
    $crate::__insert!($arr, $is_init, Some(start), $end, ($cmp), |i| v[i - start]);
    $crate::__parse_key!($arr, $is_init => $($($tt)*)?);
  }};
}

#[test]
fn test() {
  const Z: &[i32] = &ary![0; 15 => 8: 27, 9: !0];
  const X: [i32; 58] = ary![1, in [2; 40], in Z, in [3; 2] =>
    30..40: |i| -(i as i32),
    40: -1000,
  ];

  assert_eq!(
    X,
    [
      1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
      2, 2, 2, 2, 2, 2, -30, -31, -32, -33, -34, -35, -36, -37, -38, -39,
      -1000, 0, 0, 0, 0, 0, 0, 0, 0, 27, -1, 0, 0, 0, 0, 0, 3, 3
    ]
  );

  const Y: &[u8] = &ary![0; 32 =>
    5.._: "a string!".as_bytes(),
  ];
  const Y2: &[u8] = &ary![0, 0, 0, 0, 0, in b"a string!", in [0; 32 - 14]];

  assert_eq!(
    Y,
    [
      0, 0, 0, 0, 0, 97, 32, 115, 116, 114, 105, 110, 103, 33, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    ]
  );
  assert_eq!(Y, Y2);

  const _: [i32; 0] = ary![];
  const _: [i32; 0] = ary![=>];
}