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
//! ## Inferring Array Length from Ranges
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
//! let gap: &[i32; 10] = &ary![=>
//!   0..5: [1; 5],
//!   // Element 5 is missing.
//!   6..10: [2; 4],
//! ];
//! ```
//!
//! This check occurs at compile time rather than at runtime.
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
    #[allow(unused_imports)]
    use ::core::{
      primitive::bool as __bool,
      primitive::usize as __usize,
      mem::MaybeUninit as __MaybeUninit,
      option::Option as __Option,
      option::Option::Some as __Some,
      option::Option::None as __None,
    };

    // The `$cb!(..)` construction used below only works if `$cb:tt`, so
    // we need to `use` the internal macro names to give them a single-token
    // name.
    #[allow(unused_imports)]
    use $crate::{
      __mark_init as __ary__mark_init,
      __insert as __ary__insert,
    };

    // NOTE: All variables that are not unconditionally used must be prefixed
    // with _ to silence compiler warnings.

    const LEN: (__usize, __bool) = {
      let mut _total: __usize = 0;
      let mut _has_preinit = false;
      $crate::__compute_len!(@_total, _has_preinit => $($tt)*);
      (_total, _has_preinit)
    };

    const _VERIFY_EVERY_SLOT_IS_INITIALIZED: () = {
      if LEN.1 {
        const BITS: __usize = __usize::BITS as __usize;
        let mut _is_init = [0usize; LEN.0 / BITS + 1];
        let mut _idx: __usize = 0;
        $crate::__init!(__ary__mark_init, _is_init, LEN.0, _idx => $($tt)*);

        let mut i = 0;
        while i < LEN.0 {
          assert!(
            _is_init[i / BITS] >> (i % BITS) & 1 == 1,
            "failed to initialize at least one array element",
          );
          i += 1;
        }
      }
    };

    let mut _uninit_arr = [__MaybeUninit::uninit(); LEN.0];
    let mut _idx: __usize = 0;
    $crate::__init!(__ary__insert, _uninit_arr, LEN.0, _idx => $($tt)*);

    let arr = unsafe { ::core::mem::transmute(_uninit_arr) };
    $crate::__type_check(&_uninit_arr, &arr);
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
  if a > b {
    a
  } else {
    b
  }
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
  ($cb:tt, $arr:ident, $total_len:expr, $idx:ident => $e:expr; $n:expr $(=> $($tt:tt)*)?) => {{
    let _e = $e;
    $idx += $cb!($arr, $total_len, __None, __None, (<), |_| _e);
    $crate::__parse_key!($cb, $arr, $total_len => $($($tt)*)?);
  }};
  ($cb:tt, $arr:ident, $total_len:expr, $idx:ident => in $e:expr; $n:expr $(=> $($tt:tt)*)?) => {{
    let _e = $e;
    let _start = $idx;
    let _n: __usize = $n;
    $idx += $cb!($arr, $total_len, __Some($idx), __Some($idx + _n * _e.len()), (<), |_i| _e[(_i - _start) % _e.len()]);
    $crate::__parse_key!($cb, $arr, $total_len => $($($tt)*)?);
  }};
  ($cb:tt, $arr:ident, $total_len:expr, $idx:ident => $(,)? $(=> $($tt:tt)*)?) => {{
    $crate::__parse_key!($cb, $arr, $total_len => $($($tt)*)?);
  }};
  ($cb:tt, $arr:ident, $total_len:expr, $idx:ident => in $e:expr $(, $($tt:tt)*)?) => {{
    let _e = $e;
    let _start = $idx;
    $idx += $cb!($arr, $total_len, __Some($idx), __Some($idx + _e.len()), (<), |_i| _e[_i - _start]);
    $crate::__init!($cb, $arr, $total_len, $idx => $($($tt)*)?);
  }};
  ($cb:tt, $arr:ident, $total_len:expr, $idx:ident => in $e:expr $(=> $($tt:tt)*)?) => {{
    $crate::__init!($cb, $arr, $total_len, $idx => in $e);
    $crate::__parse_key!($cb, $arr, $total_len => $($($tt)*)?);
  }};
  ($cb:tt, $arr:ident, $total_len:expr, $idx:ident => $e:expr $(, $($tt:tt)*)?) => {{
    let _e = $e;
    $idx += $cb!($arr, $total_len, __Some($idx), __Some($idx + 1), (<), |_| _e);
    $crate::__init!($cb, $arr, $total_len, $idx => $($($tt)*)?);
  }};
  ($cb:tt, $arr:ident, $total_len:expr, $idx:ident => $e:expr $(=> $($tt:tt)*)?) => {{
    $crate::__init!($cb, $arr, $total_len, $idx => $e);
    $crate::__parse_key!($cb, $arr, $total_len => $($($tt)*)?);
  }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! __mark_init {
  ($arr:ident, $total_len:expr, $start:expr, $end:expr, ($cmp:tt), |$i:tt| $v:expr) => {{
    let start: __Option<__usize> = $start;
    let start = match start {
        __Some(x) => x,
        __None => 0,
    };

    let end: __Option<__usize> = $end;
    let end = match end {
        __Some(x) => x,
        __None => $total_len,
    };

    let mut i = start;
    while i $cmp end {
      $arr[i / (__usize::BITS as __usize)] |= (1 << (i % __usize::BITS as __usize));
      i += 1;
    }

    i - start
  }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! __insert {
  ($arr:ident, $total_len:expr, $start:expr, $end:expr, ($cmp:tt), |$i:tt| $v:expr) => {{
    let start: __Option<__usize> = $start;
    let start = match start {
        __Some(x) => x,
        __None => 0,
    };

    let end: __Option<__usize> = $end;
    let end = match end {
        __Some(x) => x,
        __None => $total_len,
    };

    let mut i = start;
    while i $cmp end {
      // Closure execution is not allowed in const, so we make do.
      $arr[i] = __MaybeUninit::new({ let $i = i; $v });
      i += 1;
    }

    i - start
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
  ($cb:tt, $arr:ident, $total_len:expr =>) => {};

  ($cb:tt, $arr:ident, $total_len:expr => $a:tt.._: $v:expr $(, $($tt:tt)*)?) => {{
    const START: __usize = $a;
    let _v = $v;
    let _len: __usize = _v.len();
    $cb!($arr, $total_len, __Some(START), __Some(START + _len), (<), |_i| $v[_i - START]);
    $crate::__parse_key!($cb, $arr, $total_len => $($($tt)*)?);
  }};

  ($cb:tt, $arr:ident, $total_len:expr => .._: $v:expr $(, $($tt:tt)*)?) => {{
    let _v = $v;
    let _len: __usize = _v.len();
    $cb!($arr, $total_len, __None, __Some(_len), (<), |_i| _v[_i]);
    $crate::__parse_key!($cb, $arr, $total_len => $($($tt)*)?);
  }};

  ($cb:tt, $arr:ident, $total_len:expr => $a:tt..$b:tt: $($tt:tt)*) => {{
    const START: __usize = $a;
    const END: __usize = $b;
    $crate::__parse_value!($cb, $arr, $total_len, __Some(START), __Some(END), (<) => $($tt)*);
  }};

  ($cb:tt, $arr:ident, $total_len:expr => $a:tt..=$b:tt: $($tt:tt)*) => {{
    const START: __usize = $a;
    const END: __usize = $b;
    $crate::__parse_value!($cb, $arr, $total_len, __Some(START), __Some(END), (<=) => $($tt)*);
  }};

  ($cb:tt, $arr:ident, $total_len:expr => ..$b:tt: $($tt:tt)*) => {{
    const END: __usize = $b;
    $crate::__parse_value!($cb, $arr, $total_len, __None, __Some(END), (<) => $($tt)*);
  }};

  ($cb:tt, $arr:ident, $total_len:expr => ..=$b:tt: $($tt:tt)*) => {{
    const END: __usize = $b;
    $crate::__parse_value!($cb, $arr, $total_len, __None, __Some(END), (<=) => $($tt)*);
  }};

  ($cb:tt, $arr:ident, $total_len:expr => $a:tt..: $($tt:tt)*) => {{
    const START: __usize = $a;
    $crate::__parse_value!($cb, $arr, $total_len, __Some(START), __None, (<) => $($tt)*);
  }};

  ($cb:tt, $arr:ident, $total_len:expr => ..: $($tt:tt)*) => {{
    $crate::__parse_value!($cb, $arr, $total_len, __None, __None, (<) => $($tt)*);
  }};

  ($cb:tt, $arr:ident, $total_len:expr => $a:tt: $v:expr $(, $($tt:tt)*)?) => {{
    const START: __usize = $a;
    let _v = $v;
    $cb!($arr, $total_len, __Some(START), __Some(START + 1), (<), |_| _v);
    $crate::__parse_key!($cb, $arr, $total_len => $($($tt)*)?);
  }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! __parse_value {
  ($cb:tt, $arr:ident, $total_len:expr, $start:expr, $end:expr, ($cmp:tt) => |$i:tt| $v:expr $(, $($tt:tt)*)?) => {{
    $cb!($arr, $total_len, $start, $end, ($cmp), |$i| $v);
    $crate::__parse_key!($cb, $arr, $total_len => $($($tt)*)?);
  }};

  ($cb:tt, $arr:ident, $total_len:expr, $start:expr, $end:expr, ($cmp:tt) => [$e:expr; _] $(, $($tt:tt)*)?) => {{
    let _e = $e;
    $cb!($arr, $total_len, $start, $end, ($cmp), |_| _e);
    $crate::__parse_key!($cb, $arr, $total_len => $($($tt)*)?);
  }};

  ($cb:tt, $arr:ident, $total_len:expr, $start:expr, $end:expr, ($cmp:tt) => $v:expr $(, $($tt:tt)*)?) => {{
    let v = $v;
    let start: usize = match $start {
      __Some(x) => x,
      _ => 0,
    };
    $cb!($arr, $total_len, __Some(start), $end, ($cmp), |i| v[i - start]);
    $crate::__parse_key!($cb, $arr, $total_len => $($($tt)*)?);
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
