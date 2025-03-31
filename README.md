# qvek

Convenient vector creation macros for the `vek` crate.

`qvek` simplifies the construction of `vek` vectors by allowing you to combine scalars and smaller vectors directly in macros, with automatic type conversions. It is syntax sugar library and should result into no-op in most cases

To enable potentially unsafe conversions (e.g. f32 into i8), enable feature `unsafe_conversions`


```rust
use qvek::*;
use vek::*;

// Create a Vec2<f32> from i16 and u8 and combine it with i8 to create Vec3<f64>:
let a = dvec3!(vec2!(1i16, 2 as u8), 3i8);
assert_eq!(a, Vec3::<f64>::new(1.0, 2.0, 3.0));

// Create a Vec2<i32> from two integers:
let b = ivec2!(1, 2);
assert_eq!(b, Vec2::<i32>::new(1, 2));

// Create a Vec4<f64>:
let c = dvec4!(vec3!(1, 2.0, 3 as i8), 4 as f64);
assert_eq!(c, Vec4::<f64>::new(1.0, 2.0, 3.0, 4.0));
```

<!-- i am addicted to meta libraries -->