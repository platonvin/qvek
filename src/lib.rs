//! # qvek: glm-style casts for vek. Quality (of life) vek extension
//!
//! This library provides helper traits and macros to allow creation of vectors (from the
//! [vek](https://crates.io/crates/vek) crate) using automatic conversion between scalar types.
//!
//! Instead of writing:
//!
//! ```rust
//! # use qvek::*;
//! # use vek::*;
//! let a = Vec3::new(1.0f32, 2.0f32, 3.0f32);
//! ```
//!
//! you can combine different proxy types (scalars or even smaller vectors) using the macros:
//!
//! ```rust
//! # use qvek::*;
//! # use vek::*;
//! // Create a Vec2<f32> from i16 and u8 and combine it with i8 to create Vec3<f64>:
//! let a = dvec3!(vec2!(1i16, 2 as u8), 3i8);
//! assert_eq!(a, Vec3::<f64>::new(1.0, 2.0, 3.0));
//!
//! // Create a Vec2<i32> from two integers:
//! let b = ivec2!(1, 2);
//! assert_eq!(b, Vec2::<i32>::new(1, 2));
//!
//! // Create a Vec4<f64>:
//! let c = dvec4!(vec3!(1, 2.0, 3 as i8), 4 as f64);
//! assert_eq!(c, Vec4::<f64>::new(1.0, 2.0, 3.0, 4.0));
//! ```

//TODO: figure out bool and NonZero types

// use std::num::{NonZeroI8, NonZeroI16, NonZeroI32, NonZeroI64, NonZeroI128};
// use std::num::{NonZeroU8, NonZeroU16, NonZeroU32, NonZeroU64, NonZeroU128};

use vek::num_traits::NumCast;
// use vek::num_traits::Default;
pub use vek::*;

/// Trait that converts a type into a proxy array of target scalars.
///
/// A “proxy” is simply a fixed-size array that holds one or more components that can be converted
/// into the target vector type.
pub trait IntoProxy<T> {
    /// The proxy type (typically an array of scalars).
    type Proxy: AsRef<[T]>;

    /// The number of scalar components provided by this type.
    const PROXY_SIZE: usize;

    /// Convert `self` into a proxy array.
    fn into_proxy(self) -> Self::Proxy;
}

/// Marker trait for allowed scalar types.
pub trait Scalar {}

impl Scalar for f32 {}
impl Scalar for f64 {}
impl Scalar for i8 {}
impl Scalar for i16 {}
impl Scalar for i32 {}
impl Scalar for i64 {}
impl Scalar for i128 {}
impl Scalar for isize {}
// impl Scalar for NonZeroI8 {}
// impl Scalar for NonZeroI16 {}
// impl Scalar for NonZeroI32 {}
// impl Scalar for NonZeroI64 {}
// impl Scalar for NonZeroI128 {}

impl Scalar for u8 {}
impl Scalar for u16 {}
impl Scalar for u32 {}
impl Scalar for u64 {}
impl Scalar for usize {}
// impl Scalar for NonZeroU8 {}
// impl Scalar for NonZeroU16 {}
// impl Scalar for NonZeroU32 {}
// impl Scalar for NonZeroU64 {}
// impl Scalar for NonZeroU128 {}

impl Scalar for bool {}

/// Conversion trait that converts one scalar type into another.
///
/// By default, this implementation calls `From::from`. That means for most conversions the normal
/// From implementations will be used. However, when the feature `unsafe_conversions` is enabled,
/// you can provide extra implementations (for instance, converting an f32 to an i8).
pub trait Convert<Source> {
    fn convert(src: Source) -> Self;
}

// SAFE CONVERSION: use num_traits::cast::cast to perform a generic numeric conversion.
// This branch is compiled when the feature flag "unsafe_conversions" is NOT enabled.
// Both Source and Target must implement NumCast.
// This covers conversions like i32 to f32.
#[cfg(not(feature = "unsafe_conversions"))]
impl<Source, Target> Convert<Source> for Target
where
    Source: NumCast,
    Target: NumCast,
{
    fn convert(src: Source) -> Self {
        num_traits::cast::cast(src).expect("Conversion failed")
    }
}

// When "unsafe_conversions" is enabled, use a conversion based on the as operator.
// We require that Source implements num_traits::AsPrimitive<Target> to allow the conversion.
#[cfg(feature = "unsafe_conversions")]
impl<Source, Target> Convert<Source> for Target
where
    Source: num_traits::AsPrimitive<Target>,
    Target: std::marker::Copy + 'static,
{
    fn convert(src: Source) -> Self {
        src.as_()
    }
}

/// Marker trait for vectors with a given number of elements.
pub trait VekVector<const N: usize> {}

impl<T> VekVector<2> for Vec2<T> {}
impl<T> VekVector<3> for Vec3<T> {}
impl<T> VekVector<4> for Vec4<T> {}

/// Blanket implementation for scalar types.
///
/// A scalar converts to a proxy array containing one element.
impl<E, T> IntoProxy<T> for E
where
    E: Scalar,
    T: Scalar + Convert<E>,
{
    type Proxy = [T; 1];
    const PROXY_SIZE: usize = 1;

    fn into_proxy(self) -> Self::Proxy {
        [<T as Convert<E>>::convert(self)]
    }
}

/// Implementation for Vec2<E>.
///
/// A Vec2 converts to a proxy array with two elements.
impl<E, T> IntoProxy<T> for Vec2<E>
where
    E: Copy,
    T: Scalar + Convert<E>,
{
    type Proxy = [T; 2];
    const PROXY_SIZE: usize = 2;

    fn into_proxy(self) -> Self::Proxy {
        [
            <T as Convert<E>>::convert(self.x),
            <T as Convert<E>>::convert(self.y),
        ]
    }
}

/// Implementation for Vec3<E>.
///
/// A Vec3 converts to a proxy array with three elements.
impl<E, T> IntoProxy<T> for Vec3<E>
where
    E: Copy,
    T: Scalar + Convert<E>,
{
    type Proxy = [T; 3];
    const PROXY_SIZE: usize = 3;

    fn into_proxy(self) -> Self::Proxy {
        [
            <T as Convert<E>>::convert(self.x),
            <T as Convert<E>>::convert(self.y),
            <T as Convert<E>>::convert(self.z),
        ]
    }
}

/// Implementation for Vec4<E>.
///
/// A Vec4 converts to a proxy array with four elements.
impl<E, T> IntoProxy<T> for Vec4<E>
where
    E: Copy,
    T: Scalar + Convert<E>,
{
    type Proxy = [T; 4];
    const PROXY_SIZE: usize = 4;

    fn into_proxy(self) -> Self::Proxy {
        [
            <T as Convert<E>>::convert(self.x),
            <T as Convert<E>>::convert(self.y),
            <T as Convert<E>>::convert(self.z),
            <T as Convert<E>>::convert(self.w),
        ]
    }
}

/// --- Functions for Vec2<E> ---

/// Builds a Vec2<E> from a single argument whose proxy size must be exactly 2.
pub fn vec2_1<E, T: IntoProxy<E>>(arg1: T) -> Vec2<E>
where
    E: Clone + Copy,
{
    if T::PROXY_SIZE != 2 {
        panic!(
            "Single argument must have a proxy size of 2. Got {}",
            T::PROXY_SIZE
        );
    }
    let proxy = arg1.into_proxy();
    let components = proxy.as_ref();
    Vec2::<E>::new(components[0], components[1])
}

/// Builds a Vec2<E> from two arguments whose combined proxy sizes equal 2.
pub fn vec2_2<E, T1: IntoProxy<E>, T2: IntoProxy<E>>(arg1: T1, arg2: T2) -> Vec2<E>
where
    E: Clone + Copy + Default,
{
    if T1::PROXY_SIZE + T2::PROXY_SIZE != 2 {
        panic!("Total proxy size of two arguments must be 2");
    }
    let mut components: [E; 2] = [Default::default(); 2];
    let proxy1 = arg1.into_proxy();
    let proxy2 = arg2.into_proxy();
    let slice1 = proxy1.as_ref();
    let slice2 = proxy2.as_ref();
    components[0..slice1.len()].copy_from_slice(slice1);
    components[slice1.len()..].copy_from_slice(slice2);
    Vec2::<E>::new(components[0], components[1])
}

/// --- Functions for Vec3<E> ---

/// Builds a Vec3<E> from a single argument whose proxy size must be exactly 3.
pub fn vec3_1<E, T: IntoProxy<E>>(arg1: T) -> Vec3<E>
where
    E: Clone + Copy,
{
    if T::PROXY_SIZE != 3 {
        panic!(
            "Single argument must have a proxy size of 3. Got {}",
            T::PROXY_SIZE
        );
    }
    let proxy = arg1.into_proxy();
    let components = proxy.as_ref();
    Vec3::<E>::new(components[0], components[1], components[2])
}

/// Builds a Vec3<E> from two arguments whose combined proxy sizes equal 3.
pub fn vec3_2<E, T1: IntoProxy<E>, T2: IntoProxy<E>>(arg1: T1, arg2: T2) -> Vec3<E>
where
    E: Clone + Copy + Default,
{
    if T1::PROXY_SIZE + T2::PROXY_SIZE != 3 {
        panic!("Total proxy size of two arguments must be 3");
    }
    let mut components: [E; 3] = [Default::default(); 3];
    let proxy1 = arg1.into_proxy();
    let proxy2 = arg2.into_proxy();
    let slice1 = proxy1.as_ref();
    let slice2 = proxy2.as_ref();
    components[0..slice1.len()].copy_from_slice(slice1);
    components[slice1.len()..].copy_from_slice(slice2);
    Vec3::<E>::new(components[0], components[1], components[2])
}

/// Builds a Vec3<E> from three arguments whose combined proxy sizes equal 3.
pub fn vec3_3<E, T1: IntoProxy<E>, T2: IntoProxy<E>, T3: IntoProxy<E>>(
    arg1: T1,
    arg2: T2,
    arg3: T3,
) -> Vec3<E>
where
    E: Clone + Copy + Default,
{
    if T1::PROXY_SIZE + T2::PROXY_SIZE + T3::PROXY_SIZE != 3 {
        panic!("Total proxy size of three arguments must be 3");
    }
    let mut components: [E; 3] = [Default::default(); 3];
    let proxy1 = arg1.into_proxy();
    let proxy2 = arg2.into_proxy();
    let proxy3 = arg3.into_proxy();
    let mut idx = 0;
    for comp in proxy1.as_ref().iter() {
        components[idx] = *comp;
        idx += 1;
    }
    for comp in proxy2.as_ref().iter() {
        components[idx] = *comp;
        idx += 1;
    }
    for comp in proxy3.as_ref().iter() {
        components[idx] = *comp;
        idx += 1;
    }
    Vec3::<E>::new(components[0], components[1], components[2])
}

/// --- Functions for Vec4<E> ---

/// Builds a Vec4<E> from a single argument whose proxy size must be exactly 4.
pub fn vec4_1<E, T: IntoProxy<E>>(arg1: T) -> Vec4<E>
where
    E: Clone + Copy,
{
    if T::PROXY_SIZE != 4 {
        panic!(
            "Single argument must have a proxy size of 4. Got {}",
            T::PROXY_SIZE
        );
    }
    let proxy = arg1.into_proxy();
    let comp = proxy.as_ref();
    Vec4::<E>::new(comp[0], comp[1], comp[2], comp[3])
}

/// Builds a Vec4<E> from two arguments whose combined proxy sizes equal 4.
pub fn vec4_2<E, T1: IntoProxy<E>, T2: IntoProxy<E>>(arg1: T1, arg2: T2) -> Vec4<E>
where
    E: Clone + Copy + Default,
{
    if T1::PROXY_SIZE + T2::PROXY_SIZE != 4 {
        panic!("Total proxy size of two arguments must be 4");
    }
    let mut components: [E; 4] = [Default::default(); 4];
    let proxy1 = arg1.into_proxy();
    let proxy2 = arg2.into_proxy();
    let slice1 = proxy1.as_ref();
    let slice2 = proxy2.as_ref();
    components[0..slice1.len()].copy_from_slice(slice1);
    components[slice1.len()..].copy_from_slice(slice2);
    Vec4::<E>::new(components[0], components[1], components[2], components[3])
}

/// Builds a Vec4<E> from three arguments whose combined proxy sizes equal 4.
pub fn vec4_3<E, T1: IntoProxy<E>, T2: IntoProxy<E>, T3: IntoProxy<E>>(
    arg1: T1,
    arg2: T2,
    arg3: T3,
) -> Vec4<E>
where
    E: Clone + Copy + Default,
{
    if T1::PROXY_SIZE + T2::PROXY_SIZE + T3::PROXY_SIZE != 4 {
        panic!("Total proxy size of three arguments must be 4");
    }
    let mut components: [E; 4] = [Default::default(); 4];
    let proxy1 = arg1.into_proxy();
    let proxy2 = arg2.into_proxy();
    let proxy3 = arg3.into_proxy();
    let mut idx = 0;
    for comp in proxy1.as_ref().iter() {
        components[idx] = *comp;
        idx += 1;
    }
    for comp in proxy2.as_ref().iter() {
        components[idx] = *comp;
        idx += 1;
    }
    for comp in proxy3.as_ref().iter() {
        components[idx] = *comp;
        idx += 1;
    }
    Vec4::<E>::new(components[0], components[1], components[2], components[3])
}

/// Builds a Vec4<E> from four arguments whose combined proxy sizes equal 4.
pub fn vec4_4<E, T1: IntoProxy<E>, T2: IntoProxy<E>, T3: IntoProxy<E>, T4: IntoProxy<E>>(
    arg1: T1,
    arg2: T2,
    arg3: T3,
    arg4: T4,
) -> Vec4<E>
where
    E: Clone + Copy + Default,
{
    if T1::PROXY_SIZE + T2::PROXY_SIZE + T3::PROXY_SIZE + T4::PROXY_SIZE != 4 {
        panic!("Total proxy size of four arguments must be 4");
    }
    let mut components: [E; 4] = [Default::default(); 4];
    let proxy1 = arg1.into_proxy();
    let proxy2 = arg2.into_proxy();
    let proxy3 = arg3.into_proxy();
    let proxy4 = arg4.into_proxy();
    let mut idx = 0;

    for &comp in proxy1.as_ref().iter() {
        components[idx] = comp;
        idx += 1;
    }
    for &comp in proxy2.as_ref().iter() {
        components[idx] = comp;
        idx += 1;
    }
    for &comp in proxy3.as_ref().iter() {
        components[idx] = comp;
        idx += 1;
    }
    for &comp in proxy4.as_ref().iter() {
        components[idx] = comp;
        idx += 1;
    }
    Vec4::<E>::new(components[0], components[1], components[2], components[3])
}

/// Generates vector creation macros for a given scalar type.
///
/// This macro generates three macros for creating 2-, 3-, and 4-component vectors.
/// Each macro uses your generic helper functions (e.g. `vec2_1`, `vec3_3`, etc.)
/// with the provided scalar type.
///
/// # Example
///
/// ```rust
/// # use qvek::generate_vec_macros;
/// # use qvek::vec3_3;
/// // This will generate macros i16vec2!, i16vec3!, and i16vec4!
/// generate_vec_macros!(i16, i16vec2, i16vec3, i16vec4);
///
/// let a = i16vec3!(1i16, 2i16, 3i16);
/// // now a is Vec3<i16> built using your helper functions.
/// ```
#[macro_export]
macro_rules! generate_vec_macros {
    ($scalar:ty, $vec2_macro:ident, $vec3_macro:ident, $vec4_macro:ident) => {
        #[macro_export]
        macro_rules! $vec2_macro {
            ($arg1:expr) => {
                vec2_1::<$scalar, _>($arg1)
            };
            ($arg1:expr, $arg2:expr) => {
                vec2_2::<$scalar, _, _>($arg1, $arg2)
            }; // ($($arg:tt)+) => {
               //     compile_error!(concat!(stringify!($vec2_macro), " macro only supports 1 or 2 arguments"))
               // };
        }

        #[macro_export]
        macro_rules! $vec3_macro {
            ($arg1:expr) => {
                vec3_1::<$scalar, _>($arg1)
            };
            ($arg1:expr, $arg2:expr) => {
                vec3_2::<$scalar, _, _>($arg1, $arg2)
            };
            ($arg1:expr, $arg2:expr, $arg3:expr) => {
                vec3_3::<$scalar, _, _, _>($arg1, $arg2, $arg3)
            }; // ($($arg:tt)+) => {
               //     compile_error!(concat!(stringify!($vec3_macro), " macro only supports 1, 2, or 3 arguments"))
               // };
        }

        #[macro_export]
        macro_rules! $vec4_macro {
            ($arg1:expr) => {
                vec4_1::<$scalar, _>($arg1)
            };
            ($arg1:expr, $arg2:expr) => {
                vec4_2::<$scalar, _, _>($arg1, $arg2)
            };
            ($arg1:expr, $arg2:expr, $arg3:expr) => {
                vec4_3::<$scalar, _, _, _>($arg1, $arg2, $arg3)
            };
            ($arg1:expr, $arg2:expr, $arg3:expr, $arg4:expr) => {
                vec4_4::<$scalar, _, _, _, _>($arg1, $arg2, $arg3, $arg4)
            }; // ($($arg:tt)+) => {
               //     compile_error!(concat!(stringify!($vec4_macro), " macro only supports 1, 2, 3, or 4 arguments"))
               // };
        }
    };
}

// floats
generate_vec_macros!(f32, vec2, vec3, vec4);
generate_vec_macros!(f64, dvec2, dvec3, dvec4);
// signed int
generate_vec_macros!(i8, i8vec2, i8vec3, i8vec4);
generate_vec_macros!(i16, i16vec2, i16vec3, i16vec4);
generate_vec_macros!(i32, i32vec2, i32vec3, i32vec4);
generate_vec_macros!(i32, ivec2, ivec3, ivec4); // no bits is 32 bits
generate_vec_macros!(i64, i64vec2, i64vec3, i64vec4);
// unsigned int
generate_vec_macros!(u8, u8vec2, u8vec3, u8vec4);
generate_vec_macros!(u16, u16vec2, u16vec3, u16vec4);
generate_vec_macros!(u32, u32vec2, u32vec3, u32vec4);
generate_vec_macros!(u32, uvec2, uvec3, uvec4); // no bits is 32 bits
generate_vec_macros!(u64, u64vec2, u64vec3, u64vec4);
// bool
// TODO do bvec's even make sence when there is no bvec functions?
// generate_vec_macros!(bool, bvec2, bvec3, bvec4);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vec2_combinations() {
        assert_eq!(vec2!(1.0f32, 2.0f32), Vec2::<f32>::new(1.0, 2.0));
        assert_eq!(vec2!(1i8, 2i8), Vec2::<f32>::new(1.0, 2.0));
        assert_eq!(vec2!(Vec2::new(1i16, 2i16)), Vec2::<f32>::new(1.0, 2.0));
        assert_eq!(vec2!(1.0f32, 2i8), Vec2::<f32>::new(1.0, 2.0));
        assert_eq!(vec2!(1, 2), Vec2::<f32>::new(1.0, 2.0));
        assert_eq!(i8vec2!(1i8, 2i8), Vec2::<i8>::new(1, 2));
        assert_eq!(u32vec2!(1u32, 2u32), Vec2::<u32>::new(1, 2));
        // #[cfg(feature = "unsafe_conversions")]
        // assert_eq!(bvec2!(true, false), Vec2::<bool>::new(true, false));
    }

    #[test]
    #[should_panic]
    fn test_vec2_invalid_combinations() {
        vec2!(1.0f32); // Single scalar, proxy size 1
    }

    #[test]
    fn test_vec3_combinations() {
        assert_eq!(
            vec3!(1.0f32, 2.0f32, 3.0f32),
            Vec3::<f32>::new(1.0, 2.0, 3.0),
        );
        assert_eq!(vec3!(1i8, 2i8, 3i8), Vec3::<f32>::new(1.0, 2.0, 3.0));
        assert_eq!(
            vec3!(Vec2::new(1i16, 2i16), 3i8),
            Vec3::<f32>::new(1.0, 2.0, 3.0),
        );
        assert_eq!(
            vec3!(1.0f32, Vec2::new(2i8, 3i8)),
            Vec3::<f32>::new(1.0, 2.0, 3.0),
        );
        assert_eq!(vec3!(1i8, 2.0f32, 3i16), Vec3::<f32>::new(1.0, 2.0, 3.0));
        assert_eq!(
            vec3!(Vec3::new(1i8, 2i8, 3i8)),
            Vec3::<f32>::new(1.0, 2.0, 3.0),
        );
        assert_eq!(i32vec3!(1, 2, 3), Vec3::<i32>::new(1, 2, 3));
        assert_eq!(u64vec3!(1, 2, 3), Vec3::<u64>::new(1, 2, 3));
        assert_eq!(dvec3!(1.0, 2.0, 3.0), Vec3::<f64>::new(1.0, 2.0, 3.0));
    }

    #[test]
    #[should_panic]
    fn test_vec3_invalid_combinations() {
        vec3!(1.0f32); // Single scalar, proxy size 1
    }

    #[test]
    fn test_vec4_combinations() {
        assert_eq!(
            vec4!(1.0f32, 2.0f32, 3.0f32, 4.0f32),
            Vec4::<f32>::new(1.0, 2.0, 3.0, 4.0),
        );
        assert_eq!(
            vec4!(1i8, 2i8, 3i8, 4i8),
            Vec4::<f32>::new(1.0, 2.0, 3.0, 4.0),
        );
        assert_eq!(
            vec4!(Vec3::new(1i16, 2i16, 3i16), 4i8),
            Vec4::<f32>::new(1.0, 2.0, 3.0, 4.0),
        );
        assert_eq!(
            vec4!(1.0f32, Vec3::new(2i8, 3i8, 4i8)),
            Vec4::<f32>::new(1.0, 2.0, 3.0, 4.0),
        );
        assert_eq!(
            vec4!(Vec2::new(1i8, 2i8), Vec2::new(3i8, 4i8)),
            Vec4::<f32>::new(1.0, 2.0, 3.0, 4.0),
        );
        #[cfg(feature = "unsafe_conversions")] // cause f64 into f32
        assert_eq!(
            vec4!(1i8, 2.0f32, 3i16, 4.0f64),
            Vec4::<f32>::new(1.0, 2.0, 3.0, 4.0),
        );
        assert_eq!(
            vec4!(Vec4::new(1i8, 2i8, 3i8, 4i8)),
            Vec4::<f32>::new(1.0, 2.0, 3.0, 4.0),
        );
        assert_eq!(ivec4!(1, 2, 3, 4), Vec4::<i32>::new(1, 2, 3, 4));
        assert_eq!(u8vec4!(1, 2, 3, 4), Vec4::<u8>::new(1, 2, 3, 4));
    }

    #[test]
    #[should_panic]
    fn test_vec4_invalid_combinations() {
        vec4!(1.0f32); // Single scalar, proxy size 1
    }

    #[cfg(feature = "unsafe_conversions")]
    #[test]
    fn test_unsafe_conversions() {
        assert_eq!(i8vec2!(1.99f32, 127.999f32), Vec2::<i8>::new(1, 127));
        assert_eq!(u8vec3!(1.0f32, 2.0f32, 3.0f32), Vec3::<u8>::new(1, 2, 3));
    }

    #[test]
    fn test_scalar_trait() {
        assert!(std::any::TypeId::of::<f32>() == std::any::TypeId::of::<f32>());
        assert!(std::any::TypeId::of::<i8>() == std::any::TypeId::of::<i8>());
        assert!(std::any::TypeId::of::<u64>() == std::any::TypeId::of::<u64>());
        assert!(std::any::TypeId::of::<bool>() == std::any::TypeId::of::<bool>());
    }

    // TODO: figure this out
    // #[test]
    // #[cfg(feature = "unsafe_conversions")]
    // fn test_non_Default() {
    //     assert_eq!(
    //         vec2!(NonZeroI8::new(1).unwrap(), NonZeroI8::new(2).unwrap()),
    //         Vec2::<f32>::new(1.0, 2.0),
    //     );
    //     assert_eq!(
    //         vec3!(
    //             NonZeroU32::new(1).unwrap(),
    //             NonZeroU32::new(2).unwrap(),
    //             NonZeroU32::new(3).unwrap()
    //         ),
    //         Vec3::<f32>::new(1.0, 2.0, 3.0),
    //     );
    //     assert_eq!(
    //         vec4!(
    //             NonZeroU64::new(1).unwrap(),
    //             NonZeroU64::new(2).unwrap(),
    //             NonZeroU64::new(3).unwrap(),
    //             NonZeroU64::new(4).unwrap()
    //         ),
    //         Vec4::<f32>::new(1.0, 2.0, 3.0, 4.0),
    //     );
    // }
}
