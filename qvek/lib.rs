//! # qvek: GLSL-style casts for vek. Zero-(runtime)-cost QOL [vek](https://crates.io/crates/vek) extension.
//!
//! Instead of writing:
//! ```rust
//! # use vek::*;
//! let a = vek::Vec3::new(1.0f32, 2.0f32, 3.0f32);
//! ```
//!
//! you can combine different types (scalars or smaller vecs) using GLSL-like macro:
//! ```rust
//! # use qvek::*;
//! // Create a Vec2<f32> from i16 and u8 and combine it with i8 to create Vec3<f64>:
//! let a = dvec3!(vec2!(1i16, 2 as u8), 3i8);
//! assert_eq!(a, vek::Vec3::<f64>::new(1.0, 2.0, 3.0));
//!
//! // Create a Vec2<i32> from two integers:
//! let b = ivec2!(1, 2);
//! assert_eq!(b, vek::Vec2::<i32>::new(1, 2));
//!
//! // Create a vek::Vec4<f64>:
//! let c = dvec4!(vec3!(1, 2.0, 3 as i8), 4 as f64);
//! assert_eq!(c, vek::Vec4::<f64>::new(1.0, 2.0, 3.0, 4.0));
//! ```
//! Note: at the moment, all casts between types are also GLSL-style - "unsafe" (casted with as)
#![feature(const_trait_impl)]

//TODO: figure out bool and NonZero types

pub extern crate vek;

/// Marker trait for allowed scalar types (we need this to be more specialized).
trait Scalar {}
macro_rules! impl_scalar { ($($t:ty),*) => { $( impl Scalar for $t {} )* } }
impl_scalar!(
    f32, f64, i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, usize, bool
);

/// Conversion trait that converts one scalar type into another.
///
/// Currently, it is implement by a macro with `as` cast for all primite type combinations.
#[const_trait]
trait ConvertFrom<Source> {
    fn convert_from(src: Source) -> Self;
}

// generates a bunch of ConvertFrom impls for primitive types
gen_conversions_macro::generate_conversions!();

/// Trait that converts a type into a proxy.
///
/// A "proxy" is simply a fixed-size array that holds one or more components
/// that will later be converted into the target vector type.
#[const_trait]
trait IntoProxy<T, const N: usize> {
    fn into_proxy(self) -> [T; N];
}

/// Scalar -> [T;1]
impl<S, T> const IntoProxy<T, 1> for S
where
    S: Scalar,
    T: const ConvertFrom<S> + Scalar,
{
    fn into_proxy(self) -> [T; 1] {
        [T::convert_from(self)]
    }
}
/// Scalar -> [T;2]
impl<S, T> const IntoProxy<T, 2> for S
where
    S: Scalar + Copy,
    T: const ConvertFrom<S> + Scalar,
{
    fn into_proxy(self) -> [T; 2] {
        [T::convert_from(self), T::convert_from(self)]
    }
}
/// Scalar -> [T;3]
impl<S, T> const IntoProxy<T, 3> for S
where
    S: Scalar + Copy,
    T: const ConvertFrom<S> + Scalar,
{
    fn into_proxy(self) -> [T; 3] {
        [
            T::convert_from(self),
            T::convert_from(self),
            T::convert_from(self),
        ]
    }
}
/// Scalar -> [T;4]
impl<S, T> const IntoProxy<T, 4> for S
where
    S: Scalar + Copy,
    T: const ConvertFrom<S> + Scalar,
{
    fn into_proxy(self) -> [T; 4] {
        [
            T::convert_from(self),
            T::convert_from(self),
            T::convert_from(self),
            T::convert_from(self),
        ]
    }
}

/// Vec2<E> -> [T;2]
impl<E, T> const IntoProxy<T, 2> for vek::Vec2<E>
where
    E: Copy + Scalar,
    T: const ConvertFrom<E> + Scalar,
{
    fn into_proxy(self) -> [T; 2] {
        [T::convert_from(self.x), T::convert_from(self.y)]
    }
}

/// Vec3<E> -> [T;3]
impl<E, T> const IntoProxy<T, 3> for vek::Vec3<E>
where
    E: Copy + Scalar,
    T: const ConvertFrom<E> + Scalar,
{
    fn into_proxy(self) -> [T; 3] {
        [
            T::convert_from(self.x),
            T::convert_from(self.y),
            T::convert_from(self.z),
        ]
    }
}

/// Vec4<E> -> [T;4]
impl<E, T> const IntoProxy<T, 4> for vek::Vec4<E>
where
    E: Copy + Scalar,
    T: const ConvertFrom<E> + Scalar,
{
    fn into_proxy(self) -> [T; 4] {
        [
            T::convert_from(self.x),
            T::convert_from(self.y),
            T::convert_from(self.z),
            T::convert_from(self.w),
        ]
    }
}

/// Trait to build Vec2 from various inputs.
#[const_trait]
pub trait BuildVec2<E> {
    fn build(self) -> vek::Vec2<E>;
}

/// Single Vec2<A> -> Vec2<E>
impl<E, A> const BuildVec2<E> for vek::Vec2<A>
where
    A: const IntoProxy<E, 2> + Copy + const ConvertFrom<E> + Scalar,
    E: Copy + const ConvertFrom<A> + Scalar,
{
    fn build(self) -> vek::Vec2<E> {
        let a = self.into_proxy();
        vek::Vec2::new(a[0], a[1])
    }
}

/// Two scalars (1+1) -> Vec2<E>
impl<E, S1, S2> const BuildVec2<E> for (S1, S2)
where
    S1: const IntoProxy<E, 1> + Copy + Scalar,
    S2: const IntoProxy<E, 1> + Copy + Scalar,
    E: Copy + Default + const ConvertFrom<S1> + const ConvertFrom<S2> + Scalar,
{
    fn build(self) -> vek::Vec2<E> {
        let p1 = self.0.into_proxy()[0];
        let p2 = self.1.into_proxy()[0];
        vek::Vec2::new(p1, p2)
    }
}

/// Scalar -> Vec2<E>
impl<E, S> const BuildVec2<E> for S
where
    S: const IntoProxy<E, 1> + Copy + Scalar,
    E: Copy + const ConvertFrom<S> + Scalar,
{
    fn build(self) -> vek::Vec2<E> {
        let a = self.into_proxy()[0];
        vek::Vec2::new(a, a)
    }
}

/// Trait to build Vec3 from various inputs.
#[const_trait]
pub trait BuildVec3<E> {
    fn build(self) -> vek::Vec3<E>;
}

/// Single Vec3<A> -> Vec3<E>
impl<E, A> const BuildVec3<E> for vek::Vec3<A>
where
    A: const IntoProxy<E, 3> + Copy + const ConvertFrom<E> + Scalar,
    E: Copy + Scalar + const ConvertFrom<A>,
{
    fn build(self) -> vek::Vec3<E> {
        let a = self.into_proxy();
        vek::Vec3::new(a[0], a[1], a[2])
    }
}

/// (scalar, Vec2<A>) -> Vec3<E>
impl<E, A, S> const BuildVec3<E> for (S, vek::Vec2<A>)
where
    S: const IntoProxy<E, 1> + Copy + Scalar + const ConvertFrom<A>,
    A: const IntoProxy<E, 2> + Copy + Scalar + const ConvertFrom<E>,
    E: Copy + Scalar + const ConvertFrom<A>,
{
    fn build(self) -> vek::Vec3<E> {
        let s = self.0.into_proxy()[0];
        let v = self.1.into_proxy();
        vek::Vec3::new(s, v[0], v[1])
    }
}

/// (Vec2<A>, scalar) -> Vec3<E>
impl<E, A, S> const BuildVec3<E> for (vek::Vec2<A>, S)
where
    A: const IntoProxy<E, 2> + Copy + const ConvertFrom<E> + Scalar,
    S: const IntoProxy<E, 1> + Copy + const ConvertFrom<A> + Scalar,
    E: Copy + const ConvertFrom<A> + Scalar,
{
    fn build(self) -> vek::Vec3<E> {
        let v = self.0.into_proxy();
        let s = self.1.into_proxy()[0];
        vek::Vec3::new(v[0], v[1], s)
    }
}

/// (s1, s2, s3) all scalars -> Vec3<E>
impl<E, S1, S2, S3> const BuildVec3<E> for (S1, S2, S3)
where
    S1: const IntoProxy<E, 1> + Copy + const ConvertFrom<E> + Scalar,
    S2: const IntoProxy<E, 1> + Copy + const ConvertFrom<E> + Scalar,
    S3: const IntoProxy<E, 1> + Copy + const ConvertFrom<E> + Scalar,
    E: Copy + Default + Scalar,
{
    fn build(self) -> vek::Vec3<E> {
        let x = self.0.into_proxy()[0];
        let y = self.1.into_proxy()[0];
        let z = self.2.into_proxy()[0];
        vek::Vec3::new(x, y, z)
    }
}

/// Scalar -> Vec3<E>
impl<E, S> const BuildVec3<E> for S
where
    S: const IntoProxy<E, 1> + Copy + Scalar,
    E: Copy + const ConvertFrom<S> + Scalar,
{
    fn build(self) -> vek::Vec3<E> {
        let a = self.into_proxy()[0];
        vek::Vec3::new(a, a, a)
    }
}

/// Trait to build Vec4 from various inputs.
#[const_trait]
pub trait BuildVec4<E> {
    fn build(self) -> vek::Vec4<E>;
}

/// Single Vec4<A> -> Vec4<E>
impl<E, A> const BuildVec4<E> for vek::Vec4<A>
where
    A: const IntoProxy<E, 4> + Copy + Scalar,
    E: Copy + const ConvertFrom<A> + Scalar,
{
    fn build(self) -> vek::Vec4<E> {
        let a = self.into_proxy();
        vek::Vec4::new(a[0], a[1], a[2], a[3])
    }
}

/// (Vec2<A>, Vec2<A2>) -> Vec4<E>
impl<E, A1, A2> const BuildVec4<E> for (vek::Vec2<A1>, vek::Vec2<A2>)
where
    A1: const IntoProxy<E, 2> + Copy + Scalar,
    A2: const IntoProxy<E, 2> + Copy + Scalar,
    E: Copy + const ConvertFrom<A1> + const ConvertFrom<A2> + Scalar,
{
    fn build(self) -> vek::Vec4<E> {
        let p1 = self.0.into_proxy();
        let p2 = self.1.into_proxy();
        vek::Vec4::new(p1[0], p1[1], p2[0], p2[1])
    }
}

/// (scalar, Vec3<A>) -> Vec4<E>
impl<E, S, A> const BuildVec4<E> for (S, vek::Vec3<A>)
where
    S: const IntoProxy<E, 1> + Copy + Scalar,
    A: const IntoProxy<E, 3> + Copy + Scalar,
    E: Copy + const ConvertFrom<A> + const ConvertFrom<S> + Scalar,
{
    fn build(self) -> vek::Vec4<E> {
        let s = self.0.into_proxy()[0];
        let v = self.1.into_proxy();
        vek::Vec4::new(s, v[0], v[1], v[2])
    }
}

/// (Vec3<A>, scalar) -> Vec4<E>
impl<E, A, S> const BuildVec4<E> for (vek::Vec3<A>, S)
where
    A: const IntoProxy<E, 3> + Copy + Scalar,
    S: const IntoProxy<E, 1> + Copy + Scalar,
    E: Copy + Scalar + const ConvertFrom<A> + const ConvertFrom<S> + Scalar,
{
    fn build(self) -> vek::Vec4<E> {
        let v = self.0.into_proxy();
        let s = self.1.into_proxy()[0];
        vek::Vec4::new(v[0], v[1], v[2], s)
    }
}

/// (s1, s2, Vec2<A>) -> Vec4<E>
impl<E, S1, S2, A> const BuildVec4<E> for (S1, S2, vek::Vec2<A>)
where
    S1: const IntoProxy<E, 1> + Scalar + Copy,
    S2: const IntoProxy<E, 1> + Scalar + Copy,
    A: const IntoProxy<E, 2> + Copy + Scalar,
    E: Copy + const ConvertFrom<S1> + const ConvertFrom<S2> + const ConvertFrom<A> + Scalar,
{
    fn build(self) -> vek::Vec4<E> {
        let x = self.0.into_proxy()[0];
        let y = self.1.into_proxy()[0];
        let v = self.2.into_proxy();
        vek::Vec4::new(x, y, v[0], v[1])
    }
}

/// (s1, Vec2<A>, s2) -> Vec4<E>
impl<E, S1, S2, A> const BuildVec4<E> for (S1, vek::Vec2<A>, S2)
where
    S1: const IntoProxy<E, 1> + Scalar + Copy,
    S2: const IntoProxy<E, 1> + Scalar + Copy,
    A: const IntoProxy<E, 2> + Copy + Scalar,
    E: Copy + const ConvertFrom<S1> + const ConvertFrom<S2> + const ConvertFrom<A> + Scalar,
{
    fn build(self) -> vek::Vec4<E> {
        let x = self.0.into_proxy()[0];
        let y = self.1.into_proxy();
        let v = self.2.into_proxy()[0];
        vek::Vec4::new(x, y[0], y[1], v)
    }
}

/// (Vec2<A>, s1, s2) -> Vec4<E>
impl<E, S1, S2, A> const BuildVec4<E> for (vek::Vec2<A>, S1, S2)
where
    S1: const IntoProxy<E, 1> + Scalar + Copy,
    S2: const IntoProxy<E, 1> + Scalar + Copy,
    A: const IntoProxy<E, 2> + Copy + Scalar,
    E: Copy + const ConvertFrom<S1> + const ConvertFrom<S2> + const ConvertFrom<A> + Scalar,
{
    fn build(self) -> vek::Vec4<E> {
        let x = self.0.into_proxy();
        let y = self.1.into_proxy()[0];
        let v = self.2.into_proxy()[0];
        vek::Vec4::new(x[0], x[1], y, v)
    }
}

/// (s1, s2, s3, s4) all scalars -> Vec4<E>
impl<E, S1, S2, S3, S4> const BuildVec4<E> for (S1, S2, S3, S4)
where
    S1: const IntoProxy<E, 1> + Scalar + Copy,
    S2: const IntoProxy<E, 1> + Scalar + Copy,
    S3: const IntoProxy<E, 1> + Scalar + Copy,
    S4: const IntoProxy<E, 1> + Scalar + Copy,
    E: Copy
        + const ConvertFrom<S1>
        + const ConvertFrom<S2>
        + const ConvertFrom<S3>
        + const ConvertFrom<S4>,
{
    fn build(self) -> vek::Vec4<E> {
        let a0 = self.0.into_proxy()[0];
        let a1 = self.1.into_proxy()[0];
        let a2 = self.2.into_proxy()[0];
        let a3 = self.3.into_proxy()[0];
        vek::Vec4::new(a0, a1, a2, a3)
    }
}

/// Scalar -> Vec3<E>
impl<E, S> const BuildVec4<E> for S
where
    S: const IntoProxy<E, 1> + Copy + Scalar,
    E: Copy + const ConvertFrom<S> + Scalar,
{
    fn build(self) -> vek::Vec4<E> {
        let a = self.into_proxy()[0];
        vek::Vec4::new(a, a, a, a)
    }
}

pub const fn vec2<E, A>(args: A) -> vek::Vec2<E>
where
    A: ~const BuildVec2<E>,
{
    args.build()
}

pub const fn vec3<E, A>(args: A) -> vek::Vec3<E>
where
    A: ~const BuildVec3<E>,
{
    args.build()
}

pub const fn vec4<E, A>(args: A) -> vek::Vec4<E>
where
    A: ~const BuildVec4<E>,
{
    args.build()
}

/// Macro, that generates vector creation macros for a given scalar type.
///
/// This macro generates three macros for creating 2-, 3-, and 4-component vectors.
/// Each macro uses your generic helper functions with the provided scalar type.
///
/// # Example
///
/// ```rust
/// # use qvek::generate_vec_macros;
/// // This will generate macros i16vec2!, i16vec3!, and i16vec4!
/// // Note that qvek already generates them for common types, so this is given as an example
/// generate_vec_macros!(i16, i16vec2, i16vec3, i16vec4);
///
/// let a: qvek::vek::Vec3<i16> = i16vec3!(1i16, 2i16, 3i16);
/// ```
#[macro_export]
macro_rules! generate_vec_macros {
    ($scalar:ty, $vec2_macro:ident, $vec3_macro:ident, $vec4_macro:ident) => {
        #[macro_export]
        macro_rules! $vec2_macro {
            ($a:expr) => {
                $crate::vec2::<$scalar, _>($a)
            };
            ($a:expr, $b:expr) => {
                $crate::vec2::<$scalar, _>(($a, $b))
            };
        }
        #[macro_export]
        macro_rules! $vec3_macro {
            ($a:expr) => {
                $crate::vec3::<$scalar, _>($a)
            };
            ($a:expr, $b:expr) => {
                $crate::vec3::<$scalar, _>(($a, $b))
            };
            ($a:expr, $b:expr, $c:expr) => {
                $crate::vec3::<$scalar, _>(($a, $b, $c))
            };
        }
        #[macro_export]
        macro_rules! $vec4_macro {
            ($a:expr) => {
                $crate::vec4::<$scalar, _>($a)
            };
            ($a:expr, $b:expr) => {
                $crate::vec4::<$scalar, _>(($a, $b))
            };
            ($a:expr, $b:expr, $c:expr) => {
                $crate::vec4::<$scalar, _>(($a, $b, $c))
            };
            ($a:expr, $b:expr, $c:expr, $d:expr) => {
                $crate::vec4::<$scalar, _>(($a, $b, $c, $d))
            };
        }
    };
}

// Generate typed macros for common scalar types:
generate_vec_macros!(f32, vec2, vec3, vec4);
generate_vec_macros!(f64, dvec2, dvec3, dvec4);
generate_vec_macros!(i8, i8vec2, i8vec3, i8vec4);
generate_vec_macros!(i16, i16vec2, i16vec3, i16vec4);
generate_vec_macros!(i32, i32vec2, i32vec3, i32vec4);
generate_vec_macros!(i32, ivec2, ivec3, ivec4);
generate_vec_macros!(i64, i64vec2, i64vec3, i64vec4);
generate_vec_macros!(u8, u8vec2, u8vec3, u8vec4);
generate_vec_macros!(u16, u16vec2, u16vec3, u16vec4);
generate_vec_macros!(u32, u32vec2, u32vec3, u32vec4);
generate_vec_macros!(u32, uvec2, uvec3, uvec4);
generate_vec_macros!(u64, u64vec2, u64vec3, u64vec4);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vec2_combinations() {
        assert_eq!(vec2!(1.0f32, 2.0f32), vek::Vec2::<f32>::new(1.0, 2.0));
        assert_eq!(vec2!(1i8, 2i8), vek::Vec2::<f32>::new(1.0, 2.0));
        assert_eq!(
            vec2!(vek::Vec2::new(1i16, 2i16)),
            vek::Vec2::<f32>::new(1.0, 2.0)
        );
        assert_eq!(vec2!(1.0f32, 2i8), vek::Vec2::<f32>::new(1.0, 2.0));
        assert_eq!(vec2!(1, 2), vek::Vec2::<f32>::new(1.0, 2.0));
        assert_eq!(i8vec2!(1i8, 2i8), vek::Vec2::<i8>::new(1, 2));
        assert_eq!(u32vec2!(1u32, 2u32), vek::Vec2::<u32>::new(1, 2));
    }

    #[test]
    fn test_vec3_combinations() {
        assert_eq!(
            vec3!(1.0f32, 2.0f32, 3.0f32),
            vek::Vec3::<f32>::new(1.0, 2.0, 3.0),
        );
        assert_eq!(vec3!(1i8, 2i8, 3i8), vek::Vec3::<f32>::new(1.0, 2.0, 3.0));
        assert_eq!(
            vec3!(vek::Vec2::new(1i16, 2i16), 3i8),
            vek::Vec3::<f32>::new(1.0, 2.0, 3.0),
        );
        assert_eq!(
            vec3!(1.0f32, vek::Vec2::new(2i8, 3i8)),
            vek::Vec3::<f32>::new(1.0, 2.0, 3.0),
        );
        assert_eq!(
            vec3!(1i8, 2.0f32, 3i16),
            vek::Vec3::<f32>::new(1.0, 2.0, 3.0)
        );
        assert_eq!(
            vec3!(vek::Vec3::new(1i8, 2i8, 3i8)),
            vek::Vec3::<f32>::new(1.0, 2.0, 3.0),
        );
        assert_eq!(i32vec3!(1, 2, 3), vek::Vec3::<i32>::new(1, 2, 3));
        assert_eq!(u64vec3!(1, 2, 3), vek::Vec3::<u64>::new(1, 2, 3));
        assert_eq!(dvec3!(1.0, 2.0, 3.0), vek::Vec3::<f64>::new(1.0, 2.0, 3.0));
    }

    #[test]
    fn test_vec4_combinations() {
        assert_eq!(
            vec4!(1.0f32, 2.0f32, 3.0f32, 4.0f32),
            vek::Vec4::<f32>::new(1.0, 2.0, 3.0, 4.0),
        );
        assert_eq!(
            vec4!(1i8, 2i8, 3i8, 4i8),
            vek::Vec4::<f32>::new(1.0, 2.0, 3.0, 4.0),
        );
        assert_eq!(
            vec4!(vek::Vec3::new(1i16, 2i16, 3i16), 4i8),
            vek::Vec4::<f32>::new(1.0, 2.0, 3.0, 4.0),
        );
        assert_eq!(
            vec4!(1.0f32, vek::Vec3::new(2i8, 3i8, 4i8)),
            vek::Vec4::<f32>::new(1.0, 2.0, 3.0, 4.0),
        );
        assert_eq!(
            vec4!(vek::Vec2::new(1i8, 2i8), vek::Vec2::new(3i8, 4i8)),
            vek::Vec4::<f32>::new(1.0, 2.0, 3.0, 4.0),
        );
        assert_eq!(
            vec4!(vek::Vec4::new(1i8, 2i8, 3i8, 4i8)),
            vek::Vec4::<f32>::new(1.0, 2.0, 3.0, 4.0),
        );
        assert_eq!(ivec4!(1, 2, 3, 4), vek::Vec4::<i32>::new(1, 2, 3, 4));
        assert_eq!(u8vec4!(1, 2, 3, 4), vek::Vec4::<u8>::new(1, 2, 3, 4));
    }
}
