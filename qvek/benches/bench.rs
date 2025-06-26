use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use qvek::*;
use std::hint::black_box;
use vek::*;

fn direct_i32_to_f32(v: Vec3<i32>) -> Vec3<f32> {
    Vec3::new(v.x as f32, v.y as f32, v.z as f32)
}

fn qvek_i32_to_f32(v: Vec3<i32>) -> Vec3<f32> {
    vec3!(v)
}

fn direct_i8_to_f32(v: Vec3<i8>) -> Vec3<f32> {
    Vec3::new(v.x as f32, v.y as f32, v.z as f32)
}

fn qvek_i8_to_f32(v: Vec3<i8>) -> Vec3<f32> {
    vec3!(v)
}

fn direct_vec2_to_vec3(v2: Vec2<i16>, scalar: i16) -> Vec3<f32> {
    Vec3::new(v2.x as f32, v2.y as f32, scalar as f32)
}

fn qvek_vec2_to_vec3(v2: Vec2<i16>, scalar: i16) -> Vec3<f32> {
    vec3!(v2, scalar)
}

fn direct_f64_to_f32(v: Vec3<f64>) -> Vec3<f32> {
    Vec3::new(v.x as f32, v.y as f32, v.z as f32)
}

fn qvek_f64_to_f32(v: Vec3<f64>) -> Vec3<f32> {
    vec3!(v.xy(), v.z)
}

fn direct_mixed_vec4(a: i8, b2: Vec2<i16>, c: f32) -> Vec4<f32> {
    Vec4::new(a as f32, b2.x as f32, b2.y as f32, c)
}

fn qvek_mixed_vec4(a: i8, b2: Vec2<i16>, c: f32) -> Vec4<f32> {
    vec4!(a, b2, c)
}

pub fn bench_i32_to_f32(c: &mut Criterion) {
    let mut group = c.benchmark_group("i32 -> f32");
    let v_in = black_box(Vec3::new(1i32, 2, 3));

    group.bench_function("direct", |b| {
        b.iter(|| black_box(direct_i32_to_f32(black_box(v_in))))
    });

    group.bench_function("qvek", |b| {
        b.iter(|| black_box(qvek_i32_to_f32(black_box(v_in))))
    });

    group.finish();
}

pub fn bench_i8_to_f32(c: &mut Criterion) {
    let mut group = c.benchmark_group("i8 -> f32");
    let v_in = black_box(Vec3::new(1i8, 2, 3));

    group.bench_function("direct", |b| {
        b.iter(|| black_box(direct_i8_to_f32(black_box(v_in))))
    });

    group.bench_function("qvek", |b| {
        b.iter(|| black_box(qvek_i8_to_f32(black_box(v_in))))
    });

    group.finish();
}

pub fn bench_vec2_to_vec3(c: &mut Criterion) {
    let mut group = c.benchmark_group("Vec2<i16> + i16 -> Vec3<f32>");
    let base_v2 = black_box(Vec2::new(1i16, 2));
    let scalar = black_box(3i16);

    group.bench_function("direct", |b| {
        b.iter(|| black_box(direct_vec2_to_vec3(black_box(base_v2), black_box(scalar))))
    });

    group.bench_function("qvek", |b| {
        b.iter(|| black_box(qvek_vec2_to_vec3(black_box(base_v2), black_box(scalar))))
    });

    group.finish();
}

pub fn bench_f64_to_f32(c: &mut Criterion) {
    let mut group = c.benchmark_group("f64 -> f32");
    let v_in = black_box(Vec3::new(1.0f64, 2.0, 3.0));

    group.bench_function("direct", |b| {
        b.iter(|| black_box(direct_f64_to_f32(black_box(v_in))))
    });

    group.bench_function("qvek", |b| {
        b.iter(|| black_box(qvek_f64_to_f32(black_box(v_in))))
    });

    group.finish();
}

pub fn bench_mixed_vec4(c: &mut Criterion) {
    let mut group = c.benchmark_group("mixed Vec4");
    let a = black_box(1i8);
    let b2 = black_box(Vec2::new(2i16, 3));
    let c_scalar = black_box(4f32);

    group.bench_function("direct", |b| {
        b.iter(|| {
            black_box(direct_mixed_vec4(
                black_box(a),
                black_box(b2),
                black_box(c_scalar),
            ))
        })
    });

    group.bench_function("qvek", |b| {
        b.iter(|| {
            black_box(qvek_mixed_vec4(
                black_box(a),
                black_box(b2),
                black_box(c_scalar),
            ))
        })
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_i32_to_f32,
    bench_i8_to_f32,
    bench_vec2_to_vec3,
    bench_f64_to_f32,
    bench_mixed_vec4
);
criterion_main!(benches);
