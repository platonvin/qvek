use criterion::{Criterion, black_box, criterion_group, criterion_main};
use qvek::*;

fn direct_conversion_i32_f32(c: &mut Criterion) {
    c.bench_function("direct_conversion_i32_f32", |b| {
        b.iter(|| vek::Vec3::new(1i32 as f32, 2i32 as f32, 3i32 as f32))
    });
}

fn qvek_conversion_i32_f32(c: &mut Criterion) {
    c.bench_function("qvek_conversion_i32_f32", |b| {
        b.iter(|| vec3!(1i32, 2i32, 3i32))
    });
}

fn direct_conversion_i8_f32(c: &mut Criterion) {
    c.bench_function("direct_conversion_i8_f32", |b| {
        b.iter(|| vek::Vec3::new(1i8 as f32, 2i8 as f32, 3i8 as f32))
    });
}

fn qvek_conversion_i8_f32(c: &mut Criterion) {
    c.bench_function("qvek_conversion_i8_f32", |b| {
        b.iter(|| vec3!(1i8, 2i8, 3i8))
    });
}

fn direct_conversion_vec2_f32(c: &mut Criterion) {
    c.bench_function("direct_conversion_vec2_f32", |b| {
        b.iter(|| {
            let v = vek::Vec2::new(1i16 as f32, 2i16 as f32);
            vek::Vec3::new(v.x, v.y, 3i16 as f32)
        })
    });
}

fn qvek_conversion_vec2_f32(c: &mut Criterion) {
    c.bench_function("qvek_conversion_vec2_f32", |b| {
        b.iter(|| vec3!(vek::Vec2::new(1i16, 2i16), 3i16))
    });
}

fn direct_conversion_f64_f32(c: &mut Criterion) {
    c.bench_function("direct_conversion_f64_f32", |b| {
        b.iter(|| vek::Vec3::new(1.0f64 as f32, 2.0f64 as f32, 3.0f64 as f32))
    });
}

fn qvek_conversion_f64_f32(c: &mut Criterion) {
    c.bench_function("qvek_conversion_f64_f32", |b| {
        b.iter(|| vec3!(1.0f64, 2.0f64, 3.0f64))
    });
}

fn qvek_vec4_conversion_mixed(c: &mut Criterion) {
    c.bench_function("qvek_vec4_conversion_mixed", |b| {
        b.iter(|| vec4!(1i8, vek::Vec2::<i16>::new(2, 3), 4f32))
    });
}

fn direct_vec4_conversion_mixed(c: &mut Criterion) {
    c.bench_function("direct_vec4_conversion_mixed", |b| {
        b.iter(|| {
            let v2 = vek::Vec2::new(2i16 as f32, 3i32 as f32);
            vek::Vec4::new(1i8 as f32, v2.x, v2.y, 4f32)
        })
    });
}

fn qvek_vec2_black_box(c: &mut Criterion) {
    c.bench_function("qvek_vec2_black_box", |b| {
        b.iter(|| black_box(vec2!(black_box(1i16), black_box(2i16))))
    });
}

fn direct_vec2_black_box(c: &mut Criterion) {
    c.bench_function("direct_vec2_black_box", |b| {
        b.iter(|| {
            black_box(vek::Vec2::new(
                black_box(1i16 as f32),
                black_box(2i16 as f32),
            ))
        })
    });
}

criterion_group!(
    benches_i32,
    direct_conversion_i32_f32,
    qvek_conversion_i32_f32
);

criterion_group!(benches_i8, direct_conversion_i8_f32, qvek_conversion_i8_f32);

criterion_group!(
    benches_vec2,
    direct_conversion_vec2_f32,
    qvek_conversion_vec2_f32,
    qvek_vec2_black_box,
    direct_vec2_black_box
);

criterion_group!(
    benches_f64,
    direct_conversion_f64_f32,
    qvek_conversion_f64_f32
);

criterion_group!(
    benches_vec4,
    qvek_vec4_conversion_mixed,
    direct_vec4_conversion_mixed
);

criterion_main!(
    benches_i32,
    benches_i8,
    benches_vec2,
    benches_f64,
    benches_vec4
);
