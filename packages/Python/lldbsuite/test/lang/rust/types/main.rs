#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_assignments)]

fn do_nothing() { }

fn main() {
    let vbool: bool = true;

    let vi8: i8 = -23;
    let vu8: u8 = 23;
    let vi16: i16 = -2323;
    let vu16: u16 = 2323;
    let vi32: i32 = -232323;
    let vu32: u32 = 232323;
    let vi64: i64 = -23232323;
    let vu64: u64 = 23232323;

    let visize: isize = -23232323;
    let vusize: usize = 23232323;

    let vf32: f32 = 5.25;
    let vf64: f64 = 7.5;

    let empty = ();

    do_nothing();               // breakpoint
}
