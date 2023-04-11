// Taken from gist.github/com/meganesu
pub fn half_carry(a: u8, b: u8) -> bool {
    // 1) mask a and b to only look at bits 0-3.
    // 2) add them together
    // 3) check if the fourth bit is set
    (((a & 0xF) + (b & 0xF)) & (1 << 4)) == (1 << 4)
}

pub fn half_carry_sub(a: u8, b: u8) -> bool {
    // 1) mask a and b to only look at bits 0-3.
    // 2) add them together
    // 3) check if the fourth bit is set
    (((a & 0xF) - (b & 0xF)) & (1 << 4)) == (1 << 4)
}

// TODO: test
pub fn half_carry_16(a: u16, b: u16) -> bool {
    (((a & 0x0FFF) + (b & 0x0FFF)) & (1 << 12)) == (1 << 12)
}

pub fn half_carry_16_sub(a: u16, b: u16) -> bool {
    (((a & 0x0F00) - (b & 0x0F00)) & (1 << 12)) == (1 << 12)
}

pub fn carry(a: u8, b: u8) -> bool {
    let wrapped = a.wrapping_add(b);
    let unwrapped: u16 = (a as u16) + (b as u16);
    (wrapped as u16) < unwrapped
}

pub fn carry_sub(a: u8, b: u8) -> bool {
    a < b
    // let wrapped = a.wrapping_sub(b);
    // let unwrapped: u16 = (a as u16) - (b as u16);
    // (wrapped as u16) > unwrapped
}

pub fn carry_16(a: u16, b: u16) -> bool {
    let wrapped = a.wrapping_add(b);
    let unwrapped: u32 = (a as u32) + (b as u32);
    (wrapped as u32) < unwrapped
}
