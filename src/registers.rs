#![allow(dead_code)]

use std::fmt::Display;

const HIGH_MASK: u16 = 0xFF00;
const LOW_MASK: u16 = 0x00FF;
const ZERO_FLAG: u8 = 0b10000000;
const ADD_FLAG: u8 = 0b01000000;
const HALF_CARRY_FLAG: u8 = 0b00100000;
const CARRY_FLAG: u8 = 0b00010000;

#[derive(Copy, Clone, Debug, Default)]
pub struct Registers {
    af: u16,
    bc: u16,
    de: u16,
    hl: u16,
}

impl Display for Registers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "A: {}", self.get_a())?;
        writeln!(f, "F: {}", self.get_f())?;
        writeln!(f, "B: {}", self.get_b())?;
        writeln!(f, "C: {}", self.get_c())?;
        writeln!(f, "D: {}", self.get_d())?;
        writeln!(f, "E: {}", self.get_e())?;
        writeln!(f, "H: {}", self.get_h())?;
        writeln!(f, "L: {}", self.get_l())?;
        Ok(())
    }
}

impl Registers {
    fn get_high_byte(reg: u16) -> u8 {
        (reg & HIGH_MASK) as u8
    }

    fn get_low_byte(reg: u16) -> u8 {
        (reg & LOW_MASK) as u8
    }

    pub fn get_a(&self) -> u8 {
        Self::get_high_byte(self.af)
    }

    pub fn get_f(&self) -> u8 {
        Self::get_low_byte(self.af)
    }

    pub fn get_b(&self) -> u8 {
        Self::get_high_byte(self.bc)
    }

    pub fn get_c(&self) -> u8 {
        Self::get_low_byte(self.bc)
    }

    pub fn get_d(&self) -> u8 {
        Self::get_high_byte(self.de)
    }

    pub fn get_e(&self) -> u8 {
        Self::get_low_byte(self.de)
    }

    pub fn get_h(&self) -> u8 {
        Self::get_high_byte(self.hl)
    }

    pub fn get_l(&self) -> u8 {
        Self::get_low_byte(self.hl)
    }
}

impl Registers {
    fn modified_high_byte(reg: u16, val: u8) -> u16 {
        let high_byte = (val as u16) << 8;
        let low_byte = reg & LOW_MASK;
        high_byte | low_byte
    }

    fn modified_low_byte(reg: u16, val: u8) -> u16 {
        let high_byte = reg & HIGH_MASK;
        let low_byte: u16 = val.into();
        high_byte | low_byte
    }

    pub fn set_a(&mut self, val: u8) {
        self.af = Self::modified_high_byte(self.af, val);
    }

    pub fn set_f(&mut self, val: u8) {
        self.af = Self::modified_low_byte(self.af, val);
    }

    pub fn set_b(&mut self, val: u8) {
        self.bc = Self::modified_high_byte(self.bc, val);
    }

    pub fn set_c(&mut self, val: u8) {
        self.bc = Self::modified_low_byte(self.bc, val);
    }

    pub fn set_d(&mut self, val: u8) {
        self.de = Self::modified_high_byte(self.de, val);
    }

    pub fn set_e(&mut self, val: u8) {
        self.de = Self::modified_low_byte(self.de, val);
    }

    pub fn set_h(&mut self, val: u8) {
        self.hl = Self::modified_high_byte(self.hl, val);
    }

    pub fn set_l(&mut self, val: u8) {
        self.hl = Self::modified_low_byte(self.hl, val);
    }
}

impl Registers {
    pub fn zero_flag_set(&self) -> bool {
        self.get_f() & ZERO_FLAG != 0
    }

    pub fn add_flag_set(&self) -> bool {
        self.get_f() & ADD_FLAG != 0
    }

    pub fn half_carry_flag_set(&self) -> bool {
        self.get_f() & HALF_CARRY_FLAG != 0
    }

    pub fn carry_flag_set(&self) -> bool {
        self.get_f() & CARRY_FLAG != 0
    }
}

#[cfg(test)]
pub mod test {
    use super::*;

    #[test]
    fn flag_register() {
        let mut registers = Registers::default();
        assert!(!registers.zero_flag_set());
        assert!(!registers.add_flag_set());
        assert!(!registers.half_carry_flag_set());
        assert!(!registers.carry_flag_set());

        registers.set_f(ZERO_FLAG);
        assert!(registers.zero_flag_set());

        registers.set_f(ADD_FLAG);
        assert!(registers.add_flag_set());

        registers.set_f(HALF_CARRY_FLAG);
        assert!(registers.half_carry_flag_set());

        registers.set_f(CARRY_FLAG);
        assert!(registers.carry_flag_set());

        registers.set_f(0);
        assert!(!registers.zero_flag_set());
        assert!(!registers.add_flag_set());
        assert!(!registers.half_carry_flag_set());
        assert!(!registers.carry_flag_set());
    }
}
