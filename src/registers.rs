#![allow(dead_code)]

const HIGH_MASK: u16 = 0xFF00;
const LOW_MASK: u16 = 0x00FF;
const ZERO_FLAG: u8 = 0b10000000;
const ADD_FLAG: u8 = 0b01000000;
const HALF_CARRY_FLAG: u8 = 0b00100000;
const CARRY_FLAG: u8 = 0b00010000;

#[derive(Copy, Clone, Debug, Default)]
pub struct Reg16 {
    hi: u8,
    lo: u8,
}

impl Reg16 {
    pub fn get_hi(&self) -> u8 {
        self.hi
    }

    pub fn get_lo(&self) -> u8 {
        self.lo
    }

    pub fn set_hi(&mut self, val: u8) {
        self.hi = val;
    }

    pub fn set_lo(&mut self, val: u8) {
        self.lo = val;
    }
}

impl Into<u16> for Reg16 {
    fn into(self) -> u16 {
        ((self.hi as u16) << 8) | (self.lo as u16)
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Register {
    A,
    F,
    B,
    C,
    D,
    E,
    H,
    L,
}

#[derive(Copy, Clone, Debug, Default)]
pub struct Registers {
    af: Reg16,
    bc: Reg16,
    de: Reg16,
    hl: Reg16,
}

impl Registers {
    pub fn get_reg(&self, reg: Register) -> u8 {
        match reg {
            Register::A => self.af.get_hi(),
            Register::F => self.af.get_lo(),
            Register::B => self.bc.get_hi(),
            Register::C => self.bc.get_lo(),
            Register::D => self.de.get_hi(),
            Register::E => self.de.get_lo(),
            Register::H => self.hl.get_hi(),
            Register::L => self.hl.get_lo(),
        }
    }
}

impl Registers {
    fn set_reg(&mut self, reg: Register, val: u8) {
        match reg {
            Register::A => self.af.set_hi(val),
            Register::F => self.af.set_lo(val),
            Register::B => self.bc.set_hi(val),
            Register::C => self.bc.set_lo(val),
            Register::D => self.de.set_hi(val),
            Register::E => self.de.set_lo(val),
            Register::H => self.hl.set_hi(val),
            Register::L => self.hl.set_lo(val),
        }
    }
}

impl Registers {
    pub fn zero_flag_set(&self) -> bool {
        self.get_reg(Register::F) & ZERO_FLAG != 0
    }

    pub fn add_flag_set(&self) -> bool {
        self.get_reg(Register::F) & ADD_FLAG != 0
    }

    pub fn half_carry_flag_set(&self) -> bool {
        self.get_reg(Register::F) & HALF_CARRY_FLAG != 0
    }

    pub fn carry_flag_set(&self) -> bool {
        self.get_reg(Register::F) & CARRY_FLAG != 0
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

        registers.set_reg(Register::F, ZERO_FLAG);
        assert!(registers.zero_flag_set());

        registers.set_reg(Register::F, ADD_FLAG);
        assert!(registers.add_flag_set());

        registers.set_reg(Register::F, HALF_CARRY_FLAG);
        assert!(registers.half_carry_flag_set());

        registers.set_reg(Register::F, CARRY_FLAG);
        assert!(registers.carry_flag_set());

        registers.set_reg(Register::F, 0);
        assert!(!registers.zero_flag_set());
        assert!(!registers.add_flag_set());
        assert!(!registers.half_carry_flag_set());
        assert!(!registers.carry_flag_set());
    }
}
