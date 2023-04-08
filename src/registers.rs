#![allow(dead_code)]

const HIGH_MASK: u16 = 0xFF00;
const LOW_MASK: u16 = 0x00FF;
const ZERO_FLAG: u8 = 0b10000000;
const ADD_FLAG: u8 = 0b01000000;
const HALF_CARRY_FLAG: u8 = 0b00100000;
const CARRY_FLAG: u8 = 0b00010000;

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
    af: u16,
    bc: u16,
    de: u16,
    hl: u16,
}

impl Registers {
    fn get_high_byte(reg: u16) -> u8 {
        (reg & HIGH_MASK) as u8
    }

    fn get_low_byte(reg: u16) -> u8 {
        (reg & LOW_MASK) as u8
    }

    pub fn get_reg(&self, reg: Register) -> u8 {
        match reg {
            Register::A => Self::get_high_byte(self.af),
            Register::F => Self::get_low_byte(self.af),
            Register::B => Self::get_high_byte(self.bc),
            Register::C => Self::get_low_byte(self.bc),
            Register::D => Self::get_high_byte(self.de),
            Register::E => Self::get_low_byte(self.de),
            Register::H => Self::get_high_byte(self.hl),
            Register::L => Self::get_low_byte(self.hl),
        }
    }
}

impl Registers {
    fn set_reg(&mut self, reg: Register, val: u8) {
        match reg {
            Register::A => self.af = Self::set_high_register(self.af, val),
            Register::F => self.af = Self::set_low_register(self.af, val),
            Register::B => self.bc = Self::set_high_register(self.bc, val),
            Register::C => self.bc = Self::set_low_register(self.bc, val),
            Register::D => self.de = Self::set_high_register(self.de, val),
            Register::E => self.de = Self::set_low_register(self.de, val),
            Register::H => self.hl = Self::set_high_register(self.hl, val),
            Register::L => self.hl = Self::set_low_register(self.hl, val),
        }
    }

    fn set_high_register(reg: u16, val: u8) -> u16 {
        ((val as u16) << 8) | (reg & LOW_MASK)
    }

    fn set_low_register(reg: u16, val: u8) -> u16 {
        (reg & HIGH_MASK) | (val as u16)
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
