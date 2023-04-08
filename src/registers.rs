#![allow(dead_code)]

const HIGH_MASK: u16 = 0xFF00;
const LOW_MASK: u16 = 0x00FF;
pub const ZERO_FLAG: u8 = 0b10000000;
pub const SUBTRACT_FLAG: u8 = 0b01000000;
pub const HALF_CARRY_FLAG: u8 = 0b00100000;
pub const CARRY_FLAG: u8 = 0b00010000;

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

impl From<u16> for Reg16 {
    fn from(value: u16) -> Self {
        Reg16 {
            hi: ((value & HIGH_MASK) as u8),
            lo: ((value & LOW_MASK) as u8),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
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

#[derive(Copy, Clone, Debug)]
pub struct Registers {
    pub af: Reg16,
    pub bc: Reg16,
    pub de: Reg16,
    pub hl: Reg16,
    pub sp: u16,
    pub pc: u16,
}

// All values taken from Section 2.7.1 of the Gameboy CPU manual
impl Default for Registers {
    fn default() -> Self {
        Self {
            // TODO: this depends on if it's a GB/GBP/GBC
            af: Reg16::from(0x0001),
            bc: Reg16::from(0x0013),
            de: Reg16::from(0x00D8),
            hl: Reg16::from(0x014D),
            sp: 0xFFFE,
            pc: 0,
        }
    }
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

    pub fn set_reg(&mut self, reg: Register, val: u8) {
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
    pub fn set_flag(&mut self, flag: u8) {
        let mut v = self.get_reg(Register::F);
        v |= flag;
        self.set_reg(Register::F, v);
    }

    pub fn zero_flag_set(&self) -> bool {
        self.get_reg(Register::F) & ZERO_FLAG != 0
    }

    pub fn subtract_flag_set(&self) -> bool {
        self.get_reg(Register::F) & SUBTRACT_FLAG != 0
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
        assert!(!registers.subtract_flag_set());
        assert!(!registers.half_carry_flag_set());
        assert!(!registers.carry_flag_set());

        registers.set_reg(Register::F, ZERO_FLAG);
        assert!(registers.zero_flag_set());

        registers.set_reg(Register::F, SUBTRACT_FLAG);
        assert!(registers.subtract_flag_set());

        registers.set_reg(Register::F, HALF_CARRY_FLAG);
        assert!(registers.half_carry_flag_set());

        registers.set_reg(Register::F, CARRY_FLAG);
        assert!(registers.carry_flag_set());

        registers.set_reg(Register::F, 0);
        assert!(!registers.zero_flag_set());
        assert!(!registers.subtract_flag_set());
        assert!(!registers.half_carry_flag_set());
        assert!(!registers.carry_flag_set());
    }
}
