#![allow(dead_code)]

const HIGH_MASK: u16 = 0xFF00;
const LOW_MASK: u16 = 0x00FF;
pub const ZERO_FLAG: u8 = 0b10000000;
pub const SUBTRACT_FLAG: u8 = 0b01000000;
pub const HALF_CARRY_FLAG: u8 = 0b00100000;
pub const CARRY_FLAG: u8 = 0b00010000;

// TODO:
#[derive(Copy, Clone, Debug, Default)]
pub struct FlagRegister {
    pub Z: bool,
    pub N: bool,
    pub H: bool,
    pub C: bool,
}

#[derive(Copy, Clone, Debug)]
pub struct Registers {
    pub a: u8,
    pub f: FlagRegister,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub h: u8,
    pub l: u8,
    pub sp: u16,
    pub pc: u16,
}

// All values taken from Section 2.7.1 of the Gameboy CPU manual
impl Default for Registers {
    fn default() -> Self {
        Self {
            // TODO: this depends on if it's a GB/GBP/GBC
            a: 0x00,
            // FIX: incorrect for startup
            f: FlagRegister::default(),
            b: 0x00,
            c: 0x13,
            d: 0x00,
            e: 0xD8,
            h: 0x01,
            l: 0x4D,
            sp: 0xFFFE,
            pc: 0,
        }
    }
}

impl Registers {
    pub fn hl(&self) -> u16 {
        ((self.h as u16) << 8) | (self.l as u16)
    }

    pub fn clear_flags(&mut self) {
        self.f.Z = false;
        self.f.N = false;
        self.f.H = false;
        self.f.C = false;
    }

    pub fn zero_flag_set(&self) -> bool {
        self.f.Z
    }

    pub fn subtract_flag_set(&self) -> bool {
        self.f.N
    }

    pub fn half_carry_flag_set(&self) -> bool {
        self.f.H
    }

    pub fn carry_flag_set(&self) -> bool {
        self.f.C
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

        registers.f.Z = true;
        assert!(registers.zero_flag_set());

        registers.f.N = true;
        assert!(registers.subtract_flag_set());

        registers.f.H = true;
        assert!(registers.half_carry_flag_set());

        registers.f.C = true;
        assert!(registers.carry_flag_set());

        registers.clear_flags();
        assert!(!registers.zero_flag_set());
        assert!(!registers.subtract_flag_set());
        assert!(!registers.half_carry_flag_set());
        assert!(!registers.carry_flag_set());
    }
}
