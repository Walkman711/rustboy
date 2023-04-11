#![allow(dead_code)]

use std::fmt::Display;

const HIGH_MASK: u16 = 0xFF00;
const LOW_MASK: u16 = 0x00FF;

#[derive(Copy, Clone, Debug, PartialEq)]
#[allow(non_snake_case)]
pub enum Flags {
    Z,
    N,
    H,
    C,
}

impl Into<u8> for Flags {
    fn into(self) -> u8 {
        match self {
            Flags::Z => 1 << 7,
            Flags::N => 1 << 6,
            Flags::H => 1 << 5,
            Flags::C => 1 << 4,
        }
    }
}

// TODO:
#[derive(Copy, Clone, Debug)]
#[allow(non_snake_case)]
pub struct FlagRegister {
    Z: bool,
    N: bool,
    H: bool,
    C: bool,
}

// FIX: is this right?
impl Default for FlagRegister {
    fn default() -> Self {
        Self {
            Z: true,
            N: false,
            H: false,
            C: false,
        }
    }
}

#[allow(non_snake_case)]
impl From<u8> for FlagRegister {
    fn from(value: u8) -> Self {
        let Z: bool = (value & 0b10000000) != 0;
        let N: bool = (value & 0b01000000) != 0;
        let H: bool = (value & 0b00100000) != 0;
        let C: bool = (value & 0b00010000) != 0;
        Self { Z, N, H, C }
    }
}

#[allow(non_snake_case)]
// TODO: this is definitely messier than it should be
impl Into<u8> for FlagRegister {
    fn into(self) -> u8 {
        let mut res = 0;
        if self.Z {
            let z: u8 = Flags::Z.into();
            res |= z;
        }
        if self.N {
            let n: u8 = Flags::N.into();
            res |= n;
        }
        if self.H {
            let h: u8 = Flags::H.into();
            res |= h;
        }
        if self.C {
            let c: u8 = Flags::C.into();
            res |= c;
        }

        res
    }
}

impl Display for FlagRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "==========Flags==========")?;
        writeln!(f, "Zero Flag:       {}", self.Z)?;
        writeln!(f, "Subtract Flag:   {}", self.N)?;
        writeln!(f, "Half-Carry Flag: {}", self.N)?;
        writeln!(f, "Carry Flag:      {}", self.C)?;
        Ok(())
    }
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

impl Display for Registers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "==========Registers==========")?;
        writeln!(f, "Register A:  {:#04x}", self.a)?;
        writeln!(f, "Register B:  {:#04x}", self.b)?;
        writeln!(f, "Register C:  {:#04x}", self.c)?;
        writeln!(f, "Register D:  {:#04x}", self.d)?;
        writeln!(f, "Register E:  {:#04x}", self.e)?;
        writeln!(f, "Register H:  {:#04x}", self.h)?;
        writeln!(f, "Register L:  {:#04x}", self.l)?;
        writeln!(f, "Stack Ptr:   {:#04x}", self.sp)?;
        writeln!(f, "Program Ctr: {:#04x}", self.pc)?;
        writeln!(f, "{}", self.f)?;
        Ok(())
    }
}

// All values taken from Section 2.7.1 of the Gameboy CPU manual
impl Default for Registers {
    fn default() -> Self {
        Self {
            // Using DMG0
            // TODO: this depends on if it's a GB/GBP/GBC
            a: 0x01,
            // FIX: incorrect for startup
            f: FlagRegister::from(0xB0),
            b: 0x00,
            c: 0x13,
            d: 0x00,
            e: 0xD8,
            h: 0x01,
            l: 0x4D,
            sp: 0xFFFE,
            // 3.2.3
            pc: 0x0100,
        }
    }
}

impl Registers {
    fn to_16_bit(hi: u8, lo: u8) -> u16 {
        ((hi as u16) << 8) | (lo as u16)
    }

    pub fn af(&self) -> u16 {
        let f: u8 = self.f.into();
        Self::to_16_bit(self.a, f)
    }

    pub fn bc(&self) -> u16 {
        Self::to_16_bit(self.b, self.c)
    }

    pub fn de(&self) -> u16 {
        Self::to_16_bit(self.d, self.e)
    }

    pub fn hl(&self) -> u16 {
        Self::to_16_bit(self.h, self.l)
    }

    fn split_16_bit(val: u16) -> (u8, u8) {
        let hi = ((val & 0xFF00) >> 8) as u8;
        let lo = (val & 0x00FF) as u8;
        (hi, lo)
    }

    pub fn set_af(&mut self, val: u16) {
        let (hi, lo) = Self::split_16_bit(val);
        self.a = hi;
        self.f = FlagRegister::from(lo);
    }

    pub fn set_bc(&mut self, val: u16) {
        let (hi, lo) = Self::split_16_bit(val);
        self.b = hi;
        self.c = lo;
    }

    pub fn set_de(&mut self, val: u16) {
        let (hi, lo) = Self::split_16_bit(val);
        self.d = hi;
        self.e = lo;
    }

    pub fn set_hl(&mut self, val: u16) {
        let (hi, lo) = Self::split_16_bit(val);
        self.h = hi;
        self.l = lo;
    }

    pub fn flag(&mut self, flag: Flags, val: bool) {
        match flag {
            Flags::Z => self.f.Z = val,
            Flags::N => self.f.N = val,
            Flags::H => self.f.H = val,
            Flags::C => self.f.C = val,
        }
    }

    pub fn check_flag(&mut self, flag: Flags) -> bool {
        match flag {
            Flags::Z => self.f.Z,
            Flags::N => self.f.N,
            Flags::H => self.f.H,
            Flags::C => self.f.C,
        }
    }

    pub fn clear_flags(&mut self) {
        self.f.Z = false;
        self.f.N = false;
        self.f.H = false;
        self.f.C = false;
    }
}

#[cfg(test)]
pub mod test {
    use super::*;

    #[test]
    fn flag_register() {
        // let mut registers = Registers::default();
        // assert!(!registers.zero_flag_set());
        // assert!(!registers.subtract_flag_set());
        // assert!(!registers.half_carry_flag_set());
        // assert!(!registers.carry_flag_set());

        // registers.f.Z = true;
        // assert!(registers.zero_flag_set());

        // registers.f.N = true;
        // assert!(registers.subtract_flag_set());

        // registers.f.H = true;
        // assert!(registers.half_carry_flag_set());

        // registers.f.C = true;
        // assert!(registers.carry_flag_set());

        // registers.clear_flags();
        // assert!(!registers.zero_flag_set());
        // assert!(!registers.subtract_flag_set());
        // assert!(!registers.half_carry_flag_set());
        // assert!(!registers.carry_flag_set());
    }
}
