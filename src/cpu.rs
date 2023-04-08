use crate::registers::*;
pub struct CPU {
    registers: Registers,
    clock: u16,
}

impl CPU {
    fn ld(&mut self, dst: Register, src: Register) {
        let src_val = self.registers.get_reg(src);
        self.registers.set_reg(dst, src_val);
    }
}

impl CPU {
    fn fetch_byte(&self) -> u8 {
        todo!()
    }
    fn fetch_word(&self) -> u32 {
        todo!()
    }

    // Returns cycles elapsed
    fn call(&mut self) -> u32 {
        let opcode = self.fetch_byte();
        match opcode {
            0x00 => unimplemented!("Opcode 0x00"),
            0x01 => unimplemented!("Opcode 0x01"),
            0x02 => unimplemented!("Opcode 0x02"),
            0x03 => unimplemented!("Opcode 0x03"),
            0x04 => unimplemented!("Opcode 0x04"),
            0x05 => unimplemented!("Opcode 0x05"),
            0x06 => unimplemented!("Opcode 0x06"),
            0x07 => unimplemented!("Opcode 0x07"),
            0x08 => unimplemented!("Opcode 0x08"),
            0x09 => unimplemented!("Opcode 0x09"),
            0x0A => unimplemented!("Opcode 0x0A"),
            0x0B => unimplemented!("Opcode 0x0B"),
            0x0C => unimplemented!("Opcode 0x0C"),
            0x0D => unimplemented!("Opcode 0x0D"),
            0x0E => unimplemented!("Opcode 0x0E"),
            0x0F => unimplemented!("Opcode 0x0F"),
            0x10 => unimplemented!("Opcode 0x10"),
            0x11 => unimplemented!("Opcode 0x11"),
            0x12 => unimplemented!("Opcode 0x12"),
            0x13 => unimplemented!("Opcode 0x13"),
            0x14 => unimplemented!("Opcode 0x14"),
            0x15 => unimplemented!("Opcode 0x15"),
            0x16 => unimplemented!("Opcode 0x16"),
            0x17 => unimplemented!("Opcode 0x17"),
            0x18 => unimplemented!("Opcode 0x18"),
            0x19 => unimplemented!("Opcode 0x19"),
            0x1A => unimplemented!("Opcode 0x1A"),
            0x1B => unimplemented!("Opcode 0x1B"),
            0x1C => unimplemented!("Opcode 0x1C"),
            0x1D => unimplemented!("Opcode 0x1D"),
            0x1E => unimplemented!("Opcode 0x1E"),
            0x1F => unimplemented!("Opcode 0x1F"),
            0x20 => unimplemented!("Opcode 0x20"),
            0x21 => unimplemented!("Opcode 0x21"),
            0x22 => unimplemented!("Opcode 0x22"),
            0x23 => unimplemented!("Opcode 0x23"),
            0x24 => unimplemented!("Opcode 0x24"),
            0x25 => unimplemented!("Opcode 0x25"),
            0x26 => unimplemented!("Opcode 0x26"),
            0x27 => unimplemented!("Opcode 0x27"),
            0x28 => unimplemented!("Opcode 0x28"),
            0x29 => unimplemented!("Opcode 0x29"),
            0x2A => unimplemented!("Opcode 0x2A"),
            0x2B => unimplemented!("Opcode 0x2B"),
            0x2C => unimplemented!("Opcode 0x2C"),
            0x2D => unimplemented!("Opcode 0x2D"),
            0x2E => unimplemented!("Opcode 0x2E"),
            0x2F => unimplemented!("Opcode 0x2F"),
            0x30 => unimplemented!("Opcode 0x30"),
            0x31 => unimplemented!("Opcode 0x31"),
            0x32 => unimplemented!("Opcode 0x32"),
            0x33 => unimplemented!("Opcode 0x33"),
            0x34 => unimplemented!("Opcode 0x34"),
            0x35 => unimplemented!("Opcode 0x35"),
            0x36 => unimplemented!("Opcode 0x36"),
            0x37 => unimplemented!("Opcode 0x37"),
            0x38 => unimplemented!("Opcode 0x38"),
            0x39 => unimplemented!("Opcode 0x39"),
            0x3A => unimplemented!("Opcode 0x3A"),
            0x3B => unimplemented!("Opcode 0x3B"),
            0x3C => unimplemented!("Opcode 0x3C"),
            0x3D => unimplemented!("Opcode 0x3D"),
            0x3E => unimplemented!("Opcode 0x3E"),
            0x3F => unimplemented!("Opcode 0x3F"),
            0x40 => {
                self.ld(Register::B, Register::B);
                4
            }
            0x41 => {
                self.ld(Register::B, Register::C);
                4
            }
            0x42 => {
                self.ld(Register::B, Register::D);
                4
            }
            0x43 => {
                self.ld(Register::B, Register::E);
                4
            }
            0x44 => {
                self.ld(Register::B, Register::H);
                4
            }
            0x45 => {
                self.ld(Register::B, Register::L);
                4
            }
            0x46 => unimplemented!("Opcode 0x46"),
            0x47 => {
                self.ld(Register::B, Register::A);
                4
            }
            0x48 => {
                self.ld(Register::C, Register::B);
                4
            }
            0x49 => {
                self.ld(Register::C, Register::C);
                4
            }
            0x4A => {
                self.ld(Register::C, Register::D);
                4
            }
            0x4B => {
                self.ld(Register::C, Register::E);
                4
            }
            0x4C => {
                self.ld(Register::C, Register::H);
                4
            }
            0x4D => {
                self.ld(Register::C, Register::L);
                4
            }
            0x4E => unimplemented!("Opcode 0x4E"),
            0x4F => {
                self.ld(Register::C, Register::A);
                4
            }
            0x50 => {
                self.ld(Register::D, Register::B);
                4
            }
            0x51 => {
                self.ld(Register::D, Register::C);
                4
            }
            0x52 => {
                self.ld(Register::D, Register::D);
                4
            }
            0x53 => {
                self.ld(Register::D, Register::E);
                4
            }
            0x54 => {
                self.ld(Register::D, Register::H);
                4
            }
            0x55 => {
                self.ld(Register::D, Register::L);
                4
            }
            0x56 => unimplemented!("Opcode 0x56"),
            0x57 => {
                self.ld(Register::D, Register::A);
                4
            }
            0x58 => {
                self.ld(Register::E, Register::B);
                4
            }
            0x59 => {
                self.ld(Register::E, Register::C);
                4
            }
            0x5A => {
                self.ld(Register::E, Register::D);
                4
            }
            0x5B => {
                self.ld(Register::E, Register::E);
                4
            }
            0x5C => {
                self.ld(Register::E, Register::H);
                4
            }
            0x5D => {
                self.ld(Register::E, Register::L);
                4
            }
            0x5E => unimplemented!("Opcode 0x5E"),
            0x5F => {
                self.ld(Register::E, Register::A);
                4
            }
            0x60 => {
                self.ld(Register::H, Register::B);
                4
            }
            0x61 => {
                self.ld(Register::H, Register::C);
                4
            }
            0x62 => {
                self.ld(Register::H, Register::D);
                4
            }
            0x63 => {
                self.ld(Register::H, Register::E);
                4
            }
            0x64 => {
                self.ld(Register::H, Register::H);
                4
            }
            0x65 => {
                self.ld(Register::H, Register::L);
                4
            }
            0x66 => unimplemented!("Opcode 0x66"),
            0x67 => {
                self.ld(Register::H, Register::A);
                4
            }
            0x68 => {
                self.ld(Register::L, Register::B);
                4
            }
            0x69 => {
                self.ld(Register::L, Register::C);
                4
            }
            0x6A => {
                self.ld(Register::L, Register::D);
                4
            }
            0x6B => {
                self.ld(Register::L, Register::E);
                4
            }
            0x6C => {
                self.ld(Register::L, Register::H);
                4
            }
            0x6D => {
                self.ld(Register::L, Register::L);
                4
            }
            0x6E => unimplemented!("Opcode 0x6E"),
            0x6F => {
                self.ld(Register::L, Register::A);
                4
            }
            0x70 => unimplemented!("Opcode 0x70"),
            0x71 => unimplemented!("Opcode 0x71"),
            0x72 => unimplemented!("Opcode 0x72"),
            0x73 => unimplemented!("Opcode 0x73"),
            0x74 => unimplemented!("Opcode 0x74"),
            0x75 => unimplemented!("Opcode 0x75"),
            0x76 => unimplemented!("Opcode 0x76"),
            0x77 => unimplemented!("Opcode 0x77"),
            0x78 => {
                self.ld(Register::A, Register::B);
                4
            }
            0x79 => {
                self.ld(Register::A, Register::C);
                4
            }
            0x7A => {
                self.ld(Register::A, Register::D);
                4
            }
            0x7B => {
                self.ld(Register::A, Register::E);
                4
            }
            0x7C => {
                self.ld(Register::A, Register::H);
                4
            }
            0x7D => {
                self.ld(Register::A, Register::L);
                4
            }
            0x7E => unimplemented!("Opcode 0x7E"),
            0x7F => {
                self.ld(Register::A, Register::A);
                4
            }
            0x80 => unimplemented!("Opcode 0x80"),
            0x81 => unimplemented!("Opcode 0x81"),
            0x82 => unimplemented!("Opcode 0x82"),
            0x83 => unimplemented!("Opcode 0x83"),
            0x84 => unimplemented!("Opcode 0x84"),
            0x85 => unimplemented!("Opcode 0x85"),
            0x86 => unimplemented!("Opcode 0x86"),
            0x87 => unimplemented!("Opcode 0x87"),
            0x88 => unimplemented!("Opcode 0x88"),
            0x89 => unimplemented!("Opcode 0x89"),
            0x8A => unimplemented!("Opcode 0x8A"),
            0x8B => unimplemented!("Opcode 0x8B"),
            0x8C => unimplemented!("Opcode 0x8C"),
            0x8D => unimplemented!("Opcode 0x8D"),
            0x8E => unimplemented!("Opcode 0x8E"),
            0x8F => unimplemented!("Opcode 0x8F"),
            0x90 => unimplemented!("Opcode 0x90"),
            0x91 => unimplemented!("Opcode 0x91"),
            0x92 => unimplemented!("Opcode 0x92"),
            0x93 => unimplemented!("Opcode 0x93"),
            0x94 => unimplemented!("Opcode 0x94"),
            0x95 => unimplemented!("Opcode 0x95"),
            0x96 => unimplemented!("Opcode 0x96"),
            0x97 => unimplemented!("Opcode 0x97"),
            0x98 => unimplemented!("Opcode 0x98"),
            0x99 => unimplemented!("Opcode 0x99"),
            0x9A => unimplemented!("Opcode 0x9A"),
            0x9B => unimplemented!("Opcode 0x9B"),
            0x9C => unimplemented!("Opcode 0x9C"),
            0x9D => unimplemented!("Opcode 0x9D"),
            0x9E => unimplemented!("Opcode 0x9E"),
            0x9F => unimplemented!("Opcode 0x9F"),
            0xA0 => unimplemented!("Opcode 0xA0"),
            0xA1 => unimplemented!("Opcode 0xA1"),
            0xA2 => unimplemented!("Opcode 0xA2"),
            0xA3 => unimplemented!("Opcode 0xA3"),
            0xA4 => unimplemented!("Opcode 0xA4"),
            0xA5 => unimplemented!("Opcode 0xA5"),
            0xA6 => unimplemented!("Opcode 0xA6"),
            0xA7 => unimplemented!("Opcode 0xA7"),
            0xA8 => unimplemented!("Opcode 0xA8"),
            0xA9 => unimplemented!("Opcode 0xA9"),
            0xAA => unimplemented!("Opcode 0xAA"),
            0xAB => unimplemented!("Opcode 0xAB"),
            0xAC => unimplemented!("Opcode 0xAC"),
            0xAD => unimplemented!("Opcode 0xAD"),
            0xAE => unimplemented!("Opcode 0xAE"),
            0xAF => unimplemented!("Opcode 0xAF"),
            0xB0 => unimplemented!("Opcode 0xB0"),
            0xB1 => unimplemented!("Opcode 0xB1"),
            0xB2 => unimplemented!("Opcode 0xB2"),
            0xB3 => unimplemented!("Opcode 0xB3"),
            0xB4 => unimplemented!("Opcode 0xB4"),
            0xB5 => unimplemented!("Opcode 0xB5"),
            0xB6 => unimplemented!("Opcode 0xB6"),
            0xB7 => unimplemented!("Opcode 0xB7"),
            0xB8 => unimplemented!("Opcode 0xB8"),
            0xB9 => unimplemented!("Opcode 0xB9"),
            0xBA => unimplemented!("Opcode 0xBA"),
            0xBB => unimplemented!("Opcode 0xBB"),
            0xBC => unimplemented!("Opcode 0xBC"),
            0xBD => unimplemented!("Opcode 0xBD"),
            0xBE => unimplemented!("Opcode 0xBE"),
            0xBF => unimplemented!("Opcode 0xBF"),
            0xC0 => unimplemented!("Opcode 0xC0"),
            0xC1 => unimplemented!("Opcode 0xC1"),
            0xC2 => unimplemented!("Opcode 0xC2"),
            0xC3 => unimplemented!("Opcode 0xC3"),
            0xC4 => unimplemented!("Opcode 0xC4"),
            0xC5 => unimplemented!("Opcode 0xC5"),
            0xC6 => unimplemented!("Opcode 0xC6"),
            0xC7 => unimplemented!("Opcode 0xC7"),
            0xC8 => unimplemented!("Opcode 0xC8"),
            0xC9 => unimplemented!("Opcode 0xC9"),
            0xCA => unimplemented!("Opcode 0xCA"),
            0xCB => unimplemented!("Opcode 0xCB"),
            0xCC => unimplemented!("Opcode 0xCC"),
            0xCD => unimplemented!("Opcode 0xCD"),
            0xCE => unimplemented!("Opcode 0xCE"),
            0xCF => unimplemented!("Opcode 0xCF"),
            0xD0 => unimplemented!("Opcode 0xD0"),
            0xD1 => unimplemented!("Opcode 0xD1"),
            0xD2 => unimplemented!("Opcode 0xD2"),
            0xD3 => unimplemented!("Opcode 0xD3"),
            0xD4 => unimplemented!("Opcode 0xD4"),
            0xD5 => unimplemented!("Opcode 0xD5"),
            0xD6 => unimplemented!("Opcode 0xD6"),
            0xD7 => unimplemented!("Opcode 0xD7"),
            0xD8 => unimplemented!("Opcode 0xD8"),
            0xD9 => unimplemented!("Opcode 0xD9"),
            0xDA => unimplemented!("Opcode 0xDA"),
            0xDB => unimplemented!("Opcode 0xDB"),
            0xDC => unimplemented!("Opcode 0xDC"),
            0xDD => unimplemented!("Opcode 0xDD"),
            0xDE => unimplemented!("Opcode 0xDE"),
            0xDF => unimplemented!("Opcode 0xDF"),
            0xE0 => unimplemented!("Opcode 0xE0"),
            0xE1 => unimplemented!("Opcode 0xE1"),
            0xE2 => unimplemented!("Opcode 0xE2"),
            0xE3 => unimplemented!("Opcode 0xE3"),
            0xE4 => unimplemented!("Opcode 0xE4"),
            0xE5 => unimplemented!("Opcode 0xE5"),
            0xE6 => unimplemented!("Opcode 0xE6"),
            0xE7 => unimplemented!("Opcode 0xE7"),
            0xE8 => unimplemented!("Opcode 0xE8"),
            0xE9 => unimplemented!("Opcode 0xE9"),
            0xEA => unimplemented!("Opcode 0xEA"),
            0xEB => unimplemented!("Opcode 0xEB"),
            0xEC => unimplemented!("Opcode 0xEC"),
            0xED => unimplemented!("Opcode 0xED"),
            0xEE => unimplemented!("Opcode 0xEE"),
            0xEF => unimplemented!("Opcode 0xEF"),
            0xF0 => unimplemented!("Opcode 0xF0"),
            0xF1 => unimplemented!("Opcode 0xF1"),
            0xF2 => unimplemented!("Opcode 0xF2"),
            0xF3 => unimplemented!("Opcode 0xF3"),
            0xF4 => unimplemented!("Opcode 0xF4"),
            0xF5 => unimplemented!("Opcode 0xF5"),
            0xF6 => unimplemented!("Opcode 0xF6"),
            0xF7 => unimplemented!("Opcode 0xF7"),
            0xF8 => unimplemented!("Opcode 0xF8"),
            0xF9 => unimplemented!("Opcode 0xF9"),
            0xFA => unimplemented!("Opcode 0xFA"),
            0xFB => unimplemented!("Opcode 0xFB"),
            0xFC => unimplemented!("Opcode 0xFC"),
            0xFD => unimplemented!("Opcode 0xFD"),
            0xFE => unimplemented!("Opcode 0xFE"),
            0xFF => unimplemented!("Opcode 0xFF"),
        }
    }
}
