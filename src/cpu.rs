#![allow(dead_code)]
use crate::registers::*;
pub struct CPU {
    reg: Registers,
    clock: u16,
    mmu: MMU,
    halted: bool,
    stopped: bool,
    interrupts_enabled: bool,
}

pub struct MMU {}

impl MMU {
    pub fn read_byte(&self, addr: u16) -> u8 {
        unimplemented!("MMU::read_byte() not implemented yet")
    }

    pub fn write_byte(&mut self, addr: u16, val: u8) {
        unimplemented!("MMU::write_byte() not implemented yet")
    }
}

// alu ops
impl CPU {
    fn alu_add(&mut self, val: u8) {
        todo!()
    }

    fn alu_adc(&mut self, val: u8) {
        todo!()
    }

    fn alu_sub(&mut self) {
        todo!()
    }

    fn alu_sbc(&mut self) {
        todo!()
    }

    fn alu_and(&mut self, val: u8) {
        let res = self.reg.a & val;
        self.reg.f.Z = res == 0;
        self.reg.f.N = false;
        self.reg.f.H = true;
        self.reg.f.C = false;
        self.reg.a = res;
    }

    fn alu_or(&mut self, val: u8) {
        let res = self.reg.a | val;
        self.reg.f.Z = res == 0;
        self.reg.f.N = false;
        self.reg.f.H = false;
        self.reg.f.C = false;
        self.reg.a = res;
    }

    fn alu_xor(&mut self, val: u8) {
        let res = self.reg.a ^ val;
        self.reg.f.Z = res == 0;
        self.reg.f.N = false;
        self.reg.f.H = false;
        self.reg.f.C = false;
        self.reg.a = res;
    }

    fn alu_cp(&mut self, val: u8) {
        todo!()
    }

    fn alu_inc(&mut self, val: u8) -> u8 {
        let res = val.wrapping_add(1);
        self.reg.f.Z = res == 0;
        self.reg.f.N = false;
        self.reg.f.H = (val & 0x0F) + 1 > 0x0F;
        res
    }
}

impl CPU {
    fn fetch_byte(&mut self) -> u8 {
        todo!()
    }

    fn fetch_word(&mut self) -> u16 {
        let nn_l = self.fetch_byte();
        let nn_h = self.fetch_byte();
        ((nn_h as u16) << 8) | (nn_l as u16)
    }

    // Returns cycles elapsed
    fn call(&mut self) -> u32 {
        let opcode = self.fetch_byte();
        match opcode {
            // NOP
            0x00 => 4, // No-op
            // LD BC,nn
            0x01 => {
                self.reg.c = self.fetch_byte();
                self.reg.b = self.fetch_byte();
                12
            }
            0x02 => {
                self.mmu.write_byte(self.reg.bc(), self.reg.a);
                8
            }
            0x03 => unimplemented!("Opcode 0x03"),
            0x04 => {
                self.reg.b = self.alu_inc(self.reg.b);
                4
            }
            0x05 => unimplemented!("Opcode 0x05"),
            // LD B,n
            0x06 => {
                let n = self.fetch_byte();
                self.reg.b = n;
                8
            }
            0x07 => unimplemented!("Opcode 0x07"),
            // LD (nn),SP
            0x08 => {
                let nn = self.fetch_word();
                self.reg.sp = nn;
                20
            }
            0x09 => unimplemented!("Opcode 0x09"),
            0x0A => {
                self.reg.a = self.mmu.read_byte(self.reg.bc());
                8
            }
            0x0B => unimplemented!("Opcode 0x0B"),
            0x0C => {
                self.reg.c = self.alu_inc(self.reg.c);
                4
            }
            0x0D => unimplemented!("Opcode 0x0D"),
            // LD C,n
            0x0E => {
                let n = self.fetch_byte();
                self.reg.c = n;
                8
            }
            0x0F => unimplemented!("Opcode 0x0F"),
            // STOP
            0x10 => {
                // STOP's opcode is 10 00, so if we didn't read the second byte,
                // we'd do an additional no-op and use 4 extra cycles
                let _ = self.fetch_byte();
                self.halted = true;
                self.stopped = true;
                4
            }
            // LD DE,nn
            0x11 => {
                self.reg.e = self.fetch_byte();
                self.reg.d = self.fetch_byte();
                12
            }
            0x12 => {
                self.mmu.write_byte(self.reg.de(), self.reg.a);
                8
            }
            0x13 => unimplemented!("Opcode 0x13"),
            0x14 => {
                self.reg.d = self.alu_inc(self.reg.d);
                4
            }
            0x15 => unimplemented!("Opcode 0x15"),
            // LD D,n
            0x16 => {
                let n = self.fetch_byte();
                self.reg.d = n;
                8
            }
            0x17 => unimplemented!("Opcode 0x17"),
            // JR n
            0x18 => {
                let n: u16 = self.fetch_byte().into();
                self.reg.pc += n;
                8
            }
            0x19 => unimplemented!("Opcode 0x19"),
            0x1A => {
                self.reg.a = self.mmu.read_byte(self.reg.de());
                8
            }
            0x1B => unimplemented!("Opcode 0x1B"),
            0x1C => {
                self.reg.e = self.alu_inc(self.reg.e);
                4
            }
            0x1D => unimplemented!("Opcode 0x1D"),
            // LD E,n
            0x1E => {
                let n = self.fetch_byte();
                self.reg.e = n;
                8
            }
            0x1F => unimplemented!("Opcode 0x1F"),
            // JR NZ,n
            0x20 => {
                if !self.reg.f.Z {
                    let n = self.fetch_byte().into();
                    self.reg.pc += n;
                }
                8
            }
            // LD HL,nn
            0x21 => {
                self.reg.l = self.fetch_byte();
                self.reg.h = self.fetch_byte();
                12
            }
            0x22 => unimplemented!("Opcode 0x22"),
            0x23 => unimplemented!("Opcode 0x23"),
            0x24 => {
                self.reg.h = self.alu_inc(self.reg.h);
                4
            }
            0x25 => unimplemented!("Opcode 0x25"),
            // LD H,n
            0x26 => {
                let n = self.fetch_byte();
                self.reg.h = n;
                8
            }
            0x27 => unimplemented!("Opcode 0x27"),
            // JR Z,n
            0x28 => {
                if self.reg.f.Z {
                    let n = self.fetch_byte().into();
                    self.reg.pc += n;
                }
                8
            }
            0x29 => unimplemented!("Opcode 0x29"),
            0x2A => unimplemented!("Opcode 0x2A"),
            0x2B => unimplemented!("Opcode 0x2B"),
            0x2C => {
                self.reg.l = self.alu_inc(self.reg.l);
                4
            }
            0x2D => unimplemented!("Opcode 0x2D"),
            // LD L,n
            0x2E => {
                let n = self.fetch_byte();
                self.reg.l = n;
                8
            }
            // CPL
            0x2F => {
                // bitwise-complement operator (equivalent to '~' in C)
                self.reg.a = !self.reg.a;
                self.reg.f.N = true;
                self.reg.f.H = true;
                4
            }
            // JR NC,n
            0x30 => {
                if !self.reg.f.C {
                    let n = self.fetch_byte().into();
                    self.reg.pc += n;
                }
                8
            }
            // LD SP,nn
            0x31 => {
                self.reg.sp = self.fetch_word();
                12
            }
            0x32 => {
                // FIX: need to implement decrement for HL
                self.mmu.write_byte(self.reg.hl(), self.reg.a);
                todo!("decrement HL");
                8
            }
            0x33 => unimplemented!("Opcode 0x33"),
            0x34 => {
                let inc = self.alu_inc(self.mmu.read_byte(self.reg.hl()));
                self.mmu.write_byte(self.reg.hl(), inc);
                12
            }
            0x35 => unimplemented!("Opcode 0x35"),
            0x36 => {
                let n = self.fetch_byte();
                self.mmu.write_byte(self.reg.hl(), n);
                12
            }
            // SCF
            0x37 => {
                self.reg.f.N = false;
                self.reg.f.H = false;
                self.reg.f.C = true;
                4
            }
            // JR C,n
            0x30 => {
                if self.reg.f.C {
                    let n = self.fetch_byte().into();
                    self.reg.pc += n;
                }
                8
            }
            0x39 => unimplemented!("Opcode 0x39"),
            0x3A => {
                // FIX: need to implement decrement for HL
                self.reg.a = self.mmu.read_byte(self.reg.hl());
                todo!("decrement HL");
                8
            }
            0x3B => unimplemented!("Opcode 0x3B"),
            0x3C => {
                self.alu_inc(self.reg.a);
                4
            }
            0x3D => unimplemented!("Opcode 0x3D"),
            0x3E => unimplemented!("Opcode 0x3E"),
            // CCF
            0x3F => {
                self.reg.f.N = false;
                self.reg.f.H = false;
                self.reg.f.C = !self.reg.f.C;
                4
            }
            0x40 => {
                self.reg.b = self.reg.b;
                4
            }
            0x41 => {
                self.reg.b = self.reg.c;
                4
            }
            0x42 => {
                self.reg.b = self.reg.d;
                4
            }
            0x43 => {
                self.reg.b = self.reg.e;
                4
            }
            0x44 => {
                self.reg.b = self.reg.h;
                4
            }
            0x45 => {
                self.reg.b = self.reg.l;
                4
            }
            0x46 => {
                self.reg.b = self.mmu.read_byte(self.reg.hl());
                8
            }
            0x47 => {
                self.reg.b = self.reg.a;
                4
            }
            0x48 => {
                self.reg.c = self.reg.b;
                4
            }
            0x49 => {
                self.reg.c = self.reg.c;
                4
            }
            0x4A => {
                self.reg.c = self.reg.d;
                4
            }
            0x4B => {
                self.reg.c = self.reg.e;
                4
            }
            0x4C => {
                self.reg.c = self.reg.h;
                4
            }
            0x4D => {
                self.reg.c = self.reg.l;
                4
            }
            0x4E => {
                self.reg.c = self.mmu.read_byte(self.reg.hl());
                8
            }
            0x4F => {
                self.reg.c = self.reg.a;
                4
            }
            0x50 => {
                self.reg.d = self.reg.b;
                4
            }
            0x51 => {
                self.reg.d = self.reg.c;
                4
            }
            0x52 => {
                self.reg.d = self.reg.d;
                4
            }
            0x53 => {
                self.reg.d = self.reg.e;
                4
            }
            0x54 => {
                self.reg.d = self.reg.h;
                4
            }
            0x55 => {
                self.reg.d = self.reg.l;
                4
            }
            0x56 => {
                self.reg.d = self.mmu.read_byte(self.reg.hl());
                8
            }
            0x57 => {
                self.reg.d = self.reg.a;
                4
            }
            0x58 => {
                self.reg.e = self.reg.b;
                4
            }
            0x59 => {
                self.reg.e = self.reg.c;
                4
            }
            0x5A => {
                self.reg.e = self.reg.d;
                4
            }
            0x5B => {
                self.reg.e = self.reg.e;
                4
            }
            0x5C => {
                self.reg.e = self.reg.h;
                4
            }
            0x5D => {
                self.reg.e = self.reg.l;
                4
            }
            0x5E => {
                self.reg.e = self.mmu.read_byte(self.reg.hl());
                8
            }
            0x5F => {
                self.reg.e = self.reg.a;
                4
            }
            0x60 => {
                self.reg.h = self.reg.b;
                4
            }
            0x61 => {
                self.reg.h = self.reg.c;
                4
            }
            0x62 => {
                self.reg.h = self.reg.d;
                4
            }
            0x63 => {
                self.reg.h = self.reg.e;
                4
            }
            0x64 => {
                self.reg.h = self.reg.h;
                4
            }
            0x65 => {
                self.reg.h = self.reg.l;
                4
            }
            0x66 => {
                self.reg.h = self.mmu.read_byte(self.reg.hl());
                8
            }
            0x67 => {
                self.reg.h = self.reg.a;
                4
            }
            0x68 => {
                self.reg.h = self.reg.b;
                4
            }
            0x69 => {
                self.reg.h = self.reg.c;
                4
            }
            0x6A => {
                self.reg.h = self.reg.d;
                4
            }
            0x6B => {
                self.reg.l = self.reg.e;
                4
            }
            0x6C => {
                self.reg.l = self.reg.h;
                4
            }
            0x6D => {
                self.reg.l = self.reg.l;
                4
            }
            0x6E => {
                self.reg.l = self.mmu.read_byte(self.reg.hl());
                8
            }
            0x6F => {
                self.reg.l = self.reg.a;
                4
            }
            0x70 => {
                self.mmu.write_byte(self.reg.hl(), self.reg.b);
                8
            }
            0x71 => {
                self.mmu.write_byte(self.reg.hl(), self.reg.c);
                8
            }
            0x72 => {
                self.mmu.write_byte(self.reg.hl(), self.reg.d);
                8
            }
            0x73 => {
                self.mmu.write_byte(self.reg.hl(), self.reg.e);
                8
            }
            0x74 => {
                self.mmu.write_byte(self.reg.hl(), self.reg.h);
                8
            }
            0x75 => {
                self.mmu.write_byte(self.reg.hl(), self.reg.l);
                8
            }
            0x76 => {
                self.halted = true;
                4
            }
            0x77 => {
                self.mmu.write_byte(self.reg.hl(), self.reg.a);
                8
            }
            0x78 => {
                self.reg.a = self.reg.b;
                4
            }
            0x79 => {
                self.reg.a = self.reg.c;
                4
            }
            0x7A => {
                self.reg.a = self.reg.d;
                4
            }
            0x7B => {
                self.reg.a = self.reg.e;
                4
            }
            0x7C => {
                self.reg.a = self.reg.h;
                4
            }
            0x7D => {
                self.reg.a = self.reg.l;
                4
            }
            0x7E => {
                self.reg.a = self.mmu.read_byte(self.reg.hl());
                8
            }
            0x7F => {
                self.reg.a = self.reg.a;
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
            0xA0 => {
                self.alu_and(self.reg.b);
                4
            }
            0xA1 => {
                self.alu_and(self.reg.c);
                4
            }
            0xA2 => {
                self.alu_and(self.reg.d);
                4
            }
            0xA3 => {
                self.alu_and(self.reg.e);
                4
            }
            0xA4 => {
                self.alu_and(self.reg.h);
                4
            }
            0xA5 => {
                self.alu_and(self.reg.l);
                4
            }
            0xA6 => {
                self.alu_and(self.mmu.read_byte(self.reg.hl()));
                8
            }
            0xA7 => {
                self.alu_and(self.reg.a);
                4
            }
            0xA8 => {
                self.alu_xor(self.reg.b);
                4
            }
            0xA9 => {
                self.alu_xor(self.reg.c);
                4
            }
            0xAA => {
                self.alu_xor(self.reg.d);
                4
            }
            0xAB => {
                self.alu_xor(self.reg.e);
                4
            }
            0xAC => {
                self.alu_xor(self.reg.h);
                4
            }
            0xAD => {
                self.alu_xor(self.reg.l);
                4
            }
            0xAE => {
                self.alu_xor(self.mmu.read_byte(self.reg.hl()));
                8
            }
            0xAF => {
                self.alu_xor(self.reg.a);
                4
            }
            0xB0 => {
                self.alu_or(self.reg.b);
                4
            }
            0xB1 => {
                self.alu_or(self.reg.c);
                4
            }
            0xB2 => {
                self.alu_or(self.reg.d);
                4
            }
            0xB3 => {
                self.alu_or(self.reg.e);
                4
            }
            0xB4 => {
                self.alu_or(self.reg.h);
                4
            }
            0xB5 => {
                self.alu_or(self.reg.l);
                4
            }
            0xB6 => {
                self.alu_or(self.mmu.read_byte(self.reg.hl()));
                8
            }
            0xB7 => {
                self.alu_or(self.reg.a);
                4
            }
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
            // JP NZ,nn
            0xC2 => {
                if !self.reg.f.Z {
                    let addr = self.fetch_word();
                    self.reg.pc = addr;
                }
                12
            }
            // JP nn
            0xC3 => {
                let addr = self.fetch_word();
                self.reg.pc = addr;
                12
            }
            0xC4 => unimplemented!("Opcode 0xC4"),
            0xC5 => unimplemented!("Opcode 0xC5"),
            0xC6 => unimplemented!("Opcode 0xC6"),
            0xC7 => unimplemented!("Opcode 0xC7"),
            0xC8 => unimplemented!("Opcode 0xC8"),
            0xC9 => unimplemented!("Opcode 0xC9"),
            // JP Z,nn
            0xCA => {
                if self.reg.f.Z {
                    let addr = self.fetch_word();
                    self.reg.pc = addr;
                }
                12
            }
            0xCB => unimplemented!("Opcode 0xCB"),
            0xCC => unimplemented!("Opcode 0xCC"),
            0xCD => unimplemented!("Opcode 0xCD"),
            0xCE => unimplemented!("Opcode 0xCE"),
            0xCF => unimplemented!("Opcode 0xCF"),
            0xD0 => unimplemented!("Opcode 0xD0"),
            0xD1 => unimplemented!("Opcode 0xD1"),
            // JP NC,nn
            0xD2 => {
                if !self.reg.f.C {
                    let addr = self.fetch_word();
                    self.reg.pc = addr;
                }
                12
            }
            0xD3 => unimplemented!("Opcode 0xD3"),
            0xD4 => unimplemented!("Opcode 0xD4"),
            0xD5 => unimplemented!("Opcode 0xD5"),
            0xD6 => unimplemented!("Opcode 0xD6"),
            0xD7 => unimplemented!("Opcode 0xD7"),
            0xD8 => unimplemented!("Opcode 0xD8"),
            0xD9 => unimplemented!("Opcode 0xD9"),
            // JP C,nn
            0xDA => {
                if self.reg.f.C {
                    let addr = self.fetch_word();
                    self.reg.pc = addr;
                }
                12
            }
            0xDB => unimplemented!("Opcode 0xDB"),
            0xDC => unimplemented!("Opcode 0xDC"),
            0xDD => unimplemented!("Opcode 0xDD"),
            0xDE => unimplemented!("Opcode 0xDE"),
            0xDF => unimplemented!("Opcode 0xDF"),
            0xE0 => unimplemented!("Opcode 0xE0"),
            0xE1 => unimplemented!("Opcode 0xE1"),
            0xE2 => {
                self.mmu
                    .write_byte(0xFF00 + (self.reg.c as u16), self.reg.a);
                8
            }
            0xE3 => unimplemented!("Opcode 0xE3"),
            0xE4 => unimplemented!("Opcode 0xE4"),
            0xE5 => unimplemented!("Opcode 0xE5"),
            0xE6 => {
                let n = self.fetch_byte();
                self.alu_and(n);
                8
            }
            0xE7 => unimplemented!("Opcode 0xE7"),
            0xE8 => unimplemented!("Opcode 0xE8"),
            // JP (HL)
            0xE9 => {
                self.reg.pc = self.reg.hl();
                4
            }
            0xEA => {
                let addr = self.fetch_word();
                self.mmu.write_byte(addr, self.reg.a);
                16
            }
            0xEB => unimplemented!("Opcode 0xEB"),
            0xEC => unimplemented!("Opcode 0xEC"),
            0xED => unimplemented!("Opcode 0xED"),
            0xEE => {
                let n = self.fetch_byte();
                self.alu_xor(n);
                8
            }
            0xEF => unimplemented!("Opcode 0xEF"),
            0xF0 => unimplemented!("Opcode 0xF0"),
            0xF1 => unimplemented!("Opcode 0xF1"),
            0xF2 => {
                self.reg.a = self.reg.c + self.mmu.read_byte(0xFF00);
                8
            }
            0xF3 => unimplemented!("Opcode 0xF3"),
            0xF4 => unimplemented!("Opcode 0xF4"),
            0xF5 => unimplemented!("Opcode 0xF5"),
            0xF6 => {
                let n = self.fetch_byte();
                self.alu_or(n);
                8
            }
            0xF7 => unimplemented!("Opcode 0xF7"),
            0xF8 => unimplemented!("Opcode 0xF8"),
            // LD SP,HL
            0xF9 => {
                self.reg.sp = self.reg.hl();
                8
            }
            0xFA => {
                let addr = self.fetch_word();
                self.reg.a = self.mmu.read_byte(addr);
                8
            }
            0xFB => unimplemented!("Opcode 0xFB"),
            0xFC => unimplemented!("Opcode 0xFC"),
            0xFD => unimplemented!("Opcode 0xFD"),
            0xFE => unimplemented!("Opcode 0xFE"),
            0xFF => unimplemented!("Opcode 0xFF"),
        }
    }
}
