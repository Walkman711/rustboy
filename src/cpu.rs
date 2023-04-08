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
    pub fn read_byte(&self, _addr: u16) -> u8 {
        unimplemented!("MMU::read_byte() not implemented yet")
    }

    pub fn write_byte(&mut self, _addr: u16, _val: u8) {
        unimplemented!("MMU::write_byte() not implemented yet")
    }

    pub fn read_word(&self, _addr: u16) -> u16 {
        unimplemented!("MMU::read_word() not implemented yet")
    }

    pub fn write_word(&mut self, _addr: u16, _val: u16) {
        unimplemented!("MMU::write_word() not implemented yet")
    }
}

impl CPU {
    pub fn run(&mut self) {
        loop {
            self.call();
        }
    }
}

// alu ops
impl CPU {
    fn alu_add(&mut self, _val: u8) {
        todo!()
    }

    fn alu_adc(&mut self, _val: u8) {
        todo!()
    }

    fn alu_sub(&mut self, _val: u8) {
        todo!()
    }

    fn alu_sbc(&mut self, _val: u8) {
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
        self.reg.f.Z = self.reg.a == val;
        self.reg.f.N = true;
        self.reg.f.H = todo!();
        self.reg.f.C = self.reg.a < val;
    }

    fn alu_inc(&mut self, val: u8) -> u8 {
        let res = val.wrapping_add(1);
        self.reg.f.Z = res == 0;
        self.reg.f.N = false;
        self.reg.f.H = (val & 0x0F) + 1 > 0x0F;
        res
    }

    fn push_stack(&mut self, addr: u16) {
        self.mmu.write_word(self.reg.sp, addr);
        // XXX: check if this goes up or down
        self.reg.sp += 2;
    }

    fn pop_stack(&mut self) -> u16 {
        let addr = self.mmu.read_word(self.reg.sp);
        self.reg.sp -= 2;
        addr
    }

    fn ret(&mut self) {
        let addr = self.pop_stack();
        self.reg.pc = addr;
    }

    fn rst(&mut self, n: u8) {
        self.push_stack(self.reg.pc);
        self.reg.pc = n as u16;
    }

    fn call_op(&mut self) {
        self.push_stack(self.reg.pc);
        let addr = self.fetch_word();
        self.reg.pc = addr;
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
            // INC B
            0x04 => {
                self.reg.b = self.alu_inc(self.reg.b);
                4
            }
            0x05 => unimplemented!("Opcode 0x05"),
            // LD B,n
            0x06 => {
                self.reg.b = self.fetch_byte();
                8
            }
            // RLCA
            0x07 => {
                // TODO: look this over again
                self.reg.f.C = (self.reg.a & 0b1000) == 0b1000;
                self.reg.a <<= 1;
                // Don't think we can be clever and inline this. Don't want to reset Zero flag
                if self.reg.a == 0 {
                    self.reg.f.Z = true;
                }
                4
            }
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
            // INC C
            0x0C => {
                self.reg.c = self.alu_inc(self.reg.c);
                4
            }
            0x0D => unimplemented!("Opcode 0x0D"),
            // LD C,n
            0x0E => {
                self.reg.c = self.fetch_byte();
                8
            }
            // RRCA
            // TODO: look at how the zero flag is set
            0x0F => {
                self.reg.f.C = (self.reg.a & 0b0001) == 0b0001;
                self.reg.a >>= 1;
                if self.reg.a == 0 {
                    self.reg.f.Z = true;
                }
                4
            }
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
            // INC D
            0x14 => {
                self.reg.d = self.alu_inc(self.reg.d);
                4
            }
            0x15 => unimplemented!("Opcode 0x15"),
            // LD D,n
            0x16 => {
                self.reg.d = self.fetch_byte();
                8
            }
            0x17 => unimplemented!("Opcode 0x17"),
            // JR n
            0x18 => {
                self.reg.pc += self.fetch_byte() as u16;
                8
            }
            0x19 => unimplemented!("Opcode 0x19"),
            0x1A => {
                self.reg.a = self.mmu.read_byte(self.reg.de());
                8
            }
            0x1B => unimplemented!("Opcode 0x1B"),
            // INC E
            0x1C => {
                self.reg.e = self.alu_inc(self.reg.e);
                4
            }
            0x1D => unimplemented!("Opcode 0x1D"),
            // LD E,n
            0x1E => {
                self.reg.e = self.fetch_byte();
                8
            }
            0x1F => unimplemented!("Opcode 0x1F"),
            // JR NZ,n
            0x20 => {
                if !self.reg.f.Z {
                    self.reg.pc += self.fetch_byte() as u16;
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
            // INC H
            0x24 => {
                self.reg.h = self.alu_inc(self.reg.h);
                4
            }
            0x25 => unimplemented!("Opcode 0x25"),
            // LD H,n
            0x26 => {
                self.reg.h = self.fetch_byte();
                8
            }
            0x27 => unimplemented!("Opcode 0x27"),
            // JR Z,n
            0x28 => {
                if self.reg.f.Z {
                    self.reg.pc += self.fetch_byte() as u16;
                }
                8
            }
            0x29 => unimplemented!("Opcode 0x29"),
            0x2A => unimplemented!("Opcode 0x2A"),
            0x2B => unimplemented!("Opcode 0x2B"),
            // INC L
            0x2C => {
                self.reg.l = self.alu_inc(self.reg.l);
                4
            }
            0x2D => unimplemented!("Opcode 0x2D"),
            // LD L,n
            0x2E => {
                self.reg.l = self.fetch_byte();
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
                    self.reg.pc += self.fetch_byte() as u16;
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
            // INC (HL)
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
            0x38 => {
                if self.reg.f.C {
                    self.reg.pc += self.fetch_byte() as u16;
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
            // INC A
            0x3C => {
                self.reg.a = self.alu_inc(self.reg.a);
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
            // ADD A,B
            0x80 => {
                self.alu_add(self.reg.b);
                4
            }
            // ADD A,C
            0x81 => {
                self.alu_add(self.reg.c);
                4
            }
            // ADD A,D
            0x82 => {
                self.alu_add(self.reg.d);
                4
            }
            // ADD A,E
            0x83 => {
                self.alu_add(self.reg.e);
                4
            }
            // ADD A,H
            0x84 => {
                self.alu_add(self.reg.h);
                4
            }
            // ADD A,L
            0x85 => {
                self.alu_add(self.reg.l);
                4
            }
            // ADD A,(HL)
            0x86 => {
                self.alu_add(self.mmu.read_byte(self.reg.hl()));
                8
            }
            // ADD A,A
            0x87 => {
                self.alu_add(self.reg.a);
                4
            }
            // ADC A,B
            0x88 => {
                self.alu_adc(self.reg.b);
                4
            }
            // ADC A,C
            0x89 => {
                self.alu_adc(self.reg.c);
                4
            }
            // ADC A,D
            0x8A => {
                self.alu_adc(self.reg.d);
                4
            }
            // ADC A,E
            0x8B => {
                self.alu_adc(self.reg.e);
                4
            }
            // ADC A,H
            0x8C => {
                self.alu_adc(self.reg.h);
                4
            }
            // ADC A,L
            0x8D => {
                self.alu_adc(self.reg.l);
                4
            }
            // ADC A,(HL)
            0x8E => {
                self.alu_adc(self.mmu.read_byte(self.reg.hl()));
                8
            }
            // ADC A,A
            0x8F => {
                self.alu_adc(self.reg.a);
                4
            }
            // SUB B
            0x90 => {
                self.alu_sub(self.reg.b);
                4
            }
            // SUB C
            0x91 => {
                self.alu_sub(self.reg.c);
                4
            }
            // SUB D
            0x92 => {
                self.alu_sub(self.reg.d);
                4
            }
            // SUB E
            0x93 => {
                self.alu_sub(self.reg.e);
                4
            }
            // SUB H
            0x94 => {
                self.alu_sub(self.reg.h);
                4
            }
            // SUB L
            0x95 => {
                self.alu_sub(self.reg.l);
                4
            }
            // SUB (HL)
            0x96 => {
                self.alu_sub(self.mmu.read_byte(self.reg.hl()));
                8
            }
            // SUB A
            0x97 => {
                self.alu_sub(self.reg.a);
                4
            }
            // SBC A,B
            0x98 => {
                self.alu_sbc(self.reg.b);
                4
            }
            // SBC A,C
            0x99 => {
                self.alu_sbc(self.reg.c);
                4
            }
            // SBC A,D
            0x9A => {
                self.alu_sbc(self.reg.d);
                4
            }
            // SBC A,E
            0x9B => {
                self.alu_sbc(self.reg.e);
                4
            }
            // SBC A,H
            0x9C => {
                self.alu_sbc(self.reg.h);
                4
            }
            // SBC A,L
            0x9D => {
                self.alu_sbc(self.reg.l);
                4
            }
            // SBC A,(HL)
            0x9E => {
                self.alu_sbc(self.mmu.read_byte(self.reg.hl()));
                8
            }
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
            // CP B
            0xB8 => {
                self.alu_cp(self.reg.b);
                4
            }
            // CP C
            0xB9 => {
                self.alu_cp(self.reg.c);
                4
            }
            // CP D
            0xBA => {
                self.alu_cp(self.reg.d);
                4
            }
            // CP E
            0xBB => {
                self.alu_cp(self.reg.e);
                4
            }
            // CP H
            0xBC => {
                self.alu_cp(self.reg.h);
                4
            }
            // CP L
            0xBD => {
                self.alu_cp(self.reg.l);
                4
            }
            // CP (HL)
            0xBE => {
                self.alu_cp(self.mmu.read_byte(self.reg.hl()));
                8
            }
            // CP A
            0xBF => {
                self.alu_cp(self.reg.a);
                4
            }
            // RET NZ
            0xC0 => {
                if !self.reg.f.Z {
                    self.ret();
                }
                8
            }
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
            // CALL NZ, nn
            0xC4 => {
                if !self.reg.f.Z {
                    self.call_op();
                }
                12
            }
            0xC5 => unimplemented!("Opcode 0xC5"),
            // ADD A, #
            0xC6 => {
                let n = self.fetch_byte();
                self.alu_add(n);
                8
            }
            // RST 00H
            0xC7 => {
                self.rst(0x00);
                32
            }
            // RET Z
            0xC8 => {
                if !self.reg.f.Z {
                    self.ret();
                }
                8
            }
            // RET
            0xC9 => {
                self.ret();
                8
            }
            // JP Z,nn
            0xCA => {
                if self.reg.f.Z {
                    let addr = self.fetch_word();
                    self.reg.pc = addr;
                }
                12
            }
            0xCB => unimplemented!("Opcode 0xCB"),
            // CALL Z, nn
            0xCC => {
                if self.reg.f.Z {
                    self.call_op();
                }
                12
            }
            // CALL nn
            0xCD => {
                self.call_op();
                12
            }
            // ADC A,#
            0xCE => {
                let n = self.fetch_byte();
                self.alu_adc(n);
                8
            }
            // RST 08H
            0xCF => {
                self.rst(0x08);
                32
            }
            // RET NC
            0xD0 => {
                if !self.reg.f.C {
                    self.ret();
                }
                8
            }
            0xD1 => unimplemented!("Opcode 0xD1"),
            // JP NC,nn
            0xD2 => {
                if !self.reg.f.C {
                    let addr = self.fetch_word();
                    self.reg.pc = addr;
                }
                12
            }
            // CALL NC, nn
            0xD4 => {
                if !self.reg.f.C {
                    self.call_op();
                }
                12
            }
            0xD5 => unimplemented!("Opcode 0xD5"),
            // SUB #
            0xD6 => {
                let n = self.fetch_byte();
                self.alu_sub(n);
                8
            }
            // RST 10H
            0xD7 => {
                self.rst(0x10);
                32
            }
            // RET C
            0xD8 => {
                if self.reg.f.C {
                    self.ret();
                }
                8
            }
            // RETI
            0xD9 => {
                self.ret();
                todo!("need to make enabling/disabling of interrupts a tick");
            }
            // JP C,nn
            0xDA => {
                if self.reg.f.C {
                    let addr = self.fetch_word();
                    self.reg.pc = addr;
                }
                12
            }
            // CALL C, nn
            0xDC => {
                if self.reg.f.C {
                    self.call_op();
                }
                12
            }
            0xDE => unimplemented!("Opcode 0xDE"),
            // RST 18H
            0xDF => {
                self.rst(0x18);
                32
            }
            0xE0 => unimplemented!("Opcode 0xE0"),
            0xE1 => unimplemented!("Opcode 0xE1"),
            0xE2 => {
                self.mmu
                    .write_byte(0xFF00 + (self.reg.c as u16), self.reg.a);
                8
            }
            0xE5 => unimplemented!("Opcode 0xE5"),
            0xE6 => {
                let n = self.fetch_byte();
                self.alu_and(n);
                8
            }
            // RST 20H
            0xE7 => {
                self.rst(0x20);
                32
            }
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
            0xEE => {
                let n = self.fetch_byte();
                self.alu_xor(n);
                8
            }
            // RST 28H
            0xEF => {
                self.rst(0x28);
                32
            }
            0xF0 => unimplemented!("Opcode 0xF0"),
            0xF1 => unimplemented!("Opcode 0xF1"),
            0xF2 => {
                self.reg.a = self.reg.c + self.mmu.read_byte(0xFF00);
                8
            }
            0xF3 => unimplemented!("Opcode 0xF3"),
            0xF5 => unimplemented!("Opcode 0xF5"),
            0xF6 => {
                let n = self.fetch_byte();
                self.alu_or(n);
                8
            }
            // RST 30H
            0xF7 => {
                self.rst(0x30);
                32
            }
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
            // CP #
            0xFE => {
                let n = self.fetch_byte();
                self.alu_cp(n);
                8
            }
            // RST 38H
            0xFF => {
                self.rst(0x38);
                32
            }
            0xD3 | 0xDB | 0xDD | 0xE3 | 0xE4 | 0xEB | 0xEC | 0xED | 0xF4 | 0xFC | 0xFD => {
                panic!("{opcode} is not a valid opcode")
            }
        }
    }
}
