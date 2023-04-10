#![allow(dead_code)]
use crate::{mmu::*, registers::*};

#[derive(Copy, Clone, Debug)]
enum InterruptState {
    DisableNext,
    Disabled,
    EnableNext,
    Enabled,
}

#[derive(Debug)]
pub struct CPU {
    reg: Registers,
    clock: u16,
    mmu: MMU,
    halted: bool,
    stopped: bool,
    interrupt_state: InterruptState,
}

impl CPU {
    pub fn run(&mut self) {
        loop {
            let _cycles_elapsed = self.call();
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

    /// AND reg A with `val`
    fn alu_and(&mut self, val: u8) {
        let res = self.reg.a & val;
        self.reg.flag(Flags::Z, res == 0);
        self.reg.flag(Flags::N, false);
        self.reg.flag(Flags::H, true);
        self.reg.flag(Flags::C, false);
        self.reg.a = res;
    }

    /// OR reg A with `val`
    fn alu_or(&mut self, val: u8) {
        let res = self.reg.a | val;
        self.reg.flag(Flags::Z, res == 0);
        self.reg.flag(Flags::N, false);
        self.reg.flag(Flags::H, false);
        self.reg.flag(Flags::C, false);
        self.reg.a = res;
    }

    /// XOR reg A with `val`
    fn alu_xor(&mut self, val: u8) {
        let res = self.reg.a ^ val;
        self.reg.flag(Flags::Z, res == 0);
        self.reg.flag(Flags::N, false);
        self.reg.flag(Flags::H, false);
        self.reg.flag(Flags::C, false);
        self.reg.a = res;
    }

    fn alu_cp(&mut self, val: u8) {
        self.reg.flag(Flags::Z, self.reg.a == val);
        self.reg.flag(Flags::N, true);
        self.reg.flag(Flags::H, todo!());
        self.reg.flag(Flags::C, self.reg.a < val);
    }

    fn alu_inc(&mut self, val: u8) -> u8 {
        let res = val.wrapping_add(1);
        self.reg.flag(Flags::Z, res == 0);
        self.reg.flag(Flags::N, false);
        self.reg.flag(Flags::H, (val & 0x0F) + 1 > 0x0F);
        res
    }

    fn alu_inc_16(&mut self, val: u16) -> u16 {
        todo!()
    }

    fn alu_dec(&mut self, val: u8) -> u8 {
        todo!()
    }

    fn push_stack(&mut self, addr: u16) {
        self.mmu.write_word(self.reg.sp, addr);
        // XXX: check if this goes up or down
        self.reg.sp -= 2;
    }

    fn pop_stack(&mut self) -> u16 {
        let addr = self.mmu.read_word(self.reg.sp);
        self.reg.sp += 2;
        addr
    }

    /// Pop two bytes from stack and jump to that address
    fn ret(&mut self) {
        let addr = self.pop_stack();
        self.reg.pc = addr;
    }

    /// Push present address onto stack. Jump to address 0x0000 + n.
    fn rst(&mut self, n: u8) {
        self.push_stack(self.reg.pc);
        self.reg.pc = n as u16;
    }

    /// Push address of next instruction onto stack and then jump to address nn
    fn call_op(&mut self) {
        // FIX: Will need to increment pc depending where I do that
        self.push_stack(self.reg.pc);
        let addr = self.fetch_word();
        self.reg.pc = addr;
    }
}

// CB ops
impl CPU {
    /// Test bit b in register `reg`
    fn cb_bit(&mut self, reg: u8, bit: u8) {
        let bit_set = reg & (1 << bit);
        self.reg.flag(Flags::Z, bit_set == 0);
        self.reg.flag(Flags::N, false);
        self.reg.flag(Flags::H, true);
    }

    /// Swap upper and lower nibbles of n
    fn cb_swap(&mut self, n: u8) -> u8 {
        let high_nibble = n & 0b11110000;
        let low_nibble = n & 0b00001111;
        let res = (low_nibble << 4) | (high_nibble >> 4);
        self.reg.flag(Flags::Z, res == 0);
        self.reg.flag(Flags::N, false);
        self.reg.flag(Flags::H, false);
        self.reg.flag(Flags::C, false);
        res
    }
}

impl CPU {
    fn fetch_byte(&mut self) -> u8 {
        let b = self.mmu.read_byte(self.reg.pc);
        self.reg.pc += 1;
        b
    }

    fn fetch_word(&mut self) -> u16 {
        let lo = self.fetch_byte();
        let hi = self.fetch_byte();
        ((hi as u16) << 8) | (lo as u16)
    }

    fn read_hl_byte(&self) -> u8 {
        self.mmu.read_byte(self.reg.hl())
    }

    fn write_hl_byte(&mut self, byte: u8) {
        self.mmu.write_byte(self.reg.hl(), byte)
    }

    // Returns cycles elapsed
    fn call(&mut self) -> u32 {
        let opcode = self.fetch_byte();
        match opcode {
            // NOP
            0x00 => 4,
            // LD BC,nn
            0x01 => {
                self.reg.c = self.fetch_byte();
                self.reg.b = self.fetch_byte();
                12
            }
            // LD (BC), A
            0x02 => {
                self.mmu.write_byte(self.reg.bc(), self.reg.a);
                8
            }
            // INC BC
            0x03 => {
                let inc = self.alu_inc_16(self.reg.bc());
                self.reg.set_bc(inc);
                8
            }
            // INC B
            0x04 => {
                self.reg.b = self.alu_inc(self.reg.b);
                4
            }
            // DEC B
            0x05 => {
                self.reg.b = self.alu_dec(self.reg.b);
                4
            }
            // LD B,n
            0x06 => {
                self.reg.b = self.fetch_byte();
                8
            }
            // RLCA
            0x07 => {
                // TODO: look this over again
                self.reg.flag(Flags::C, (self.reg.a & 0b1000) == 0b1000);
                self.reg.a <<= 1;
                // Don't think we can be clever and inline this. Don't want to reset Zero flag
                if self.reg.a == 0 {
                    self.reg.flag(Flags::Z, true);
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
            // DEC C
            0x0D => {
                self.reg.c = self.alu_dec(self.reg.c);
                4
            }
            // LD C,n
            0x0E => {
                self.reg.c = self.fetch_byte();
                8
            }
            // RRCA
            // TODO: look at how the zero flag is set
            0x0F => {
                self.reg.flag(Flags::C, (self.reg.a & 0b0001) == 0b0001);
                self.reg.a >>= 1;
                if self.reg.a == 0 {
                    self.reg.flag(Flags::Z, true);
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
            // INC DE
            0x13 => {
                let inc = self.alu_inc_16(self.reg.de());
                self.reg.set_de(inc);
                8
            }
            // INC D
            0x14 => {
                self.reg.d = self.alu_inc(self.reg.d);
                4
            }
            // DEC D
            0x15 => {
                self.reg.d = self.alu_dec(self.reg.d);
                4
            }
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
            // DEC E
            0x1D => {
                self.reg.e = self.alu_dec(self.reg.e);
                4
            }
            // LD E,n
            0x1E => {
                self.reg.e = self.fetch_byte();
                8
            }
            0x1F => unimplemented!("Opcode 0x1F"),
            // JR NZ,n
            0x20 => {
                if !self.reg.zero_flag_set() {
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
            // INC HL
            0x23 => {
                let inc = self.alu_inc_16(self.reg.hl());
                self.reg.set_hl(inc);
                8
            }
            // INC H
            0x24 => {
                self.reg.h = self.alu_inc(self.reg.h);
                4
            }
            // DEC H
            0x25 => {
                self.reg.h = self.alu_dec(self.reg.h);
                4
            }
            // LD H,n
            0x26 => {
                self.reg.h = self.fetch_byte();
                8
            }
            0x27 => unimplemented!("Opcode 0x27"),
            // JR Z,n
            0x28 => {
                if self.reg.zero_flag_set() {
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
            // DEC L
            0x2D => {
                self.reg.l = self.alu_dec(self.reg.l);
                4
            }
            // LD L,n
            0x2E => {
                self.reg.l = self.fetch_byte();
                8
            }
            // CPL
            0x2F => {
                // bitwise-complement operator (equivalent to '~' in C)
                self.reg.a = !self.reg.a;
                self.reg.flag(Flags::N, true);
                self.reg.flag(Flags::H, true);
                4
            }
            // JR NC,n
            0x30 => {
                if !self.reg.carry_flag_set() {
                    self.reg.pc += self.fetch_byte() as u16;
                }
                8
            }
            // LD SP,nn
            0x31 => {
                self.reg.sp = self.fetch_word();
                12
            }
            // LD (HL-),A or LD (HLD),A or LDD (HL),A
            0x32 => {
                // FIX: need to implement decrement for HL
                self.write_hl_byte(self.reg.a);
                todo!("decrement HL");
                8
            }
            // INC SP
            0x33 => {
                let inc = self.alu_inc_16(self.reg.sp);
                self.reg.sp = inc;
                8
            }
            // INC (HL)
            0x34 => {
                let inc = self.alu_inc(self.read_hl_byte());
                self.write_hl_byte(inc);
                12
            }
            // DEC (HL)
            0x35 => {
                let dec = self.alu_dec(self.read_hl_byte());
                self.write_hl_byte(dec);
                12
            }
            0x36 => {
                let n = self.fetch_byte();
                self.write_hl_byte(n);
                12
            }
            // SCF
            0x37 => {
                self.reg.flag(Flags::N, false);
                self.reg.flag(Flags::H, false);
                self.reg.flag(Flags::C, true);
                4
            }
            // JR C,n
            0x38 => {
                if self.reg.carry_flag_set() {
                    self.reg.pc += self.fetch_byte() as u16;
                }
                8
            }
            0x39 => unimplemented!("Opcode 0x39"),
            // LDD A,(HL)
            0x3A => {
                // FIX: need to implement decrement for HL
                self.reg.a = self.read_hl_byte();
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
                self.reg.flag(Flags::N, false);
                self.reg.flag(Flags::H, false);
                self.reg.flag(Flags::C, !self.reg.carry_flag_set());
                4
            }
            // LD B,B
            0x40 => {
                self.reg.b = self.reg.b;
                4
            }
            // LD B,C
            0x41 => {
                self.reg.b = self.reg.c;
                4
            }
            // LD B,D
            0x42 => {
                self.reg.b = self.reg.d;
                4
            }
            // LD B,E
            0x43 => {
                self.reg.b = self.reg.e;
                4
            }
            // LD B,H
            0x44 => {
                self.reg.b = self.reg.h;
                4
            }
            // LD B,L
            0x45 => {
                self.reg.b = self.reg.l;
                4
            }
            // LD B,(HL)
            0x46 => {
                self.reg.b = self.read_hl_byte();
                8
            }
            // LD B,A
            0x47 => {
                self.reg.b = self.reg.a;
                4
            }
            // LD C,B
            0x48 => {
                self.reg.c = self.reg.b;
                4
            }
            // LD C,C
            0x49 => {
                self.reg.c = self.reg.c;
                4
            }
            // LD C,D
            0x4A => {
                self.reg.c = self.reg.d;
                4
            }
            // LD C,E
            0x4B => {
                self.reg.c = self.reg.e;
                4
            }
            // LD C,H
            0x4C => {
                self.reg.c = self.reg.h;
                4
            }
            // LD C,L
            0x4D => {
                self.reg.c = self.reg.l;
                4
            }
            // LD C,(HL)
            0x4E => {
                self.reg.c = self.read_hl_byte();
                8
            }
            // LD C,A
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
                self.reg.d = self.read_hl_byte();
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
                self.reg.e = self.read_hl_byte();
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
                self.reg.h = self.read_hl_byte();
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
                self.reg.l = self.read_hl_byte();
                8
            }
            0x6F => {
                self.reg.l = self.reg.a;
                4
            }
            0x70 => {
                self.write_hl_byte(self.reg.b);
                8
            }
            0x71 => {
                self.write_hl_byte(self.reg.c);
                8
            }
            0x72 => {
                self.write_hl_byte(self.reg.d);
                8
            }
            0x73 => {
                self.write_hl_byte(self.reg.e);
                8
            }
            0x74 => {
                self.write_hl_byte(self.reg.h);
                8
            }
            0x75 => {
                self.write_hl_byte(self.reg.l);
                8
            }
            0x76 => {
                self.halted = true;
                4
            }
            0x77 => {
                self.write_hl_byte(self.reg.a);
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
            // LD A,(HL)
            0x7E => {
                self.reg.a = self.read_hl_byte();
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
                self.alu_add(self.read_hl_byte());
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
                self.alu_adc(self.read_hl_byte());
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
                self.alu_sub(self.read_hl_byte());
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
                self.alu_sbc(self.read_hl_byte());
                8
            }
            // SBC A, A
            0x9F => {
                self.alu_sbc(self.reg.a);
                4
            }
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
                self.alu_and(self.read_hl_byte());
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
                self.alu_xor(self.read_hl_byte());
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
                self.alu_or(self.read_hl_byte());
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
                self.alu_cp(self.read_hl_byte());
                8
            }
            // CP A
            0xBF => {
                self.alu_cp(self.reg.a);
                4
            }
            // RET NZ
            0xC0 => {
                if !self.reg.zero_flag_set() {
                    self.ret();
                }
                8
            }
            // POP BC
            0xC1 => {
                let addr = self.pop_stack();
                self.reg.set_bc(addr);
                12
            }
            // JP NZ,nn
            0xC2 => {
                if !self.reg.zero_flag_set() {
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
                if !self.reg.zero_flag_set() {
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
                if !self.reg.zero_flag_set() {
                    self.ret()
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
                if self.reg.zero_flag_set() {
                    let addr = self.fetch_word();
                    self.reg.pc = addr;
                }
                12
            }
            // CB-prefixed opcodes
            0xCB => self.call_cb(),
            // CALL Z, nn
            0xCC => {
                if self.reg.zero_flag_set() {
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
                if !self.reg.carry_flag_set() {
                    self.ret();
                }
                8
            }
            // POP DE
            0xD1 => {
                let addr = self.pop_stack();
                self.reg.set_de(addr);
                12
            }
            // JP NC,nn
            0xD2 => {
                if !self.reg.carry_flag_set() {
                    let addr = self.fetch_word();
                    self.reg.pc = addr;
                }
                12
            }
            // CALL NC, nn
            0xD4 => {
                if !self.reg.carry_flag_set() {
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
                if self.reg.carry_flag_set() {
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
                if self.reg.carry_flag_set() {
                    let addr = self.fetch_word();
                    self.reg.pc = addr;
                }
                12
            }
            // CALL C, nn
            0xDC => {
                if self.reg.carry_flag_set() {
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
            // POP HL
            0xE1 => {
                let addr = self.pop_stack();
                self.reg.set_hl(addr);
                12
            }
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
            // POP AF
            0xF1 => {
                let addr = self.pop_stack();
                self.reg.set_af(addr);
                12
            }
            0xF2 => {
                self.reg.a = self.reg.c + self.mmu.read_byte(0xFF00);
                8
            }
            // DI
            0xF3 => {
                self.interrupt_state = InterruptState::DisableNext;
                4
            }
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
            // LD A,(nn)
            0xFA => {
                let addr = self.fetch_word();
                self.reg.a = self.mmu.read_byte(addr);
                8
            }
            // EI
            0xFB => {
                self.interrupt_state = InterruptState::EnableNext;
                4
            }
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

    fn call_cb(&mut self) -> u32 {
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
            // SWAP B
            0x30 => {
                self.reg.b = self.cb_swap(self.reg.b);
                8
            }
            // SWAP C
            0x31 => {
                self.reg.c = self.cb_swap(self.reg.c);
                8
            }
            // SWAP D
            0x32 => {
                self.reg.d = self.cb_swap(self.reg.d);
                8
            }
            // SWAP E
            0x33 => {
                self.reg.e = self.cb_swap(self.reg.e);
                8
            }
            // SWAP H
            0x34 => {
                self.reg.h = self.cb_swap(self.reg.h);
                8
            }
            // SWAP L
            0x35 => {
                self.reg.l = self.cb_swap(self.reg.l);
                8
            }
            // SWAP (HL)
            0x36 => {
                let swapped_hl_byte = self.cb_swap(self.read_hl_byte());
                self.write_hl_byte(swapped_hl_byte);
                16
            }
            // SWAP A
            0x37 => {
                self.reg.a = self.cb_swap(self.reg.a);
                8
            }
            0x38 => unimplemented!("Opcode 0x38"),
            0x39 => unimplemented!("Opcode 0x39"),
            0x3A => unimplemented!("Opcode 0x3A"),
            0x3B => unimplemented!("Opcode 0x3B"),
            0x3C => unimplemented!("Opcode 0x3C"),
            0x3D => unimplemented!("Opcode 0x3D"),
            0x3E => unimplemented!("Opcode 0x3E"),
            0x3F => unimplemented!("Opcode 0x3F"),
            // BIT b,B
            0x40 => {
                let b = self.fetch_byte();
                self.cb_bit(b, self.reg.b);
                8
            }
            // BIT b,C
            0x41 => {
                let b = self.fetch_byte();
                self.cb_bit(b, self.reg.c);
                8
            }
            // BIT b,D
            0x42 => {
                let b = self.fetch_byte();
                self.cb_bit(b, self.reg.d);
                8
            }
            // BIT b,E
            0x43 => {
                let b = self.fetch_byte();
                self.cb_bit(b, self.reg.e);
                8
            }
            // BIT b,H
            0x44 => {
                let b = self.fetch_byte();
                self.cb_bit(b, self.reg.h);
                8
            }
            // BIT b,L
            0x45 => {
                let b = self.fetch_byte();
                self.cb_bit(b, self.reg.l);
                8
            }
            // BIT b,(HL)
            0x46 => {
                let b = self.fetch_byte();
                self.cb_bit(b, self.read_hl_byte());
                16
            }
            // BIT b,A
            0x47 => {
                let b = self.fetch_byte();
                self.cb_bit(b, self.reg.a);
                8
            }
            0x48 => unimplemented!("Opcode 0x48"),
            0x49 => unimplemented!("Opcode 0x49"),
            0x4A => unimplemented!("Opcode 0x4A"),
            0x4B => unimplemented!("Opcode 0x4B"),
            0x4C => unimplemented!("Opcode 0x4C"),
            0x4D => unimplemented!("Opcode 0x4D"),
            0x4E => unimplemented!("Opcode 0x4E"),
            0x4F => unimplemented!("Opcode 0x4F"),
            0x50 => unimplemented!("Opcode 0x50"),
            0x51 => unimplemented!("Opcode 0x51"),
            0x52 => unimplemented!("Opcode 0x52"),
            0x53 => unimplemented!("Opcode 0x53"),
            0x54 => unimplemented!("Opcode 0x54"),
            0x55 => unimplemented!("Opcode 0x55"),
            0x56 => unimplemented!("Opcode 0x56"),
            0x57 => unimplemented!("Opcode 0x57"),
            0x58 => unimplemented!("Opcode 0x58"),
            0x59 => unimplemented!("Opcode 0x59"),
            0x5A => unimplemented!("Opcode 0x5A"),
            0x5B => unimplemented!("Opcode 0x5B"),
            0x5C => unimplemented!("Opcode 0x5C"),
            0x5D => unimplemented!("Opcode 0x5D"),
            0x5E => unimplemented!("Opcode 0x5E"),
            0x5F => unimplemented!("Opcode 0x5F"),
            0x60 => unimplemented!("Opcode 0x60"),
            0x61 => unimplemented!("Opcode 0x61"),
            0x62 => unimplemented!("Opcode 0x62"),
            0x63 => unimplemented!("Opcode 0x63"),
            0x64 => unimplemented!("Opcode 0x64"),
            0x65 => unimplemented!("Opcode 0x65"),
            0x66 => unimplemented!("Opcode 0x66"),
            0x67 => unimplemented!("Opcode 0x67"),
            0x68 => unimplemented!("Opcode 0x68"),
            0x69 => unimplemented!("Opcode 0x69"),
            0x6A => unimplemented!("Opcode 0x6A"),
            0x6B => unimplemented!("Opcode 0x6B"),
            0x6C => unimplemented!("Opcode 0x6C"),
            0x6D => unimplemented!("Opcode 0x6D"),
            0x6E => unimplemented!("Opcode 0x6E"),
            0x6F => unimplemented!("Opcode 0x6F"),
            0x70 => unimplemented!("Opcode 0x70"),
            0x71 => unimplemented!("Opcode 0x71"),
            0x72 => unimplemented!("Opcode 0x72"),
            0x73 => unimplemented!("Opcode 0x73"),
            0x74 => unimplemented!("Opcode 0x74"),
            0x75 => unimplemented!("Opcode 0x75"),
            0x76 => unimplemented!("Opcode 0x76"),
            0x77 => unimplemented!("Opcode 0x77"),
            0x78 => unimplemented!("Opcode 0x78"),
            0x79 => unimplemented!("Opcode 0x79"),
            0x7A => unimplemented!("Opcode 0x7A"),
            0x7B => unimplemented!("Opcode 0x7B"),
            0x7C => unimplemented!("Opcode 0x7C"),
            0x7D => unimplemented!("Opcode 0x7D"),
            0x7E => unimplemented!("Opcode 0x7E"),
            0x7F => unimplemented!("Opcode 0x7F"),
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
            // SET b,B
            0xC0 => {
                let b = self.fetch_byte();
                self.reg.b |= 1 << b;
                8
            }
            // SET b,C
            0xC1 => {
                let b = self.fetch_byte();
                self.reg.c |= 1 << b;
                8
            }
            // SET b,D
            0xC2 => {
                let b = self.fetch_byte();
                self.reg.d |= 1 << b;
                8
            }
            // SET b,E
            0xC3 => {
                let b = self.fetch_byte();
                self.reg.e |= 1 << b;
                8
            }
            // SET b,H
            0xC4 => {
                let b = self.fetch_byte();
                self.reg.h |= 1 << b;
                8
            }
            // SET b,L
            0xC5 => {
                let b = self.fetch_byte();
                self.reg.l |= 1 << b;
                8
            }
            // SET b,(HL)
            0xC6 => {
                let b = self.fetch_byte();
                let hl_byte = self.read_hl_byte() | (1 << b);
                self.write_hl_byte(hl_byte);
                16
            }
            // SET b,A
            0xC7 => {
                let b = self.fetch_byte();
                self.reg.a |= 1 << b;
                8
            }
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
