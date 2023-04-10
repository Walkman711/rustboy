#![allow(dead_code)]
use crate::{mmu::*, registers::*};

#[derive(Debug)]
pub struct CPU {
    reg: Registers,
    // TODO: is this 32 bit? also does it matter?
    clock: u32,
    mmu: MMU,
    halted: bool,
    stopped: bool,
    interrupts_enabled: bool,
    disable_interrupts_in: u16,
    enable_interrupts_in: u16,
}

impl CPU {
    pub fn new(rom: &str) -> Self {
        Self {
            reg: Registers::default(),
            clock: 0,
            mmu: MMU::new(rom),
            halted: false,
            stopped: false,
            interrupts_enabled: true,
            disable_interrupts_in: 99,
            enable_interrupts_in: 99,
        }
    }

    fn debug_step(&self, opcode: u8) {
        println!("==========STEP==========");
        println!("exec {:#02x}", opcode);
        println!("{}", self.reg);
        println!("clock: {}", self.clock);
        println!("\n\n\n\n");
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Instruction {
    // 3.3.1: 8-bit loads
    LD8(Dst8, Src8, u32),
    LDD(Dst8, Src8, u32),
    LDI(Dst8, Src8, u32),
    // 3.3.2: 16-bit loads
    LD16(Dst16, Src16, u32),
    PUSH(Src16, u32),
    POP(Dst16, u32),
    // 3.3.3: 8-bit ALU ops
    ADD(Src8, u32),
    ADC(Src8, u32),
    SUB(Src8, u32),
    SBC(Src8, u32),
    AND(Src8, u32),
    OR(Src8, u32),
    XOR(Src8, u32),
    CP(Src8, u32),
    INC(Dst8, u32),
    DEC(Dst8, u32),
    // 3.3.4: 16-bit ALU ops
    ADD16(Dst16, Src16, u32),
    INC16(Dst16, u32),
    DEC16(Dst16, u32),
    // 3.3.5: Misc
    SWAP(Dst8, u32),
    DAA(u32),
    CPL(u32),
    CCF(u32),
    SCF(u32),
    NOP(u32),
    HALT(u32),
    STOP(u32),
    DI(u32),
    EI(u32),
    // 3.3.6: Rotates & Shifts
    RLCA(u32),
    RLA(u32),
    RRCA(u32),
    RRA(u32),
    RLC(Dst8, u32),
    RL(Dst8, u32),
    RRC(Dst8, u32),
    RR(Dst8, u32),
    SLA(Dst8, u32),
    SRA(Dst8, u32),
    SRL(Dst8, u32),
    // 3.3.7: Bit Opcodes
    BIT(u8, Dst8, u32),
    SET(u8, Dst8, u32),
    RES(u8, Dst8, u32),
    // 3.3.8: Jumps
    JP(Src16, u32),
    JPC(Src16, Flags, bool, u32),
    // 3.3.9: Calls
    CALL(u16, Flags, bool, u32),
    // 3.3.10: Restarts
    RST(u8, u32),
    // 3.3.11: Returns
    RET(u32),
    // TODO: consolidate with RET
    RETC(Flags, bool, u32),
    RETI(u32),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Src8 {
    A,
    F,
    B,
    C,
    D,
    E,
    H,
    L,
    N,
    HLContents,
    Addr(u16),
}

// Same as Src8 except for cutting out the immediate one byte option.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Dst8 {
    A,
    F,
    B,
    C,
    D,
    E,
    H,
    L,
    HLContents,
    Addr(u16),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Src16 {
    AF,
    BC,
    DE,
    HL,
    NN,
    Addr(u16),
}

// Same as Dst16 except for cutting out the immediate one word option.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Dst16 {
    AF,
    BC,
    DE,
    HL,
    Addr(u16),
}

impl CPU {
    pub fn run(&mut self) {
        loop {
            // Fetch
            let opcode = self.fetch_byte();
            self.debug_step(opcode);
            // let inst = self.decode(opcode);
            // dbg!(inst);
            // Decode & Execute
            let cycles_elapsed: u32 = self.call(opcode).into();
            self.clock += cycles_elapsed;
        }
    }

    fn get_8(&mut self, src: Src8) -> u8 {
        match src {
            Src8::A => self.reg.a,
            Src8::F => todo!(),
            Src8::B => self.reg.b,
            Src8::C => self.reg.c,
            Src8::D => self.reg.d,
            Src8::E => self.reg.e,
            Src8::H => self.reg.h,
            Src8::L => self.reg.l,
            Src8::N => self.fetch_byte(),
            Src8::HLContents => self.read_hl_byte(),
            Src8::Addr(addr) => self.mmu.read_byte(addr),
        }
    }

    fn set_8(&mut self, dst: Dst8, val: u8) {
        match dst {
            Dst8::A => self.reg.a = val,
            Dst8::F => self.reg.f = FlagRegister::from(val),
            Dst8::B => self.reg.b = val,
            Dst8::C => self.reg.c = val,
            Dst8::D => self.reg.d = val,
            Dst8::E => self.reg.e = val,
            Dst8::H => self.reg.h = val,
            Dst8::L => self.reg.l = val,
            Dst8::HLContents => self.write_hl_byte(val),
            Dst8::Addr(addr) => self.mmu.write_byte(addr, val),
        }
    }

    fn get_16(&mut self, src: Src16) -> u16 {
        match src {
            Src16::AF => self.reg.af(),
            Src16::BC => self.reg.bc(),
            Src16::DE => self.reg.de(),
            Src16::HL => self.reg.hl(),
            Src16::NN => self.fetch_word(),
            Src16::Addr(addr) => self.mmu.read_word(addr),
        }
    }

    fn set_16(&mut self, dst: Dst16, val: u16) {
        match dst {
            Dst16::AF => self.reg.set_af(val),
            Dst16::BC => self.reg.set_bc(val),
            Dst16::DE => self.reg.set_de(val),
            Dst16::HL => self.reg.set_hl(val),
            Dst16::Addr(addr) => self.mmu.write_word(addr, val),
        }
    }

    // Instruction::NOP(_) => {}
    // Instruction::LD16(dst, src, _) => {
    //     let s = self.get_16(src);
    //     self.set_16(dst, s);
    // }
    // _ => unimplemented!(),
    pub fn execute(&mut self, inst: Instruction) {
        match inst {
            Instruction::LD8(dst, src, _) => {
                let s = self.get_8(src);
                self.set_8(dst, s);
            }
            Instruction::LDD(_, _, _) => todo!(),
            Instruction::LDI(_, _, _) => todo!(),
            Instruction::LD16(dst, src, _) => {
                let s = self.get_16(src);
                self.set_16(dst, s);
            }
            Instruction::PUSH(src, _) => {
                match src {
                    Src16::AF | Src16::BC | Src16::DE | Src16::HL => {}
                    Src16::NN | Src16::Addr(_) => {
                        panic!("Tried to push a non-register onto the stack")
                    }
                }
                let s = self.get_16(src);
                self.push_stack(s);
            }
            Instruction::POP(dst, _) => {
                let popped_value = self.pop_stack();
                self.set_16(dst, popped_value);
            }
            Instruction::ADD(src, _) => {
                let s = self.get_8(src);
                self.alu_add(s);
            }
            Instruction::ADC(src, _) => {
                let s = self.get_8(src);
                self.alu_adc(s);
            }
            Instruction::SUB(src, _) => {
                let s = self.get_8(src);
                self.alu_sub(s);
            }
            Instruction::SBC(src, _) => {
                let s = self.get_8(src);
                self.alu_sbc(s);
            }
            Instruction::AND(src, _) => {
                let s = self.get_8(src);
                self.alu_and(s);
            }
            Instruction::OR(src, _) => {
                let s = self.get_8(src);
                self.alu_or(s);
            }
            Instruction::XOR(src, _) => {
                let s = self.get_8(src);
                self.alu_xor(s);
            }
            Instruction::CP(_, _) => todo!(),
            Instruction::INC(_, _) => todo!(),
            Instruction::DEC(_, _) => todo!(),
            Instruction::ADD16(_, _, _) => todo!(),
            Instruction::INC16(_, _) => todo!(),
            Instruction::DEC16(_, _) => todo!(),
            Instruction::SWAP(_, _) => todo!(),
            Instruction::DAA(_) => todo!(),
            Instruction::CPL(_) => {
                // bitwise-complement operator (equivalent to '~' in C)
                self.reg.a = !self.reg.a;
                self.reg.flag(Flags::N, true);
                self.reg.flag(Flags::H, true);
            }
            Instruction::CCF(_) => {
                self.reg.flag(Flags::N, false);
                self.reg.flag(Flags::H, false);
                self.reg.flag(Flags::C, !self.reg.carry_flag_set());
            }
            Instruction::SCF(_) => {
                self.reg.flag(Flags::N, false);
                self.reg.flag(Flags::H, false);
                self.reg.flag(Flags::C, true);
            }
            Instruction::NOP(_) => {}
            Instruction::HALT(_) => self.halted = true,
            Instruction::STOP(_) => {
                self.halted = true;
                self.stopped = true;
            }
            Instruction::DI(_) => todo!(),
            Instruction::EI(_) => todo!(),
            Instruction::RLCA(_) => {
                // TODO: look this over again
                self.reg.flag(Flags::C, (self.reg.a & 0b1000) == 0b1000);
                self.reg.a <<= 1;
                // Don't think we can be clever and inline this. Don't want to reset Zero flag
                if self.reg.a == 0 {
                    self.reg.flag(Flags::Z, true);
                }
            }
            Instruction::RLA(_) => todo!(),
            // TODO: look at how the zero flag is set
            Instruction::RRCA(_) => {
                self.reg.flag(Flags::C, (self.reg.a & 0b0001) == 0b0001);
                self.reg.a >>= 1;
                if self.reg.a == 0 {
                    self.reg.flag(Flags::Z, true);
                }
            }
            Instruction::RRA(_) => {
                let old_bit_0 = self.reg.a & 0x1;
                self.reg.a >>= 1;
                self.reg.flag(Flags::Z, self.reg.a == 0);
                self.reg.flag(Flags::N, false);
                self.reg.flag(Flags::H, false);
                self.reg.flag(Flags::C, old_bit_0 == 1);
            }
            Instruction::RLC(_, _) => todo!(),
            Instruction::RL(_, _) => todo!(),
            Instruction::RRC(_, _) => todo!(),
            Instruction::RR(_, _) => todo!(),
            Instruction::SLA(_, _) => todo!(),
            Instruction::SRA(_, _) => todo!(),
            Instruction::SRL(_, _) => todo!(),
            Instruction::BIT(_, _, _) => todo!(),
            Instruction::SET(_, _, _) => todo!(),
            Instruction::RES(_, _, _) => todo!(),
            Instruction::JP(src, _) => {
                if let Src16::Addr(addr) = src {
                    self.reg.pc = addr;
                } else {
                    panic!("Tried to execute a jump without an address as the source")
                }
            }
            Instruction::JPC(src, flag_to_check, desired_state, _) => {
                if let Src16::Addr(addr) = src {
                    let flag_val = match flag_to_check {
                        Flags::Z => self.reg.zero_flag_set(),
                        Flags::N => self.reg.subtract_flag_set(),
                        Flags::H => self.reg.half_carry_flag_set(),
                        Flags::C => self.reg.carry_flag_set(),
                    };
                    if flag_val == desired_state {
                        self.reg.pc = addr;
                    }
                } else {
                    panic!("Tried to execute a conditional jump without an address as the source")
                }
            }
            Instruction::CALL(_, _, _, _) => todo!(),
            Instruction::RST(incr, _) => {
                let valid_incrs = vec![0x00, 0x08, 0x10, 0x18, 0x20, 0x28, 0x30, 0x38];
                assert!(
                    valid_incrs.contains(&incr),
                    "Tried to restart with a bad value"
                );
                self.push_stack(self.reg.pc);
                self.reg.pc = 0x0000 + (incr as u16);
            }
            // TODO: pull out and consolidate with RETC
            Instruction::RET(_) => {
                let addr = self.pop_stack();
                self.reg.pc = addr;
            }
            Instruction::RETC(flag_to_check, desired_state, _) => {
                // Make flag check a fn somewhere
                let flag_val = match flag_to_check {
                    Flags::Z => self.reg.zero_flag_set(),
                    Flags::N => self.reg.subtract_flag_set(),
                    Flags::H => self.reg.half_carry_flag_set(),
                    Flags::C => self.reg.carry_flag_set(),
                };
                if flag_val == desired_state {
                    let addr = self.pop_stack();
                    self.reg.pc = addr;
                }
            }
            Instruction::RETI(_) => todo!(),
        }
    }
}

// Flag ops
impl CPU {
    // Taken from gist.github/com/meganesu
    fn half_carry(a: u8, b: u8) -> bool {
        // 1) mask a and b to only look at bits 0-3.
        // 2) add them together
        // 3) check if the fourth bit is set
        (((a & 0xF) + (b & 0xF)) & (1 << 4)) == (1 << 4)
    }

    // TODO: test
    fn half_carry_16(a: u16, b: u16) -> bool {
        (((a & 0x0F00) + (b & 0x0F00)) & (1 << 12)) == (1 << 12)
    }

    fn carry(a: u8, b: u8) -> bool {
        let wrapped = a.wrapping_add(b);
        let unwrapped: u16 = (a as u16) + (b as u16);
        (wrapped as u16) < unwrapped
    }

    fn carry_16(a: u16, b: u16) -> bool {
        let wrapped = a.wrapping_add(b);
        let unwrapped: u32 = (a as u32) + (b as u32);
        (wrapped as u32) < unwrapped
    }
}

// alu ops
impl CPU {
    fn alu_add(&mut self, _val: u8) {
        todo!()
    }

    fn alu_add_16(&mut self, val: u16) {
        let hl = self.reg.hl();
        let res = hl.wrapping_add(val);
        self.reg.flag(Flags::N, false);
        self.reg.flag(Flags::H, Self::half_carry_16(hl, val));
        self.reg.flag(Flags::C, Self::carry_16(hl, val));
        self.reg.set_hl(res);
    }

    fn alu_adc(&mut self, val: u8) {
        let addend = val.wrapping_add(1);
        let res = self.reg.a.wrapping_add(addend);
        self.reg.flag(Flags::Z, res == 0);
        self.reg.flag(Flags::N, false);
        // FIX: this seems sus
        self.reg.flag(Flags::H, Self::half_carry(res, addend));
        self.reg.flag(Flags::C, Self::carry(res, addend));
        self.reg.a = res;
    }

    fn alu_sub(&mut self, val: u8) {
        let res = self.reg.a.wrapping_sub(val);
        self.reg.flag(Flags::Z, res == 0);
        self.reg.flag(Flags::N, true);
        // FIX: this is temporary until i figure out how carry works with subtraction
        self.reg.flag(Flags::H, false);
        self.reg.flag(Flags::C, false);
    }

    fn alu_sbc(&mut self, _val: u8) {
        todo!("alu sbc")
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
        self.reg.flag(Flags::H, Self::half_carry(val, val));
        self.reg.flag(Flags::C, self.reg.a < val);
    }

    fn alu_inc(&mut self, val: u8) -> u8 {
        let res = val.wrapping_add(1);
        self.reg.flag(Flags::Z, res == 0);
        self.reg.flag(Flags::N, false);
        self.reg.flag(Flags::H, Self::half_carry(val, 1));
        res
    }

    // NOTE: seems weird that this doesn't modify flags??
    fn alu_inc_16(&mut self, val: u16) -> u16 {
        val.wrapping_add(1)
    }

    fn alu_dec(&mut self, val: u8) -> u8 {
        let res = val.wrapping_sub(1);
        self.reg.flag(Flags::Z, res == 0);
        self.reg.flag(Flags::N, true);
        // FIX: is this okay?
        self.reg.flag(Flags::H, Self::half_carry(val, 0xFF));
        res
    }

    fn alu_dec_16(&mut self, val: u16) -> u16 {
        let res = val.wrapping_sub(1);
        self.reg.flag(Flags::Z, res == 0);
        self.reg.flag(Flags::N, true);
        // FIX: is this okay?
        self.reg.flag(Flags::H, Self::half_carry_16(val, 0xFF));
        res
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
        assert!(bit <= 7);
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
    fn call(&mut self, opcode: u8) -> u32 {
        match opcode {
            // NOP
            0x00 => 4,
            // LD BC,nn
            0x01 => {
                let word = self.fetch_word();
                self.reg.set_bc(word);
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
            // ADD HL,BC
            0x09 => {
                self.alu_add_16(self.reg.bc());
                8
            }
            // LD A,(BC)
            0x0A => {
                self.reg.a = self.mmu.read_byte(self.reg.bc());
                8
            }
            // DEC BC
            0x0B => {
                let dec = self.alu_dec_16(self.reg.bc());
                self.reg.set_bc(dec);
                8
            }
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
            // LD (DE),A
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
            // RLA
            0x17 => unimplemented!("Opcode 0x17"),
            // JR n
            0x18 => {
                self.reg.pc += self.fetch_byte() as u16;
                8
            }
            // ADD HL,DE
            0x19 => {
                self.alu_add_16(self.reg.de());
                8
            }
            // LD A,(DE)
            0x1A => {
                self.reg.a = self.mmu.read_byte(self.reg.de());
                8
            }
            // DEC DE
            0x1B => {
                let dec = self.alu_dec_16(self.reg.de());
                self.reg.set_de(dec);
                8
            }
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
            // RRA
            0x1F => {
                let old_bit_0 = self.reg.a & 0x1;
                self.reg.a >>= 1;
                self.reg.flag(Flags::Z, self.reg.a == 0);
                self.reg.flag(Flags::N, false);
                self.reg.flag(Flags::H, false);
                self.reg.flag(Flags::C, old_bit_0 == 1);
                4
            }
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
            // LD (HL+),A
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
            // DAA
            0x27 => unimplemented!("Opcode 0x27"),
            // JR Z,n
            0x28 => {
                if self.reg.zero_flag_set() {
                    self.reg.pc += self.fetch_byte() as u16;
                }
                8
            }
            // ADD HL,HL
            0x29 => {
                self.alu_add_16(self.reg.hl());
                8
            }
            // LD A,(HL+)
            0x2A => unimplemented!("Opcode 0x2A"),
            // DEC HL
            0x2B => {
                let dec = self.alu_dec_16(self.reg.hl());
                self.reg.set_hl(dec);
                8
            }
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
                let hl_dec = self.alu_dec_16(self.reg.hl());
                self.reg.set_hl(hl_dec);
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
            // ADD HL,SP
            0x39 => {
                self.alu_add_16(self.reg.sp);
                8
            }
            // LDD A,(HL)
            0x3A => {
                self.reg.a = self.read_hl_byte();
                let hl_byte = self.alu_dec_16(self.reg.hl());
                self.reg.set_hl(hl_byte);
                8
            }
            // DEC SP
            0x3B => {
                let dec = self.alu_dec_16(self.reg.sp);
                self.reg.sp = dec;
                8
            }
            // INC A
            0x3C => {
                self.reg.a = self.alu_inc(self.reg.a);
                4
            }
            // DEC A
            0x3D => {
                self.reg.a = self.alu_dec(self.reg.a);
                4
            }
            // LD A,d8
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
            // LD D,B
            0x50 => {
                self.reg.d = self.reg.b;
                4
            }
            // LD D,C
            0x51 => {
                self.reg.d = self.reg.c;
                4
            }
            // LD D,D
            0x52 => {
                self.reg.d = self.reg.d;
                4
            }
            // LD D,E
            0x53 => {
                self.reg.d = self.reg.e;
                4
            }
            // LD D,H
            0x54 => {
                self.reg.d = self.reg.h;
                4
            }
            // LD D,L
            0x55 => {
                self.reg.d = self.reg.l;
                4
            }
            // LD D,(HL)
            0x56 => {
                self.reg.d = self.read_hl_byte();
                8
            }
            // LD D,A
            0x57 => {
                self.reg.d = self.reg.a;
                4
            }
            // LD E,B
            0x58 => {
                self.reg.e = self.reg.b;
                4
            }
            // LD E,C
            0x59 => {
                self.reg.e = self.reg.c;
                4
            }
            // LD E,D
            0x5A => {
                self.reg.e = self.reg.d;
                4
            }
            // LD E,E
            0x5B => {
                self.reg.e = self.reg.e;
                4
            }
            // LD E,H
            0x5C => {
                self.reg.e = self.reg.h;
                4
            }
            // LD E,L
            0x5D => {
                self.reg.e = self.reg.l;
                4
            }
            // LD E,(HL)
            0x5E => {
                self.reg.e = self.read_hl_byte();
                8
            }
            // LD E,A
            0x5F => {
                self.reg.e = self.reg.a;
                4
            }
            // LD H,B
            0x60 => {
                self.reg.h = self.reg.b;
                4
            }
            // LD H,C
            0x61 => {
                self.reg.h = self.reg.c;
                4
            }
            // LD H,D
            0x62 => {
                self.reg.h = self.reg.d;
                4
            }
            // LD H,E
            0x63 => {
                self.reg.h = self.reg.e;
                4
            }
            // LD H,H
            0x64 => {
                self.reg.h = self.reg.h;
                4
            }
            // LD H,L
            0x65 => {
                self.reg.h = self.reg.l;
                4
            }
            // LD H,(HL)
            0x66 => {
                self.reg.h = self.read_hl_byte();
                8
            }
            // LD H,A
            0x67 => {
                self.reg.h = self.reg.a;
                4
            }
            // LD L,B
            0x68 => {
                self.reg.l = self.reg.b;
                4
            }
            // LD L,C
            0x69 => {
                self.reg.l = self.reg.c;
                4
            }
            // LD L,D
            0x6A => {
                self.reg.l = self.reg.d;
                4
            }
            // LD L,E
            0x6B => {
                self.reg.l = self.reg.e;
                4
            }
            // LD L,H
            0x6C => {
                self.reg.l = self.reg.h;
                4
            }
            // LD L,L
            0x6D => {
                self.reg.l = self.reg.l;
                4
            }
            // LD L,(HL)
            0x6E => {
                self.reg.l = self.read_hl_byte();
                8
            }
            // LD L,A
            0x6F => {
                self.reg.l = self.reg.a;
                4
            }
            // LD (HL),B
            0x70 => {
                self.write_hl_byte(self.reg.b);
                8
            }
            // LD (HL),C
            0x71 => {
                self.write_hl_byte(self.reg.c);
                8
            }
            // LD (HL),D
            0x72 => {
                self.write_hl_byte(self.reg.d);
                8
            }
            // LD (HL),E
            0x73 => {
                self.write_hl_byte(self.reg.e);
                8
            }
            // LD (HL),H
            0x74 => {
                self.write_hl_byte(self.reg.h);
                8
            }
            // LD (HL),L
            0x75 => {
                self.write_hl_byte(self.reg.l);
                8
            }
            // HAL
            0x76 => {
                self.halted = true;
                4
            }
            // LD (HL),A
            0x77 => {
                self.write_hl_byte(self.reg.a);
                8
            }
            // LD A,B
            0x78 => {
                self.reg.a = self.reg.b;
                4
            }
            // LD A,C
            0x79 => {
                self.reg.a = self.reg.c;
                4
            }
            // LD A,D
            0x7A => {
                self.reg.a = self.reg.d;
                4
            }
            // LD A,E
            0x7B => {
                self.reg.a = self.reg.e;
                4
            }
            // LD A,H
            0x7C => {
                self.reg.a = self.reg.h;
                4
            }
            // LD A,L
            0x7D => {
                self.reg.a = self.reg.l;
                4
            }
            // LD A,(HL)
            0x7E => {
                self.reg.a = self.read_hl_byte();
                8
            }
            // LD A,A
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
            // AND B
            0xA0 => {
                self.alu_and(self.reg.b);
                4
            }
            // AND C
            0xA1 => {
                self.alu_and(self.reg.c);
                4
            }
            // AND D
            0xA2 => {
                self.alu_and(self.reg.d);
                4
            }
            // AND E
            0xA3 => {
                self.alu_and(self.reg.e);
                4
            }
            // AND H
            0xA4 => {
                self.alu_and(self.reg.h);
                4
            }
            // AND L
            0xA5 => {
                self.alu_and(self.reg.l);
                4
            }
            // AND (HL)
            0xA6 => {
                self.alu_and(self.read_hl_byte());
                8
            }
            // AND A
            0xA7 => {
                self.alu_and(self.reg.a);
                4
            }
            // XOR B
            0xA8 => {
                self.alu_xor(self.reg.b);
                4
            }
            // XOR C
            0xA9 => {
                self.alu_xor(self.reg.c);
                4
            }
            // XOR D
            0xAA => {
                self.alu_xor(self.reg.d);
                4
            }
            // XOR E
            0xAB => {
                self.alu_xor(self.reg.e);
                4
            }
            // XOR H
            0xAC => {
                self.alu_xor(self.reg.h);
                4
            }
            // XOR L
            0xAD => {
                self.alu_xor(self.reg.l);
                4
            }
            // XOR (HL)
            0xAE => {
                self.alu_xor(self.read_hl_byte());
                8
            }
            // XOR A
            0xAF => {
                self.alu_xor(self.reg.a);
                4
            }
            // OR B
            0xB0 => {
                self.alu_or(self.reg.b);
                4
            }
            // OR C
            0xB1 => {
                self.alu_or(self.reg.c);
                4
            }
            // OR D
            0xB2 => {
                self.alu_or(self.reg.d);
                4
            }
            // OR E
            0xB3 => {
                self.alu_or(self.reg.e);
                4
            }
            // OR H
            0xB4 => {
                self.alu_or(self.reg.h);
                4
            }
            // OR L
            0xB5 => {
                self.alu_or(self.reg.l);
                4
            }
            // OR (HL)
            0xB6 => {
                self.alu_or(self.read_hl_byte());
                8
            }
            // OR A
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
            // PUSH BC
            0xC5 => {
                self.push_stack(self.reg.bc());
                16
            }
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
            0xCB => {
                let _ = self.decode_cb();
                todo!()
            }
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
            // PUSH DE
            0xD5 => {
                self.push_stack(self.reg.de());
                16
            }
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
            // SBC A,d8
            0xDE => unimplemented!("Opcode 0xDE"),
            // RST 18H
            0xDF => {
                self.rst(0x18);
                32
            }
            // LDH (a8),A
            0xE0 => unimplemented!("Opcode 0xE0"),
            // POP HL
            0xE1 => {
                let addr = self.pop_stack();
                self.reg.set_hl(addr);
                12
            }
            // LD (C),A
            // XXX: make note that FF00 is IO?
            0xE2 => {
                self.mmu
                    .write_byte(0xFF00 + (self.reg.c as u16), self.reg.a);
                8
            }
            // PUSH HL
            0xE5 => {
                self.push_stack(self.reg.hl());
                16
            }
            // AND d8
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
            // ADD SP,r8
            0xE8 => unimplemented!("Opcode 0xE8"),
            // JP (HL)
            0xE9 => {
                self.reg.pc = self.reg.hl();
                4
            }
            // LD (a16),A
            0xEA => {
                let addr = self.fetch_word();
                self.mmu.write_byte(addr, self.reg.a);
                16
            }
            // XOR d8
            0xEE16 => {
                let n = self.fetch_byte();
                self.alu_xor(n);
                8
            }
            // RST 28H
            0xEF => {
                self.rst(0x28);
                32
            }
            // LDH A,(a8)
            0xF0 => unimplemented!("Opcode 0xF0"),
            // POP AF
            0xF1 => {
                let addr = self.pop_stack();
                self.reg.set_af(addr);
                12
            }
            // LD A,(C)
            // FIX: this looks incorrect
            0xF2 => {
                self.reg.a = self.reg.c + self.mmu.read_byte(0xFF00);
                8
            }
            // DI
            0xF3 => {
                unimplemented!();
                // self.interrupt_state = InterruptState::DisableNext;
                4
            }
            // PUSH AF
            0xF5 => unimplemented!("Opcode 0xF5"),
            // OR d8
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
            // LD HL,SP+r8
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
                unimplemented!();
                // self.interrupt_state = InterruptState::EnableNext;
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

    fn decode_cb(&mut self) -> Instruction {
        let opcode = self.fetch_byte();
        match opcode {
            0x00 => Instruction::RLC(Dst8::B, 8),            // RLC B
            0x01 => Instruction::RLC(Dst8::C, 8),            // RLC C
            0x02 => Instruction::RLC(Dst8::D, 8),            // RLC D
            0x03 => Instruction::RLC(Dst8::E, 8),            // RLC E
            0x04 => Instruction::RLC(Dst8::H, 8),            // RLC H
            0x05 => Instruction::RLC(Dst8::L, 8),            // RLC L
            0x06 => Instruction::RLC(Dst8::HLContents, 16),  // RLC (HL)
            0x07 => Instruction::RLC(Dst8::A, 8),            // RLC A
            0x08 => Instruction::RRC(Dst8::B, 8),            // RRC B
            0x09 => Instruction::RRC(Dst8::C, 8),            // RRC C
            0x0A => Instruction::RRC(Dst8::D, 8),            // RRC D
            0x0B => Instruction::RRC(Dst8::E, 8),            // RRC E
            0x0C => Instruction::RRC(Dst8::H, 8),            // RRC H
            0x0D => Instruction::RRC(Dst8::L, 8),            // RRC L
            0x0E => Instruction::RRC(Dst8::HLContents, 16),  // RRC (HL)
            0x0E => Instruction::RRC(Dst8::A, 8),            // RRC A
            0x10 => Instruction::RL(Dst8::B, 8),             // RL B
            0x11 => Instruction::RL(Dst8::C, 8),             // RL C
            0x12 => Instruction::RL(Dst8::D, 8),             // RL D
            0x13 => Instruction::RL(Dst8::E, 8),             // RL E
            0x14 => Instruction::RL(Dst8::H, 8),             // RL H
            0x15 => Instruction::RL(Dst8::L, 8),             // RL L
            0x16 => Instruction::RL(Dst8::HLContents, 16),   // RL (HL)
            0x17 => Instruction::RL(Dst8::A, 8),             // RL A
            0x18 => Instruction::RR(Dst8::B, 8),             // RR B
            0x19 => Instruction::RR(Dst8::C, 8),             // RR C
            0x1A => Instruction::RR(Dst8::D, 8),             // RR D
            0x1B => Instruction::RR(Dst8::E, 8),             // RR E
            0x1C => Instruction::RR(Dst8::H, 8),             // RR H
            0x1D => Instruction::RR(Dst8::L, 8),             // RR L
            0x1E => Instruction::RR(Dst8::HLContents, 16),   // RR (HL)
            0x1F => Instruction::RR(Dst8::A, 8),             // RR A
            0x20 => Instruction::SLA(Dst8::B, 8),            // SLA B
            0x21 => Instruction::SLA(Dst8::C, 8),            // SLA C
            0x22 => Instruction::SLA(Dst8::D, 8),            // SLA D
            0x23 => Instruction::SLA(Dst8::E, 8),            // SLA E
            0x24 => Instruction::SLA(Dst8::H, 8),            // SLA H
            0x25 => Instruction::SLA(Dst8::L, 8),            // SLA L
            0x26 => Instruction::SLA(Dst8::HLContents, 16),  // SLA (HL)
            0x27 => Instruction::SLA(Dst8::A, 8),            // SLA A
            0x28 => Instruction::SRA(Dst8::B, 8),            // SRA B
            0x29 => Instruction::SRA(Dst8::C, 8),            // SRA C
            0x2A => Instruction::SRA(Dst8::D, 8),            // SRA D
            0x2B => Instruction::SRA(Dst8::E, 8),            // SRA E
            0x2C => Instruction::SRA(Dst8::H, 8),            // SRA H
            0x2D => Instruction::SRA(Dst8::L, 8),            // SRA L
            0x2E => Instruction::SRA(Dst8::HLContents, 16),  // SRA (HL)
            0x2F => Instruction::SRA(Dst8::A, 8),            // SRA A
            0x30 => Instruction::SWAP(Dst8::B, 8),           // SWAP B
            0x31 => Instruction::SWAP(Dst8::C, 8),           // SWAP C
            0x32 => Instruction::SWAP(Dst8::D, 8),           // SWAP D
            0x33 => Instruction::SWAP(Dst8::E, 8),           // SWAP E
            0x34 => Instruction::SWAP(Dst8::H, 8),           // SWAP H
            0x35 => Instruction::SWAP(Dst8::L, 8),           // SWAP L
            0x36 => Instruction::SWAP(Dst8::HLContents, 16), // SWAP (HL)
            0x37 => Instruction::SWAP(Dst8::A, 8),           // SWAP A
            0x38 => Instruction::SRL(Dst8::B, 8),            // SRL B
            0x39 => Instruction::SRL(Dst8::C, 8),            // SRL C
            0x3A => Instruction::SRL(Dst8::D, 8),            // SRL D
            0x3B => Instruction::SRL(Dst8::E, 8),            // SRL E
            0x3C => Instruction::SRL(Dst8::H, 8),            // SRL H
            0x3D => Instruction::SRL(Dst8::L, 8),            // SRL L
            0x3E => Instruction::SRL(Dst8::HLContents, 16),  // SRL (HL)
            0x3F => Instruction::SRL(Dst8::A, 8),            // SRL A
            0x40 => {
                let b = self.fetch_byte();
                Instruction::BIT(b, Dst8::B, 8) // BIT b, B
            }
            0x41 => {
                let b = self.fetch_byte();
                Instruction::BIT(b, Dst8::C, 8) // BIT b, C
            }
            0x42 => {
                let b = self.fetch_byte();
                Instruction::BIT(b, Dst8::D, 8) // BIT b, D
            }
            0x43 => {
                let b = self.fetch_byte();
                Instruction::BIT(b, Dst8::E, 8) // BIT b, E
            }
            0x44 => {
                let b = self.fetch_byte();
                Instruction::BIT(b, Dst8::H, 8) // BIT b, H
            }
            0x45 => {
                let b = self.fetch_byte();
                Instruction::BIT(b, Dst8::L, 8) // BIT b, L
            }
            0x46 => {
                let b = self.fetch_byte();
                Instruction::BIT(b, Dst8::HLContents, 16) // BIT b, (HL)
            }
            0x47 => {
                let b = self.fetch_byte();
                Instruction::BIT(b, Dst8::A, 8) // BIT b, A
            }
            0x48 => Instruction::BIT(1, Dst8::B, 8), // BIT 1, B
            0x49 => Instruction::BIT(1, Dst8::C, 8), // BIT 1, C
            0x4A => Instruction::BIT(1, Dst8::D, 8), // BIT 1, D
            0x4B => Instruction::BIT(1, Dst8::E, 8), // BIT 1, E
            0x4C => Instruction::BIT(1, Dst8::H, 8), // BIT 1, H
            0x4D => Instruction::BIT(1, Dst8::L, 8), // BIT 1, L
            0x4E => Instruction::BIT(1, Dst8::Addr(self.reg.hl()), 16), // BIT 1, (HL)
            0x4F => Instruction::BIT(1, Dst8::A, 8), // BIT 1, A
            0x50 => Instruction::BIT(2, Dst8::B, 8), // BIT 2, B
            0x51 => Instruction::BIT(2, Dst8::C, 8), // BIT 2, C
            0x52 => Instruction::BIT(2, Dst8::D, 8), // BIT 2, D
            0x53 => Instruction::BIT(2, Dst8::E, 8), // BIT 2, E
            0x54 => Instruction::BIT(2, Dst8::H, 8), // BIT 2, H
            0x55 => Instruction::BIT(2, Dst8::L, 8), // BIT 2, L
            0x56 => Instruction::BIT(2, Dst8::Addr(self.reg.hl()), 16), // BIT 2, (HL)
            0x57 => Instruction::BIT(2, Dst8::A, 8), // BIT 2, A
            0x58 => Instruction::BIT(3, Dst8::B, 8), // BIT 3, B
            0x59 => Instruction::BIT(3, Dst8::C, 8), // BIT 3, C
            0x5A => Instruction::BIT(3, Dst8::D, 8), // BIT 3, D
            0x5B => Instruction::BIT(3, Dst8::E, 8), // BIT 3, E
            0x5C => Instruction::BIT(3, Dst8::H, 8), // BIT 3, H
            0x5D => Instruction::BIT(3, Dst8::L, 8), // BIT 3, L
            0x5E => Instruction::BIT(3, Dst8::Addr(self.reg.hl()), 16), // BIT 3, (HL)
            0x5F => Instruction::BIT(3, Dst8::A, 8), // BIT 3, A
            0x60 => Instruction::BIT(4, Dst8::B, 8), // BIT 4, B
            0x61 => Instruction::BIT(4, Dst8::C, 8), // BIT 4, C
            0x62 => Instruction::BIT(4, Dst8::D, 8), // BIT 4, D
            0x63 => Instruction::BIT(4, Dst8::E, 8), // BIT 4, E
            0x64 => Instruction::BIT(4, Dst8::H, 8), // BIT 4, H
            0x65 => Instruction::BIT(4, Dst8::L, 8), // BIT 4, L
            0x66 => Instruction::BIT(4, Dst8::Addr(self.reg.hl()), 16), // BIT 4, (HL)
            0x67 => Instruction::BIT(4, Dst8::A, 8), // BIT 4, A
            0x68 => Instruction::BIT(5, Dst8::B, 8), // BIT 5, B
            0x69 => Instruction::BIT(5, Dst8::C, 8), // BIT 5, C
            0x6A => Instruction::BIT(5, Dst8::D, 8), // BIT 5, D
            0x6B => Instruction::BIT(5, Dst8::E, 8), // BIT 5, E
            0x6C => Instruction::BIT(5, Dst8::H, 8), // BIT 5, H
            0x6D => Instruction::BIT(5, Dst8::L, 8), // BIT 5, L
            0x6E => Instruction::BIT(5, Dst8::Addr(self.reg.hl()), 16), // BIT 5, (HL)
            0x6F => Instruction::BIT(5, Dst8::A, 8), // BIT 5, A
            0x70 => Instruction::BIT(6, Dst8::B, 8), // BIT 6, B
            0x71 => Instruction::BIT(6, Dst8::C, 8), // BIT 6, C
            0x72 => Instruction::BIT(6, Dst8::D, 8), // BIT 6, D
            0x73 => Instruction::BIT(6, Dst8::E, 8), // BIT 6, E
            0x74 => Instruction::BIT(6, Dst8::H, 8), // BIT 6, H
            0x75 => Instruction::BIT(6, Dst8::L, 8), // BIT 6, L
            0x76 => Instruction::BIT(6, Dst8::Addr(self.reg.hl()), 16), // BIT 6, (HL)
            0x77 => Instruction::BIT(6, Dst8::A, 8), // BIT 6, A
            0x78 => Instruction::BIT(7, Dst8::B, 8), // BIT 7, B
            0x79 => Instruction::BIT(7, Dst8::C, 8), // BIT 7, C
            0x7A => Instruction::BIT(7, Dst8::D, 8), // BIT 7, D
            0x7B => Instruction::BIT(7, Dst8::E, 8), // BIT 7, E
            0x7C => Instruction::BIT(7, Dst8::H, 8), // BIT 7, H
            0x7D => Instruction::BIT(7, Dst8::L, 8), // BIT 7, L
            0x7E => Instruction::BIT(7, Dst8::Addr(self.reg.hl()), 16), // BIT 7, (HL)
            0x7F => Instruction::BIT(7, Dst8::A, 8), // BIT 7, A
            0x80 => Instruction::RES(0, Dst8::B, 8), // RES 0, B
            0x81 => Instruction::RES(0, Dst8::C, 8), // RES 0, C
            0x80 => Instruction::RES(0, Dst8::D, 8), // RES 0, D
            0x83 => Instruction::RES(0, Dst8::E, 8), // RES 0, E
            0x84 => Instruction::RES(0, Dst8::H, 8), // RES 0, H
            0x85 => Instruction::RES(0, Dst8::L, 8), // RES 0, L
            0x86 => Instruction::RES(0, Dst8::HLContents, 16), // RES 0, (HL)
            0x87 => Instruction::RES(0, Dst8::A, 8), // RES 0, A
            0x88 => Instruction::RES(1, Dst8::B, 8), // RES 1, B
            0x89 => Instruction::RES(1, Dst8::C, 8), // RES 1, C
            0x8A => Instruction::RES(1, Dst8::D, 8), // RES 1, D
            0x8B => Instruction::RES(1, Dst8::E, 8), // RES 1, E
            0x8C => Instruction::RES(1, Dst8::H, 8), // RES 1, H
            0x8D => Instruction::RES(1, Dst8::L, 8), // RES 1, L
            0x8E => Instruction::RES(1, Dst8::HLContents, 16), // RES 1, (HL)
            0x8F => Instruction::RES(1, Dst8::A, 8), // RES 1, A
            0x90 => Instruction::RES(2, Dst8::B, 8), // RES 2, B
            0x91 => Instruction::RES(2, Dst8::C, 8), // RES 2, C
            0x92 => Instruction::RES(2, Dst8::D, 8), // RES 2, D
            0x93 => Instruction::RES(2, Dst8::E, 8), // RES 2, E
            0x94 => Instruction::RES(2, Dst8::H, 8), // RES 2, H
            0x95 => Instruction::RES(2, Dst8::L, 8), // RES 2, L
            0x96 => Instruction::RES(2, Dst8::HLContents, 16), // RES 2, (HL)
            0x97 => Instruction::RES(2, Dst8::A, 8), // RES 2, A
            0x98 => Instruction::RES(3, Dst8::B, 8), // RES 3, B
            0x99 => Instruction::RES(3, Dst8::C, 8), // RES 3, C
            0x9A => Instruction::RES(3, Dst8::D, 8), // RES 3, D
            0x9B => Instruction::RES(3, Dst8::E, 8), // RES 3, E
            0x9C => Instruction::RES(3, Dst8::H, 8), // RES 3, H
            0x9D => Instruction::RES(3, Dst8::L, 8), // RES 3, L
            0x9E => Instruction::RES(3, Dst8::HLContents, 16), // RES 3, (HL)
            0x9F => Instruction::RES(3, Dst8::A, 8), // RES 3, A
            0xA0 => Instruction::RES(4, Dst8::B, 8), // RES 4, B
            0xA1 => Instruction::RES(4, Dst8::C, 8), // RES 4, C
            0xA2 => Instruction::RES(4, Dst8::D, 8), // RES 4, D
            0xA3 => Instruction::RES(4, Dst8::E, 8), // RES 4, E
            0xA4 => Instruction::RES(4, Dst8::H, 8), // RES 4, H
            0xA5 => Instruction::RES(4, Dst8::L, 8), // RES 4, L
            0xA6 => Instruction::RES(4, Dst8::HLContents, 16), // RES 4, (HL)
            0xA7 => Instruction::RES(4, Dst8::A, 8), // RES 4, A
            0xA8 => Instruction::RES(5, Dst8::B, 8), // RES 5, B
            0xA9 => Instruction::RES(5, Dst8::C, 8), // RES 5, C
            0xAA => Instruction::RES(5, Dst8::D, 8), // RES 5, D
            0xAB => Instruction::RES(5, Dst8::E, 8), // RES 5, E
            0xAC => Instruction::RES(5, Dst8::H, 8), // RES 5, H
            0xAD => Instruction::RES(5, Dst8::L, 8), // RES 5, L
            0xAE => Instruction::RES(5, Dst8::HLContents, 16), // RES 5, (HL)
            0xAF => Instruction::RES(5, Dst8::A, 8), // RES 5, A
            0xB0 => Instruction::RES(6, Dst8::B, 8), // RES 6, B
            0xB1 => Instruction::RES(6, Dst8::C, 8), // RES 6, C
            0xB2 => Instruction::RES(6, Dst8::D, 8), // RES 6, D
            0xB3 => Instruction::RES(6, Dst8::E, 8), // RES 6, E
            0xB4 => Instruction::RES(6, Dst8::H, 8), // RES 6, H
            0xB5 => Instruction::RES(6, Dst8::L, 8), // RES 6, L
            0xB6 => Instruction::RES(6, Dst8::HLContents, 16), // RES 6, (HL)
            0xB7 => Instruction::RES(6, Dst8::A, 8), // RES 6, A
            0xB8 => Instruction::RES(7, Dst8::B, 8), // RES 7, B
            0xB9 => Instruction::RES(7, Dst8::C, 8), // RES 7, C
            0xBA => Instruction::RES(7, Dst8::D, 8), // RES 7, D
            0xBB => Instruction::RES(7, Dst8::E, 8), // RES 7, E
            0xBC => Instruction::RES(7, Dst8::H, 8), // RES 7, H
            0xBD => Instruction::RES(7, Dst8::L, 8), // RES 7, L
            0xBE => Instruction::RES(7, Dst8::HLContents, 16), // RES 7, (HL)
            0xBF => Instruction::RES(7, Dst8::A, 8), // RES 7, A
            0xC0 => {
                let b = self.fetch_byte();
                Instruction::SET(b, Dst8::B, 8) // SET b, B
            }
            0xC1 => {
                let b = self.fetch_byte();
                Instruction::SET(b, Dst8::C, 8) // SET b, C
            }
            0xC2 => {
                let b = self.fetch_byte();
                Instruction::SET(b, Dst8::D, 8) // SET b, D
            }
            0xC3 => {
                let b = self.fetch_byte();
                Instruction::SET(b, Dst8::E, 8) // SET b, E
            }
            0xC4 => {
                let b = self.fetch_byte();
                Instruction::SET(b, Dst8::H, 8) // SET b, H
            }
            0xC5 => {
                let b = self.fetch_byte();
                Instruction::SET(b, Dst8::L, 8) // SET b, L
            }
            0xC6 => {
                let b = self.fetch_byte();
                Instruction::SET(b, Dst8::HLContents, 16) // SET b,(HL)
            }
            0xC7 => {
                let b = self.fetch_byte();
                Instruction::SET(b, Dst8::A, 8) // SET b,A
            }
            0xC8 => Instruction::SET(1, Dst8::B, 8), // SET 1, B
            0xC9 => Instruction::SET(1, Dst8::C, 8), // SET 1, C
            0xCA => Instruction::SET(1, Dst8::D, 8), // SET 1, D
            0xCB => Instruction::SET(1, Dst8::E, 8), // SET 1, E
            0xCC => Instruction::SET(1, Dst8::H, 8), // SET 1, H
            0xCD => Instruction::SET(1, Dst8::L, 8), // SET 1, L
            0xCE => Instruction::SET(1, Dst8::HLContents, 16), // SET 1, (HL)
            0xCF => Instruction::SET(1, Dst8::A, 8), // SET 1, A
            0xD0 => Instruction::SET(2, Dst8::B, 8), // SET 2, B
            0xD1 => Instruction::SET(2, Dst8::C, 8), // SET 2, C
            0xD2 => Instruction::SET(2, Dst8::D, 8), // SET 2, D
            0xD3 => Instruction::SET(2, Dst8::E, 8), // SET 2, E
            0xD4 => Instruction::SET(2, Dst8::H, 8), // SET 2, H
            0xD5 => Instruction::SET(2, Dst8::L, 8), // SET 2, L
            0xD6 => Instruction::SET(2, Dst8::HLContents, 16), // SET 2, (HL)
            0xD7 => Instruction::SET(2, Dst8::A, 8), // SET 2, A
            0xD8 => Instruction::SET(3, Dst8::B, 8), // SET 3, B
            0xD9 => Instruction::SET(3, Dst8::C, 8), // SET 3, C
            0xDA => Instruction::SET(3, Dst8::D, 8), // SET 3, D
            0xDB => Instruction::SET(3, Dst8::E, 8), // SET 3, E
            0xDC => Instruction::SET(3, Dst8::H, 8), // SET 3, H
            0xDD => Instruction::SET(3, Dst8::L, 8), // SET 3, L
            0xDE => Instruction::SET(3, Dst8::HLContents, 16), // SET 3, (HL)
            0xDF => Instruction::SET(3, Dst8::A, 8), // SET 3, A
            0xE0 => Instruction::SET(4, Dst8::B, 8), // SET 4, B
            0xE1 => Instruction::SET(4, Dst8::C, 8), // SET 4, C
            0xE2 => Instruction::SET(4, Dst8::D, 8), // SET 4, D
            0xE3 => Instruction::SET(4, Dst8::E, 8), // SET 4, E
            0xE4 => Instruction::SET(4, Dst8::H, 8), // SET 4, H
            0xE5 => Instruction::SET(4, Dst8::L, 8), // SET 4, L
            0xE6 => Instruction::SET(4, Dst8::HLContents, 16), // SET 4, (HL)
            0xE7 => Instruction::SET(4, Dst8::A, 8), // SET 4, A
            0xE8 => Instruction::SET(5, Dst8::B, 8), // SET 5, B
            0xE9 => Instruction::SET(5, Dst8::C, 8), // SET 5, C
            0xEA => Instruction::SET(5, Dst8::D, 8), // SET 5, D
            0xEB => Instruction::SET(5, Dst8::E, 8), // SET 5, E
            0xEC => Instruction::SET(5, Dst8::H, 8), // SET 5, H
            0xED => Instruction::SET(5, Dst8::L, 8), // SET 5, L
            0xEE => Instruction::SET(5, Dst8::HLContents, 16), // SET 5, (HL)
            0xEF => Instruction::SET(5, Dst8::A, 8), // SET 5, A
            0xF0 => Instruction::SET(6, Dst8::B, 8), // SET 6, B
            0xF1 => Instruction::SET(6, Dst8::C, 8), // SET 6, C
            0xF2 => Instruction::SET(6, Dst8::D, 8), // SET 6, D
            0xF3 => Instruction::SET(6, Dst8::E, 8), // SET 6, E
            0xF4 => Instruction::SET(6, Dst8::H, 8), // SET 6, H
            0xF5 => Instruction::SET(6, Dst8::L, 8), // SET 6, L
            0xF6 => Instruction::SET(6, Dst8::HLContents, 16), // SET 6, (HL)
            0xF7 => Instruction::SET(6, Dst8::A, 8), // SET 6, A
            0xF8 => Instruction::SET(7, Dst8::B, 8), // SET 7, B
            0xF9 => Instruction::SET(7, Dst8::C, 8), // SET 7, C
            0xFA => Instruction::SET(7, Dst8::D, 8), // SET 7, D
            0xFB => Instruction::SET(7, Dst8::E, 8), // SET 7, E
            0xFC => Instruction::SET(7, Dst8::H, 8), // SET 7, H
            0xFD => Instruction::SET(7, Dst8::L, 8), // SET 7, L
            0xFE => Instruction::SET(7, Dst8::HLContents, 16), // SET 7, (HL)
            0xFF => Instruction::SET(7, Dst8::A, 8), // SET 7, A
        }
    }
}
