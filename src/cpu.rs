#![allow(dead_code)]
use crate::{instructions::*, mmu::*, registers::*, utils::*};

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

    fn debug_step(&self, opcode: u8, inst: Inst) {
        println!("==========STEP==========");
        println!("exec {:#04x}", opcode);
        println!("{:?}", inst);
        println!("{}", self.reg);
        println!("clock: {}", self.clock);
        println!("\n\n\n\n");
    }
}

impl CPU {
    pub fn run(&mut self) {
        loop {
            // Fetch
            let opcode = self.fetch_byte();
            // Decode
            let inst = self.decode(opcode);
            self.debug_step(opcode, inst);
            // Execute
            self.execute(inst);
            self.clock += inst.cycles();
        }
    }

    fn decode(&mut self, opcode: u8) -> Inst {
        match opcode {
            0x00 => Inst::NOP(4),                                              // NOP
            0x01 => Inst::LD16(Dst16::BC, Src16::NN, 12),                      // LD BC,d16
            0x02 => Inst::LD8(Dst8::Addr(self.reg.bc()), Src8::A, 8),          // LD (BC),A
            0x03 => Inst::INC16(Dst16::BC, 8),                                 // INC BC
            0x04 => Inst::INC(Dst8::B, 4),                                     // INC B
            0x05 => Inst::DEC(Dst8::B, 4),                                     // DEC B
            0x06 => Inst::LD8(Dst8::B, Src8::N, 8),                            // LD B,d8
            0x07 => Inst::RLCA(4),                                             // RLCA
            0x08 => Inst::LD16(Dst16::Addr(self.fetch_word()), Src16::SP, 20), // LD (a16),SP
            0x09 => Inst::ADD16(Src16::BC, 8),                                 // ADD HL,BC
            0x0A => Inst::LD8(Dst8::A, Src8::Addr(self.reg.bc()), 8),          // LD A,(BC)
            0x0B => Inst::DEC16(Dst16::BC, 8),                                 // DEC BC
            0x0C => Inst::INC(Dst8::C, 4),                                     // INC C
            0x0D => Inst::DEC(Dst8::C, 4),                                     // DEC C
            0x0E => Inst::LD8(Dst8::C, Src8::N, 8),                            // LD C,d8
            0x0F => Inst::RRCA(4),                                             // RRCA
            0x10 => Inst::STOP(4),                                             // STOP
            0x11 => Inst::LD16(Dst16::DE, Src16::NN, 12),                      // LD DE,d16
            0x12 => Inst::LD8(Dst8::Addr(self.reg.de()), Src8::A, 8),          // LD (DE),A
            0x13 => Inst::INC16(Dst16::DE, 8),                                 // INC DE
            0x14 => Inst::INC(Dst8::D, 4),                                     // INC D
            0x15 => Inst::DEC(Dst8::D, 4),                                     // DEC D
            0x16 => Inst::LD8(Dst8::D, Src8::N, 8),                            // LD D,d8
            0x17 => Inst::RLA(4),                                              // RLA
            0x18 => Inst::JP(Src16::Addr(self.reg.pc + (self.fetch_byte() as u16)), 12), // JR r8
            0x19 => Inst::ADD16(Src16::DE, 8),                                 // ADD HL,DE
            0x1A => Inst::LD8(Dst8::A, Src8::Addr(self.reg.de()), 8),          // LD A,(DE)
            0x1B => Inst::DEC16(Dst16::DE, 8),                                 // DEC DE
            0x1C => Inst::INC(Dst8::E, 4),                                     // INC E
            0x1D => Inst::DEC(Dst8::E, 4),                                     // DEC E
            0x1E => Inst::LD8(Dst8::E, Src8::N, 8),                            // LD E,d8
            0x1F => Inst::RRA(4),                                              // RRA
            0x20 => Inst::JPC(
                Src16::Addr(self.reg.pc + (self.fetch_byte() as u16)),
                Flags::Z,
                false,
                8,
            ), // JR NZ,r8
            0x21 => Inst::LD16(Dst16::HL, Src16::NN, 12),                      // LD HL,d16
            0x22 => unimplemented!("{:#04x}", opcode),                         // LD (HL+),A
            0x23 => Inst::INC16(Dst16::HL, 8),                                 // INC HL
            0x24 => Inst::INC(Dst8::H, 4),                                     // INC H
            0x25 => Inst::DEC(Dst8::H, 4),                                     // DEC H
            0x26 => Inst::LD8(Dst8::H, Src8::N, 8),                            // LD H,d8
            0x27 => Inst::DAA(4),                                              // DAA
            0x28 => Inst::JPC(
                Src16::Addr(self.reg.pc + (self.fetch_byte() as u16)),
                Flags::Z,
                true,
                8,
            ), // JR Z,r8
            0x29 => Inst::ADD16(Src16::HL, 8),                                 // ADD HL,HL
            0x2A => unimplemented!("{:#04x}", opcode),                         // LD A,(HL+)
            0x2B => Inst::DEC16(Dst16::HL, 8),                                 // DEC HL
            0x2C => Inst::INC(Dst8::L, 4),                                     // INC L
            0x2D => Inst::DEC(Dst8::L, 4),                                     // DEC L
            0x2E => Inst::LD8(Dst8::L, Src8::N, 8),                            // LD L,d8
            0x2F => Inst::CPL(4),                                              // CPL
            0x30 => Inst::JPC(
                Src16::Addr(self.reg.pc + (self.fetch_byte() as u16)),
                Flags::C,
                false,
                8,
            ), // JR NC,r8
            0x31 => Inst::LD16(Dst16::SP, Src16::NN, 12),                      // LD SP,d16
            0x32 => unimplemented!("{:#04x}", opcode),                         // LD (HL-),A
            0x33 => Inst::INC16(Dst16::SP, 8),                                 // INC SP
            0x34 => Inst::INC(Dst8::HLContents, 12),                           // INC (HL)
            0x35 => Inst::DEC(Dst8::HLContents, 12),                           // DEC (HL)
            0x36 => Inst::LD8(Dst8::HLContents, Src8::N, 12),                  // LD (HL),d8
            0x37 => Inst::SCF(4),                                              // SCF
            0x38 => Inst::JPC(
                Src16::Addr(self.reg.pc + (self.fetch_byte() as u16)),
                Flags::C,
                true,
                8,
            ), // JR C,r8
            0x39 => Inst::ADD16(Src16::SP, 8),                                 // ADD HL,SP
            0x3A => unimplemented!("{:#04x}", opcode),                         // LD A,(HL-)
            0x3B => Inst::DEC16(Dst16::SP, 8),                                 // DEC SP
            0x3C => Inst::INC(Dst8::A, 4),                                     // INC A
            0x3D => Inst::DEC(Dst8::A, 4),                                     // DEC A
            0x3E => Inst::LD8(Dst8::A, Src8::N, 8),                            // LD A,d8
            0x3F => Inst::CCF(4),                                              // CCF
            0x40 => Inst::LD8(Dst8::B, Src8::B, 4),                            // LD B,B
            0x41 => Inst::LD8(Dst8::B, Src8::C, 4),                            // LD B,C
            0x42 => Inst::LD8(Dst8::B, Src8::D, 4),                            // LD B,D
            0x43 => Inst::LD8(Dst8::B, Src8::E, 4),                            // LD B,E
            0x44 => Inst::LD8(Dst8::B, Src8::H, 4),                            // LD B,H
            0x45 => Inst::LD8(Dst8::B, Src8::L, 4),                            // LD B,L
            0x46 => Inst::LD8(Dst8::B, Src8::HLContents, 8),                   // LD B,(HL)
            0x47 => Inst::LD8(Dst8::B, Src8::A, 4),                            // LD B,A
            0x48 => Inst::LD8(Dst8::C, Src8::B, 4),                            // LD C,B
            0x49 => Inst::LD8(Dst8::C, Src8::C, 4),                            // LD C,C
            0x4A => Inst::LD8(Dst8::C, Src8::D, 4),                            // LD C,D
            0x4B => Inst::LD8(Dst8::C, Src8::E, 4),                            // LD C,E
            0x4C => Inst::LD8(Dst8::C, Src8::H, 4),                            // LD C,H
            0x4D => Inst::LD8(Dst8::C, Src8::L, 4),                            // LD C,L
            0x4E => Inst::LD8(Dst8::C, Src8::HLContents, 8),                   // LD C,(HL)
            0x4F => Inst::LD8(Dst8::C, Src8::A, 4),                            // LD C,A
            0x50 => Inst::LD8(Dst8::D, Src8::B, 4),                            // LD D,B
            0x51 => Inst::LD8(Dst8::D, Src8::C, 4),                            // LD D,C
            0x52 => Inst::LD8(Dst8::D, Src8::D, 4),                            // LD D,D
            0x53 => Inst::LD8(Dst8::D, Src8::E, 4),                            // LD D,E
            0x54 => Inst::LD8(Dst8::D, Src8::H, 4),                            // LD D,H
            0x55 => Inst::LD8(Dst8::D, Src8::L, 4),                            // LD D,L
            0x56 => Inst::LD8(Dst8::D, Src8::HLContents, 8),                   // LD D,(HL)
            0x57 => Inst::LD8(Dst8::D, Src8::A, 4),                            // LD D,A
            0x58 => Inst::LD8(Dst8::E, Src8::B, 4),                            // LD E,B
            0x59 => Inst::LD8(Dst8::E, Src8::C, 4),                            // LD E,C
            0x5A => Inst::LD8(Dst8::E, Src8::D, 4),                            // LD E,D
            0x5B => Inst::LD8(Dst8::E, Src8::E, 4),                            // LD E,E
            0x5C => Inst::LD8(Dst8::E, Src8::H, 4),                            // LD E,H
            0x5D => Inst::LD8(Dst8::E, Src8::L, 4),                            // LD E,L
            0x5E => Inst::LD8(Dst8::E, Src8::HLContents, 8),                   // LD E,(HL)
            0x5F => Inst::LD8(Dst8::E, Src8::A, 4),                            // LD E,A
            0x60 => Inst::LD8(Dst8::H, Src8::B, 4),                            // LD H,B
            0x61 => Inst::LD8(Dst8::H, Src8::C, 4),                            // LD H,C
            0x62 => Inst::LD8(Dst8::H, Src8::D, 4),                            // LD H,D
            0x63 => Inst::LD8(Dst8::H, Src8::E, 4),                            // LD H,E
            0x64 => Inst::LD8(Dst8::H, Src8::H, 4),                            // LD H,H
            0x65 => Inst::LD8(Dst8::H, Src8::L, 4),                            // LD H,L
            0x66 => Inst::LD8(Dst8::H, Src8::HLContents, 8),                   // LD H,(HL)
            0x67 => Inst::LD8(Dst8::H, Src8::A, 4),                            // LD H,A
            0x68 => Inst::LD8(Dst8::L, Src8::B, 4),                            // LD L,B
            0x69 => Inst::LD8(Dst8::L, Src8::C, 4),                            // LD L,C
            0x6A => Inst::LD8(Dst8::L, Src8::D, 4),                            // LD L,D
            0x6B => Inst::LD8(Dst8::L, Src8::E, 4),                            // LD L,E
            0x6C => Inst::LD8(Dst8::L, Src8::H, 4),                            // LD L,H
            0x6D => Inst::LD8(Dst8::L, Src8::L, 4),                            // LD L,L
            0x6E => Inst::LD8(Dst8::L, Src8::HLContents, 8),                   // LD L,(HL)
            0x6F => Inst::LD8(Dst8::L, Src8::A, 4),                            // LD L,A
            0x70 => Inst::LD8(Dst8::HLContents, Src8::B, 8),                   // LD (HL),B
            0x71 => Inst::LD8(Dst8::HLContents, Src8::C, 8),                   // LD (HL),C
            0x72 => Inst::LD8(Dst8::HLContents, Src8::D, 8),                   // LD (HL),D
            0x73 => Inst::LD8(Dst8::HLContents, Src8::E, 8),                   // LD (HL),E
            0x74 => Inst::LD8(Dst8::HLContents, Src8::H, 8),                   // LD (HL),H
            0x75 => Inst::LD8(Dst8::HLContents, Src8::L, 8),                   // LD (HL),L
            0x76 => Inst::HALT(4),                                             // HALT
            0x77 => Inst::LD8(Dst8::HLContents, Src8::A, 8),                   // LD (HL),A
            0x78 => Inst::LD8(Dst8::A, Src8::B, 4),                            // LD A,B
            0x79 => Inst::LD8(Dst8::A, Src8::C, 4),                            // LD A,C
            0x7A => Inst::LD8(Dst8::A, Src8::D, 4),                            // LD A,D
            0x7B => Inst::LD8(Dst8::A, Src8::E, 4),                            // LD A,E
            0x7C => Inst::LD8(Dst8::A, Src8::H, 4),                            // LD A,H
            0x7D => Inst::LD8(Dst8::A, Src8::L, 4),                            // LD A,L
            0x7E => Inst::LD8(Dst8::A, Src8::HLContents, 8),                   // LD A,(HL)
            0x7F => Inst::LD8(Dst8::A, Src8::A, 4),                            // LD A,A
            0x80 => Inst::ADD(Src8::B, 4),                                     // ADD A,B
            0x81 => Inst::ADD(Src8::C, 4),                                     // ADD A,C
            0x82 => Inst::ADD(Src8::D, 4),                                     // ADD A,D
            0x83 => Inst::ADD(Src8::E, 4),                                     // ADD A,E
            0x84 => Inst::ADD(Src8::H, 4),                                     // ADD A,H
            0x85 => Inst::ADD(Src8::L, 4),                                     // ADD A,L
            0x86 => Inst::ADD(Src8::HLContents, 8),                            // ADD A,(HL)
            0x87 => Inst::ADD(Src8::A, 4),                                     // ADD A,A
            0x88 => Inst::ADC(Src8::B, 4),                                     // ADC A,B
            0x89 => Inst::ADC(Src8::C, 4),                                     // ADC A,C
            0x8A => Inst::ADC(Src8::D, 4),                                     // ADC A,D
            0x8B => Inst::ADC(Src8::E, 4),                                     // ADC A,E
            0x8C => Inst::ADC(Src8::H, 4),                                     // ADC A,H
            0x8D => Inst::ADC(Src8::L, 4),                                     // ADC A,L
            0x8E => Inst::ADC(Src8::HLContents, 8),                            // ADC A,(HL)
            0x8F => Inst::ADC(Src8::A, 4),                                     // ADC A,A
            0x90 => Inst::SUB(Src8::B, 4),                                     // SUB B
            0x91 => Inst::SUB(Src8::C, 4),                                     // SUB C
            0x92 => Inst::SUB(Src8::D, 4),                                     // SUB D
            0x93 => Inst::SUB(Src8::E, 4),                                     // SUB E
            0x94 => Inst::SUB(Src8::H, 4),                                     // SUB H
            0x95 => Inst::SUB(Src8::L, 4),                                     // SUB L
            0x96 => Inst::SUB(Src8::HLContents, 8),                            // SUB (HL)
            0x97 => Inst::SUB(Src8::A, 4),                                     // SUB A
            0x98 => Inst::SBC(Src8::B, 4),                                     // SBC A,B
            0x99 => Inst::SBC(Src8::C, 4),                                     // SBC A,C
            0x9A => Inst::SBC(Src8::D, 4),                                     // SBC A,D
            0x9B => Inst::SBC(Src8::E, 4),                                     // SBC A,E
            0x9C => Inst::SBC(Src8::H, 4),                                     // SBC A,H
            0x9D => Inst::SBC(Src8::L, 4),                                     // SBC A,L
            0x9E => Inst::SBC(Src8::HLContents, 8),                            // SBC A,(HL)
            0x9F => Inst::SBC(Src8::A, 4),                                     // SBC A,A
            0xA0 => Inst::AND(Src8::B, 4),                                     // AND B
            0xA1 => Inst::AND(Src8::C, 4),                                     // AND C
            0xA2 => Inst::AND(Src8::D, 4),                                     // AND D
            0xA3 => Inst::AND(Src8::E, 4),                                     // AND E
            0xA4 => Inst::AND(Src8::H, 4),                                     // AND H
            0xA5 => Inst::AND(Src8::L, 4),                                     // AND L
            0xA6 => Inst::AND(Src8::HLContents, 8),                            // AND (HL)
            0xA7 => Inst::AND(Src8::A, 4),                                     // AND A
            0xA8 => Inst::XOR(Src8::B, 4),                                     // XOR B
            0xA9 => Inst::XOR(Src8::C, 4),                                     // XOR C
            0xAA => Inst::XOR(Src8::D, 4),                                     // XOR D
            0xAB => Inst::XOR(Src8::E, 4),                                     // XOR E
            0xAC => Inst::XOR(Src8::H, 4),                                     // XOR H
            0xAD => Inst::XOR(Src8::L, 4),                                     // XOR L
            0xAE => Inst::XOR(Src8::HLContents, 8),                            // XOR (HL)
            0xAF => Inst::XOR(Src8::A, 4),                                     // XOR A
            0xB0 => Inst::OR(Src8::B, 4),                                      // OR B
            0xB1 => Inst::OR(Src8::C, 4),                                      // OR C
            0xB2 => Inst::OR(Src8::D, 4),                                      // OR D
            0xB3 => Inst::OR(Src8::E, 4),                                      // OR E
            0xB4 => Inst::OR(Src8::H, 4),                                      // OR H
            0xB5 => Inst::OR(Src8::L, 4),                                      // OR L
            0xB6 => Inst::OR(Src8::HLContents, 8),                             // OR (HL)
            0xB7 => Inst::OR(Src8::A, 4),                                      // OR A
            0xB8 => Inst::CP(Src8::B, 4),                                      // CP B
            0xB9 => Inst::CP(Src8::C, 4),                                      // CP C
            0xBA => Inst::CP(Src8::D, 4),                                      // CP D
            0xBB => Inst::CP(Src8::E, 4),                                      // CP E
            0xBC => Inst::CP(Src8::H, 4),                                      // CP H
            0xBD => Inst::CP(Src8::L, 4),                                      // CP L
            0xBE => Inst::CP(Src8::HLContents, 8),                             // CP (HL)
            0xBF => Inst::CP(Src8::A, 4),                                      // CP A
            0xC0 => Inst::RETC(Flags::Z, false, 8),                            // RET NZ
            0xC1 => Inst::POP(Dst16::BC, 12),                                  // POP BC
            0xC2 => Inst::JPC(Src16::NN, Flags::Z, false, 12),                 // JP NZ,a16
            0xC3 => Inst::JP(Src16::NN, 12),                                   // JP a16
            0xC4 => Inst::CALLC(Flags::Z, false, 12),                          // CALL NZ,a16
            0xC5 => Inst::PUSH(Src16::BC, 16),                                 // PUSH BC
            0xC6 => Inst::ADD(Src8::N, 8),                                     // ADD A,d8
            0xC7 => Inst::RST(0x00, 32),                                       // RST 00H
            0xC8 => Inst::RETC(Flags::Z, true, 8),                             // RET Z
            0xC9 => Inst::RET(8),                                              // RET
            0xCA => Inst::JPC(Src16::NN, Flags::Z, true, 12),                  // JP Z,a16
            0xCB => self.decode_cb(),                                          // CB prefix
            0xCC => Inst::CALLC(Flags::Z, true, 12),                           // CALL Z,a16
            0xCD => Inst::CALL(24),                                            // CALL a16
            0xCE => Inst::ADC(Src8::N, 8),                                     // ADC A,d8
            0xCF => Inst::RST(0x08, 32),                                       // RST 08H
            0xD0 => Inst::RETC(Flags::C, false, 8),                            // RET NC
            0xD1 => Inst::POP(Dst16::DE, 12),                                  // POP DE
            0xD2 => Inst::JPC(Src16::NN, Flags::C, false, 12),                 // JP NC,a16
            0xD4 => Inst::CALLC(Flags::C, false, 12),                          // CALL NC,a16
            0xD5 => Inst::PUSH(Src16::DE, 16),                                 // PUSH DE
            0xD6 => Inst::SUB(Src8::N, 8),                                     // SUB d8
            0xD7 => Inst::RST(0x10, 32),                                       // RST 10H
            0xD8 => Inst::RETC(Flags::C, true, 8),                             // RET C
            0xD9 => Inst::RETI(16),                                            // RETI
            0xDA => Inst::JPC(Src16::NN, Flags::C, true, 12),                  // JP C,a16
            0xDC => Inst::CALLC(Flags::C, true, 12),                           // CALL C,a16
            0xDE => Inst::SBC(Src8::N, 8),                                     // SBC A,d8
            0xDF => Inst::RST(0x18, 32),                                       // RST 18H
            0xE0 => unimplemented!("{:#04x}", opcode),                         // LDH (a8),A
            0xE1 => Inst::POP(Dst16::HL, 12),                                  // POP HL
            0xE2 => Inst::LD8(Dst8::Addr(0xFF00 + (self.reg.c as u16)), Src8::A, 8), // LD (C),A
            0xE5 => Inst::PUSH(Src16::HL, 16),                                 // PUSH HL
            0xE6 => Inst::AND(Src8::N, 8),                                     // AND d8
            0xE7 => Inst::RST(0x20, 32),                                       // RST 20H
            0xE8 => unimplemented!("{:#04x}", opcode),                         // ADD SP,r8
            0xE9 => Inst::JP(Src16::Addr(self.reg.hl()), 4),                   // JP (HL)
            0xEA => Inst::LD8(Dst8::Addr(self.fetch_word()), Src8::A, 16),     // LD (a16),A
            0xEE => Inst::XOR(Src8::N, 8),                                     // XOR d8
            0xEF => Inst::RST(0x28, 32),                                       // RST 28H
            0xF0 => unimplemented!("{:#04x}", opcode),                         // LDH A,(a8)
            0xF1 => Inst::POP(Dst16::AF, 12),                                  // POP AF
            0xF2 => Inst::LD8(Dst8::A, Src8::Addr(0xFF00 + (self.reg.c as u16)), 8), // LD A,(C)
            0xF3 => Inst::DI(4),                                               // DI
            0xF5 => Inst::PUSH(Src16::AF, 16),                                 // PUSH AF
            0xF6 => Inst::OR(Src8::N, 8),                                      // OR d8
            0xF7 => Inst::RST(0x30, 32),                                       // RST 30H
            0xF8 => unimplemented!("{:#04x}", opcode),                         // LD HL,SP+r8
            0xF9 => Inst::LD16(Dst16::SP, Src16::HL, 8),                       // LD SP,HL
            0xFA => Inst::LD8(Dst8::A, Src8::Addr(self.fetch_word()), 8),      // LD A,(a16)
            0xFB => Inst::EI(4),                                               // EI
            0xFE => Inst::CP(Src8::N, 8),                                      // CP d8
            0xFF => Inst::RST(0x38, 32),                                       // RST 38H
            0xD3 | 0xDB | 0xDD | 0xE3 | 0xE4 | 0xEB | 0xEC | 0xED | 0xF4 | 0xFC | 0xFD => {
                panic!("{opcode} is not a valid opcode")
            }
        }
    }

    fn decode_cb(&mut self) -> Inst {
        let opcode = self.fetch_byte();
        match opcode {
            0x00 => Inst::RLC(Dst8::B, 8),                              // RLC B
            0x01 => Inst::RLC(Dst8::C, 8),                              // RLC C
            0x02 => Inst::RLC(Dst8::D, 8),                              // RLC D
            0x03 => Inst::RLC(Dst8::E, 8),                              // RLC E
            0x04 => Inst::RLC(Dst8::H, 8),                              // RLC H
            0x05 => Inst::RLC(Dst8::L, 8),                              // RLC L
            0x06 => Inst::RLC(Dst8::HLContents, 16),                    // RLC (HL)
            0x07 => Inst::RLC(Dst8::A, 8),                              // RLC A
            0x08 => Inst::RRC(Dst8::B, 8),                              // RRC B
            0x09 => Inst::RRC(Dst8::C, 8),                              // RRC C
            0x0A => Inst::RRC(Dst8::D, 8),                              // RRC D
            0x0B => Inst::RRC(Dst8::E, 8),                              // RRC E
            0x0C => Inst::RRC(Dst8::H, 8),                              // RRC H
            0x0D => Inst::RRC(Dst8::L, 8),                              // RRC L
            0x0E => Inst::RRC(Dst8::HLContents, 16),                    // RRC (HL)
            0x0F => Inst::RRC(Dst8::A, 8),                              // RRC A
            0x10 => Inst::RL(Dst8::B, 8),                               // RL B
            0x11 => Inst::RL(Dst8::C, 8),                               // RL C
            0x12 => Inst::RL(Dst8::D, 8),                               // RL D
            0x13 => Inst::RL(Dst8::E, 8),                               // RL E
            0x14 => Inst::RL(Dst8::H, 8),                               // RL H
            0x15 => Inst::RL(Dst8::L, 8),                               // RL L
            0x16 => Inst::RL(Dst8::HLContents, 16),                     // RL (HL)
            0x17 => Inst::RL(Dst8::A, 8),                               // RL A
            0x18 => Inst::RR(Dst8::B, 8),                               // RR B
            0x19 => Inst::RR(Dst8::C, 8),                               // RR C
            0x1A => Inst::RR(Dst8::D, 8),                               // RR D
            0x1B => Inst::RR(Dst8::E, 8),                               // RR E
            0x1C => Inst::RR(Dst8::H, 8),                               // RR H
            0x1D => Inst::RR(Dst8::L, 8),                               // RR L
            0x1E => Inst::RR(Dst8::HLContents, 16),                     // RR (HL)
            0x1F => Inst::RR(Dst8::A, 8),                               // RR A
            0x20 => Inst::SLA(Dst8::B, 8),                              // SLA B
            0x21 => Inst::SLA(Dst8::C, 8),                              // SLA C
            0x22 => Inst::SLA(Dst8::D, 8),                              // SLA D
            0x23 => Inst::SLA(Dst8::E, 8),                              // SLA E
            0x24 => Inst::SLA(Dst8::H, 8),                              // SLA H
            0x25 => Inst::SLA(Dst8::L, 8),                              // SLA L
            0x26 => Inst::SLA(Dst8::HLContents, 16),                    // SLA (HL)
            0x27 => Inst::SLA(Dst8::A, 8),                              // SLA A
            0x28 => Inst::SRA(Dst8::B, 8),                              // SRA B
            0x29 => Inst::SRA(Dst8::C, 8),                              // SRA C
            0x2A => Inst::SRA(Dst8::D, 8),                              // SRA D
            0x2B => Inst::SRA(Dst8::E, 8),                              // SRA E
            0x2C => Inst::SRA(Dst8::H, 8),                              // SRA H
            0x2D => Inst::SRA(Dst8::L, 8),                              // SRA L
            0x2E => Inst::SRA(Dst8::HLContents, 16),                    // SRA (HL)
            0x2F => Inst::SRA(Dst8::A, 8),                              // SRA A
            0x30 => Inst::SWAP(Dst8::B, 8),                             // SWAP B
            0x31 => Inst::SWAP(Dst8::C, 8),                             // SWAP C
            0x32 => Inst::SWAP(Dst8::D, 8),                             // SWAP D
            0x33 => Inst::SWAP(Dst8::E, 8),                             // SWAP E
            0x34 => Inst::SWAP(Dst8::H, 8),                             // SWAP H
            0x35 => Inst::SWAP(Dst8::L, 8),                             // SWAP L
            0x36 => Inst::SWAP(Dst8::HLContents, 16),                   // SWAP (HL)
            0x37 => Inst::SWAP(Dst8::A, 8),                             // SWAP A
            0x38 => Inst::SRL(Dst8::B, 8),                              // SRL B
            0x39 => Inst::SRL(Dst8::C, 8),                              // SRL C
            0x3A => Inst::SRL(Dst8::D, 8),                              // SRL D
            0x3B => Inst::SRL(Dst8::E, 8),                              // SRL E
            0x3C => Inst::SRL(Dst8::H, 8),                              // SRL H
            0x3D => Inst::SRL(Dst8::L, 8),                              // SRL L
            0x3E => Inst::SRL(Dst8::HLContents, 16),                    // SRL (HL)
            0x3F => Inst::SRL(Dst8::A, 8),                              // SRL A
            0x40 => Inst::BIT(self.fetch_byte(), Src8::B, 8),           // BIT b, B
            0x41 => Inst::BIT(self.fetch_byte(), Src8::C, 8),           // BIT b, C
            0x42 => Inst::BIT(self.fetch_byte(), Src8::D, 8),           // BIT b, D
            0x43 => Inst::BIT(self.fetch_byte(), Src8::E, 8),           // BIT b, E
            0x44 => Inst::BIT(self.fetch_byte(), Src8::H, 8),           // BIT b, H
            0x45 => Inst::BIT(self.fetch_byte(), Src8::L, 8),           // BIT b, L
            0x46 => Inst::BIT(self.fetch_byte(), Src8::HLContents, 16), // BIT b, (HL)
            0x47 => Inst::BIT(self.fetch_byte(), Src8::A, 8),           // BIT b, A
            0x48 => Inst::BIT(1, Src8::B, 8),                           // BIT 1, B
            0x49 => Inst::BIT(1, Src8::C, 8),                           // BIT 1, C
            0x4A => Inst::BIT(1, Src8::D, 8),                           // BIT 1, D
            0x4B => Inst::BIT(1, Src8::E, 8),                           // BIT 1, E
            0x4C => Inst::BIT(1, Src8::H, 8),                           // BIT 1, H
            0x4D => Inst::BIT(1, Src8::L, 8),                           // BIT 1, L
            0x4E => Inst::BIT(1, Src8::HLContents, 16),                 // BIT 1, (HL)
            0x4F => Inst::BIT(1, Src8::A, 8),                           // BIT 1, A
            0x50 => Inst::BIT(2, Src8::B, 8),                           // BIT 2, B
            0x51 => Inst::BIT(2, Src8::C, 8),                           // BIT 2, C
            0x52 => Inst::BIT(2, Src8::D, 8),                           // BIT 2, D
            0x53 => Inst::BIT(2, Src8::E, 8),                           // BIT 2, E
            0x54 => Inst::BIT(2, Src8::H, 8),                           // BIT 2, H
            0x55 => Inst::BIT(2, Src8::L, 8),                           // BIT 2, L
            0x56 => Inst::BIT(2, Src8::HLContents, 16),                 // BIT 2, (HL)
            0x57 => Inst::BIT(2, Src8::A, 8),                           // BIT 2, A
            0x58 => Inst::BIT(3, Src8::B, 8),                           // BIT 3, B
            0x59 => Inst::BIT(3, Src8::C, 8),                           // BIT 3, C
            0x5A => Inst::BIT(3, Src8::D, 8),                           // BIT 3, D
            0x5B => Inst::BIT(3, Src8::E, 8),                           // BIT 3, E
            0x5C => Inst::BIT(3, Src8::H, 8),                           // BIT 3, H
            0x5D => Inst::BIT(3, Src8::L, 8),                           // BIT 3, L
            0x5E => Inst::BIT(3, Src8::HLContents, 16),                 // BIT 3, (HL)
            0x5F => Inst::BIT(3, Src8::A, 8),                           // BIT 3, A
            0x60 => Inst::BIT(4, Src8::B, 8),                           // BIT 4, B
            0x61 => Inst::BIT(4, Src8::C, 8),                           // BIT 4, C
            0x62 => Inst::BIT(4, Src8::D, 8),                           // BIT 4, D
            0x63 => Inst::BIT(4, Src8::E, 8),                           // BIT 4, E
            0x64 => Inst::BIT(4, Src8::H, 8),                           // BIT 4, H
            0x65 => Inst::BIT(4, Src8::L, 8),                           // BIT 4, L
            0x66 => Inst::BIT(4, Src8::HLContents, 16),                 // BIT 4, (HL)
            0x67 => Inst::BIT(4, Src8::A, 8),                           // BIT 4, A
            0x68 => Inst::BIT(5, Src8::B, 8),                           // BIT 5, B
            0x69 => Inst::BIT(5, Src8::C, 8),                           // BIT 5, C
            0x6A => Inst::BIT(5, Src8::D, 8),                           // BIT 5, D
            0x6B => Inst::BIT(5, Src8::E, 8),                           // BIT 5, E
            0x6C => Inst::BIT(5, Src8::H, 8),                           // BIT 5, H
            0x6D => Inst::BIT(5, Src8::L, 8),                           // BIT 5, L
            0x6E => Inst::BIT(5, Src8::HLContents, 16),                 // BIT 5, (HL)
            0x6F => Inst::BIT(5, Src8::A, 8),                           // BIT 5, A
            0x70 => Inst::BIT(6, Src8::B, 8),                           // BIT 6, B
            0x71 => Inst::BIT(6, Src8::C, 8),                           // BIT 6, C
            0x72 => Inst::BIT(6, Src8::D, 8),                           // BIT 6, D
            0x73 => Inst::BIT(6, Src8::E, 8),                           // BIT 6, E
            0x74 => Inst::BIT(6, Src8::H, 8),                           // BIT 6, H
            0x75 => Inst::BIT(6, Src8::L, 8),                           // BIT 6, L
            0x76 => Inst::BIT(6, Src8::HLContents, 16),                 // BIT 6, (HL)
            0x77 => Inst::BIT(6, Src8::A, 8),                           // BIT 6, A
            0x78 => Inst::BIT(7, Src8::B, 8),                           // BIT 7, B
            0x79 => Inst::BIT(7, Src8::C, 8),                           // BIT 7, C
            0x7A => Inst::BIT(7, Src8::D, 8),                           // BIT 7, D
            0x7B => Inst::BIT(7, Src8::E, 8),                           // BIT 7, E
            0x7C => Inst::BIT(7, Src8::H, 8),                           // BIT 7, H
            0x7D => Inst::BIT(7, Src8::L, 8),                           // BIT 7, L
            0x7E => Inst::BIT(7, Src8::HLContents, 16),                 // BIT 7, (HL)
            0x7F => Inst::BIT(7, Src8::A, 8),                           // BIT 7, A
            0x80 => Inst::RES(0, Dst8::B, 8),                           // RES 0, B
            0x81 => Inst::RES(0, Dst8::C, 8),                           // RES 0, C
            0x82 => Inst::RES(0, Dst8::D, 8),                           // RES 0, D
            0x83 => Inst::RES(0, Dst8::E, 8),                           // RES 0, E
            0x84 => Inst::RES(0, Dst8::H, 8),                           // RES 0, H
            0x85 => Inst::RES(0, Dst8::L, 8),                           // RES 0, L
            0x86 => Inst::RES(0, Dst8::HLContents, 16),                 // RES 0, (HL)
            0x87 => Inst::RES(0, Dst8::A, 8),                           // RES 0, A
            0x88 => Inst::RES(1, Dst8::B, 8),                           // RES 1, B
            0x89 => Inst::RES(1, Dst8::C, 8),                           // RES 1, C
            0x8A => Inst::RES(1, Dst8::D, 8),                           // RES 1, D
            0x8B => Inst::RES(1, Dst8::E, 8),                           // RES 1, E
            0x8C => Inst::RES(1, Dst8::H, 8),                           // RES 1, H
            0x8D => Inst::RES(1, Dst8::L, 8),                           // RES 1, L
            0x8E => Inst::RES(1, Dst8::HLContents, 16),                 // RES 1, (HL)
            0x8F => Inst::RES(1, Dst8::A, 8),                           // RES 1, A
            0x90 => Inst::RES(2, Dst8::B, 8),                           // RES 2, B
            0x91 => Inst::RES(2, Dst8::C, 8),                           // RES 2, C
            0x92 => Inst::RES(2, Dst8::D, 8),                           // RES 2, D
            0x93 => Inst::RES(2, Dst8::E, 8),                           // RES 2, E
            0x94 => Inst::RES(2, Dst8::H, 8),                           // RES 2, H
            0x95 => Inst::RES(2, Dst8::L, 8),                           // RES 2, L
            0x96 => Inst::RES(2, Dst8::HLContents, 16),                 // RES 2, (HL)
            0x97 => Inst::RES(2, Dst8::A, 8),                           // RES 2, A
            0x98 => Inst::RES(3, Dst8::B, 8),                           // RES 3, B
            0x99 => Inst::RES(3, Dst8::C, 8),                           // RES 3, C
            0x9A => Inst::RES(3, Dst8::D, 8),                           // RES 3, D
            0x9B => Inst::RES(3, Dst8::E, 8),                           // RES 3, E
            0x9C => Inst::RES(3, Dst8::H, 8),                           // RES 3, H
            0x9D => Inst::RES(3, Dst8::L, 8),                           // RES 3, L
            0x9E => Inst::RES(3, Dst8::HLContents, 16),                 // RES 3, (HL)
            0x9F => Inst::RES(3, Dst8::A, 8),                           // RES 3, A
            0xA0 => Inst::RES(4, Dst8::B, 8),                           // RES 4, B
            0xA1 => Inst::RES(4, Dst8::C, 8),                           // RES 4, C
            0xA2 => Inst::RES(4, Dst8::D, 8),                           // RES 4, D
            0xA3 => Inst::RES(4, Dst8::E, 8),                           // RES 4, E
            0xA4 => Inst::RES(4, Dst8::H, 8),                           // RES 4, H
            0xA5 => Inst::RES(4, Dst8::L, 8),                           // RES 4, L
            0xA6 => Inst::RES(4, Dst8::HLContents, 16),                 // RES 4, (HL)
            0xA7 => Inst::RES(4, Dst8::A, 8),                           // RES 4, A
            0xA8 => Inst::RES(5, Dst8::B, 8),                           // RES 5, B
            0xA9 => Inst::RES(5, Dst8::C, 8),                           // RES 5, C
            0xAA => Inst::RES(5, Dst8::D, 8),                           // RES 5, D
            0xAB => Inst::RES(5, Dst8::E, 8),                           // RES 5, E
            0xAC => Inst::RES(5, Dst8::H, 8),                           // RES 5, H
            0xAD => Inst::RES(5, Dst8::L, 8),                           // RES 5, L
            0xAE => Inst::RES(5, Dst8::HLContents, 16),                 // RES 5, (HL)
            0xAF => Inst::RES(5, Dst8::A, 8),                           // RES 5, A
            0xB0 => Inst::RES(6, Dst8::B, 8),                           // RES 6, B
            0xB1 => Inst::RES(6, Dst8::C, 8),                           // RES 6, C
            0xB2 => Inst::RES(6, Dst8::D, 8),                           // RES 6, D
            0xB3 => Inst::RES(6, Dst8::E, 8),                           // RES 6, E
            0xB4 => Inst::RES(6, Dst8::H, 8),                           // RES 6, H
            0xB5 => Inst::RES(6, Dst8::L, 8),                           // RES 6, L
            0xB6 => Inst::RES(6, Dst8::HLContents, 16),                 // RES 6, (HL)
            0xB7 => Inst::RES(6, Dst8::A, 8),                           // RES 6, A
            0xB8 => Inst::RES(7, Dst8::B, 8),                           // RES 7, B
            0xB9 => Inst::RES(7, Dst8::C, 8),                           // RES 7, C
            0xBA => Inst::RES(7, Dst8::D, 8),                           // RES 7, D
            0xBB => Inst::RES(7, Dst8::E, 8),                           // RES 7, E
            0xBC => Inst::RES(7, Dst8::H, 8),                           // RES 7, H
            0xBD => Inst::RES(7, Dst8::L, 8),                           // RES 7, L
            0xBE => Inst::RES(7, Dst8::HLContents, 16),                 // RES 7, (HL)
            0xBF => Inst::RES(7, Dst8::A, 8),                           // RES 7, A
            0xC0 => Inst::SETN(Dst8::B, 8),                             // SET b, B
            0xC1 => Inst::SETN(Dst8::C, 8),                             // SET b, C
            0xC2 => Inst::SETN(Dst8::D, 8),                             // SET b, D
            0xC3 => Inst::SETN(Dst8::E, 8),                             // SET b, E
            0xC4 => Inst::SETN(Dst8::H, 8),                             // SET b, H
            0xC5 => Inst::SETN(Dst8::L, 8),                             // SET b, L
            0xC6 => Inst::SETN(Dst8::HLContents, 16),                   // SET b,(HL)
            0xC7 => Inst::SETN(Dst8::A, 8),                             // SET b,A
            0xC8 => Inst::SET(1, Dst8::B, 8),                           // SET 1, B
            0xC9 => Inst::SET(1, Dst8::C, 8),                           // SET 1, C
            0xCA => Inst::SET(1, Dst8::D, 8),                           // SET 1, D
            0xCB => Inst::SET(1, Dst8::E, 8),                           // SET 1, E
            0xCC => Inst::SET(1, Dst8::H, 8),                           // SET 1, H
            0xCD => Inst::SET(1, Dst8::L, 8),                           // SET 1, L
            0xCE => Inst::SET(1, Dst8::HLContents, 16),                 // SET 1, (HL)
            0xCF => Inst::SET(1, Dst8::A, 8),                           // SET 1, A
            0xD0 => Inst::SET(2, Dst8::B, 8),                           // SET 2, B
            0xD1 => Inst::SET(2, Dst8::C, 8),                           // SET 2, C
            0xD2 => Inst::SET(2, Dst8::D, 8),                           // SET 2, D
            0xD3 => Inst::SET(2, Dst8::E, 8),                           // SET 2, E
            0xD4 => Inst::SET(2, Dst8::H, 8),                           // SET 2, H
            0xD5 => Inst::SET(2, Dst8::L, 8),                           // SET 2, L
            0xD6 => Inst::SET(2, Dst8::HLContents, 16),                 // SET 2, (HL)
            0xD7 => Inst::SET(2, Dst8::A, 8),                           // SET 2, A
            0xD8 => Inst::SET(3, Dst8::B, 8),                           // SET 3, B
            0xD9 => Inst::SET(3, Dst8::C, 8),                           // SET 3, C
            0xDA => Inst::SET(3, Dst8::D, 8),                           // SET 3, D
            0xDB => Inst::SET(3, Dst8::E, 8),                           // SET 3, E
            0xDC => Inst::SET(3, Dst8::H, 8),                           // SET 3, H
            0xDD => Inst::SET(3, Dst8::L, 8),                           // SET 3, L
            0xDE => Inst::SET(3, Dst8::HLContents, 16),                 // SET 3, (HL)
            0xDF => Inst::SET(3, Dst8::A, 8),                           // SET 3, A
            0xE0 => Inst::SET(4, Dst8::B, 8),                           // SET 4, B
            0xE1 => Inst::SET(4, Dst8::C, 8),                           // SET 4, C
            0xE2 => Inst::SET(4, Dst8::D, 8),                           // SET 4, D
            0xE3 => Inst::SET(4, Dst8::E, 8),                           // SET 4, E
            0xE4 => Inst::SET(4, Dst8::H, 8),                           // SET 4, H
            0xE5 => Inst::SET(4, Dst8::L, 8),                           // SET 4, L
            0xE6 => Inst::SET(4, Dst8::HLContents, 16),                 // SET 4, (HL)
            0xE7 => Inst::SET(4, Dst8::A, 8),                           // SET 4, A
            0xE8 => Inst::SET(5, Dst8::B, 8),                           // SET 5, B
            0xE9 => Inst::SET(5, Dst8::C, 8),                           // SET 5, C
            0xEA => Inst::SET(5, Dst8::D, 8),                           // SET 5, D
            0xEB => Inst::SET(5, Dst8::E, 8),                           // SET 5, E
            0xEC => Inst::SET(5, Dst8::H, 8),                           // SET 5, H
            0xED => Inst::SET(5, Dst8::L, 8),                           // SET 5, L
            0xEE => Inst::SET(5, Dst8::HLContents, 16),                 // SET 5, (HL)
            0xEF => Inst::SET(5, Dst8::A, 8),                           // SET 5, A
            0xF0 => Inst::SET(6, Dst8::B, 8),                           // SET 6, B
            0xF1 => Inst::SET(6, Dst8::C, 8),                           // SET 6, C
            0xF2 => Inst::SET(6, Dst8::D, 8),                           // SET 6, D
            0xF3 => Inst::SET(6, Dst8::E, 8),                           // SET 6, E
            0xF4 => Inst::SET(6, Dst8::H, 8),                           // SET 6, H
            0xF5 => Inst::SET(6, Dst8::L, 8),                           // SET 6, L
            0xF6 => Inst::SET(6, Dst8::HLContents, 16),                 // SET 6, (HL)
            0xF7 => Inst::SET(6, Dst8::A, 8),                           // SET 6, A
            0xF8 => Inst::SET(7, Dst8::B, 8),                           // SET 7, B
            0xF9 => Inst::SET(7, Dst8::C, 8),                           // SET 7, C
            0xFA => Inst::SET(7, Dst8::D, 8),                           // SET 7, D
            0xFB => Inst::SET(7, Dst8::E, 8),                           // SET 7, E
            0xFC => Inst::SET(7, Dst8::H, 8),                           // SET 7, H
            0xFD => Inst::SET(7, Dst8::L, 8),                           // SET 7, L
            0xFE => Inst::SET(7, Dst8::HLContents, 16),                 // SET 7, (HL)
            0xFF => Inst::SET(7, Dst8::A, 8),                           // SET 7, A
        }
    }

    pub fn execute(&mut self, inst: Inst) -> u32 {
        let mut cycles = inst.cycles();
        match inst {
            Inst::LD8(dst, src, _) => {
                let s = self.get_8(src);
                self.set_8(dst, s);
            }
            Inst::LDD(_) => todo!(),
            Inst::LDI(_, _, _) => todo!(),
            Inst::LD16(dst, src, _) => {
                let s = self.get_16(src);
                self.set_16(dst, s);
            }
            Inst::PUSH(src, _) => {
                match src {
                    Src16::NN | Src16::Addr(_) => {
                        panic!("Tried to push a non-register onto the stack")
                    }
                    _ => {}
                }
                let s = self.get_16(src);
                self.push_stack(s);
            }
            Inst::POP(dst, _) => {
                let popped_value = self.pop_stack();
                self.set_16(dst, popped_value);
            }
            Inst::ADD(src, _) => self.alu_add(src),
            Inst::ADC(src, _) => self.alu_adc(src),
            Inst::SUB(src, _) => self.alu_sub(src),
            Inst::SBC(src, _) => self.alu_sbc(src),
            Inst::AND(src, _) => self.alu_and(src),
            Inst::OR(src, _) => self.alu_or(src),
            Inst::XOR(src, _) => self.alu_xor(src),
            Inst::CP(src, _) => self.alu_cp(src),
            Inst::INC(_, _) => todo!(),
            Inst::DEC(_, _) => todo!(),
            Inst::ADD16(src, _) => self.alu_add_16(src),
            Inst::INC16(_, _) => todo!(),
            Inst::DEC16(_, _) => todo!(),
            Inst::SWAP(dst, _) => todo!(),
            Inst::DAA(_) => todo!(),
            Inst::CPL(_) => {
                // bitwise-complement operator (equivalent to '~' in C)
                self.reg.a = !self.reg.a;
                self.reg.flag(Flags::N, true);
                self.reg.flag(Flags::H, true);
            }
            Inst::CCF(_) => {
                self.reg.flag(Flags::N, false);
                self.reg.flag(Flags::H, false);
                let old_flag = self.reg.check_flag(Flags::C);
                self.reg.flag(Flags::C, !old_flag);
            }
            Inst::SCF(_) => {
                self.reg.flag(Flags::N, false);
                self.reg.flag(Flags::H, false);
                self.reg.flag(Flags::C, true);
            }
            Inst::NOP(_) => {}
            Inst::HALT(_) => self.halted = true,
            Inst::STOP(_) => {
                self.halted = true;
                self.stopped = true;
            }
            Inst::DI(_) => todo!(),
            Inst::EI(_) => todo!(),
            Inst::RLCA(_) => {
                // TODO: look this over again
                self.reg.flag(Flags::C, (self.reg.a & 0b1000) == 0b1000);
                self.reg.a <<= 1;
                // Don't think we can be clever and inline this. Don't want to reset Zero flag
                if self.reg.a == 0 {
                    self.reg.flag(Flags::Z, true);
                }
            }
            Inst::RLA(_) => todo!(),
            // TODO: look at how the zero flag is set
            Inst::RRCA(_) => {
                self.reg.flag(Flags::C, (self.reg.a & 0b0001) == 0b0001);
                self.reg.a >>= 1;
                if self.reg.a == 0 {
                    self.reg.flag(Flags::Z, true);
                }
            }
            Inst::RRA(_) => {
                let old_bit_0 = self.reg.a & 0x1;
                self.reg.a >>= 1;
                self.reg.flag(Flags::Z, self.reg.a == 0);
                self.reg.flag(Flags::N, false);
                self.reg.flag(Flags::H, false);
                self.reg.flag(Flags::C, old_bit_0 == 1);
            }
            Inst::RLC(_, _) => todo!(),
            Inst::RL(_, _) => todo!(),
            Inst::RRC(_, _) => todo!(),
            Inst::RR(_, _) => todo!(),
            Inst::SLA(_, _) => todo!(),
            Inst::SRA(_, _) => todo!(),
            Inst::SRL(_, _) => todo!(),
            Inst::BIT(b, test, _) => self.cb_bit(test, b),
            Inst::SETN(_, _) => todo!(),
            Inst::SET(_, _, _) => todo!(),
            Inst::RES(b, dst, _) => self.cb_reset(dst, b),
            Inst::JP(src, _) => self.jp(src),
            Inst::JPC(src, flag, desired_state, _) => {
                if self.reg.check_flag(flag) == desired_state {
                    self.jp(src);
                    cycles += 4
                }
            }
            Inst::CALL(_) => self.call(),
            Inst::CALLC(flag, desired_state, _) => {
                if self.reg.check_flag(flag) == desired_state {
                    self.call();
                    cycles += 12;
                }
            }
            Inst::RST(incr, _) => {
                // TODO: enum?
                let valid_incrs = vec![0x00, 0x08, 0x10, 0x18, 0x20, 0x28, 0x30, 0x38];
                assert!(
                    valid_incrs.contains(&incr),
                    "Tried to restart with a bad value"
                );
                self.push_stack(self.reg.pc);
                self.reg.pc = 0x0000 + (incr as u16);
            }
            Inst::RET(_) => {
                self.ret();
            }
            Inst::RETC(flag, desired_state, _) => {
                if self.reg.check_flag(flag) == desired_state {
                    self.ret();
                    cycles += 12;
                }
            }
            Inst::RETI(_) => todo!(),
        }
        cycles
    }
}

// Jump ops
impl CPU {
    fn push_stack(&mut self, addr: u16) {
        self.mmu.write_word(self.reg.sp, addr);
        self.reg.sp -= 2;
    }

    fn pop_stack(&mut self) -> u16 {
        let addr = self.mmu.read_word(self.reg.sp);
        self.reg.sp += 2;
        addr
    }

    fn jp(&mut self, src: Src16) {
        let addr = match src {
            Src16::NN | Src16::Addr(_) => self.get_16(src),
            _ => {
                panic!("Tried to jump without an address as the source.")
            }
        };
        self.reg.pc = addr;
    }

    /// Pop two bytes from stack and jump to that address
    fn ret(&mut self) {
        let addr = self.pop_stack();
        self.reg.pc = addr;
    }

    /// Push present address onto stack. Jump to address 0x0000 + n.
    fn rst(&mut self, n: u8) {
        self.push_stack(self.reg.pc);
        self.reg.pc = n.into();
    }

    /// Push address of next instruction onto stack and then jump to address nn
    fn call(&mut self) {
        // FIX: Will need to increment pc depending where I do that
        self.push_stack(self.reg.pc);
        let addr = self.fetch_word();
        self.reg.pc = addr;
    }
}

// alu ops
impl CPU {
    fn alu_add(&mut self, src: Src8) {
        let val = self.get_8(src);
        todo!()
    }

    fn alu_add_16(&mut self, src: Src16) {
        let val = self.get_16(src);
        let hl = self.reg.hl();
        let res = hl.wrapping_add(val);
        self.reg.flag(Flags::N, false);
        self.reg.flag(Flags::H, half_carry_16(hl, val));
        self.reg.flag(Flags::C, carry_16(hl, val));
        self.reg.set_hl(res);
    }

    fn alu_adc(&mut self, src: Src8) {
        let val = self.get_8(src);
        let addend = val.wrapping_add(1);
        let res = self.reg.a.wrapping_add(addend);
        self.reg.flag(Flags::Z, res == 0);
        self.reg.flag(Flags::N, false);
        // FIX: this seems sus
        self.reg.flag(Flags::H, half_carry(res, addend));
        self.reg.flag(Flags::C, carry(res, addend));
        self.reg.a = res;
    }

    fn alu_sub(&mut self, src: Src8) {
        let val = self.get_8(src);
        let res = self.reg.a.wrapping_sub(val);
        self.reg.flag(Flags::Z, res == 0);
        self.reg.flag(Flags::N, true);
        // FIX: this is temporary until i figure out how carry works with subtraction
        self.reg.flag(Flags::H, false);
        self.reg.flag(Flags::C, false);
    }

    fn alu_sbc(&mut self, _src: Src8) {
        todo!("alu sbc")
    }

    /// AND reg A with `val`
    fn alu_and(&mut self, src: Src8) {
        let val = self.get_8(src);
        let res = self.reg.a & val;
        self.reg.flag(Flags::Z, res == 0);
        self.reg.flag(Flags::N, false);
        self.reg.flag(Flags::H, true);
        self.reg.flag(Flags::C, false);
        self.reg.a = res;
    }

    /// OR reg A with `val`
    fn alu_or(&mut self, src: Src8) {
        let val = self.get_8(src);
        let res = self.reg.a | val;
        self.reg.flag(Flags::Z, res == 0);
        self.reg.flag(Flags::N, false);
        self.reg.flag(Flags::H, false);
        self.reg.flag(Flags::C, false);
        self.reg.a = res;
    }

    /// XOR reg A with `val`
    fn alu_xor(&mut self, src: Src8) {
        let val = self.get_8(src);
        let res = self.reg.a ^ val;
        self.reg.flag(Flags::Z, res == 0);
        self.reg.flag(Flags::N, false);
        self.reg.flag(Flags::H, false);
        self.reg.flag(Flags::C, false);
        self.reg.a = res;
    }

    fn alu_cp(&mut self, src: Src8) {
        let val = self.get_8(src);
        self.reg.flag(Flags::Z, self.reg.a == val);
        self.reg.flag(Flags::N, true);
        self.reg.flag(Flags::H, half_carry(val, val));
        self.reg.flag(Flags::C, self.reg.a < val);
    }

    fn alu_inc(&mut self, val: u8) -> u8 {
        let res = val.wrapping_add(1);
        self.reg.flag(Flags::Z, res == 0);
        self.reg.flag(Flags::N, false);
        self.reg.flag(Flags::H, half_carry(val, 1));
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
        self.reg.flag(Flags::H, half_carry(val, 0xFF));
        res
    }

    fn alu_dec_16(&mut self, val: u16) -> u16 {
        let res = val.wrapping_sub(1);
        self.reg.flag(Flags::Z, res == 0);
        self.reg.flag(Flags::N, true);
        // FIX: is this okay?
        self.reg.flag(Flags::H, half_carry_16(val, 0xFF));
        res
    }
}

// 3.3.7: Bit Opcodes
impl CPU {
    /// Test bit b in register `reg`
    fn cb_bit(&mut self, src: Src8, bit: u8) {
        assert!(bit <= 7);
        let reg = self.get_8(src);
        let bit_set = reg & (1 << bit);
        self.reg.flag(Flags::Z, bit_set == 0);
        self.reg.flag(Flags::N, false);
        self.reg.flag(Flags::H, true);
    }

    /// Test bit b in register `reg`
    fn cb_set(&mut self, src: Src8, bit: u8) {
        assert!(bit <= 7);
        let reg = self.get_8(src);
        let bit_set = reg & (1 << bit);
        self.reg.flag(Flags::Z, bit_set == 0);
        self.reg.flag(Flags::N, false);
        self.reg.flag(Flags::H, true);
    }

    /// Reset bit b in register `reg`
    fn cb_reset(&mut self, _dst: Dst8, bit: u8) {
        assert!(bit <= 7);
        todo!()
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
}

impl CPU {
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
            Src16::SP => self.reg.sp,
            Src16::N => self.fetch_byte().into(),
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
            Dst16::SP => self.reg.sp = val,
            Dst16::Addr(addr) => self.mmu.write_word(addr, val),
        }
    }
}
