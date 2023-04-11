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
