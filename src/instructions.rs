use crate::registers::Flags;

#[derive(Copy, Clone, Debug)]
pub enum Inst {
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
    // All 16-bit adds use HL as the DST
    ADD16(Src16, u32),
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
    BIT(u8, Src8, u32),
    SETN(Dst8, u32),
    SET(u8, Dst8, u32),
    RES(u8, Dst8, u32),
    // 3.3.8: Jumps
    JP(Src16, u32),
    // FIX: all JPC ops have different cycles depending on if the jump happens
    JPC(Src16, Flags, bool, u32),
    JR(u32),
    JRC(Flags, bool, u32),
    // 3.3.9: Calls
    CALL(u32),
    // TODO: consolidate with CALL
    // FIX: all CALLC ops have different cycles depending on if the call happens
    CALLC(Flags, bool, u32),
    // 3.3.10: Restarts
    RST(JumpVector, u32),
    // 3.3.11: Returns
    RET(u32),
    // TODO: consolidate with RET
    // FIX: all RETC ops have different cycles depending on if the ret happens
    RETC(Flags, bool, u32),
    RETI(u32),
}

impl Inst {
    pub fn cycles(&self) -> u32 {
        match self {
            Inst::LD8(_, _, cycles)
            | Inst::LDD(_, _, cycles)
            | Inst::LDI(_, _, cycles)
            | Inst::LD16(_, _, cycles)
            | Inst::PUSH(_, cycles)
            | Inst::POP(_, cycles)
            | Inst::ADD(_, cycles)
            | Inst::ADC(_, cycles)
            | Inst::SUB(_, cycles)
            | Inst::SBC(_, cycles)
            | Inst::AND(_, cycles)
            | Inst::OR(_, cycles)
            | Inst::XOR(_, cycles)
            | Inst::CP(_, cycles)
            | Inst::INC(_, cycles)
            | Inst::DEC(_, cycles)
            | Inst::ADD16(_, cycles)
            | Inst::INC16(_, cycles)
            | Inst::DEC16(_, cycles)
            | Inst::SWAP(_, cycles)
            | Inst::DAA(cycles)
            | Inst::CPL(cycles)
            | Inst::CCF(cycles)
            | Inst::SCF(cycles)
            | Inst::NOP(cycles)
            | Inst::HALT(cycles)
            | Inst::STOP(cycles)
            | Inst::DI(cycles)
            | Inst::EI(cycles)
            | Inst::RLCA(cycles)
            | Inst::RLA(cycles)
            | Inst::RRCA(cycles)
            | Inst::RRA(cycles)
            | Inst::RLC(_, cycles)
            | Inst::RL(_, cycles)
            | Inst::RRC(_, cycles)
            | Inst::RR(_, cycles)
            | Inst::SLA(_, cycles)
            | Inst::SRA(_, cycles)
            | Inst::SRL(_, cycles)
            | Inst::BIT(_, _, cycles)
            | Inst::SETN(_, cycles)
            | Inst::SET(_, _, cycles)
            | Inst::RES(_, _, cycles)
            | Inst::JP(_, cycles)
            | Inst::JPC(_, _, _, cycles)
            | Inst::JR(cycles)
            | Inst::JRC(_, _, cycles)
            | Inst::CALL(cycles)
            | Inst::CALLC(_, _, cycles)
            | Inst::RST(_, cycles)
            | Inst::RET(cycles)
            | Inst::RETC(_, _, cycles)
            | Inst::RETI(cycles) => *cycles,
        }
    }
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

impl Dst8 {
    // used for in-place modifies (e.x. INC or DEC)
    pub fn to_src(&self) -> Src8 {
        match self {
            Dst8::A => Src8::A,
            Dst8::F => Src8::F,
            Dst8::B => Src8::B,
            Dst8::C => Src8::C,
            Dst8::D => Src8::D,
            Dst8::E => Src8::E,
            Dst8::H => Src8::H,
            Dst8::L => Src8::L,
            Dst8::HLContents => Src8::HLContents,
            Dst8::Addr(addr) => Src8::Addr(*addr),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Src16 {
    AF,
    BC,
    DE,
    HL,
    SP,
    N,
    NN,
    Addr(u16),
    Imm(u16),
}

// Same as Dst16 except for cutting out the immediate one word option.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Dst16 {
    AF,
    BC,
    DE,
    HL,
    SP,
    Addr(u16),
}

impl Dst16 {
    // used for in-place modifies (e.x. INC or DEC)
    pub fn to_src(&self) -> Src16 {
        match self {
            Dst16::AF => Src16::AF,
            Dst16::BC => Src16::BC,
            Dst16::DE => Src16::DE,
            Dst16::HL => Src16::HL,
            Dst16::SP => Src16::SP,
            Dst16::Addr(addr) => Src16::Addr(*addr),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum JumpVector {
    Zero,
    Eight,
    Ten,
    Eighteen,
    Twenty,
    TwentyEight,
    Thirty,
    ThirtyEight,
}

impl Into<u16> for JumpVector {
    fn into(self) -> u16 {
        match self {
            JumpVector::Zero => 0x00,
            JumpVector::Eight => 0x08,
            JumpVector::Ten => 0x10,
            JumpVector::Eighteen => 0x18,
            JumpVector::Twenty => 0x20,
            JumpVector::TwentyEight => 0x28,
            JumpVector::Thirty => 0x30,
            JumpVector::ThirtyEight => 0x38,
        }
    }
}
