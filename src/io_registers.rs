use crate::mem_constants::{IO_END, IO_START};

const IO_SIZE: usize = (IO_END - IO_START + 1) as usize;

const TIMA: u16 = 0xFF05;
const TMA: u16 = 0xFF06;
const TAC: u16 = 0xFF07;
const NR10: u16 = 0xFF10;
const NR11: u16 = 0xFF11;
const NR12: u16 = 0xFF12;
const NR14: u16 = 0xFF14;
const NR21: u16 = 0xFF16;
const NR22: u16 = 0xFF17;
const NR24: u16 = 0xFF19;
const NR30: u16 = 0xFF1A;
const NR31: u16 = 0xFF1B;
const NR32: u16 = 0xFF1C;
const NR33: u16 = 0xFF1E;
const NR41: u16 = 0xFF20;
const NR42: u16 = 0xFF21;
const NR43: u16 = 0xFF22;
const NR30_2: u16 = 0xFF23;
const NR50: u16 = 0xFF24;
const NR51: u16 = 0xFF25;
const NR52: u16 = 0xFF26;
const LCDC: u16 = 0xFF40;
const SCY: u16 = 0xFF42;
const SCX: u16 = 0xFF43;
const LYC: u16 = 0xFF45;
const BGP: u16 = 0xFF47;
const BP0: u16 = 0xFF48;
const BP1: u16 = 0xFF49;
const WY: u16 = 0xFF4A;
const WX: u16 = 0xFF4B;
const IE: u16 = 0xFFFF;

#[derive(Clone, Debug)]
pub struct IORegisters {
    io_registers: [u8; IO_SIZE],
}

// Default IO register values are taken from 2.7.1 of the Gameboy CPU Manual
impl Default for IORegisters {
    fn default() -> Self {
        let mut io = Self {
            io_registers: [0; IO_SIZE],
        };

        io.set_byte(TIMA, 0x00);
        io.set_byte(TMA, 0x00);
        io.set_byte(TAC, 0x00);
        io.set_byte(NR10, 0x80);
        io.set_byte(NR11, 0xBF);
        io.set_byte(NR12, 0xF3);
        io.set_byte(NR14, 0xBF);
        io.set_byte(NR21, 0x3F);
        io.set_byte(NR22, 0x00);
        io.set_byte(NR24, 0xBF);
        io.set_byte(NR30, 0x7F);
        io.set_byte(NR31, 0xFF);
        io.set_byte(NR32, 0x9F);
        io.set_byte(NR33, 0xBF);
        io.set_byte(NR41, 0xFF);
        io.set_byte(NR42, 0x00);
        io.set_byte(NR43, 0x00);
        io.set_byte(NR30_2, 0xBF);
        io.set_byte(NR50, 0x77);
        io.set_byte(NR51, 0xF3);
        // FIX: depends on GB model
        // 0xF1: gameboy
        // 0xF0 for super gameboy
        io.set_byte(NR52, 0xF1);
        io.set_byte(LCDC, 0x91);
        io.set_byte(SCY, 0x00);
        io.set_byte(SCX, 0x00);
        io.set_byte(LYC, 0x00);
        io.set_byte(BGP, 0xFC);
        io.set_byte(BP0, 0xFF);
        io.set_byte(BP1, 0xFF);
        io.set_byte(WY, 0x00);
        io.set_byte(WX, 0x00);
        io.set_byte(IE, 0x00);
        io
    }
}

// TODO: make trait
impl IORegisters {
    pub fn read_byte(&self, addr: u16) -> u8 {
        self.io_registers[(addr - IO_START) as usize]
    }

    pub fn set_byte(&mut self, addr: u16, val: u8) {
        self.io_registers[(addr - IO_START) as usize] = val;
    }
}
