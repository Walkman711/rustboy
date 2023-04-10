use crate::mem_constants::{IO_END, IO_START};

const IO_SIZE: usize = (IO_END - IO_START + 1) as usize;

/// Register for reading joypad info and determining system type (R/W)
pub const P1: u16 = 0xFF00;
/// Serial transfer data (R/W)
pub const SB: u16 = 0xFF01;
/// SIO control (R/W)
pub const SC: u16 = 0xFF02;
/// Divider register (R/W). Incremented 16384Hz (~16779 on SGB). Writing any value sets it to 0x00.
pub const DIV: u16 = 0xFF04;
/// Timer counter (R/W). Incremented by freq specified in TAC register. Generates an interrupt when
/// it overflows.
pub const TIMA: u16 = 0xFF05;
/// Timer Modulo (R/W). When TIMA overflows, this data will be loaded
pub const TMA: u16 = 0xFF06;
/// Timer Control (R/W)
pub const TAC: u16 = 0xFF07;
/// Interrupt Flag (R/W)
pub const IF: u16 = 0xFF0F;
/// Sound Mode 1 Register (R/W)
pub const NR10: u16 = 0xFF10;
pub const NR11: u16 = 0xFF11;
pub const NR12: u16 = 0xFF12;
pub const NR14: u16 = 0xFF14;
pub const NR21: u16 = 0xFF16;
pub const NR22: u16 = 0xFF17;
pub const NR24: u16 = 0xFF19;
pub const NR30: u16 = 0xFF1A;
pub const NR31: u16 = 0xFF1B;
pub const NR32: u16 = 0xFF1C;
pub const NR33: u16 = 0xFF1E;
pub const NR41: u16 = 0xFF20;
pub const NR42: u16 = 0xFF21;
pub const NR43: u16 = 0xFF22;
pub const NR30_2: u16 = 0xFF23;
pub const NR50: u16 = 0xFF24;
pub const NR51: u16 = 0xFF25;
pub const NR52: u16 = 0xFF26;
// TODO: should wave ram be here or in MMU (0xFF30 - 0xFF3F)?
/// LCD Control (R/W)
pub const LCDC: u16 = 0xFF40;
/// LCD Status (R/W)
pub const STAT: u16 = 0xFF41;
/// Scroll Y (R/W)
pub const SCY: u16 = 0xFF42;
/// Scroll X (R/W)
pub const SCX: u16 = 0xFF43;
/// LCDC Y-Coordinate (R)
pub const LY: u16 = 0xFF44;
/// LY Compare (R/!)
pub const LYC: u16 = 0xFF45;
/// DMA Transfer and Start Address (W)
pub const DMA: u16 = 0xFF46;
/// BG & Window Palette Data (R/W)
pub const BGP: u16 = 0xFF47;
/// Object Palette 0 Data (R/W)
pub const OBP0: u16 = 0xFF48;
/// Object Palette 1 Data (R/W)
pub const OBP1: u16 = 0xFF49;
/// Window Y Position (R/W)
pub const WY: u16 = 0xFF4A;
/// Window X Position (R/W)
pub const WX: u16 = 0xFF4B;
/// Interrupt Enable (R/W)
pub const IE: u16 = 0xFFFF;

#[derive(Copy, Clone, Debug)]
pub enum Interrupts {
    VBlank = 0x00,
    LCDCStatus = 0x01,
    TimerOverflow = 0x04,
    SerialTransferCompletion = 0x08,
    HighToLowP10P13 = 0x10,
}

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
        io.set_byte(OBP0, 0xFF);
        io.set_byte(OBP1, 0xFF);
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
