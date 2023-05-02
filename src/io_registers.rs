use strum_macros::EnumIter;

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

#[derive(Copy, Clone, Debug, EnumIter)]
pub enum Interrupts {
    VBlank = 0b0000_0001,
    LCDCStatus = 0b0000_0010,
    TimerOverflow = 0b0000_0100,
    SerialTransferCompletion = 0b0000_1000,
    HighToLowP10P13 = 0b0001_0000,
}
