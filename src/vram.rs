#![allow(dead_code)]
pub struct OAM {
    entries: [OAMEntry; 40],
}

pub struct OAMEntry {
    y: u8,
    x: u8,
    tile_index: u8,
    flags: u8,
}

pub struct LCDStatusRegisters {
    /// FF44 — LY: LCD Y coordinate (read-only)
    /// LY indicates the current horizontal line, which might be about to be drawn,
    /// being drawn, or just been drawn. LY can hold any value from 0 to 153, with
    /// values from 144 to 153 indicating the VBlank period.
    ly: u8,

    /// FF45 — LYC: LY compare
    /// The Game Boy constantly compares the value of the LYC and LY registers.
    /// When both values are identical, the "LYC=LY" flag in the STAT register
    /// is set, and (if enabled) a STAT interrupt is requested.
    lyc: u8,

    /// FF41 — STAT: LCD status
    /// Bit 6 - LYC=LY STAT Interrupt source         (1=Enable) (Read/Write)
    /// Bit 5 - Mode 2 OAM STAT Interrupt source     (1=Enable) (Read/Write)
    /// Bit 4 - Mode 1 VBlank STAT Interrupt source  (1=Enable) (Read/Write)
    /// Bit 3 - Mode 0 HBlank STAT Interrupt source  (1=Enable) (Read/Write)
    /// Bit 2 - LYC=LY Flag                          (0=Different, 1=Equal) (Read Only)
    /// Bit 1-0 - Mode Flag                          (Mode 0-3, see below) (Read Only)
    ///           0: HBlank
    ///           1: VBlank
    ///           2: Searching OAM
    ///           3: Transferring Data to LCD Controller
    stat: u8,
}

// TODO: this is going to be a flag situation
pub struct LCDControl {
    /// FF40 — LCDC: LCD control
    /// **LCDC** is the main **LCD C**ontrol register. Its bits toggle what
    /// elements are displayed on the screen, and how.
    ///
    /// Bit | Name                           | Usage notes
    /// ----|--------------------------------|-------------------------
    ///  7  | LCD and PPU enable             | 0=Off, 1=On
    ///  6  | Window tile map area           | 0=9800-9BFF, 1=9C00-9FFF
    ///  5  | Window enable                  | 0=Off, 1=On
    ///  4  | BG and Window tile data area   | 0=8800-97FF, 1=8000-8FFF
    ///  3  | BG tile map area               | 0=9800-9BFF, 1=9C00-9FFF
    ///  2  | OBJ size                       | 0=8x8, 1=8x16
    ///  1  | OBJ enable                     | 0=Off, 1=On
    ///  0  | BG and Window enable/priority  | 0=Off, 1=On
    lcdc: u8,
}

pub struct LCDScrolling {
    /// FF42–FF43 — SCY, SCX: Viewport Y position, X position
    /// Those specify the top-left coordinates of the visible 160×144 pixel area within the
    /// 256×256 pixels BG map. Values in the range 0–255 may be used.
    scy: u8,
    scx: u8,

    /// FF4A–FF4B — WY, WX: Window Y position, X position plus 7
    /// Specify the top-left coordinates of [the Window](#Window).
    ///
    /// The Window is visible (if enabled) when both coordinates are in the ranges
    /// WX=0..166, WY=0..143 respectively. Values WX=7, WY=0 place the Window at the
    /// top left of the screen, completely covering the background.
    wy: u8,
    wx: u8,
}

pub enum MonochromeColors {
    White = 0,
    LightGray = 1,
    DarkGray = 2,
    Black = 3,
}

// FIX: make fallible
// XXX: maybe two bools?
impl From<u8> for MonochromeColors {
    fn from(value: u8) -> Self {
        assert!(value < 4);
        match value {
            0 => MonochromeColors::White,
            1 => MonochromeColors::LightGray,
            2 => MonochromeColors::DarkGray,
            3 => MonochromeColors::Black,
            _ => unreachable!("Checked by assert! statement above"),
        }
    }
}

pub struct LCDMonochromePalettes {
    /// ### FF47 — BGP (Non-CGB Mode only): BG palette data
    ///
    /// This register assigns gray shades to the color indexes of the BG and
    /// Window tiles.
    ///
    /// ```
    /// Bit 7-6 - Color for index 3
    /// Bit 5-4 - Color for index 2
    /// Bit 3-2 - Color for index 1
    /// Bit 1-0 - Color for index 0
    /// ```
    ///
    /// Value | Color
    /// ------|-------
    ///   0   | White
    ///   1   | Light gray
    ///   2   | Dark gray
    ///   3   | Black
    bgp: u8,

    /// ### FF48–FF49 — OBP0, OBP1 (Non-CGB Mode only): OBJ palette 0, 1 data
    ///
    /// These registers assigns gray shades to the color indexes of the OBJs that use the corresponding palette.
    /// They work exactly like BGP, except that the lower two bits are ignored because color index 0 is transparent for OBJs.
    obp0: u8,
    obp1: u8,
}

pub struct Tile {
    data: u16,
}

impl Tile {
    pub fn row(&self) -> Vec<MonochromeColors> {
        let mut colors = vec![];
        let hi: u8 = ((self.data & 0xFF00) >> 16) as u8;
        let lo: u8 = (self.data & 0x00FF) as u8;
        for i in 0..=7 {
            let mask: u8 = 1 << i;
            let hi_bit = (hi & mask) >> i;
            let lo_bit = (lo & mask) >> i;
            let color = (hi_bit << 1) + lo_bit;
            colors.push(MonochromeColors::from(color));
        }
        colors
    }
}
