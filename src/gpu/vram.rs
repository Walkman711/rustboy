#![allow(dead_code)]
pub struct VRAM {
    oam: OAM,

    /// FF40 — LCDC: LCD control
    /// **LCDC** is the main **LCD C**ontrol register. Its bits toggle what
    /// elements are displayed on the screen, and how.
    lcdc: LCDC,

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

    /// FF42–FF43 — SCY, SCX: Viewport Y position, X position
    /// Those specify the top-left coordinates of the visible 160×144 pixel area within the
    /// 256×256 pixels BG map. Values in the range 0–255 may be used.
    scy: u8,
    scx: u8,

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

    /// FF47 — BGP (Non-CGB Mode only): BG palette data
    ///
    /// This register assigns gray shades to the color indexes of the BG and
    /// Window tiles.
    bgp: u8,

    /// FF48–FF49 — OBP0, OBP1 (Non-CGB Mode only): OBJ palette 0, 1 data
    ///
    /// These registers assigns gray shades to the color indexes of the OBJs that use the corresponding palette.
    /// They work exactly like BGP, except that the lower two bits are ignored because color index 0 is transparent for OBJs.
    obp0: u8,
    obp1: u8,

    /// FF4A–FF4B — WY, WX: Window Y position, X position plus 7
    /// Specify the top-left coordinates of [the Window](#Window).
    ///
    /// The Window is visible (if enabled) when both coordinates are in the ranges
    /// WX=0..166, WY=0..143 respectively. Values WX=7, WY=0 place the Window at the
    /// top left of the screen, completely covering the background.
    wy: u8,
    wx: u8,
}

// TODO: when we do CGB
pub struct CGBRegisters {
    /// FF68: background color palette specification/background palette index
    bcps: u8,

    /// FF69: background color palette data
    bcpd: u8,

    /// FF6A: OBJ color palette specification/OBJ palette index
    ocps: u8,

    /// FF69: OBJ color palette data
    ocpd: u8,
}

pub struct LCDC {
    pub data: u8,
}

impl LCDC {
    pub fn lcd_enabled(&self) -> bool {
        self.data & (1 << 7) == (1 << 7)
    }

    pub fn tile_map_area(&self) -> u16 {
        if self.data & (1 << 6) == (1 << 6) {
            0x9C00
        } else {
            0x9800
        }
    }

    pub fn window_enabled(&self) -> bool {
        self.data & (1 << 5) == (1 << 5)
    }

    pub fn bg_and_window_tile_data_area(&self) -> u16 {
        if self.data & (1 << 4) == (1 << 4) {
            0x8000
        } else {
            0x8800
        }
    }

    pub fn bg_tile_map_area(&self) -> u16 {
        if self.data & (1 << 3) == (1 << 3) {
            0x9C00
        } else {
            0x9800
        }
    }

    pub fn obj_size(&self) -> u8 {
        if self.data & (1 << 2) == (1 << 2) {
            8 * 16
        } else {
            8 * 8
        }
    }

    pub fn obj_enabled(&self) -> bool {
        self.data & (1 << 1) == (1 << 1)
    }

    pub fn bg_and_window_enable(&self) -> bool {
        self.data & 1 == 1
    }
}

pub struct OAM {
    entries: [OAMEntry; 40],
}

pub struct OAMEntry {
    y: u8,
    x: u8,
    tile_index: u8,
    flags: u8,
}
