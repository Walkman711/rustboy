#![allow(dead_code)]
/// # VRAM Sprite Attribute Table (OAM)
///
/// The Game Boy PPU can display up to 40 sprites either in 8x8 or
/// in 8x16 pixels. Because of a limitation of hardware, only ten sprites
/// can be displayed per scan line. Sprite tiles have the same format as
/// BG tiles, but they are taken from the Sprite Tiles Table located at
/// $8000-8FFF and have unsigned numbering.
///
/// Sprite attributes reside in the Sprite Attribute Table (OAM: Object
/// Attribute Memory) at \$FE00-FE9F. Each of the 40 entries consists of
/// four bytes with the following meanings:
/// ## Byte 0 — Y Position
///
/// Y = Sprite's vertical position on the screen + 16. So for example,
/// Y=0 hides a sprite,
/// Y=2 hides an 8×8 sprite but displays the last two rows of an 8×16 sprite,
/// Y=16 displays a sprite at the top of the screen,
/// Y=144 displays an 8×16 sprite aligned with the bottom of the screen,
/// Y=152 displays an 8×8 sprite aligned with the bottom of the screen,
/// Y=154 displays the first six rows of a sprite at the bottom of the screen,
/// Y=160 hides a sprite.
///
/// ## Byte 1 — X Position
///
/// X = Sprite's horizontal position on the screen + 8. This works similarly
/// to the examples above, except that the width of a sprite is always 8. An
/// off-screen value (X=0 or X\>=168) hides the sprite, but the sprite still
/// affects the priority ordering, thus other sprites with lower priority may be
/// left out due to the ten sprites limit per scan-line.
/// A better way to hide a sprite is to set its Y-coordinate off-screen.
///
/// ## Byte 2 — Tile Index
///
/// In 8x8 mode (LCDC bit 2 = 0), this byte specifies the sprite's only tile index ($00-$FF).
/// This unsigned value selects a tile from the memory area at $8000-$8FFF.
/// In CGB Mode this could be either in
/// VRAM bank 0 or 1, depending on bit 3 of the following byte.
/// In 8x16 mode (LCDC bit 2 = 1), the memory area at $8000-$8FFF is still interpreted
/// as a series of 8x8 tiles, where every 2 tiles form a sprite. In this mode, this byte
/// specifies the index of the first (top) tile of the sprite. This is enforced by the
/// hardware: the least significant bit of the tile index is ignored; that is, the top 8x8
/// tile is "NN & $FE", and the bottom 8x8 tile is "NN | $01".
///
/// ## Byte 3 — Attributes/Flags
///
/// ```
///  Bit7   BG and Window over OBJ (0=No, 1=BG and Window colors 1-3 over the OBJ)
///  Bit6   Y flip          (0=Normal, 1=Vertically mirrored)
///  Bit5   X flip          (0=Normal, 1=Horizontally mirrored)
///  Bit4   Palette number  **Non CGB Mode Only** (0=OBP0, 1=OBP1)
///  Bit3   Tile VRAM-Bank  **CGB Mode Only**     (0=Bank 0, 1=Bank 1)
///  Bit2-0 Palette number  **CGB Mode Only**     (OBP0-7)
/// ```
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
