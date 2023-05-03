// CGB
#[allow(dead_code)]
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

#[derive(Clone, Copy, Debug)]
pub struct LCDC {
    pub data: u8,
}

impl From<u8> for LCDC {
    fn from(value: u8) -> Self {
        Self { data: value }
    }
}

impl LCDC {
    pub fn lcd_enabled(&self) -> bool {
        self.data & (1 << 7) == (1 << 7)
    }

    pub fn window_tile_map_area(&self) -> u16 {
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

#[derive(Clone, Debug)]
#[allow(dead_code)]
pub struct OAM {
    entries: [OAMEntry; 40],
}

#[derive(Clone, Copy, Debug)]
#[allow(dead_code)]
pub struct OAMEntry {
    y: u8,
    x: u8,
    tile_index: u8,
    flags: u8,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
// TODO: cycle through modes.
// TODO: prohibit CPU memory access in the relevant modes
pub enum StatMode {
    HBlank,
    VBlank,
    SearchingOAM,
    DataTransfer,
}

impl From<u8> for StatMode {
    fn from(value: u8) -> Self {
        assert!(value < 4);
        match value {
            0 => Self::HBlank,
            1 => Self::VBlank,
            2 => Self::SearchingOAM,
            3 => Self::DataTransfer,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct STAT {
    pub data: u8,
}

impl From<u8> for STAT {
    fn from(value: u8) -> Self {
        Self { data: value }
    }
}

// TODO: this and LCDC should be bitflags!{} macro structs
impl STAT {
    pub fn lyc_equals_ly_src(&self) -> bool {
        self.data & (1 << 6) == (1 << 6)
    }

    pub fn mode_2_src(&self) -> bool {
        self.data & (1 << 5) == (1 << 5)
    }

    pub fn mode_1_vblank_src(&self) -> bool {
        self.data & (1 << 4) == (1 << 4)
    }

    pub fn mode_0_hblank_src(&self) -> bool {
        self.data & (1 << 3) == (1 << 3)
    }

    pub fn lyc_equals_ly(&self) -> bool {
        self.data & (1 << 2) == (1 << 2)
    }

    pub fn mode(&self) -> StatMode {
        let mode_bits = self.data & 0x3;
        StatMode::from(mode_bits)
    }
}
