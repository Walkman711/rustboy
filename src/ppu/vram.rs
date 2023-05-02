#![allow(dead_code)]
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