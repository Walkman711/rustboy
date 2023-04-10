use crate::{io_registers::IORegisters, mem_constants::*};

#[derive(Debug)]
pub struct MMU {
    ext_ram: [u8; (EXT_RAM_END - EXT_RAM_START + 1) as usize],
    // ROM 0
    // SWITCHABLE ROM
    // VRAM
    // EXT RAM
    // WRAM I
    // WRAM II
    // SPRITE TABLE
    // IO Registers
    io: IORegisters,
    // HRAM
    // INTERRUPT
}

impl Default for MMU {
    fn default() -> Self {
        Self {
            ext_ram: [0; (EXT_RAM_END - EXT_RAM_START + 1) as usize],
            io: IORegisters::default(),
        }
    }
}

impl MMU {
    pub fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            // FIX: assuming a 32 kb cart
            ROM_BANK_0_START..=ROM_BANK_0_END => unimplemented!("16kb ROM bank #0"),
            SWITCHABLE_ROM_START..=SWITCHABLE_ROM_END => unimplemented!("16kb switchable rom bank"),
            VRAM_START..=VRAM_END => unimplemented!("8kb VRAM"),
            EXT_RAM_START..=EXT_RAM_END => unimplemented!("8kb switchable RAM bank"),
            WRAM_I_START..=WRAM_I_END => unimplemented!("8kb internal RAM"),
            WRAM_II_START..=WRAM_II_END => panic!("Nintendo forbids reading from Echo RAM"),
            SPRITE_TABLE_START..=SPRITE_TABLE_END => unimplemented!("Sprite Attrib Memory"),
            FORBIDDEN_START..=FORBIDDEN_END => {
                panic!("Nintendo forbids reading from {FORBIDDEN_START}-{FORBIDDEN_END}")
            }
            IO_START..=IO_END => self.io.read_byte(addr),
            HRAM_START..=HRAM_END => unimplemented!("HRAM"),
            INTERRUPT_START..=INTERRUPT_END => unimplemented!("interrupt"),
            _ => unimplemented!("other {addr} needs to be read from"),
        };
        unimplemented!("MMU::read_byte() not implemented yet")
    }

    pub fn write_byte(&mut self, addr: u16, val: u8) {
        match addr {
            // FIX: assuming a 32 kb cart
            ROM_BANK_0_START..=ROM_BANK_0_END => unimplemented!("16kb ROM bank #0"),
            SWITCHABLE_ROM_START..=SWITCHABLE_ROM_END => unimplemented!("16kb switchable rom bank"),
            VRAM_START..=VRAM_END => unimplemented!("8kb VRAM"),
            EXT_RAM_START..=EXT_RAM_END => unimplemented!("8kb switchable RAM bank"),
            WRAM_I_START..=WRAM_I_END => unimplemented!("8kb internal RAM"),
            WRAM_II_START..=WRAM_II_END => panic!("Nintendo forbids writing to Echo RAM"),
            SPRITE_TABLE_START..=SPRITE_TABLE_END => unimplemented!("Sprite Attrib Memory"),
            FORBIDDEN_START..=FORBIDDEN_END => {
                panic!("Nintendo forbids writing to {FORBIDDEN_START}-{FORBIDDEN_END}")
            }
            IO_START..=IO_END => self.io.set_byte(addr, val),
            HRAM_START..=HRAM_END => unimplemented!("HRAM"),
            INTERRUPT_START..=INTERRUPT_END => unimplemented!("interrupt"),
            _ => unimplemented!("other {addr} needs to be read from"),
        }
    }

    pub fn read_word(&self, addr: u16) -> u16 {
        let lo: u16 = self.read_byte(addr).into();
        let hi: u16 = self.read_byte(addr + 1).into();
        (hi << 8) | lo
    }

    pub fn write_word(&mut self, addr: u16, val: u16) {
        let lo = (val & 0x00FF) as u8;
        let hi = ((val & 0xFF00) >> 8) as u8;
        self.write_byte(addr, lo);
        self.write_byte(addr + 1, hi);
    }
}
