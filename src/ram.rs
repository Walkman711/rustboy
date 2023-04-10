#![allow(dead_code)]

use crate::mem_constants::*;

pub struct RAM {
    buf: [u8; (EXT_RAM_END - EXT_RAM_START + 1) as usize],
}

impl RAM {
    pub fn read_byte(&self, _addr: u16) -> u8 {
        unimplemented!("RAM::read_byte() not implemented yet")
    }
}
