pub struct Tile {
    pub data: [u8; 16],
}

impl Tile {
    pub fn render(&self) -> Vec<Vec<u8>> {
        let mut tile = vec![];
        for i in 0..8 {
            tile.push(self.row(i));
        }
        tile
    }

    fn row(&self, n: usize) -> Vec<u8> {
        assert!(n < 8, "Tiles only handle 8 rows");
        let mut colors = vec![];
        // LSB first
        let lo: u8 = self.data[2 * n];
        let hi: u8 = self.data[(2 * n) + 1];
        for i in (0..=7).rev() {
            let mask: u8 = 1 << i;
            let hi_bit_set = (hi & mask) == mask;
            let lo_bit_set = (lo & mask) == mask;
            let mut color_id = 0;
            if hi_bit_set {
                color_id += 2;
            }
            if lo_bit_set {
                color_id += 1;
            }
            colors.push(color_id);
        }
        colors
    }
}

#[cfg(test)]
pub mod test {
    use super::Tile;

    #[test]
    fn render_tile() {
        let tile = Tile {
            data: [
                0x3C, 0x7E, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x7E, 0x5E, 0x7E, 0x0A, 0x7C, 0x56,
                0x38, 0x7C,
            ],
        };
        let expected = vec![
            vec![0, 2, 3, 3, 3, 3, 2, 0],
            vec![0, 3, 0, 0, 0, 0, 3, 0],
            vec![0, 3, 0, 0, 0, 0, 3, 0],
            vec![0, 3, 0, 0, 0, 0, 3, 0],
            vec![0, 3, 1, 3, 3, 3, 3, 0],
            vec![0, 1, 1, 1, 3, 1, 3, 0],
            vec![0, 3, 1, 3, 1, 3, 2, 0],
            vec![0, 2, 3, 3, 3, 2, 0, 0],
        ];
        assert_eq!(tile.render(), expected);
    }
}
