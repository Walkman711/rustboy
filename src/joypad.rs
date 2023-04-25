pub enum Input {
    DirectionButton(Direction),
    ActionButton(Action),
}

impl Into<u8> for Input {
    fn into(self) -> u8 {
        match self {
            Input::DirectionButton(d) => d.into(),
            Input::ActionButton(a) => a.into(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Direction {
    Down,
    Up,
    Left,
    Right,
}

impl Into<u8> for Direction {
    fn into(self) -> u8 {
        match self {
            Direction::Down => 0b0000_1000,
            Direction::Up => 0b0000_0100,
            Direction::Left => 0b0000_0010,
            Direction::Right => 0b0000_0001,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Action {
    Start,
    Select,
    B,
    A,
}

impl Into<u8> for Action {
    fn into(self) -> u8 {
        match self {
            Action::Start => 0b0000_1000,
            Action::Select => 0b0000_0100,
            Action::B => 0b0000_0010,
            Action::A => 0b0000_0001,
        }
    }
}
