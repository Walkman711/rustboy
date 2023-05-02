pub enum Input {
    DirectionButton(Direction),
    ActionButton(Action),
}

impl From<Input> for u8 {
    fn from(value: Input) -> Self {
        match value {
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

impl From<Direction> for u8 {
    fn from(value: Direction) -> Self {
        match value {
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

impl From<Action> for u8 {
    fn from(value: Action) -> Self {
        match value {
            Action::Start => 0b0000_1000,
            Action::Select => 0b0000_0100,
            Action::B => 0b0000_0010,
            Action::A => 0b0000_0001,
        }
    }
}
