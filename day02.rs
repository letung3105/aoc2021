use std::env;

fn main() {
    let fpath = env::args()
        .nth(1)
        .expect("Path to input file is not given!");
    let s = std::fs::read_to_string(fpath).unwrap();
    let lines = s.lines();

    let mut pos1 = PositionV1::default();
    pos1.follow_commands(lines.clone());
    println!("Coordinates' product: {}", pos1.x * pos1.y);

    let mut pos2 = PositionV2::default();
    pos2.follow_commands(lines);
    println!("Coordinates' product: {}", pos2.x * pos2.y);
}

fn parse_command<S>(cmd: S) -> (String, i64)
where
    S: AsRef<str>,
{
    let mut cmd = cmd.as_ref().split_whitespace().take(2);
    let action = cmd.next().unwrap().to_string();
    let offset = cmd
        .next()
        .unwrap()
        .parse::<i64>()
        .expect("Invalid position offset!");
    (action, offset)
}

trait Position {
    fn update<T>(&mut self, cmd: T)
    where
        T: AsRef<str>;

    fn follow_commands<T, V>(&mut self, commands: V)
    where
        T: AsRef<str>,
        V: Iterator<Item = T>,
    {
        for cmd in commands {
            self.update(cmd)
        }
    }
}

#[derive(Default)]
struct PositionV1 {
    x: i64,
    y: i64,
}

impl Position for PositionV1 {
    fn update<T>(&mut self, cmd: T)
    where
        T: AsRef<str>,
    {
        let (action, offset) = parse_command(cmd);
        match action.as_str() {
            "forward" => self.x += offset,
            "down" => self.y += offset,
            "up" => self.y -= offset,
            _ => panic!("Invalid command!"),
        }
    }
}

#[derive(Default)]
struct PositionV2 {
    x: i64,
    y: i64,
    aim: i64,
}

impl Position for PositionV2 {
    fn update<T>(&mut self, cmd: T)
    where
        T: AsRef<str>,
    {
        let (action, offset) = parse_command(cmd);
        match action.as_str() {
            "forward" => {
                self.x += offset;
                self.y += offset * self.aim;
            }
            "down" => self.aim += offset,
            "up" => self.aim -= offset,
            _ => panic!("Invalid command!"),
        }
    }
}
