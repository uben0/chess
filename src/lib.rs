#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PieceType {
    King,
    Queen,
    Rook,
    Knight,
    Bishop,
    Pawn,
}
impl std::fmt::Display for PieceType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::King => '♚',
                Self::Queen => '♛',
                Self::Rook => '♜',
                Self::Knight => '♞',
                Self::Bishop => '♝',
                Self::Pawn => '♟',
            }
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Color {
    Black,
    White,
}
impl Color {
    pub fn oponent(self) -> Self {
        match self {
            Self::White => Self::Black,
            Self::Black => Self::White,
        }
    }
}

impl std::ops::Mul<Color> for PieceType {
    type Output = Piece;
    fn mul(self, rhs: Color) -> Self::Output {
        Piece { t: self, c: rhs }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Piece {
    t: PieceType,
    c: Color,
}
impl std::fmt::Display for Piece {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let color = match self.c {
            Color::White => "\x1b[96m",
            Color::Black => "\x1b[91m",
        };
        write!(f, "{}{}\x1b[0m", color, self.t)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Position {
    x: usize,
    y: usize,
}
impl Position {
    fn iter(self) -> PositionIter {
        PositionIter::new(self.x, self.y)
    }
}
impl std::str::FromStr for Position {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let [file, rank] = s.as_bytes() {
            Ok(Self {
                x: match file {
                    b'a' => 0,
                    b'b' => 1,
                    b'c' => 2,
                    b'd' => 3,
                    b'e' => 4,
                    b'f' => 5,
                    b'g' => 6,
                    b'h' => 7,
                    _ => return Err("file must be a letter in [a-h]"),
                },
                y: match rank {
                    b'1' => 0,
                    b'2' => 1,
                    b'3' => 2,
                    b'4' => 3,
                    b'5' => 4,
                    b'6' => 5,
                    b'7' => 6,
                    b'8' => 7,
                    _ => return Err("rank must be a digit in [1-8]"),
                },
            })
        } else {
            Err("expects two characters")
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Game {
    board: [[Option<Piece>; 8]; 8],
    turn: Color,
    moves: Vec<(Move, Option<Piece>)>,
}
impl Default for Game {
    fn default() -> Self {
        let line = [
            PieceType::Rook,
            PieceType::Knight,
            PieceType::Bishop,
            PieceType::Queen,
            PieceType::King,
            PieceType::Bishop,
            PieceType::Knight,
            PieceType::Rook,
        ];
        Self {
            board: [
                line.map(|t| Some(t * Color::White)),
                [Some(PieceType::Pawn * Color::White); 8],
                [None; 8],
                [None; 8],
                [None; 8],
                [None; 8],
                [Some(PieceType::Pawn * Color::Black); 8],
                line.map(|t| Some(t * Color::Black)),
            ],
            turn: Color::White,
            moves: Vec::new(),
        }
    }
}

impl std::ops::Index<Position> for Game {
    type Output = Option<Piece>;
    fn index(&self, idx: Position) -> &Option<Piece> {
        assert!((0..8).contains(&idx.x));
        assert!((0..8).contains(&idx.y));
        &self.board[idx.y][idx.x]
    }
}
impl<T> std::ops::Index<Position> for [[T; 8]; 8] {
    type Output = T;
    fn index(&self, idx: Position) -> &Self::Output {
        assert!((0..8).contains(&idx.x));
        assert!((0..8).contains(&idx.y));
        &self[idx.y][idx.x]
    }
}
impl<T> std::ops::IndexMut<Position> for [[T; 8]; 8] {
    fn index_mut(&mut self, idx: Position) -> &mut Self::Output {
        assert!((0..8).contains(&idx.x));
        assert!((0..8).contains(&idx.y));
        &mut self[idx.y][idx.x]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PlayMoveErr {
    NoPieceAtPosition,
    CantMoveOponantPiece,
    SquareUnreachable,
    WouldBeInCheck,
}
impl std::fmt::Display for PlayMoveErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::NoPieceAtPosition => "no piece to move at given position",
                Self::CantMoveOponantPiece => "cannot move oponant piece",
                Self::SquareUnreachable => "not reachable by piece",
                Self::WouldBeInCheck => "would be in check",
            }
        )
    }
}

// #[derive(Debug, Clone, Copy, PartialEq)]
// enum Exploration {
//     Win,
//     Loose,
//     Heuristic(f64),
// }

impl Game {
    pub fn new() -> Self {
        Default::default()
    }
    pub fn push_move(&mut self, m: Move) -> Result<(), PlayMoveErr> {
        let piece = self[m.src].ok_or(PlayMoveErr::NoPieceAtPosition)?;
        if piece.c != self.turn {
            return Err(PlayMoveErr::CantMoveOponantPiece);
        }
        let mut reachable = false;
        self.reach_by_piece(m.src, |pos| {
            if pos == m.dst {
                reachable = true
            }
        });
        if reachable {
            return Err(PlayMoveErr::SquareUnreachable);
        }
        let piece = self.board[m.src].take().unwrap();
        let removed = self.board[m.dst].replace(piece);
        if self.is_in_check() {
            self.board[m.dst] = removed;
            self.board[m.src] = Some(piece);
            return Err(PlayMoveErr::WouldBeInCheck);
        }
        self.turn = self.turn.oponent();
        self.moves.push((m, removed));
        Ok(())
    }
    pub fn pop_move(&mut self) {
        let (m, removed) = self.moves.pop().unwrap();
        assert!(self[m.dst].is_some());
        assert!(self[m.src].is_none());
        let piece = self.board[m.dst].take();
        self.board[m.src] = piece;
        self.board[m.dst] = removed;
    }
    pub fn print(&self) {
        let square = |x: usize, y: usize| match self.board[y][x] {
            Some(piece) => {
                print!("{} ", piece);
            }
            None => {
                print!("\x1b[2m.\x1b[0m ");
            }
        };
        match self.turn {
            Color::White => {
                println!("  a b c d e f g h  ");
                for y in (0..8).rev() {
                    print!("{} ", y + 1);
                    for x in 0..8 {
                        square(x, y);
                    }
                    println!("{}", y + 1);
                }
                println!("  a b c d e f g h  ");
            }
            Color::Black => {
                println!("  h g f e d c b a  ");
                for y in 0..8 {
                    print!("{} ", y + 1);
                    for x in (0..8).rev() {
                        square(x, y);
                    }
                    println!("{}", y + 1);
                }
                println!("  h g f e d c b a  ");
            }
        }
    }
    // fn best_move(&mut self) -> Option<Move> {
    //     unimplemented!()
    // }
    // fn explore_moves(&mut self, depth: usize) -> Exploration {

    //     for (pos, piece) in self.pieces().filter(|(_, p)| p.c == self.turn) {
    //         let mut reachable = [[false; 8]; 8];
    //         self.reachable_by_piece(pos, &mut reachable);
    //     }
    //     unimplemented!()
    // }
    // fn heuristic(&self) -> f64 {
    //     self.pieces()
    //         .map(|(pos, piece)| match piece.t {
    //             PieceType::Pawn => {
    //                 (match piece.c {
    //                     Color::White => pos.y,
    //                     Color::Black => 8 - pos.y,
    //                 }) as f64
    //                     * 0.1
    //                     + 1.0
    //             }
    //             PieceType::Bishop => 3.0,
    //             PieceType::Knight => 3.0,
    //             PieceType::Rook => 5.0,
    //             PieceType::Queen => 9.0,
    //             PieceType::King => 0.0,
    //         })
    //         .sum()
    // }
    fn reach_by_pos_iter<T: Iterator<Item = Position>>(
        &self,
        pos_iter: T,
        player: Color,
        mut map: impl FnMut(Position),
    ) {
        for pos in pos_iter {
            if let Some(p) = self[pos] {
                if p.c != player {
                    map(pos);
                }
                break;
            } else {
                map(pos);
            }
        }
    }
    fn reach_by_piece(&self, pos: Position, mut map: impl FnMut(Position)) {
        if let Some(piece) = self[pos] {
            match piece.t {
                PieceType::Pawn => {
                    let (pos_iter, start) = match piece.c {
                        Color::Black => (pos.iter().north(), 6),
                        Color::White => (pos.iter().south(), 1),
                    };
                    for pos in pos_iter.take(if pos.y == start { 2 } else { 1 }) {
                        if self[pos].is_some() {
                            break;
                        }
                        map(pos);
                    }
                    if let Some(pos) = pos_iter.est().next() {
                        if let Some(dst_piece) = self[pos] {
                            if dst_piece.c != piece.c {
                                map(pos);
                            }
                        }
                    }
                    if let Some(pos) = pos_iter.west().next() {
                        if let Some(dst_piece) = self[pos] {
                            if dst_piece.c != piece.c {
                                map(pos);
                            }
                        }
                    }
                }
                PieceType::Knight => {
                    for pos_iter in pos.iter().north().north().est().radials() {
                        self.reach_by_pos_iter(pos_iter.take(1), piece.c, &mut map)
                    }
                    for pos_iter in pos.iter().north().north().west().radials() {
                        self.reach_by_pos_iter(pos_iter.take(1), piece.c, &mut map)
                    }
                }
                PieceType::King => {
                    for pos_iter in pos.iter().axes() {
                        self.reach_by_pos_iter(pos_iter.take(1), piece.c, &mut map)
                    }
                    for pos_iter in pos.iter().diagonals() {
                        self.reach_by_pos_iter(pos_iter.take(1), piece.c, &mut map)
                    }
                }
                PieceType::Rook => {
                    for pos_iter in pos.iter().axes() {
                        self.reach_by_pos_iter(pos_iter, piece.c, &mut map)
                    }
                }
                PieceType::Bishop => {
                    for pos_iter in pos.iter().diagonals() {
                        self.reach_by_pos_iter(pos_iter, piece.c, &mut map)
                    }
                }
                PieceType::Queen => {
                    for pos_iter in pos.iter().axes() {
                        self.reach_by_pos_iter(pos_iter, piece.c, &mut map)
                    }
                    for pos_iter in pos.iter().diagonals() {
                        self.reach_by_pos_iter(pos_iter, piece.c, &mut map)
                    }
                }
            }
        }
    }
    fn reach_by_player(&self, player: Color, mut map: impl FnMut(Position)) {
        for (pos, _) in self.pieces().filter(|(_, p)| p.c == player) {
            self.reach_by_piece(pos, &mut map);
        }
    }
    pub fn is_in_check(&self) -> bool {
        let mut in_check = false;
        self.reach_by_player(self.turn.oponent(), |pos| {
            if self[pos] == Some(PieceType::King * self.turn) {
                in_check = true
            }
        });
        in_check
    }
    fn pieces(&self) -> BoardIter {
        BoardIter {
            board: &self.board,
            pos: Position { x: 0, y: 0 },
        }
    }
    // fn possible_piece_moves(&self, pos: Position, moves: &mut Vec<Move>) {
    //     if let Some(piece) = self[pos] {
    //         let mut map = [[false; 8]; 8];
    //         self.reachable_by_piece(pos, &mut map);
    //         for y in 0..8 {
    //             for x in 0..8 {
    //                 let dst = Position { x, y };
    //                 if map[dst] {
    //                     let mut predict = self.clone();
    //                     let m = Move { src: pos, dst };
    //                     predict.move_piece(m);
    //                     if !predict.is_in_check(piece.c) {
    //                         moves.push(m);
    //                     }
    //                 }
    //             }
    //         }
    //     }
    // }
    // pub fn possible_player_moves(&self, player: Color, moves: &mut Vec<Move>) {
    //     for (pos, piece) in self.pieces() {
    //         if piece.c == player {
    //             self.possible_piece_moves(pos, moves);
    //         }
    //     }
    // }
}

struct BoardIter<'a> {
    board: &'a [[Option<Piece>; 8]; 8],
    pos: Position,
}

impl<'a> Iterator for BoardIter<'a> {
    type Item = (Position, Piece);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.pos.y == 8 {
                return None;
            }
            let pos = self.pos;
            self.pos.x += 1;
            if self.pos.x == 8 {
                self.pos.y += 1;
                self.pos.x = 0;
            }
            if let Some(p) = self.board[pos] {
                return Some((pos, p));
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Move {
    pub src: Position,
    pub dst: Position,
}
impl std::str::FromStr for Move {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let [b'a'..=b'h', b'1'..=b'8', b'a'..=b'h', b'1'..=b'8'] = s.as_bytes() {
            let (src, dst) = s.split_at(2);
            Ok(Self {
                src: src.parse()?,
                dst: dst.parse()?,
            })
        } else {
            Err("expects four characters [a-h][1-8][a-h][1-8]")
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct PositionIter {
    x: usize,
    y: usize,
    dx: isize,
    dy: isize,
}
impl PositionIter {
    fn new(x: usize, y: usize) -> Self {
        Self { x, y, dx: 0, dy: 0 }
    }
    fn north(&self) -> Self {
        Self {
            dy: self.dy - 1,
            ..*self
        }
    }
    fn south(&self) -> Self {
        Self {
            dy: self.dy + 1,
            ..*self
        }
    }
    fn west(&self) -> Self {
        Self {
            dx: self.dx - 1,
            ..*self
        }
    }
    fn est(&self) -> Self {
        Self {
            dx: self.dx + 1,
            ..*self
        }
    }
    fn axes(&self) -> [Self; 4] {
        [self.north(), self.est(), self.south(), self.west()]
    }
    fn diagonals(&self) -> [Self; 4] {
        [
            self.north().est(),
            self.est().south(),
            self.south().west(),
            self.west().north(),
        ]
    }
    fn radials(&self) -> [Self; 4] {
        assert!(self.dx != 0 || self.dy != 0);
        [
            Self {
                dx: self.dx,
                dy: self.dy,
                ..*self
            },
            Self {
                dx: -self.dy,
                dy: self.dx,
                ..*self
            },
            Self {
                dx: -self.dx,
                dy: -self.dy,
                ..*self
            },
            Self {
                dx: self.dy,
                dy: -self.dx,
                ..*self
            },
        ]
    }
}
impl Iterator for PositionIter {
    type Item = Position;

    fn next(&mut self) -> Option<Self::Item> {
        if self.dx == 0 && self.dy == 0 {
            return None;
        }

        let res_x = self.x as isize + self.dx;
        let res_y = self.y as isize + self.dy;
        if (0..8).contains(&res_x) && (0..8).contains(&res_y) {
            self.x = res_x as usize;
            self.y = res_y as usize;
            Some(Position {
                x: self.x,
                y: self.y,
            })
        } else {
            None
        }
    }
}
