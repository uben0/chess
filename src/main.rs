fn user_input<T: std::str::FromStr>() -> T
where <T as std::str::FromStr>::Err: std::fmt::Debug
{
	let mut l = String::new();
	std::io::stdin().read_line(&mut l).unwrap();
	T::from_str(l.as_str().trim()).unwrap()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PieceType {
    King,
    Queen,
    Rook,
    Knight,
    Bishop,
    Pawn,
}
use PieceType::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Color {
    Black,
    White,
}
use Color::*;
impl Color {
    fn oponent(self: Color) -> Color {
        match self {White=>Black, Black=>White}
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Piece {
    t: PieceType,
    c: Color,
}
impl Piece {
    fn to_unicode(self: Piece) -> char {
        match (self.c, self.t) {
            (Black, King  ) => '♚',
            (Black, Queen ) => '♛',
            (Black, Rook  ) => '♜',
            (Black, Knight) => '♞',
            (Black, Bishop) => '♝',
            (Black, Pawn  ) => '♟',
            (White, King  ) => '♔',
            (White, Queen ) => '♕',
            (White, Rook  ) => '♖',
            (White, Knight) => '♘',
            (White, Bishop) => '♗',
            (White, Pawn  ) => '♙',
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Game {
    board: [[Option<Piece>; 8]; 8],
}

impl Game {
    fn new() -> Game {
        let mut g = Game {
            board: [[None; 8]; 8],
        };

        let line = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook];

        for x in 0..8 {
            g.board[0][x] = Some(Piece{t: line[x], c: Black});
            g.board[1][x] = Some(Piece{t: Pawn,    c: Black});
            g.board[6][x] = Some(Piece{t: Pawn,    c: White});
            g.board[7][x] = Some(Piece{t: line[x], c: White});
        }

        return g;
    }
    fn print_with_reachable(self: &Game, map: &[[bool; 8]; 8]) {
        println!("  a b c d e f g h");
        for y in 0..8 {
            print!("{} ", y + 1);
            for x in 0..8 {
                if (x + y) % 2 != 0 {
                    print!("\x1b[100m");
                }
                match self.board[y][x] {
                    Some(piece) => {
                        if map[y][x] {
                            print!("\x1b[92m{}\x1b[39m ", piece.to_unicode());
                        }
                        else {
                            print!("{} ", piece.to_unicode());
                        }
                    }
                    None => {
                        if map[y][x] {
                            print!("\x1b[96m╶╴\x1b[39m");
                        }
                        else {
                            print!("  ");
                        }
                    }
                }
                if (x + y) % 2 != 0 {
                    print!("\x1b[49m");
                }
            }
            println!("");
        }
    }
    // fn print(self: &Game) {
    //     println!("  a b c d e f g h");
    //     for y in 0..8 {
    //         print!("{} ", y + 1);
    //         for x in 0..8 {
    //             match self.board[y][x] {
    //                 Some(piece) => {
    //                     print!("{} ", piece.to_unicode());
    //                 }
    //                 None => {
    //                     print!("  ");
    //                 }
    //             }
    //         }
    //         println!("");
    //     }
    // }
    fn pieces_iter(self: &Game) -> BoardIter {
        BoardIter{
            board: &self.board,
            x: 0,
            y: 0,
            alive: true,
        }
    }
    fn reachable_by_piece(self: &Game, x: usize, y: usize, map: &mut [[bool; 8]; 8]) {
        fn line<T>(g: &Game, player: Color, map: &mut [[bool; 8]; 8], pos_iter: T)
        where T: Iterator<Item=(usize, usize)>
        {
            for (x, y) in pos_iter {
                if let Some(p) = g.board[y][x] {
                    if p.c != player {
                        map[y][x] = true;
                    }
                    break
                }
                else {
                    map[y][x] = true;
                }
            }
        }
        if let Some(p) = self.board[y][x] {
            let pos_iter = PosIter::new(x, y);
            match p.t {
                Pawn => {
                    let (pos_iter, start) = match p.c {
                        Black => {(pos_iter.south(), 1)}
                        White => {(pos_iter.north(), 6)}
                    };
                    for (x, y) in pos_iter.take(1 + (y == start) as usize) {
                        if self.board[y][x].is_some() {
                            break;
                        }
                        map[y][x] = true;
                    }
                    if let Some((x, y)) = pos_iter.est().next() {
                        if let Some(dst_piece) = self.board[y][x] {
                            if dst_piece.c != p.c {
                                map[y][x] = true;
                            }
                        }
                    }
                    if let Some((x, y)) = pos_iter.west().next() {
                        if let Some(dst_piece) = self.board[y][x] {
                            if dst_piece.c != p.c {
                                map[y][x] = true;
                            }
                        }
                    }
                }
                King => {
                    for pos_iter in &pos_iter.axes() {
                        line(self, p.c, map, pos_iter.take(1));
                    }
                    for pos_iter in &pos_iter.diagonals() {
                        line(self, p.c, map, pos_iter.take(1));
                    }
                }
                Rook => {
                    for pos_iter in &pos_iter.axes() {
                        line(self, p.c, map, *pos_iter);
                    }
                }
                Bishop => {
                    for pos_iter in &pos_iter.diagonals() {
                        line(self, p.c, map, *pos_iter);
                    }
                }
                Queen => {
                    for pos_iter in &pos_iter.axes() {
                        line(self, p.c, map, *pos_iter);
                    }
                    for pos_iter in &pos_iter.diagonals() {
                        line(self, p.c, map, *pos_iter);
                    }
                }
                Knight => {
                    for pos_iter in &pos_iter.north().north().est().radials() {
                        line(self, p.c, map, pos_iter.take(1));
                    }
                    for pos_iter in &pos_iter.north().north().west().radials() {
                        line(self, p.c, map, pos_iter.take(1));
                    }
                }
            }
        }
    }
    fn reachable_by_player(self: &Game, player: Color, map: &mut [[bool; 8]; 8]) {
        for (x, y, piece) in self.pieces_iter() {
            if piece.c == player {
                self.reachable_by_piece(x, y, map);
            }
        }
    }
    fn is_in_check(self: &Game, player: Color) -> bool {
        let mut map = [[false; 8]; 8];
        self.reachable_by_player(player.oponent(), &mut map);
        for (x, y, piece) in self.pieces_iter() {
            if piece.c == player && piece.t == King {
                if map[y][x] {
                    return true;
                }
            }
        }
        false
    }
    fn move_piece(self: &mut Game, m: Move) -> Option<Piece> {
        if let Some(piece) = self.board[m.src_y][m.src_x].take() {
            self.board[m.dst_y][m.dst_x].replace(piece)
        }
        else {None}
    }
    fn play_move(self: &mut Game, m: Move, player: Color) -> Result<(), String> {
        if let Some(piece) = self.board[m.src_y][m.src_x] {
            if piece.c == player {
                let mut map = [[false; 8]; 8];
                self.reachable_by_piece(m.src_x, m.src_y, &mut map);
                if map[m.dst_y][m.dst_x] {
                    let mut projection = self.clone();
                    projection.move_piece(m);
                    if !projection.is_in_check(player) {
                        self.move_piece(m);
                        Ok(())
                    }
                    else {
                        Err(String::from("that would put you in check"))
                    }
                }
                else {
                    // DEBUG
                    self.print_with_reachable(&map);
                    Err(String::from("this square is not reachable"))
                }
            }
            else {
                Err(String::from("this piece does not belong to you"))
            }
        }
        else {
            Err(String::from("no piece selected"))
        }
    }
}

struct BoardIter<'a> {
    board: &'a [[Option<Piece>; 8]; 8],
    x: usize,
    y: usize,
    alive: bool,
}
impl<'a> Iterator for BoardIter<'a> {
    type Item = (usize, usize, Piece);
    fn next(self: &mut Self) -> Option<Self::Item> {
        while self.alive {
            let mut result = None;
            if let Some(p) = self.board[self.y][self.x] {
                result = Some((self.x, self.y, p));
            }
            if self.x < 8 {
                self.x += 1;
            }
            if self.x == 8 {
                self.x = 0;
                self.y += 1;
            }
            if self.y == 8 {
                self.alive = false;
            }
            if result.is_some() {
                return result;
            }
        }
        None
    }
}

#[derive(Clone, Copy)]
struct PosIter {
    x: usize,
    y: usize,
    dx: isize,
    dy: isize,
}
impl PosIter {
    fn new(x: usize, y: usize) -> PosIter {
        PosIter {x, y, dx: 0, dy: 0}
    }
    fn axes(self: PosIter) -> [PosIter; 4] {
        [
            self.north(),
            self.est(),
            self.south(),
            self.west(),
        ]
    }
    fn diagonals(self: PosIter) -> [PosIter; 4] {
        [
            self.north().est(),
            self.est().south(),
            self.south().west(),
            self.west().north(),
        ]
    }
    fn radials(self: PosIter) -> [PosIter; 4] {
        assert!(self.dx != 0 || self.dy != 0);
        [
            PosIter{x: self.x, y: self.y, dx:  self.dx, dy:  self.dy},
            PosIter{x: self.x, y: self.y, dx: -self.dy, dy:  self.dx},
            PosIter{x: self.x, y: self.y, dx: -self.dx, dy: -self.dy},
            PosIter{x: self.x, y: self.y, dx:  self.dy, dy: -self.dx},
        ]
    }
    fn north(mut self: PosIter) -> PosIter {
        self.dy -= 1;
        self
    }
    fn south(mut self: PosIter) -> PosIter {
        self.dy += 1;
        self
    }
    fn west(mut self: PosIter) -> PosIter {
        self.dx -= 1;
        self
    }
    fn est(mut self: PosIter) -> PosIter {
        self.dx += 1;
        self
    }
}
impl Iterator for PosIter {
    type Item = (usize, usize);
    fn next(self: &mut PosIter) -> Option<(usize, usize)> {
        if self.dx == 0 && self.dy == 0 {
            return None
        }
        let res_x = self.x as isize + self.dx;
        let res_y = self.y as isize + self.dy;
        if (0..8).contains(&res_x) && (0..8).contains(&res_y) {
            self.x = res_x as usize;
            self.y = res_y as usize;
            Some((self.x, self.y))
        }
        else {None}
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Move {
	src_x: usize,
	src_y: usize,
	dst_x: usize,
	dst_y: usize,
}

fn get_user_move_loop() -> Move {
	loop {
		if let Some(m) = get_user_move() {
			return m
		}
		println!("invalid syntax");
	}
}

fn get_user_move() -> Option<Move> {
	println!("enter move:");
	let s: String = user_input();
	let mut iter = s.chars();
	Some(Move {
		src_x: match iter.next()? {
			'a' => 0, 'b' => 1, 'c' => 2, 'd' => 3,
			'e' => 4, 'f' => 5, 'g' => 6, 'h' => 7,
			_ => return None
		},
		src_y: match iter.next()? {
			'1' => 0, '2' => 1, '3' => 2, '4' => 3,
			'5' => 4, '6' => 5, '7' => 6, '8' => 7,
			_ => return None
		},
		dst_x: match iter.next()? {
			'a' => 0, 'b' => 1, 'c' => 2, 'd' => 3,
			'e' => 4, 'f' => 5, 'g' => 6, 'h' => 7,
			_ => return None
		},
		dst_y: match iter.next()? {
			'1' => 0, '2' => 1, '3' => 2, '4' => 3,
			'5' => 4, '6' => 5, '7' => 6, '8' => 7,
			_ => return None
		},
	})
}

fn main() {
    let mut g = Game::new();
    loop {
        println!("** WHITE PLAYER **");
        let mut map = [[false; 8]; 8];
        g.reachable_by_player(White, &mut map);
        g.print_with_reachable(&map);
        while let Err(msg) = g.play_move(get_user_move_loop(), White) {
            println!("{}", msg);
        }

        println!("** BLACK PLAYER **");
        let mut map = [[false; 8]; 8];
        g.reachable_by_player(Black, &mut map);
        g.print_with_reachable(&map);
        while let Err(msg) = g.play_move(get_user_move_loop(), Black) {
            println!("{}", msg);
        }
    }
}
