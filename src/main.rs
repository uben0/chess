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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Piece {
    t: PieceType,
    c: Color,
}
impl Piece {
    fn to_unicode(self) -> char {
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
    fn new() -> Self {
        let mut g = Self {
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
    fn print(&self) {
        println!("  a b c d e f g h");
        for y in 0..8 {
            print!("{} ", y + 1);
            for x in 0..8 {
                match self.board[y][x] {
                    Some(piece) => {
                        print!("{} ", piece.to_unicode());
                    }
                    None => {
                        print!("  ");
                    }
                }
            }
            println!(" {}", y + 1);
        }
        println!("  a b c d e f g h");
    }
	fn reachable_by_pos_iter<
		T: Iterator< Item=(usize, usize) >
	>(
		&self,
		pos_iter: T,
		player  : Color,
		map     : &mut [[bool; 8]; 8]
	) {
		for (x, y) in pos_iter {
			if let Some(p) = self.board[y][x] {
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
	fn reachable_by_piece(
		&self,
		x: usize, y: usize,
		map: &mut [[bool; 8]; 8]
	) {
		if let Some(piece) = self.board[y][x] {
			match piece.t {
				Pawn => {
					let (pos_iter, start) = match piece.c {
						Black => {(PosIter::new(x, y).south(), 1)}
						White => {(PosIter::new(x, y).north(), 6)}
					};
                    for (x, y) in pos_iter.take(1 + (y == start) as usize) {
                        if self.board[y][x].is_some() {
                            break;
                        }
                        map[y][x] = true;
                    }
                    if let Some((x, y)) = pos_iter.est().next() {
                        if let Some(dst_piece) = self.board[y][x] {
                            if dst_piece.c != piece.c {
                                map[y][x] = true;
                            }
                        }
                    }
                    if let Some((x, y)) = pos_iter.west().next() {
                        if let Some(dst_piece) = self.board[y][x] {
                            if dst_piece.c != piece.c {
                                map[y][x] = true;
                            }
                        }
                    }
				}
				Knight => {
					for pos_iter in &PosIter::new(x, y).north().north().est().radials() {
						self.reachable_by_pos_iter(pos_iter.take(1), piece.c, map)
                    }
                    for pos_iter in &PosIter::new(x, y).north().north().west().radials() {
						self.reachable_by_pos_iter(pos_iter.take(1), piece.c, map)
                    }
				}
				King => {
					for pos_iter in &PosIter::new(x, y).axes() {
						self.reachable_by_pos_iter(pos_iter.take(1), piece.c, map)
					}
					for pos_iter in &PosIter::new(x, y).diagonals() {
						self.reachable_by_pos_iter(pos_iter.take(1), piece.c, map)
					}
				}
				Rook => {
					for pos_iter in &PosIter::new(x, y).axes() {
						self.reachable_by_pos_iter(*pos_iter, piece.c, map)
					}
				}
				Bishop => {
					for pos_iter in &PosIter::new(x, y).diagonals() {
						self.reachable_by_pos_iter(*pos_iter, piece.c, map)
					}
				}
				Queen => {
					for pos_iter in &PosIter::new(x, y).axes() {
						self.reachable_by_pos_iter(*pos_iter, piece.c, map)
					}
					for pos_iter in &PosIter::new(x, y).diagonals() {
						self.reachable_by_pos_iter(*pos_iter, piece.c, map)
					}
				}
			}
		}
	}
	fn reachable_by_player(&self, player: Color, map: &mut [[bool; 8]; 8]) {
        for (x, y, piece) in self.pieces() {
            if piece.c == player {
                self.reachable_by_piece(x, y, map);
            }
        }
	}
    fn is_in_check(&self, player: Color) -> bool {
		let mut map = [[false; 8]; 8];
		let oponent = match player {Black=>White, White=>Black};
        self.reachable_by_player(oponent, &mut map);
        self.pieces().any(|(x, y, p)| map[y][x] && p.c == player && p.t == King)
	}
    fn move_piece(&mut self, m: Move) -> Option<Piece> {
        if let Some(piece) = self.board[m.src_y][m.src_x].take() {
            self.board[m.dst_y][m.dst_x].replace(piece)
        }
        else {None}
	}
	fn play_move(&mut self, m: Move, player: Color) -> bool {
        if let Some(piece) = self.board[m.src_y][m.src_x] {
            if piece.c == player {
                let mut map = [[false; 8]; 8];
                self.reachable_by_piece(m.src_x, m.src_y, &mut map);
                if map[m.dst_y][m.dst_x] {
                    let mut projection = self.clone();
                    projection.move_piece(m);
                    if !projection.is_in_check(player) {
                        self.move_piece(m);
                        return true
                    }
                }
            }
		}
		false
    }
    fn pieces(&self) -> BoardIter {
        BoardIter{
            board: &self.board,
            x: 0, y: 0,
        }
    }
    fn possible_piece_moves(&self, x: usize, y: usize, moves: &mut Vec<Move>) {
        if let Some(piece) = self.board[y][x] {
            let mut map = [[false; 8]; 8];
            self.reachable_by_piece(x, y, &mut map);
            for dst_y in 0..8 {
                for dst_x in 0..8 {
                    if map[dst_y][dst_x] {
                        let mut predict = self.clone();
                        let m = Move{src_x: x, src_y: y, dst_x, dst_y};
                        predict.move_piece(m);
                        if !predict.is_in_check(piece.c) {
                            moves.push(m);
                        }
                    }
                }
            }
        }
    }
    fn possible_player_moves(&self, player: Color, moves: &mut Vec<Move>) {
        for (x, y, piece) in self.pieces() {
            if piece.c == player {
                self.possible_piece_moves(x, y, moves);
            }
        }
    }
}

struct BoardIter<'a> {
    board: &'a [[Option<Piece>; 8]; 8],
    x: usize,
    y: usize,
}

impl<'a> Iterator for BoardIter<'a> {
    type Item = (usize, usize, Piece);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.y == 8 {
                return None
            }
            let (x, y) = (self.x, self.y);
            self.x += 1;
            if self.x == 8 {
                self.y += 1;
                self.x = 0;
            }    
            if let Some(p) = self.board[y][x] {
                return Some((x, y, p))
            }
        }
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

#[derive(Clone, Copy)]
struct PosIter {
    x: usize,
    y: usize,
    dx: isize,
    dy: isize,
}
impl PosIter {
    fn new(x: usize, y: usize) -> Self {
        Self {x, y, dx: 0, dy: 0}
    }
	fn north(&self) -> Self {
		Self{ dy: self.dy - 1, .. *self}
	}
	fn south(&self) -> Self {
		Self{ dy: self.dy + 1, .. *self}
	}
	fn west(&self) -> Self {
		Self{ dx: self.dx - 1, .. *self}
	}
	fn est(&self) -> Self {
		Self{ dx: self.dx + 1, .. *self}
	}
	fn axes(&self) -> [Self; 4] {
        [
            self.north(),
            self.est(),
            self.south(),
            self.west(),
        ]
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
            Self{dx:  self.dx, dy:  self.dy, .. *self},
            Self{dx: -self.dy, dy:  self.dx, .. *self},
            Self{dx: -self.dx, dy: -self.dy, .. *self},
            Self{dx:  self.dy, dy: -self.dx, .. *self},
        ]
    }
}
impl Iterator for PosIter {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<(usize, usize)> {
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
        else {
            None
        }
    }
}

fn main() {
    let mut game = Game::new();
    let mut possible_moves = Vec::new();

	for &player in [White, Black].iter().cycle() {
		game.print();
        println!("{:?} player", player);
        
        game.possible_player_moves(player, &mut possible_moves);
        match (game.is_in_check(player), possible_moves.is_empty()) {
            ( true,  true) => {println!("CHECKMATE!"); break},
            ( true, false) => println!("IN CHECK!"),
            (false,  true) => {println!("STALEMATE!"); break},
            (false, false) => (),
        }
        possible_moves.clear();

		loop {
			let m = get_user_move_loop();
			if game.play_move(m, player) {
				break
			}
			println!("this is not a valid move");
		}
    }
}
