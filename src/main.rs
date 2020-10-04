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
    fn print(self: &Game) {
        for y in 0..8 {
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
            println!("");
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

fn main() {
    let g = Game::new();
    g.print();
    let m = get_user_move_loop();
    println!("{:?}", m);
}
