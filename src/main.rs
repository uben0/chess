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



fn main() {
    println!("Hello, world!");
}
