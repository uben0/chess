
fn get_user_move_loop() -> chess::Move {
    let mut l = String::new();
    loop {
        l.clear();
        std::io::stdin().read_line(&mut l).unwrap();
        match l.as_str().trim().parse() {
            Ok(m) => return m,
            Err(e) => println!("{}", e),
        }
    }
}

fn main() {
    let mut game = chess::Game::new();

    loop {
        game.print(chess::Color::White);

        let ia_move = game.best_move().unwrap();
        game.push_move(ia_move).unwrap();

        // while game.push_move(get_user_move_loop()).map_err(|e| println!("{}", e)).is_err() {}

        game.print(chess::Color::White);

        let ia_move = game.best_move().unwrap();
        game.push_move(ia_move).unwrap();
    }
}
