
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
    // let mut possible_moves = Vec::new();

    loop {
        game.print(chess::Color::White);
        // game.possible_player_moves(game.player, &mut possible_moves);
        // match (game.is_in_check(game.player), possible_moves.is_empty()) {
        //     (true, true) => {
        //         println!("CHECKMATE!");
        //         break;
        //     }
        //     (true, false) => println!("IN CHECK!"),
        //     (false, true) => {
        //         println!("STALEMATE!");
        //         break;
        //     }
        //     (false, false) => (),
        // }
        // possible_moves.clear();

        while game.push_move(get_user_move_loop()).map_err(|e| println!("{}", e)).is_err() {}

        game.print(chess::Color::White);

        let ia_move = game.best_move().unwrap();

        game.push_move(ia_move).unwrap();
    }
}
