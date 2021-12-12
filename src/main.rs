use std::path::PathBuf;
use structopt::StructOpt;

#[derive(StructOpt)]
#[structopt(name = "chess", about = "A chess game in the terminal.")]
struct Opt {
    /// Load a previously save game
    file: Option<PathBuf>,
}

const F_BOLD: &'static str = "\x1b[1m";
const F_NONE: &'static str = "\x1b[0m";
const F_EMPH: &'static str = "\x1b[1;93m";

fn prompt() {
    use std::io::Write;
    println!();
    print!("{}>{} ", F_BOLD, F_EMPH);
    let stdout = std::io::stdout();
    stdout.lock().flush().unwrap();
}

fn main() {
    let Opt { file } = Opt::from_args();
    let mut game = match file {
        Some(file) => serde_json::from_reader(std::fs::File::open(file).unwrap()).unwrap(),
        None => chess::Game::new(),
    };

    println!();
    println!(
        "Enter a move matching {}[a-h][1-8][a-h][1-8]{}.",
        F_EMPH, F_NONE
    );
    println!(
        "Or a command starting with '{}/{}', see {}/help{}.",
        F_EMPH, F_NONE, F_EMPH, F_NONE
    );
    println!();

    let mut buffer = String::new();

    game.print(chess::Color::White);
    loop {
        buffer.clear();
        prompt();
        std::io::stdin().read_line(&mut buffer).unwrap();
        println!("{}", F_NONE);
        if buffer.is_empty() {
            return;
        }
        let line = buffer.as_str().trim();
        if let Some('/') = line.chars().next() {
            match line {
                "/save" => {
                    let file = std::fs::File::create("chess.json").unwrap();
                    serde_json::to_writer_pretty(file, &game).unwrap();
                    println!("board saved to \"chess.json\"");
                }
                "/help" => {
                    println!("{}/help{}    print this help message", F_EMPH, F_NONE);
                    println!(
                        "{}/save{}    save the current game in a file",
                        F_EMPH, F_NONE
                    );
                    println!("{}/show{}    print the board", F_EMPH, F_NONE);
                    println!("{}/quit{}    exit the program", F_EMPH, F_NONE);
                    println!("{}/undo{}    undo the last two moves", F_EMPH, F_NONE);
                    println!("{}/auto{}    use AI to play next move", F_EMPH, F_NONE);
                }
                "/undo" => {
                    game.pop_move();
                    game.print(chess::Color::White);
                    game.pop_move();
                }
                "/show" => {
                    game.print(chess::Color::White);
                }
                "/auto" => {
                    game.print(chess::Color::White);
                    let ia_move = game
                        .best_move_timed(std::time::Duration::from_secs(4))
                        .expect("no move available");
                    game.push_move(ia_move).unwrap();
                    println!();
                    game.print(chess::Color::White);
                    let ia_move = game
                        .best_move_timed(std::time::Duration::from_secs(4))
                        .expect("no move available");
                    game.push_move(ia_move).unwrap();
                    println!();
                    game.print(chess::Color::White);
                }
                "/quit" => return,
                _ => println!(
                    "unknown command {}{:?}{}, try {}/help{}",
                    F_EMPH, line, F_NONE, F_EMPH, F_NONE
                ),
            }
        } else {
            match line.parse::<chess::Move>().map(|m| game.push_move(m)) {
                Ok(Ok(())) => {
                    game.print(chess::Color::White);
                    let ia_move = game
                        .best_move_timed(std::time::Duration::from_secs(4))
                        .expect("no move available");
                    game.push_move(ia_move).unwrap();
                    println!();
                    game.print(chess::Color::White);
                }
                Ok(Err(e)) => println!("{}", e),
                Err(e) => println!("{}", e),
            }
        }
    }
}
