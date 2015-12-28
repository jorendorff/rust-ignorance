extern crate rand;

use rand::random;
use std::io::Write;
use std::io;
use std::str::FromStr;

fn play() -> io::Result<()> {
    let (num, mut lie) =
        if random::<f64>() < 0.05 {
            (18, Some("it was a number from 1 to 17".to_string()))
        } else {
            (1 + random::<u32>() % 17, None)
        };

    println!("I am thinking of a whole number from 1 to 17. See if you can guess it.");
    println!("But be careful: I always lie exactly once per game.");
    println!("");

    loop {
        print!("your guess> ");
        {
            let out = io::stdout();
            try!(out.lock().flush());
        }
        let mut line = String::new();
        try!(io::stdin().read_line(&mut line));
        let guess = match u32::from_str(line.trim()) {
            Ok(num) => num,
            Err(_) => {
                println!("Please guess a number.");
                continue;
            }
        };

        if guess == num {
            if lie == None && random::<f64>() < 0.15 {
                lie = Some(format!("it wasn't {}", guess));
                println!("Nope, my number is {} than that",
                         if random::<f64>() < 0.5 { "lower" } else { "higher"});
            } else {
                break;
            }
        } else {
            let mut lower = num < guess;
            if lie == None && random::<f64>() < 0.3 {
                lower = !lower;
                lie = Some(format!("it was {} than {}",
                                   if lower { "lower" } else { "higher" },
                                   guess));
            }
            println!("Nope, my number is {} than that",
                     if lower { "lower" } else { "higher" });
        }
    }

    println!("You got it!");
    println!("I lied to you when I said {}. :)\n",
             lie.unwrap_or("I was going to lie to you".to_string()));
    Ok(())
}

fn main() {
    play().unwrap();
}
