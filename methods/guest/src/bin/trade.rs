#![no_main]

use std::io::Read;

use ttc;
use risc0_zkvm::guest::env;

risc0_zkvm::guest::entry!(main);

fn main() {
    let mut input_bytes = Vec::<u8>::new();
    env::stdin().read_to_end(&mut input_bytes).unwrap();

    let mut g = ttc::ttc_trading::decode_input(input_bytes);
    let solution = g.solve_preferences().unwrap();
    let output = ttc::ttc_trading::encode_output(&solution);

    env::commit_slice(&output);
}
