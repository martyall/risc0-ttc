#![no_main]

use risc0_zkvm::guest::env;
use ttc::{self, ttc_trading::contract};

risc0_zkvm::guest::entry!(main);

fn main() {
    let input: contract::TokenDetailsEmittedFilter = env::read();

    let mut g = ttc::ttc_trading::decode_input(input);
    let solution = g.solve_preferences().unwrap();
    let output: contract::StoreResultCall = ttc::ttc_trading::encode_output(&solution);

    env::commit(&output);

}
