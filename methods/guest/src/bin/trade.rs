#![no_main]

use risc0_zkvm::guest::env;
use ttc;
use hex::FromHex;

risc0_zkvm::guest::entry!(main);

fn main() {
    let data: String = env::read();
    let input_bytes = <Vec<u8>>::from_hex(data).unwrap();

    let mut g = ttc::ttc_trading::decode_input(input_bytes);
    let solution = g.solve_preferences().unwrap();
    let output: Vec<u8> = ttc::ttc_trading::encode_output(&solution);

    env::commit(&output);

}
