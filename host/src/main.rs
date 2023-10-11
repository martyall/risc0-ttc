use ttc_methods::{TRADE_ELF, TRADE_ID};
use risc0_zkvm::{
    default_prover,
    serde::{to_vec, from_slice},
    ExecutorEnv, Receipt,
};
use ttc::ttc_trading::contract;
use hex;
use ethers_core::types::U256;


fn provably_ttc(input: &contract::TokenDetailsEmittedFilter) -> Receipt {
      let env = ExecutorEnv::builder()
        .add_input(&to_vec(input).unwrap())
        .build()
        .unwrap();

    let prover = default_prover();
    prover.prove_elf(env, TRADE_ELF).unwrap()
}

fn main() {

    let input = contract::TokenDetailsEmittedFilter {
        token_ids: [U256::from(571628), U256::from(207230), U256::from(188615), U256::from(779527), U256::from(634530), U256::from(366674)],
        preference_lists: 
              [ vec![U256::from(188615),U256::from(634530),U256::from(779527)]
              , vec![U256::from(188615),U256::from(571628),U256::from(366674),U256::from(207230)]
              , vec![U256::from(188615),U256::from(207230)]
              , vec![U256::from(571628),U256::from(366674),U256::from(634530),U256::from(779527)]
              , vec![U256::from(207230),U256::from(188615),U256::from(571628)]
              , vec![U256::from(571628),U256::from(634530),U256::from(779527),U256::from(366674)]
              ]
        };

    let receipt = provably_ttc(&input);
    
    receipt.verify(TRADE_ID).expect("failed to verify receipt");

    let call: contract::StoreResultCall = from_slice(&receipt.journal).expect("Journal should contain a StoreResultsCall object");

    println!("Hello, world! I know the factors of {:?}, and I can prove it!", call);

    

}
