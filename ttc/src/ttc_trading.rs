use ethers_core::types::{U256};


use crate::algorithm;



pub mod contract {

  use ethers_contract_derive::{EthAbiCodec, EthAbiType};
  use serde::{Deserialize, Serialize};
  use ethers_core::types::{U256};

  #[derive(Debug, Clone, EthAbiType, EthAbiCodec, Deserialize, Serialize)]
  pub struct TradeData {
      pub token_ids: [U256; 6],
      pub preference_lists: [Vec<U256>; 6]
  }
  
  #[derive(Debug, Clone, EthAbiType, EthAbiCodec, Deserialize, Serialize)]
  pub struct TradeResult {
      pub result: Vec<Vec<U256>>
  }

}


pub fn decode_input(input: contract::TradeData) -> algorithm::PreferenceGraph<U256> {

    let assigned_prefs = input
        .token_ids
        .iter()
        .map(|x| x.clone())
        .zip(input.preference_lists);

    let prefs = algorithm::Preferences::new(assigned_prefs.into_iter().collect()).unwrap();

    algorithm::PreferenceGraph::new(prefs).unwrap()
}

pub fn encode_output(output: &algorithm::Solution<U256>) -> contract::TradeResult {
    let abi_output = output.res.iter().map(|x| x.values.clone()).collect();
    contract::TradeResult { result: abi_output }
}

#[cfg(test)]
mod tests {

    use algorithm::Cycle;
    use super::*;

    #[test]
    fn basic_test() {
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

        let mut g: algorithm::PreferenceGraph<U256> = decode_input(input);
        let ps: Vec<Cycle<U256>> = g.solve_preferences().unwrap().res;

        assert_eq!(
            vec![
                Cycle {
                    values: vec![U256::from(188615)]
                },
                Cycle {
                    values: vec![U256::from(571628), U256::from(634530), U256::from(207230)]
                },
                Cycle {
                    values: vec![U256::from(779527), U256::from(366674)]
                }
            ],
            ps
        );
    }
}
