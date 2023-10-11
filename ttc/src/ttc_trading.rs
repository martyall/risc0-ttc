use ethers_core::types::U256;
use crate::algorithm;



pub mod contract {
    use serde::{Deserialize, Serialize};

    use ethers_contract::{EthCall, EthDisplay, EthEvent};

    #[derive(Clone, EthEvent, EthDisplay, Default, Debug, PartialEq, Eq, Hash, Deserialize, Serialize)]
    #[ethevent(
        name = "TokenDetailsEmitted",
        abi = "TokenDetailsEmitted(uint256[6],uint256[][6])"
    )]
    pub struct TokenDetailsEmittedFilter {
        pub token_ids: [::ethers_core::types::U256; 6],
        pub preference_lists: [::std::vec::Vec<::ethers_core::types::U256>; 6],
    }

    #[derive(Clone, EthCall, EthDisplay, Default, Debug, PartialEq, Eq, Hash, Deserialize, Serialize)]
    #[ethcall(name = "storeResult", abi = "storeResult(uint256[][])")]
    pub struct StoreResultCall {
        pub result: ::std::vec::Vec<::std::vec::Vec<::ethers_core::types::U256>>,
    }
}

pub fn decode_input(input: contract::TokenDetailsEmittedFilter) -> algorithm::PreferenceGraph<U256> {

    let assigned_prefs = input
        .token_ids
        .iter()
        .map(|x| x.clone())
        .zip(input.preference_lists);

    let prefs = algorithm::Preferences::new(assigned_prefs.into_iter().collect()).unwrap();

    algorithm::PreferenceGraph::new(prefs).unwrap()
}

pub fn encode_output(output: &algorithm::Solution<U256>) -> contract::StoreResultCall {
    let abi_output = output.res.iter().map(|x| x.values.clone()).collect();
    contract::StoreResultCall { result: abi_output }
}

#[cfg(test)]
mod tests {

    use algorithm::Cycle;
    use hex::FromHex;

    use super::*;

    #[test]
    fn basic_test() {
        let log_str: &str =
        "000000000000000000000000000000000000000000000000000000000008b8ec000000000000000000000000000000000000000000000000000000000003297e000000000000000000000000000000000000000000000000000000000002e0c700000000000000000000000000000000000000000000000000000000000be507000000000000000000000000000000000000000000000000000000000009aea2000000000000000000000000000000000000000000000000000000000005985200000000000000000000000000000000000000000000000000000000000000e000000000000000000000000000000000000000000000000000000000000000c0000000000000000000000000000000000000000000000000000000000000014000000000000000000000000000000000000000000000000000000000000001e0000000000000000000000000000000000000000000000000000000000000024000000000000000000000000000000000000000000000000000000000000002e000000000000000000000000000000000000000000000000000000000000003600000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000002e0c7000000000000000000000000000000000000000000000000000000000009aea200000000000000000000000000000000000000000000000000000000000be5070000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000002e0c7000000000000000000000000000000000000000000000000000000000008b8ec0000000000000000000000000000000000000000000000000000000000059852000000000000000000000000000000000000000000000000000000000003297e0000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000002e0c7000000000000000000000000000000000000000000000000000000000003297e0000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000008b8ec0000000000000000000000000000000000000000000000000000000000059852000000000000000000000000000000000000000000000000000000000009aea200000000000000000000000000000000000000000000000000000000000be5070000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000003297e000000000000000000000000000000000000000000000000000000000002e0c7000000000000000000000000000000000000000000000000000000000008b8ec0000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000008b8ec000000000000000000000000000000000000000000000000000000000009aea200000000000000000000000000000000000000000000000000000000000be5070000000000000000000000000000000000000000000000000000000000059852";
        let log_data = <Vec<u8>>::from_hex(log_str).unwrap();

        let mut g: algorithm::PreferenceGraph<U256> = decode_input(log_data);

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
