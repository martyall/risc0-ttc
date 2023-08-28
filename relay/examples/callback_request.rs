// Copyright 2023 RISC Zero, Inc.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

use anyhow::Context;
use bonsai_ethereum_relay::sdk::client::{CallbackRequest, Client};
use clap::Parser;
use ethers::{types::Address, contract::EthCall};
use methods::TRADE_ID;
use ttc::ttc_trading::contract::StoreResultCall;
use risc0_zkvm::sha::Digest;
use hex::FromHex;

/// Exmaple code for sending a REST API request to the Bonsai relay service to
/// requests, execution, proving, and on-chain callback for a zkVM guest
/// application.
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Adress for the BonsaiStarter application contract.
    address: Address,

    // an abi encoded TokenDetailsEmittedFilter event as a hex string
    token_details_emitted_event: String,

    /// Bonsai Relay API URL.
    #[arg(long, env, default_value = "http://localhost:8080")]
    bonsai_relay_api_url: String,

    /// Bonsai API key. Used by the relay to send requests to the Bonsai proving
    /// service. Defaults to empty, providing no authentication.
    #[arg(long, env, default_value = "")]
    bonsai_api_key: String,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    // initialize a relay client
    let relay_client = Client::from_parts(
        args.bonsai_relay_api_url.clone(), // Set BONSAI_API_URL or replace this line.
        args.bonsai_api_key.clone(),       // Set BONSAI_API_KEY or replace this line.
    )
    .context("Failed to initialize the relay client")?;

    // Initialize the input for the FIBONACCI guest.

    let input = <Vec<u8>>::from_hex(args.token_details_emitted_event).unwrap();

    // Create a CallbackRequest for your contract
    // example: (contracts/BonsaiStarter.sol).
    let request = CallbackRequest {
        callback_contract: args.address,
        // you can use the command `solc --hashes contracts/BonsaiStarter.sol`
        // to get the value for your actual contract (9f2275c0: storeResult(uint256,uint256))
        function_selector: <StoreResultCall as EthCall>::selector(),
        gas_limit: 3000000,
        image_id: Digest::from(TRADE_ID).into(),
        input,
    };

    // Send the callback request to the Bonsai Relay.
    relay_client
        .callback_request(request)
        .await
        .context("Callback request failed")?;

    Ok(())
}
