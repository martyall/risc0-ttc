# Summary

This repo contains a smart contract + risc0 coprocessor implementation of the [Top Trading Cycle](https://en.wikipedia.org/wiki/Top_trading_cycle) (TTC) algorithm applied to the setting of NFTs. Roughly speaking, there is a solidity smart contract `TTCTrading` that facilitates direct trades of NFTs between users according to the users' ranked preferences. The algorithm that decides the trades is the TTC algorithm, and is known to be optimal in several  game-theoretic senses. Because the TTC algorithm is sufficiently complex and would lead to prohibitively expensive trades if implemented in on-chain bytecode, we offload it to the risc0 coprocessor. Finally, we simulate the lifecycle of the application using a script and show that in this example we get the correct results.


## Why?

The reason I thought this would be a good project is that it is of medium complexity and I believe it is generally useful. The current methods for trading
NFTs most often involve liquidating one in some sort of auction contract and attempting to purchase another with the royalties. In some cases a direct
transfer would not only be cheaper for everyone involved in terms of transaction costs and time, but it also might better fit the domain of the NFTs. The problem
is that direct transfer is inefficient and requires a very particular configuration of the market. TTC creates a more fluid marketplace to facilitate more trades
in an optimal manner. It is complicated enough to implement that coding it in solidity would be a terrible idea -- not only would it almost surely be riddled with
bugs, but the costs of running it on chain would make the whole contract moot. Offloading it to a risc0 coprocessor makes way more sense, it solves the cost problem
and allows you to take advantage of the battle tested rust libraries to implement the algorithm.

## Components

### Smart Contracts

There are two smart contracts in the `contracts` directory:
1. `Token.sol`: This is a simple wrapper around the standard ERC721 Token implementation found in the open-zeppelin library
2. `TTCTrading.sol`: This is the core contract that facilitates the trades between users.

The `TTCTrading` contract has a relatively simple implementation, it is only meant to demonstrate the application without accounting for _any_ security issues.
The contract acts as an escrow, where a certain number of users can deposit their NFTs, submit their ranked preferences of the available tokens, and collect
tokens designated to them after the TTC algorithm runs. The contract has 4 phases:

1. `TokenSubmission`: This is the period where users are depositing their tokens and the market is still open.
2. `Ranking`: After a certain number of users have entered the market, the user pool is closed. During this phase, each user submits a (strict) ranking of their
preferences for the available tokens. NOTE: they don't have to rank all the tokens, ranking a strict subset of them is equivalent to saying "if I can't have any of these, I would prefer the token that I already own". 
3. `Execution`: During this phase the ranking is closed and the necessary data about the users and their rankings are sent to the relay contract. They ultimately are submitted as input to the guest code, where the result is computed along with a proof of its correctness.
4. `Distribution`: When they relay posts the results in the form of a callback, the trade assignments are recorded in the contract and the phase changes to `Distribution`. During this phase, users can request their new tokens from the contract.

### TTC Guest Code

There is a rust crate `ttc` at the root level of the repository. It consists of two modules:
1. `algorithm`: This is where the core algorithm is implemented, making heavy use of the [petgraph](https://github.com/petgraph/petgraph) graph theory library.
2. `ttc_trading`: This contains utility functions to parse the input data coming from the ethereum smart contract, as well as encode the algorithm's output
for use by the relay's callback transaction.

### Simulation Script

There is a `purs` directory where you can find a `Main.purs` file. This is a script (written in PureScript) which tests the application life cycle for a given example. This script uses the [purescript-web3](https://github.com/f-o-a-m/purescript-web3), as well as [chanterelle](https://github.com/f-o-a-m/chanterelle) to generate the FFI bindings to the contracts. The reason is that I am the primary author of both of these libraries, so I am particularly effective in using them.


## Steps to run

Before you can do anything you need to build the project, meaning cloning all the submodules and then running

```bash
> git clone --recurse-submodules <repo-URL>
> cd risc0-ttc
> cargo build
> npm i
> npm run chanterelle-build
> npm run build
```

NOTE: This assumes you installed certian risc0 packages via `cargo install` and some `foundry` tools as is described in the README.

Start the blockchain with anvil

```bash 
> anvil
```

Run the deploy script for the smart contracts:

```bash
> RISC0_DEV_MODE=true forge script script/Deploy.s.sol --rpc-url http://localhost:8545 --broadcast
```

You should see some output indicating the contract address, something lke 

```
== Logs ==
  Deployed BonsaiTestRelay to  0x5FbDB2315678afecb367f032d93F642f64180aa3
  Image ID for TRADE is  0x6f204468e12dd3d51f4a04cc5954246a585888a68fdd660bc67ecf385990ecc7
  0xe7f1725E7734CE288F8367e1Bb143E90bb3F0512
  0x9fE46736679d2D9a65F0992F2272dE9f3c7fa6e0

```

The first address is the `Token` contract, the second is the `TTCTrading` contract. You can start the relay service up with

```bash
BONSAI_RELAY_ADDRESS=0x5FbDB2315678afecb367f032d93F642f64180aa3 \
  APP_ADDRESS=0x9fE46736679d2D9a65F0992F2272dE9f3c7fa6e0 \
  RISC0_DEV_MODE=true cargo run --bin bonsai-ethereum-relay-cli -- run --relay-address "$BONSAI_RELAY_ADDRESS"
```

Now you can run the script: 

```bash
TTC_ADDRESS=0x9fE46736679d2D9a65F0992F2272dE9f3c7fa6e0 \
  TOKEN_ADDRESS=0xe7f1725E7734CE288F8367e1Bb143E90bb3F0512 \
  npx spago run
```

You should see a bunch of logs printed to the console, including a matrix representing the ranked choices as well as a mapping indicating the final trades, 
e.g. something like

```
Corresponds to preference matrix: 
873652| 998605| 725886| 447867| 305078| 124632
**********************************************
447867| 998605| 998605| 998605| 447867| 725886
----------------------------------------------
124632| 725886| 447867| 124632| 873652| 998605
----------------------------------------------
305078|      X| 873652| 305078| 124632| 447867
----------------------------------------------
873652|      X| 725886|      X| 305078|      X
----------------------------------------------
     X|      X|      X|      X|      X|      X
----------------------------------------------
     X|      X|      X|      X|      X|      X

...

Trades:

User f39fd6e5: 725886 ==> 447867
User 70997970: 447867 ==> 124632
User 3c44cddd: 998605 ==> 998605
User 90f79bf6: 873652 ==> 305078
User 15d34aaf: 124632 ==> 725886
User 9965507d: 305078 ==> 873652
```
