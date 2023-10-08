// SPDX-License-Identifier: MIT
pragma solidity ^0.8.17;

import {BonsaiTestRelay} from "bonsai/BonsaiTestRelay.sol";

contract TestRelay is BonsaiTestRelay {
    constructor(uint chainId) BonsaiTestRelay(chainId) {}
}
