// SPDX-License-Identifier: MIT
pragma solidity ^0.8.17;

import {ERC721} from "openzeppelin/token/ERC721/ERC721.sol";

contract Token is ERC721 {
    constructor() ERC721("TTC-Token", "TTC") {}

    function mintToken(address to, uint256 tokenId) external {
        // Implement any necessary access control mechanisms (e.g., onlyOwner)
        require(tokenId > 0, "Token Id must be greater than 0");
        _mint(to, tokenId);
    }
}
