// SPDX-License-Identifier: MIT
pragma solidity ^0.8.17;

import {IERC721} from "../lib/bonsai-lib-sol/lib/murky/lib/openzeppelin-contracts/contracts/token/ERC721/IERC721.sol";
import {IBonsaiRelay} from "../lib/bonsai-lib-sol/src/IBonsaiRelay.sol";
import {BonsaiCallbackReceiver} from "../lib/bonsai-lib-sol/src/BonsaiCallbackReceiver.sol";
import {Script, console} from "../lib/forge-std/src/Script.sol";

contract TTCTrading is BonsaiCallbackReceiver {
    uint8 public constant MAX_PARTICIPANTS = 6;

    uint64 private constant _BONSAI_CALLBACK_GAS_LIMIT = 100000000;

    /// @notice Image ID of the only zkVM binary to accept callbacks from.
    bytes32 public immutable ttcImageId;

    IERC721 public token;

    enum TradePhase {
        TokenSubmission,
        Ranking,
        Execution,
        Distribution
    }
    TradePhase public phase = TradePhase.TokenSubmission;

    event PhaseChanged(TradePhase newPhase);
    event TokenDetailsEmitted(
        uint256[MAX_PARTICIPANTS] tokenIds,
        uint256[][MAX_PARTICIPANTS] preferenceLists
    );

    event TTCResult(uint256[][] result);

    address[MAX_PARTICIPANTS] public ownersArray;
    uint256[MAX_PARTICIPANTS] public tokenIdsArray;
    uint256[][MAX_PARTICIPANTS] public preferenceListsArray;
    uint8 public submissionCounter = 0;
    mapping(uint => uint) trades;

    modifier inTokenSubmissionPhase() {
        require(
            phase == TradePhase.TokenSubmission,
            "Not in token submission phase"
        );
        _;
    }

    modifier inRankingPhase() {
        require(phase == TradePhase.Ranking, "Not in ranking phase");
        _;
    }

    modifier inExecutionPhase() {
        require(phase == TradePhase.Execution, "Not in execution phase");
        _;
    }

    modifier inDistributionPhase() {
        require(phase == TradePhase.Distribution, "Not in distribution phase");
        _;
    }

    /// @notice Initialize the contract, binding it to a specified Bonsai relay and RISC Zero guest image.
    constructor(
        IERC721 _token,
        IBonsaiRelay bonsaiRelay,
        bytes32 _imageId
    ) BonsaiCallbackReceiver(bonsaiRelay) {
        token = _token;
        ttcImageId = _imageId;
    }

    function submitToken(uint256 _tokenId) external inTokenSubmissionPhase {
        require(
            submissionCounter <= MAX_PARTICIPANTS,
            "Submission limit reached"
        );

        require(token.ownerOf(_tokenId) == msg.sender, "Not owner of token");

        token.transferFrom(msg.sender, address(this), _tokenId); // Transfers the token to this contract

        ownersArray[submissionCounter] = msg.sender;
        tokenIdsArray[submissionCounter] = _tokenId;
        submissionCounter++;
    }

    function sealTokensAndStartRanking() external inTokenSubmissionPhase {
        phase = TradePhase.Ranking;
        emit PhaseChanged(phase);
    }

    function submitPreferences(
        uint256[] memory _preferenceList
    ) external inRankingPhase {
        bool found = false;
        for (uint8 i = 0; i < submissionCounter; i++) {
            if (ownersArray[i] == msg.sender) {
                preferenceListsArray[i] = _preferenceList;
                found = true;
                break;
            }
        }
        require(found, "Token not submitted by user");
    }

    function lockRankingAndExecuteTTC() external inRankingPhase {
        phase = TradePhase.Execution;
        emit PhaseChanged(phase);

        emit TokenDetailsEmitted(tokenIdsArray, preferenceListsArray);
        console.log("TokenDetailsEmitted");

        bonsaiRelay.requestCallback(
            ttcImageId,
            abi.encode(tokenIdsArray, preferenceListsArray),
            address(this),
            this.storeResult.selector,
            _BONSAI_CALLBACK_GAS_LIMIT
        );
        console.log("Requested Callback");

        phase = TradePhase.Distribution;
        emit PhaseChanged(phase);
    }

    function storeResult(
        uint[][] memory result
    ) external onlyBonsaiCallback(ttcImageId) returns (bytes memory) {
        for (uint cycle = 0; cycle < result.length; cycle++) {
            if (result[cycle].length == 1) {
                trades[result[cycle][0]] = result[cycle][0];
            } else {
                for (uint i = 0; i < result[cycle].length; i++) {
                    if (i < result[cycle].length - 1) {
                        trades[result[cycle][i]] = result[cycle][i + 1];
                    } else {
                        trades[result[cycle][i]] = result[cycle][0];
                    }
                }
            }
        }
        emit TTCResult(result);
        return new bytes(0);
    }

    function retrieveToken() external inDistributionPhase {
        bool found = false;
        for (uint8 i = 0; i < submissionCounter; i++) {
            if (ownersArray[i] == msg.sender) {
                token.transferFrom(
                    address(this),
                    msg.sender,
                    trades[tokenIdsArray[i]]
                );
                found = true;
                break;
            }
        }
        require(found, "Token not found for user");
    }
}
