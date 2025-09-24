# Decentralized Truth Verification System

## Overview

This pull request implements the core smart contracts for the TruthChain-Media platform, enabling decentralized verification of news sources and journalist credibility assessment.

## Contract Details

### Source Authenticity Verification

This contract provides cryptographic mechanisms for verifying the authenticity of news sources, documents, and multimedia content:

- **Source Registration**: News sources can register with cryptographic public keys
- **Document Submission**: Articles and media can be submitted with content hashes
- **Consensus Verification**: Multiple verifiers must reach consensus on authenticity
- **Stake-Based Incentives**: Verifiers stake tokens when submitting verifications

Key features:
- Multi-stage verification workflows
- Cryptographic proof validation
- Time-bound verification periods
- Reputation-based access control

### Journalist Credibility System

This contract implements a comprehensive journalist reputation scoring system:

- **Journalist Profiles**: Detailed tracking of journalist credentials and history
- **Article Submission**: Journalists can submit articles for peer review
- **Peer Review Process**: Qualified reviewers evaluate accuracy and bias
- **Credibility Scoring**: Dynamic scoring based on verification outcomes

Key features:
- Bias detection mechanisms
- Historical accuracy tracking
- Peer review aggregation
- Automatic score decay for inactive journalists

## Technical Implementation

Both contracts are built with these considerations:

1. **Security First**: Extensive validation of inputs and access control
2. **Data Integrity**: Immutable record-keeping with cryptographic proofs
3. **Incentive Alignment**: Token-based incentives to encourage honest behavior
4. **Scalability**: Optimized data structures for efficient operations

## Testing Approach

The contracts include comprehensive test suites that validate:
- Basic functionality and success paths
- Edge cases and error handling
- Security properties and authorization
- Economic incentive alignment

## Next Steps

1. Implement front-end components for contract interaction
2. Develop integration with existing news platforms
3. Expand token economics for sustainable operations
4. Deploy to testnet for community testing