# 📰 Newspad - Decentralized Publishing DAO for Journalists

> 🗳️ Fund and vote on stories through a decentralized autonomous organization

## 🌟 Overview

Newspad is a revolutionary platform that empowers journalists through community funding and democratic decision-making. Journalists can propose stories, receive funding from the community, and publish content only after community approval through voting.

## ✨ Features

- 📝 **Story Proposals**: Journalists can propose stories with funding goals
- 💰 **Community Funding**: Anyone can fund promising story proposals
- 🗳️ **Democratic Voting**: Community votes on funded stories before publication
- 📊 **Reputation System**: Track journalist performance and funding history
- 💸 **Automatic Refunds**: Failed stories automatically refund contributors
- 🔒 **Secure Payments**: Built on Stacks blockchain for transparency

## 🚀 How It Works

### For Journalists 👩‍💼

1. **Propose a Story**: Submit your story idea with title, description, and funding goal
2. **Wait for Funding**: Community members fund stories they want to see
3. **Community Vote**: Once funded, the community votes on story approval
4. **Publish & Get Paid**: If approved, publish your story and receive the funds

### For Community Members 🌍

1. **Browse Stories**: Discover interesting story proposals
2. **Fund Stories**: Support journalism by funding stories you care about
3. **Vote on Quality**: Vote on funded stories to ensure quality content
4. **Get Refunds**: Automatically receive refunds for rejected stories

## 📋 Contract Functions

### Public Functions

- `propose-story(title, description, funding-goal)` - Submit a new story proposal
- `fund-story(story-id, amount)` - Fund a story proposal with STX
- `vote-on-story(story-id, vote-for)` - Vote for or against a funded story
- `publish-story(story-id, content-hash)` - Publish approved story content
- `refund-story(story-id)` - Claim refund for failed stories

### Read-Only Functions

- `get-story(story-id)` - Get complete story information
- `get-story-status(story-id)` - Check story funding/voting status
- `get-journalist-reputation(journalist)` - View journalist's track record
- `get-story-counter()` - Get total number of stories proposed

## 🛠️ Usage Examples

### Propose a Story
```clarity
(contract-call? .newspad propose-story 
  "Climate Change Investigation" 
  "Deep dive into local environmental issues affecting our community" 
  u5000000) ;; 5 STX funding goal
```

### Fund a Story
```clarity
(contract-call? .newspad fund-story u1 u1000000) ;; Fund story #1 with 1 STX
```

### Vote on a Story
```clarity
(contract-call? .newspad vote-on-story u1 true) ;; Vote YES on story #1
```

## ⚙️ Configuration

- **Minimum Funding**: 1 STX (adjustable by contract owner)
- **Voting Period**: 144 blocks (~24 hours, adjustable)
- **Approval Threshold**: Simple majority (votes-for > votes-against)

## 🔧 Development Setup

1. Install Clarinet
2. Clone this repository
3. Run tests: `clarinet test`
4. Deploy locally: `clarinet console`

## 🤝 Contributing

We welcome contributions! Please feel free to submit pull requests or open issues for bugs and feature requests.

## 📄 License

This project is open source and available under the MIT License.


