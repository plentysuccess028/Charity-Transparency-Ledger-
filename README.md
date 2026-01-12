# 🏥 Charity Transparency Ledger

> 💡 **A blockchain-based donation tracking system that ensures complete transparency in charitable giving**

## 🌟 Overview

The Charity Transparency Ledger is a Clarity smart contract that enables fully transparent donation tracking on the Stacks blockchain. Donors can trace exactly where their contributions go, and charities can prove their legitimacy through on-chain verification.

## ✨ Features

- 🏛️ **Charity Registration** - Organizations can register and get verified
- 💰 **Transparent Donations** - All donations are recorded on-chain
- 🔍 **Full Traceability** - Track every donation from donor to charity
- ✅ **Verification System** - Only verified charities can receive donations
- 📊 **Real-time Analytics** - View donation statistics and charity performance
- 🔒 **Secure Withdrawals** - Only authorized charity wallets can withdraw funds

## 🚀 Quick Start

### Prerequisites
- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Node.js and npm

### Installation
```bash
git clone <repository-url>
cd charity-transparency-ledger
clarinet check
```

## 📋 Contract Functions

### 🏛️ Charity Management

#### Register a Charity
```clarity
(register-charity "Charity Name" "Description of charity work" 'SP1CHARITY-WALLET-ADDRESS)
```

#### Verify a Charity (Owner Only)
```clarity
(verify-charity u1)
```

### 💝 Donation Functions

#### Make a Donation
```clarity
(donate u1 (some "Thank you for your amazing work!"))
```

#### Withdraw Donations (Charity Only)
```clarity
(withdraw-donation u1)
```

### 🔍 Read-Only Functions

#### Get Charity Information
```clarity
(get-charity u1)
(get-charity-by-wallet 'SP1CHARITY-WALLET)
```

#### Trace a Donation
```clarity
(trace-donation u1)
```

#### View Donation History
```clarity
(get-donor-donations 'SP1DONOR-WALLET u10)
(get-charity-donations u1 u10)
```

#### Contract Statistics
```clarity
(get-contract-stats)
```

## 📖 Usage Examples

### 1. 🏛️ Register Your Charity
```bash
clarinet console
>>> (contract-call? .charity-transparency-ledger register-charity "Hope Foundation" "Providing clean water to communities" 'SP2CHARITY-WALLET)
```

### 2. ✅ Get Verified (Contract Owner)
```bash
>>> (contract-call? .charity-transparency-ledger verify-charity u1)
```

### 3. 💰 Make a Donation
```bash
>>> (contract-call? .charity-transparency-ledger donate u1 (some "Keep up the great work!"))
```

### 4. 🔍 Trace Your Donation
```bash
>>> (contract-call? .charity-transparency-ledger trace-donation u1)
```

### 5. 💸 Withdraw Funds (Charity)
```bash
>>> (contract-call? .charity-transparency-ledger withdraw-donation u1)
```

## 🏗️ Contract Architecture

```
📦 Charity Transparency Ledger
├── 🏛️ Charity Registry
│   ├── Registration system
│   ├── Verification process
│   └── Wallet mapping
├── 💰 Donation Tracking
│   ├── Donation records
│   ├── Donor history
│   └── Charity receipts
└── 🔍 Transparency Layer
    ├── Full traceability
    ├── Public statistics
    └── Audit trails
```

## 🛡️ Security Features

- ✅ **Owner-only verification** - Only contract owner can verify charities
- 🔒 **Wallet authorization** - Only charity wallets can withdraw their funds
- 💯 **Double-spend protection** - Donations can only be withdrawn once
- 🔍 **Full audit trail** - Every transaction is permanently recorded

## 📊 Data Structures

### Charity Record
```clarity
{
  name: (string-ascii 128),
  description: (string-ascii 256),
  wallet: principal,
  verified: bool,
  total-received: uint,
  registration-block: uint
}
```

### Donation Record
```clarity
{
  donor: principal,
  charity-id: uint,
  amount: uint,
  timestamp: uint,
  message: (optional (string-ascii 256)),
  withdrawn: bool
}
```

## 🧪 Testing

Run the test suite:
```bash
clarinet test
```

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 📄 License

This project is licensed under the MIT License.

## 🙏 Acknowledgments

- Built with ❤️ for transparency in charitable giving
- Powered by Stacks blockchain and Clarity smart contracts
- Inspired by the need for accountable philanthropy

---

**🌍 Making charity transparent, one donation at a time!**
