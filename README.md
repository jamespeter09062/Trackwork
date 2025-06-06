# 🏗️ Trackworks - Public Works Tracker

A Clarity smart contract for tracking contractor payments and project status in public works projects. Built for transparency and accountability in government contracting.

## 🚀 Features

- ✅ **Project Management**: Create and track public works projects
- 💰 **Payment Tracking**: Monitor contractor payments and budget utilization  
- 📊 **Status Updates**: Real-time project status management
- 🔒 **Access Control**: Owner-only administrative functions
- 📈 **Budget Analytics**: Track spending vs budget with completion percentages

## 🛠️ Contract Functions

### Public Functions

#### `create-project`
Creates a new public works project
```clarity
(create-project "Road Repair Project" 'SP1234...CONTRACTOR 1000000)
```

#### `update-project-status` 
Updates project status (active, completed, suspended, etc.)
```clarity
(update-project-status u1 "completed")
```

#### `create-payment`
Creates a payment request for a project
```clarity
(create-payment u1 50000 "Phase 1 completion payment")
```

#### `approve-payment`
Approves a pending payment request
```clarity
(approve-payment u1)
```

#### `reject-payment`
Rejects a pending payment request
```clarity
(reject-payment u1)
```

### Read-Only Functions

#### `get-project`
Retrieves project details by ID
```clarity
(get-project u1)
```

#### `get-payment`
Retrieves payment details by ID
```clarity
(get-payment u1)
```

#### `get-project-budget-status`
Gets budget analysis for a project
```clarity
(get-project-budget-status u1)
```

## 🏃‍♂️ Quick Start

### Prerequisites
- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

1. Clone or download the contract files
2. Initialize Clarinet project:
```bash
clarinet new trackworks-project
```

3. Replace the generated contract with Trackworks.clar
4. Test the contract:
```bash
clarinet test
```

5. Deploy locally:
```bash
clarinet integrate
```

## 📋 Usage Example

```clarity
;; 1. Create a new project
(contract-call? .Trackworks create-project "Highway Bridge Repair" 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7 500000)

;; 2. Create payment request
(contract-call? .Trackworks create-payment u1 100000 "Materials and labor - Week 1")

;; 3. Approve payment
(contract-call? .Trackworks approve-payment u1)

;; 4. Check project status
(contract-call? .Trackworks get-project-budget-status u1)
```

## 🔐 Security Features

- **Owner-only Access**: Only contract deployer can manage projects and payments
- **Budget Validation**: Prevents overpayment beyond project budget
- **Status Tracking**: Immutable payment history and status changes
- **Error Handling**: Comprehensive error codes for all edge cases

## 📊 Data Structure

### Projects
- Project ID, name, contractor address
- Total budget and paid amounts
- Status and timestamps
- Budget utilization tracking

### Payments  
- Payment ID, project association
- Amount, description, status
- Creation and payment timestamps
- Approval/rejection tracking

## 🤝 Contributing

This is an MVP implementation. Future enhancements could include:
- Multi-signature approvals
- Milestone-based payments  
- Contractor self-service portal
- Integration with external audit systems

## 📄 License

Open source - feel free to fork and modify for your public works tracking needs!



