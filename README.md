# haskell_binarysearchtree

A simple Binary Search Tree (BST) implementation produced using a test-driven development methodology. Includes automated unit tests and property-based testing. This project provides functionality to insert, lookup, and list BST entries in key order.

📦 Installation

Using Stack

Clone the repository and navigate to the project directory:

```bash
git clone https://github.com/tomdennisdev/haskell_binarysearchtree.git
cd haskell_binarysearchtree
```

Ensure Stack is installed:

```bash
stack --version
```

If not installed, follow Stack installation guide.

Build the project:

```bash
stack setup
stack build
```

🚀 Running the Program

To execute the main program:

```bash
stack run
```

🛠 Running Tests

This project includes HUnit tests and QuickCheck property-based tests.
To run all tests:

```bash
stack test
```

Alternatively, you can load the test suite in GHCi:

```bash
stack ghci test/Spec.hs
> main
```

📂 Project Structure

📂 Haskell-BinarySearchTree/
```bash
├── 📂 app/
│   ├── Main.hs         # Main function (not used in this project)
├── 📂 src/
│   ├── BST.hs         # Binary Search Tree implementation
│   ├── Lib.hs         # File to store functions
├── 📂 test/
│   ├── Spec.hs        # Unit and property tests
├── 📜 package.yaml    # Stack project configuration
├── 📜 stack.yaml      # Stack resolver configuration
├── 📜 README.md       # Documentation
├── 📜 LICENSE         # License information
└── 📜 .gitignore      # Ignored files
```

📝 License

This project is licensed under the MIT License. See the LICENSE file for details.


