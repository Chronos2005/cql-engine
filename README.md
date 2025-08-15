# CQL Engine: A High-Performance CSV Query Language

**CQL Engine** is a lightweight, high-performance query engine written in Haskell that allows users to perform SQL-like queries directly on CSV files. Built from the ground up with the **Alex** lexer and **Happy** parser toolchains, it transforms raw data into meaningful insights without the need for a traditional database setup.

The project features a complete interpreter, a robust set of query operations, and a **VS Code extension for full syntax highlighting**.

---

## Key Features

* **SQL-Inspired Syntax**: Leverage a familiar `SELECT`, `FROM`, `WHERE`, `ORDER BY` structure for intuitive and powerful data manipulation.
* **Multi-File Joins**: Perform complex joins across multiple CSV files by specifying them in the `FROM` clause.
* **Advanced Set Operations**: Combine query results with `UNION`, `INTERSECT`, and `EXCEPT` for sophisticated data analysis.
* **Rich Expression Language**: Includes column references, aliasing, string concatenation, and built-in functions like `COALESCE` and `EXISTS`.
* **Robust Error Handling**: Provides clear, user-friendly error messages for file I/O, parsing, and runtime issues—no stack traces.
* **VS Code Integration**: Comes with a dedicated VS Code extension that provides full syntax highlighting for `.cql` files, improving developer productivity.

---

## Syntax Highlighting Showcase

The included VS Code extension makes writing queries simple and readable.

### VS Code Extension Setup

To enable syntax highlighting for `.cql` files, you can manually install the extension:

1.  Navigate to the `CQLSyntaxHilighting` directory within this repository.
2.  Copy the entire directory.
3.  Paste it into your local VS Code extensions folder. The location is typically:
    * **Windows:** `%USERPROFILE%\.vscode\extensions`
    * **macOS / Linux:** `~/.vscode\extensions`
4.  Restart VS Code. Files with the `.cql` extension will now have syntax highlighting.

---

## Getting Started

### Prerequisites

* [Stack (The Haskell Tool Stack)](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

### Installation & Build

1.  **Clone the repository:**
    ```bash
    git clone [https://github.com/Chronos2005/cql-engine.git](https://github.com/Chronos2005/cql-engine.git)
    cd cql-engine
    ```

2.  **Build the project:**
    This command will download all necessary dependencies and compile the executable.
    ```bash
    stack build
    ```

---

## Usage Example

Given the following CSV files:

**`employees.csv`**
```csv
1,Alice,Dev
2,Bob,Sales
3,Charlie,Dev
````

**`departments.csv`**

```csv
Dev,Technology
Sales,Business
```

You can run a query to find all employees in the "Technology" department:

**`query.cql`**

```cql
-- Find all employees in the Technology department and show their ID and name.
FROM employees AS e, departments AS d
WHERE e.3 == d.1 AND d.2 == "Technology"
SELECT e.1 AS EmployeeID, e.2 AS Name
ORDER BY Name ASC
```

**Execute the query:**

```bash
stack exec cql-engine-exe -- examples/query.cql
```

**Output:**

```csv
1,Alice
3,Charlie
```

-----

## Full Documentation

For a complete guide to the CQL language, including syntax, features, design rationale, and the full BNF grammar, please see the [**User Manual**](./docs/userManual.pdf).

-----

## Technology Stack

  * **Core Language**: Haskell
  * **Parser Generator**: Happy
  * **Lexical Analyzer**: Alex
  * **Build Tool**: Stack

-----

## License

This project is licensed under the BSD-3-Clause License. See the [LICENSE](https://www.google.com/search?q=./LICENSE) file for details.

