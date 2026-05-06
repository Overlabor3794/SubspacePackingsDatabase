## Part 1: Environment Setup

This script is intended to run in a Unix-like environment with SageMath.

### Recommended OS

- **Linux (recommended)**
- **macOS**
- **Windows → use WSL (Windows Subsystem for Linux)**

---

### Windows Users

Do **not** run this natively on Windows. Use WSL.

Install WSL:
```bash
wsl --install
```

Then open a Linux shell (e.g., Ubuntu).

---

### Install Sage

```bash
sudo apt update
sudo apt install sagemath
```

---

## Part 2: Running the File

- Open terminal
- Navigate to the script directory:
```bash
cd /path/to/sage/folder
```

- Run the script:
```bash
sage SymPGoSKopp.sage
```
- Or run in wsl:
```bash
wsl sage SymPGoSKopp.sage
```

- Enter `d` and `n` when prompted

- If multiple matching files exist, you will be asked to choose one:
  - Enter an integer (e.g., `3`) to select the 3rd file

---

## Part 3: Modifying Input Source

Relevant lines in the script:

```sage
folder = "../Packings"
prefix = f"etf_{d}x{n}"
tol = 1e-5
```

- `folder`: path to directory containing `.gos` files  
- `prefix`: filename pattern used for matching  
- `tol`: numerical tolerance (adjust for precision issues)


## Part 4: Output Description

When the script runs, it prints several sections:

---

### File Selection

```text
Using file: etf_dxn_*.gos
```

Confirms which input file is being used.

---

### Gram Matrix Check

```text
=== CHECK 1: Gram built ===
```

Indicates the data was successfully loaded and processed.

---

### Number of Edge Labels

```text
=== CHECK 2: number of colors ===
<number>
```

- Shows how many distinct edge labels (triple product multiset invariants) were found  

---

### Automorphism Group

```text
=== Expected Group : GG ez===
filename: ...
Order: <integer>
```

- Displays the computed automorphism group
- `Order` = size of the group

---

### Generators

```text
Generators:
(g1)
(g2)
...
```

- Lists generators of the automorphism group
- Each generator is given as a permutation

---

### Verification of Generators

```text
=== Further Checks: Verify on Generators ===
 Passes for generator: ...
 Fails for generator: ...
```

- Checks whether generators preserve the triple-product invariant
- If all pass:
```text
All generators preserve triple product
```

---

### Additional Checks (if needed)

If some generators fail:

#### Small groups: (currently set to less than 100, chnage in part 7 to update)
- The script checks **all elements**
```text
=== Small Group : Checking Elements ===
```

- Outputs which elements pass/fail
- Final result:
```text
Order of symmetric group: <integer>
```

---

#### Intermediate groups (special cases like n = d²):

- Checks selected permutations (cycles)
- Outputs valid transformations

---

### Final Interpretation

- If all generators pass → automorphism group is valid  
- If not → filtered subgroup (preserving triple product) is reported  

---
