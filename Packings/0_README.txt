***********************************
LABEL AND DATA CONVENTIONS
***********************************

***********************************
ISOLATED ETFs
***********************************
An ETF is considered "isolated" if there exists an open neighborhood of it that does not contain any ETF that it is not unitarily equivalent to, and "nonisolated" otherwise. It is possible to test if an ETF is isolated by perturbing it by a small complex quantity in all directions, reoptimizing (minimizing the 4-frame potential), and checking whether the triple products remain the same (isolated) or have changed (nonisolated).

An ETF label for an isolated ETF is of the form "etf_dxn_tripalpha", where d, n, trip, and alpha will be replaced by certain data.
d = dimension
n = number of vectors
trip = the number of distinct triple products, including the two degenerate triple products
alpha = a lowercase letter or sequence of lowercase letters, uniquely specifying the projective unitary permutation equivalence class of the ETF among those with parameters (d,n,trip)

The data alpha for isolated ETFs with parameters (d,n,trip) are chosen in order from {a, b, c, ..., z, aa, ab, ...} based on the order in which ETFs are added to the database.

Labels for isolated ETFs are static (from May 5, 2026 onward, unless altered to correct mistakes). Each projective unitary permutation equivalence class will receive one label. New isolated ETFs added to the database should be checked for equivalence against ETFs already in the database.

***********************************
(COMPONENTS OF) NONISOLATED ETFs
***********************************
An ETF label for a nonisolated ETF is of the form "etf_dxn_tripALPHAdim" or "etf_dxn_tripALPHA", where d, n, trip, ALPHA, and dim will be replaced by certain data.
d = dimension
n = number of vectors
trip = the number of distinct triple products, including the two degenerate triple products
ALPHA = an uppercase letter or sequence of uppercase letters
dim = the dimension of the irreducible component of the ETF in the real algebraic "variety" of projective permutation unitary equivalence classes of ETFs. This data is only included in the dimension of the irreducible component is known (either proven or computed by a convincing heuristic algorithm)

An ETF label for a nonisolated ETF represents not a single projective unitary equivalence class of ETFs, but the irreducible component of that class in the real algebraic "variety" of projective permutation unitary equivalence classes of ETFs. The representative ETF is assumed to be a generic point on the component. (Note that the number of triple products is constant on a Zariski open subset of any component.) The word "variety" is in scare quotes because this object is not really a variety. Tentatively, it is the GIT quotient of the real algebraic variety of (d,n)-ETFs by the action of the product of the unitary group U(d) and the group of nxn generalized permutation matrices.

The data ALPHA for nonisolated ETFs with parameters (d,n,trip) are chosen in order from {A, B, C, ..., Z, AA, AB, ...} based on the order in which ETFs are added to the database. Labels past A will only be used if a proof or convincing heuristic is developed to distinguish different irreducible components with the same number of distinct triple products.

Labels for nonisolated ETFs of the form "etf_dxn_tripALPHAdim" are static (from May 5, 2026 onward, unless altered to correct mistakes). Each irreducible component of the real algebraic "variety" of projective permutation unitary equivalence classes will receive one label. New nonisolated ETFs with the same number of distinct triple products as existing ETFs should not be added until an algorithm is developed to distinguish components.

Labels for nonisolated ETFs of the form "etf_dxn_tripALPHA" are temporary and subject to change. They should be relabled once the dimension of the component is known.

***********************************
SPECIAL NONISOLATED ETFs
***********************************

"Special" nonisolated ETFs have temporary labels of the form "etf_dxn_tripALPHA_special", where d, n, trip, and ALPHA will be replaced by certain data.
d = dimension
n = number of vectors
trip = the number of distinct triple products, including the two degenerate triple products
ALPHA = an uppercase letter or sequence of uppercase letters

Special nonisolated ETFs are ETFs of interest that exist as parts of continuous families but are not generic representations of those families. They should have at least one of the following traits:
    1. They have fewer distict triple products than nearby ETFs.
    2. They have a larger symmetry group than nearby ETFs.
    3. They have a different matroid structure than nearby ETFs.
    4. They are real ETFs.
    5. They have been imported into the database from a difference set construction or other combinatorial construction in the literature, and their "special" status has not yet been determined.

The labeling scheme for special nonisolated ETFs is tentative and subject to breaking changes. All "special" labels should be treated as temporary.

***********************************
ETF file types
***********************************

Files associated to an ETF label are as follows.
    1. "{label}.gos" is a text file consisting of a list of real numbers that specifies the ETF numerically in the Game of Sloanes format.
    2. "{label}.exa" is a text file consisting of data specifying the ETF exactly by specifying the position of every unique entry in the first slice of the triple product tensor.
    3. "{label}.inv" is a text file specifying certain invariants of the ETF. The structure of this file is TBD.
    4. "{label}.tp" is a text file consisting of data specifying the ETF exactly by specifying the position of every unique entry in the entire triple product tensor.
