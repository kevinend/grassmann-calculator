This folder includes the files that transform the AST in some form or fashion.
The order of transforms are applied in the following order:
    1. resolve-symbols
    2. flatten
    3. distribute-products
    4. semantic check