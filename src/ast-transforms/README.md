This folder includes the files that transform the AST in some form or fashion.
The order of transforms are applied in the following order:
    1. resolve-symbols
    2. expand/distribute
    3. flatten/roll-up
    4. semantic check