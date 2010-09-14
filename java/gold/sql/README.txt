
NATURAL KEYS

We avoid integer surrogate keys

- these don't model anything in the domain
- these slow bulkloading
- typically require extra joins

Note we may use an integer key e.g. for ncbi_taxon, but this isn't a
surrogate, we treat this as a natural key

We avoid n-place primary keys - all primary keys are single
valued. where we have complex data lacking an assigned identifier we
use a "skolem expression pattern" - we generate a natural key using
some standard syntax. Handily, this is often isomorphic to what goes
in the GAF column.

Examples:

- col16 expressions
- evidence expressions

This gives us a considerable amount of future proofing. For example,
defer on evidence chain / evidence set, define syntax later

FOREIGN KEYS

we deliberately avoid foreign key assignments in the core schema

