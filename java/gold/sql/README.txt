
NATURAL KEYS

We avoid integer surrogate keys

- these don't model anything in the domain
- these slow bulkloading
- typically require extra joins

Note we may use an integer key e.g. for ncbi_taxon, but this isn't a
surrogate, we treat this as a natural key

We avoid n-place primary keys - all primary keys are single
valued. where 
