-- terms in 3 ontologies
SELECT term_type, count(*) FROM term GROUP BY term_type;

-- obsolete terms in 3 ontologies
SELECT term_type, count(*) FROM term WHERE is_obsolete=1
 GROUP BY term_type;

-- undefined terms
SELECT term_type, count(distinct term.id)
FROM term LEFT OUTER JOIN term_definition ON (term.id=term_id)
WHERE term_definition IS NULL
GROUP BY term_type;

-- undefined non-obsolete terms
SELECT term_type, count(distinct term.id)
FROM term LEFT OUTER JOIN term_definition ON (term.id=term_id)
WHERE term_definition IS NULL
 AND is_obsolete=0
GROUP BY term_type;

-- terms with DIRECT annotations
SELECT term_type, count(distinct term.id)
FROM term INNER JOIN association ON (term.id=term_id)
GROUP BY term_type;

-- terms with annotations (direct or transitive)
SELECT term_type, count(distinct term.id)
FROM term INNER JOIN graph_path ON (term.id=term1_id)
          INNER JOIN association ON (term2_id=association.term_id)
GROUP BY term_type;
