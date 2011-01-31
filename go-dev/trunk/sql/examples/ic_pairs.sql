SELECT DISTINCT
       term.acc,
       term.term_type,
       term.name,
       term2.acc AS acc2,
       term2.term_type AS term_type2,
       term2.name AS name2
 FROM evidence AS ic 
       INNER JOIN evidence_dbxref AS ex ON (ic.id=evidence_id)
       INNER JOIN dbxref ON (ex.dbxref_id=dbxref.id)
       INNER JOIN association AS a ON (a.id=ic.association_id)
       INNER JOIN term ON (a.term_id=term.id) 
       INNER JOIN term AS term2 ON (concat(xref_dbname,':',xref_key) = term2.acc)
 WHERE ic.code = 'IC';

