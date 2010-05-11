SELECT ancestor.*, 
        graph_path.distance,
        graph_path.term1_id AS ancestor_id
 FROM 
  term child, graph_path, term ancestor
where
 child.id=graph_path.term2_id
  AND ancestor.id=graph_path.term1_id
AND child.name='nucleus';

