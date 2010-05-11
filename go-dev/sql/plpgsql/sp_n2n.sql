-- get all nodes b/n 2 nodes, from 1st node to 2nd node
-- depth counts from 2nd node to 1st node

-- get all nodes down to leaf from from_node and then get all node in this tree from to_node

CREATE OR REPLACE Function n2n(integer, integer)
    returns text as
'
Declare
    tmptbl text := ''tmpn2n'';
    fromid alias for $1;
    toid alias for $2;
    myrc record;
    pcount integer := 1;
    depth integer := 1; -- no need when check for duplicate
    query text := '''';
    iquery text := '''';
Begin
    execute ''create temp table '' || tmptbl || 
        '' (id integer, name varchar(255), cid integer, depth integer);'';
    execute ''create temp table tmpn2n_temp 
            (id integer, name varchar(255), cid integer, depth integer);'';
    execute ''create temp table tmpn2n_temp2 
            (id integer, name varchar(255));'';
    execute ''create temp table tmpchildn2n (pid integer);'';
    execute ''create temp table tmpchildn2n2 (pid integer);'';
    execute ''insert into tmpchildn2n values('' || fromid || '');'';
    execute ''insert into tmpchildn2n2 values('' || fromid || '');'';
    -- get all down to leaf from 1st node
    while (pcount > 0) loop
        query := ''insert into tmpn2n_temp select distinct c.id, c.name,
            t2tc.term2_id as cid,'' || depth || '' from term c, term2term t2t,
            tmpchildn2n p, term2term t2tc where p.pid = t2t.term1_id  
            and c.id = t2t.term2_id and c.id = t2tc.term1_id;'';
--        raise notice ''%'', query;
        execute query;

        execute ''delete from tmpchildn2n;'';
        execute ''insert into tmpchildn2n select distinct t2t.term2_id 
            from term2term t2t, tmpchildn2n2 temp 
            where t2t.term1_id = temp.pid;''; -- get childn2n as next parent from parent
        execute ''delete from tmpchildn2n2;'';
        execute ''insert into tmpchildn2n2 select * from tmpchildn2n;'';
        execute ''select * from tmpchildn2n2;'';
        get diagnostics pcount = row_count;
        depth := depth + 1;
    end loop;

    execute ''insert into tmpn2n_temp2 select distinct id, name from tmpn2n_temp;'';

    -- now get all parents in the temp table to prune node that is not me 
    -- or my parents or their parents
    pcount := 1;
    depth := 1;
    execute ''delete from tmpchildn2n;'';
    execute ''delete from tmpchildn2n2;'';
    execute ''insert into tmpchildn2n values('' || toid || '');'';
    execute ''insert into tmpchildn2n2 values('' || toid || '');'';
    while (pcount > 0) loop
        query := ''insert into '' || tmptbl || '' select distinct p.id, p.name,
            t2t.term2_id as cid,'' || depth || '' from tmpn2n_temp2 p, term2term t2t,
            tmpchildn2n c where c.pid = t2t.term2_id  
            and p.id = t2t.term1_id;'';
        execute query;
        execute ''delete from tmpchildn2n;'';
        execute ''insert into tmpchildn2n select distinct t2t.term1_id 
            from term2term t2t, tmpchildn2n2 temp 
            where t2t.term2_id = temp.pid;''; -- get parent of child
        execute ''delete from tmpchildn2n2;'';
        execute ''insert into tmpchildn2n2 select * from tmpchildn2n;'';
        execute ''select * from tmpchildn2n2;'';
        get diagnostics pcount = row_count;
        depth := depth + 1;
    end loop;
    
    execute ''insert into '' || tmptbl || '' select t.id, t.name, t2t.term2_id, '' ||
            depth || '' from term t, term2term t2t where t.id = t2t.term1_id 
            and t.id = '' || fromid || '';'';
    execute ''insert into '' || tmptbl || '' select id, name, 0, 0 from term
            where id = '' || toid || '';'';

    execute ''drop table tmpchildn2n;'';
    execute ''drop table tmpchildn2n2;'';
    execute ''drop table tmpn2n_temp;'';
    execute ''drop table tmpn2n_temp2;'';
    return tmptbl;
End;   
'
LANGUAGE 'plpgsql';
