-- get all childrens of one term (r2ld = root to leaf with depth)
-- depth counts from root to leaf
-- depth for leaf node (cid = 0) is meaningless

-- usage: call this function first (e.g 'select r2ld(58);'), which will return a temp table name;
--        WITHIN the same session, you can get results from the temp table ('select * from tmpr2ld;')
-- NOTE: make sure plpgsql is installed for your GO db of pgsql RDBMS and load this fnc before use

CREATE OR REPLACE Function r2ld(integer)
    returns text as
'
Declare
    tmptbl text := ''tmpr2ld'';
    termid alias for $1;
    myrc record;
    pcount integer := 1;
    depth integer := 1; -- no need when check for duplicate
    query text := '''';
    iquery text := '''';
Begin
    execute ''create temp table '' || tmptbl || 
        '' (id integer, name varchar(255), cid integer, depth integer);'';
    execute ''create temp table tmpchildr2ld (pid integer);'';
    execute ''create temp table tmpchildr2ld2 (pid integer);'';
    execute ''insert into tmpchildr2ld values('' || termid || '');'';
    execute ''insert into tmpchildr2ld2 values('' || termid || '');'';
    while (pcount > 0) loop
        -- get all children
        query := ''insert into '' || tmptbl || '' select distinct c.id, c.name,
            t2tc.term2_id as cid, '' || depth || '' from term c, term2term t2t,
            tmpchildr2ld p, term2term t2tc where p.pid = t2t.term1_id  
            and c.id = t2t.term2_id and c.id = t2tc.term1_id;'';
--        raise notice ''%'', query;
        execute query;

        execute ''delete from tmpchildr2ld;'';
        execute ''insert into tmpchildr2ld select distinct t2t.term2_id 
            from term2term t2t, tmpchildr2ld2 p 
            where t2t.term1_id = p.pid;''; -- get children of parent
        execute ''delete from tmpchildr2ld2;'';
        execute ''insert into tmpchildr2ld2 select * from tmpchildr2ld;'';
        execute ''select * from tmpchildr2ld2;'';
        get diagnostics pcount = row_count;
        depth := depth + 1;
    end loop;

    execute ''insert into '' || tmptbl || '' select t.id, t.name, t2t.term2_id, 0
            from term t, term2term t2t where t.id = t2t.term1_id and t.id = '' || 
            termid || '';'';

    -- get nodes that have no child
    execute ''delete from tmpchildr2ld;'';
    execute ''insert into tmpchildr2ld select distinct t.cid from '' ||
            tmptbl || '' t left join term2term t2t on t.cid = t2t.term1_id
            where t2t.term1_id is null;'';
    execute ''insert into '' || tmptbl || '' select t.id, t.name, 0, '' || depth-2 || 
            '' from term t, tmpchildr2ld p where p.pid = t.id;'';

    -- get the parent without child
    EXECUTE ''insert into '' || tmptbl || '' select id, name, 0, 0 from term where id = '' || termid || '' and not exists (select * from '' || tmptbl || '');'';

    execute ''drop table tmpchildr2ld;'';
    execute ''drop table tmpchildr2ld2;'';
    return tmptbl;
End;   
'
LANGUAGE 'plpgsql';
