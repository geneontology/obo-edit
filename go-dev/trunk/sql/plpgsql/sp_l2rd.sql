-- get all parents of one term (l2rd = leaf to root with depth)
-- depth counts from leaf to root
-- depth is not actual graph node legnth from root, it is rather the order of sets

-- usage: call this function first (e.g 'select l2rd(60);'), which will return a temp table name;
--        WITHIN the same session, you can get results from the temp table ('select * from tmpl2rd;')
-- NOTE: make sure plpgsql is installed for your GO db of pgsql RDBMS and load this fnc before use

CREATE OR REPLACE Function l2rd(integer)
    returns text as
'
Declare
    tmptbl text := ''tmpl2rd'';
    termid alias for $1;
    myrc record;
    pcount integer := 1;
    depth integer := 1; -- no need when check for duplicate
Begin
    execute ''create temp table '' || tmptbl || 
        '' (id integer, name varchar(255), cid integer, depth integer);'';
    execute ''create temp table tmpchildl2rd (pid integer);'';
    execute ''create temp table tmpchildl2rd2 (pid integer);'';
    execute ''insert into tmpchildl2rd values('' || termid || '');'';
    execute ''insert into tmpchildl2rd2 values('' || termid || '');'';

    while (pcount > 0) loop
        -- get all parents of child
        execute ''insert into '' || tmptbl || '' select distinct p.id, p.name, 
            t2t.term2_id as cid, '' || depth || '' from term p, term2term t2t,
            tmpchildl2rd c where p.id = t2t.term1_id
            and t2t.term2_id = c.pid;'';

        execute ''delete from tmpchildl2rd;'';
        execute ''insert into tmpchildl2rd select distinct t2t.term1_id 
            from term2term t2t, tmpchildl2rd2 c 
            where t2t.term2_id = c.pid;''; -- get parent id from childl2rd
        execute ''delete from tmpchildl2rd2;'';
        execute ''insert into tmpchildl2rd2 select * from tmpchildl2rd;'';
        execute ''select * from tmpchildl2rd2;'';
        get diagnostics pcount = row_count;
        depth := depth + 1;
    end loop;

    execute ''insert into '' || tmptbl || '' select id, name, 0, 0 
            from term where id = '' || termid || '';'';

    execute ''drop table tmpchildl2rd;'';
    execute ''drop table tmpchildl2rd2;'';
    return tmptbl;
End;   
'
LANGUAGE 'plpgsql';
