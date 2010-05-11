-- get all parents of one term (l2rd = leaf to root with depth)
-- depth counts from leaf to root
-- depth for leaf node (cid = 0) is meaningless


-- NOTE: this sp works only in PostgreSQL 7.3 or later
-- a related note: in v7.3, you can return setof table_name (table type)
-- for both sql and plpgsql language stored procedures (functions)

-- usage: select * from r2ld_new(4); where 4 is parent term id

-- NOTE: make sure plpgsql is installed for your GO db of pgsql RDBMS and load this fnc before use


DROP TYPE spt_r2ld CASCADE;
CREATE TYPE spt_r2ld AS (
   pid integer,
   name varchar(255),
--   term_type text,
--   acc text,
   cid integer,
   depth integer
);

CREATE OR REPLACE FUNCTION r2ld_new(integer) RETURNS SETOF spt_r2ld AS
'
DECLARE
    termid alias for $1;
    myrc spt_r2ld%ROWTYPE;
    pcount integer := 1;
    depth integer := 1; -- no need when check for duplicate
    query text := '''';
BEGIN
    EXECUTE ''create temp table tmpr2ld (id integer, name varchar(255), cid integer, depth integer);'';
    EXECUTE ''create temp table tmpchildr2ld (pid integer, status integer default 0);'';
    EXECUTE ''insert into tmpchildr2ld values('' || termid || '', 0);'';
    WHILE (pcount > 0) LOOP
        -- get all children
        query := ''insert into tmpr2ld select distinct c.id, c.name,
            t2tc.term2_id as cid, '' || depth || '' from term c, term2term t2t,
            tmpchildr2ld p, term2term t2tc where p.pid = t2t.term1_id  
            and c.id = t2t.term2_id and c.id = t2tc.term1_id and p.status = 0;'';
--        raise notice ''%'', query;
        EXECUTE query;

        EXECUTE ''update tmpchildr2ld set status = 1 where status = 0;'';
        EXECUTE ''insert into tmpchildr2ld select distinct t2t.term2_id, 0 
            from term2term t2t, tmpchildr2ld p 
            where t2t.term1_id = p.pid and p.status = 1;''; -- get children of parent
        EXECUTE ''update tmpchildr2ld set status = 2 where status = 1;'';
        EXECUTE ''select * from tmpchildr2ld where status = 0;'';
        get diagnostics pcount = row_count;
        depth := depth + 1;
    end loop;

    EXECUTE ''insert into tmpr2ld select t.id, t.name, t2t.term2_id, 0
            from term t, term2term t2t where t.id = t2t.term1_id and t.id = '' || 
            termid || '';'';

    -- get nodes that have no child
    EXECUTE ''delete from tmpchildr2ld;'';
    EXECUTE ''insert into tmpchildr2ld select distinct t.cid from tmpr2ld t left join term2term t2t on t.cid = t2t.term1_id
            where t2t.term1_id is null;'';
    EXECUTE ''insert into tmpr2ld select t.id, t.name, 0, '' || depth-2 || 
            '' from term t, tmpchildr2ld p where p.pid = t.id;'';

    -- get the parent without child
    IF depth = 2 THEN
        EXECUTE ''insert into tmpr2ld select id, name, 0, 0 from term where id = '' || termid || '' and not exists (select * from tmpr2ld);'';
    END IF;
    EXECUTE ''drop table tmpchildr2ld;'';
    -- FOR myrc IN SELECT * FROM tmpr2ld LOOP: 2nd query wont work in one session
    -- as tmpr2ld is not static table!!
    FOR myrc IN EXECUTE ''SELECT * FROM tmpr2ld order by depth;'' LOOP
        RETURN NEXT myrc;
    END LOOP;
    EXECUTE ''drop table tmpr2ld;'';
    RETURN NULL;
END;   
'
LANGUAGE 'plpgsql';

