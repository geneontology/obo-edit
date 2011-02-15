for num in 0 1 2 3 4 5 6 7 8 9 
do 
 echo -n ancest.sql ;  time /tools/mysql/current/bin/mysql --socket=/db0/mysql/admin/golite/mysql.sock --user=goadmin go -phoot1Nov < sql/examples/ancest.sql > ancest.sql.out

 echo -n annotated_to_nucleous.sql ;  time /tools/mysql/current/bin/mysql --socket=/db0/mysql/admin/golite/mysql.sock --user=goadmin go -phoot1Nov < sql/examples/annotated_to_nucleous.sql > annotated_to_nucleous.sql.out

 echo -n child.sql ;  time /tools/mysql/current/bin/mysql --socket=/db0/mysql/admin/golite/mysql.sock --user=goadmin go -phoot1Nov < sql/examples/child.sql > child.sql.out

 echo -n distance_from_root.sql ;  time /tools/mysql/current/bin/mysql --socket=/db0/mysql/admin/golite/mysql.sock --user=goadmin go -phoot1Nov < sql/examples/distance_from_root.sql > distance_from_root.sql.out

 echo -n ev_by_db.sql ;  time /tools/mysql/current/bin/mysql --socket=/db0/mysql/admin/golite/mysql.sock --user=goadmin go -phoot1Nov < sql/examples/ev_by_db.sql > ev_by_db.sql.out

 echo -n ev_wb.sql ;  time /tools/mysql/current/bin/mysql --socket=/db0/mysql/admin/golite/mysql.sock --user=goadmin go -phoot1Nov < sql/examples/ev_wb.sql > ev_wb.sql.out

 echo -n gp_by_db.sql ;  time /tools/mysql/current/bin/mysql --socket=/db0/mysql/admin/golite/mysql.sock --user=goadmin go -phoot1Nov < sql/examples/gp_by_db.sql > gp_by_db.sql.out

 echo -n gp_by_species.sql ;  time /tools/mysql/current/bin/mysql --socket=/db0/mysql/admin/golite/mysql.sock --user=goadmin go -phoot1Nov < sql/examples/gp_by_species.sql > gp_by_species.sql.out

 echo -n ic_pairs.sql ;  time /tools/mysql/current/bin/mysql --socket=/db0/mysql/admin/golite/mysql.sock --user=goadmin go -phoot1Nov < sql/examples/ic_pairs.sql > ic_pairs.sql.out

 echo -n gp_like_name.sql ; time /tools/mysql/current/bin/mysql --socket=/db0/mysql/admin/golite/mysql.sock --user=goadmin go -phoot1Nov < sql/examples/gp_like_name.sql > gp_like_name.out

done
