;;;;
;;;; See mysql.el in this directory for instruction.
;;;;

(provide 'my2sql)
(require 'cl)
(require 'sql)

(defvar my2sql-dictionary '())

(defvar my2sql-keywords
'("ADD" "ALL" "ALTER" "ANALYZE" "AND" "AS" "ASC" "ASENSITIVE" "BEFORE" "BETWEEN" "BIGINT" "BINARY" "BLOB" "BOTH" "BY" "CALL" "CASCADE" "CASE" "CHANGE" "CHAR" "CHARACTER" "CHECK" "COLLATE" "COLUMN" "CONDITION" "CONSTRAINT" "CONTINUE" "CONVERT" "CREATE" "CROSS" "CURRENT_DATE" "CURRENT_TIME" "CURRENT_TIMESTAMP" "CURRENT_USER" "CURSOR" "DATABASE" "DATABASES" "DAY_HOUR" "DAY_MICROSECOND" "DAY_MINUTE" "DAY_SECOND" "DEC" "DECIMAL" "DECLARE" "DEFAULT" "DELAYED" "DELETE" "DESC" "DESCRIBE" "DETERMINISTIC" "DISTINCT" "DISTINCTROW" "DIV" "DOUBLE" "DROP" "DUAL" "EACH" "ELSE" "ELSEIF" "ENCLOSED" "ESCAPED" "EXISTS" "EXIT" "EXPLAIN" "FALSE" "FETCH" "FLOAT" "FLOAT4" "FLOAT8" "FOR" "FORCE" "FOREIGN" "FROM" "FULLTEXT" "GRANT" "GROUP" "HAVING" "HIGH_PRIORITY" "HOUR_MICROSECOND" "HOUR_MINUTE" "HOUR_SECOND" "IF" "IGNORE" "IN" "INDEX" "INFILE" "INNER" "INOUT" "INSENSITIVE" "INSERT" "INT" "INT1" "INT2" "INT3" "INT4" "INT8" "INTEGER" "INTERVAL" "INTO" "IS" "ITERATE" "JOIN" "KEY" "KEYS" "KILL" "LEADING" "LEAVE" "LEFT" "LIKE" "LIMIT" "LINES" "LOAD" "LOCALTIME" "LOCALTIMESTAMP" "LOCK" "LONG" "LONGBLOB" "LONGTEXT" "LOOP" "LOW_PRIORITY" "MATCH" "MEDIUMBLOB" "MEDIUMINT" "MEDIUMTEXT" "MIDDLEINT" "MINUTE_MICROSECOND" "MINUTE_SECOND" "MOD" "MODIFIES" "NATURAL" "NOT" "NO_WRITE_TO_BINLOG" "NULL" "NUMERIC" "ON" "OPTIMIZE" "OPTION" "OPTIONALLY" "OR" "ORDER" "OUT" "OUTER" "OUTFILE" "PRECISION" "PRIMARY" "PROCEDURE" "PURGE" "READ" "READS" "REAL" "REFERENCES" "REGEXP" "RELEASE" "RENAME" "REPEAT" "REPLACE" "REQUIRE" "RESTRICT" "RETURN" "REVOKE" "RIGHT" "RLIKE" "SCHEMA" "SCHEMAS" "SECOND_MICROSECOND" "SELECT" "SENSITIVE" "SEPARATOR" "SET" "SHOW" "SMALLINT" "SONAME" "SPATIAL" "SPECIFIC" "SQL" "SQLEXCEPTION" "SQLSTATE" "SQLWARNING" "SQL_BIG_RESULT" "SQL_CALC_FOUND_ROWS" "SQL_SMALL_RESULT" "SSL" "STARTING" "STRAIGHT_JOIN" "TABLE" "TERMINATED" "THEN" "TINYBLOB" "TINYINT" "TINYTEXT" "TO" "TRAILING" "TRIGGER" "TRUE" "UNDO" "UNION" "UNIQUE" "UNLOCK" "UNSIGNED" "UPDATE" "USAGE" "USE" "USING" "UTC_DATE" "UTC_TIME" "UTC_TIMESTAMP" "VALUES" "VARBINARY" "VARCHAR" "VARCHARACTER" "VARYING" "WHEN" "WHERE" "WHILE" "WITH" "WRITE" "XOR" "YEAR_MONTH" "ZEROFILL"))

(defun my2sql-raw-mysqlshow (&optional table)
  "Get raw database (optional table) information using mysqlshow."
  (let ((cmd-list (list "mysqlshow"
			(concat "--host=" sql-server)
			(concat "--user=" sql-user)
			(concat "--password=" sql-password)
			sql-database)))
    (shell-command-to-string
    ;;(message "%s"
     (mapconcat 'identity
		(if table (append cmd-list
				  (list (my2sql-cmd-escape-underscore table)))
		  cmd-list)
		" "))))

(defun my2sql-cmd-trim-front (s)
  "Remove the first little bit."
  (replace-regexp-in-string "^| " "" s))

(defun my2sql-cmd-trim-back (s)
  "Remove everything after the first piece of text."
  (string-match "^[0-9A-Za-z_]+" s)
  (subseq s 0 (match-end 0)))

(defun my2sql-cmd-escape-underscore (s)
  "Properly replace underscores for the shell. This is horribly
weird, but needed."
  (replace-regexp-in-string "_" "\\\\\\\\_" s))

(defun my2sql-first-column (&optional table)
  (mapcar 'my2sql-cmd-trim-back
	  (mapcar 'my2sql-cmd-trim-front
		  (let ((row-input (split-string (my2sql-raw-mysqlshow table) "\n")))
		    (subseq row-input 4 (- (length row-input) 2))))))

(defun my2sql-tables ()
  (my2sql-first-column))

(defun my2sql-columns (table)
  (my2sql-first-column table))

(defun my2sql-make-dictionary ()
  (interactive)
  (let ((retlist my2sql-keywords))
    (dolist (tbl (my2sql-tables))
      (setq retlist (cons tbl retlist))
      ;;(message "=>: %s" retlist)
      (dolist (col (my2sql-columns tbl))
	(setq retlist (cons col retlist))
	(setq retlist (cons (concat tbl "." col) retlist))))
	;;(setq retlist (cons col retlist))))
    (setq my2sql-dictionary (sort retlist 'string<))))

;; Actual completer.
(defun my2sql-complete ()
  (interactive)
  (comint-dynamic-simple-complete 
   (comint-word "A-Za-z0-9_\.")
   my2sql-dictionary))
