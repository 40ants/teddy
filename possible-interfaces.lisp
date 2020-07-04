;; select: selects a subset of columns from a dataframe.
(select df col1 col2 col3)

;; filter: filters rows of a dataframe to those that satisfy some conditions.
(filter df (< col1 col2))

(filter df (and (< col1 col2)
		(= col2 0)))

;; group-by: groups a dataframe by columns.
(group-by df col1 col2)

;; mutate: create new variables in a dataframe.
(mutate df (col4 (/ col2 col3)))
	      
(mutate df ((col4 (* col1 col2))
	    (col5 (string-upcase col3))))

;; arrange: arrange a dataframe by a column
(arrange df col1 col2)

;; summarize: summarize a dataframe with aggregating functions
(summarize df ((avg-col1 (mean col1))
	       (max-col2 (max col2))))

;; head: retrieve the top 5 entries of a dataframe.
(head df 50)
