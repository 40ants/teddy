I want you to meet Teddy. Teddy wanna be like pandas. Pandas are cool. Teddy want be cool too!

![](teddy-bear.png)

# Reasoning

This library provides some Common Lisp facitilies to work with data frames.

Common Lisp already has [numcl](https://github.com/numcl/numcl) to operate on arrays, and now we need
a more abstract tool to work with data like data sheets.

Teddy make it possible to define a dataframe full of data, to slice it in different ways, to join data frames
and see some statistics about the data.

# How to create a data-frame

```lisp
CL-USER> (teddy/data-frame:make-data-frame '("Integers" "Floats")
                                           :rows (loop repeat 10
                                                       collect (list (random 100)
                                                                     (random 1.0))))
+----------+--------+
| Integers | Floats |
+----------+--------+
|       50 |   0.23 |
|       98 |   0.04 |
|       89 |   0.73 |
|       65 |   0.20 |
|       85 |   0.82 |
|       78 |   0.22 |
|       94 |   0.51 |
|       25 |   0.07 |
|       67 |   0.01 |
|       51 |   0.53 |
+----------+--------+

CL-USER> (teddy/data-frame:stats *)
+----------+------+------+-------+------+------+-------+-------+------+
| Column   | Min  | p25  | p50   | p75  | Max  | Mean  | SD    | Sum  |
+----------+------+------+-------+------+------+-------+-------+------+
| Integers |   25 |   51 | 72.50 |   89 |   98 | 70.20 | 23.17 |  702 |
| Floats   | 0.01 | 0.07 |  0.23 | 0.53 | 0.82 |  0.34 |  0.29 | 3.35 |
+----------+------+------+-------+------+------+-------+-------+------+
```

#