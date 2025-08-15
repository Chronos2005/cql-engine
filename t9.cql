FROM R AS X, R AS Y, R AS Z
WHERE
     X.2 == Y.1
 AND Y.2 == Z.1
SELECT
  X.1,
  Z.2


  