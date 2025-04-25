FROM P, Q
  WHERE P.1 == Q.1
  SELECT P.1, 
         COALESCE(P.2, Q.2), 
         COALESCE(P.3, Q.3), 
         COALESCE(P.4, Q.4)