# tableRule works

    Code
      find_rules(table)
    Output
      [[1]]
      path=2,1 range=2:3
      
      [[2]]
      path=3,1 range=2:3
      
      [[3]]
      path=4,1 range=2:3
      
      [[4]]
      path=5,1 range=2:3
      

---

    Code
      find_rule(table, 2)
    Output
      path=3,1 range=2:2

