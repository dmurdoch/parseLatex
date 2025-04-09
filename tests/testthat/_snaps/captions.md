# captions functions work

    Code
      find_caption(table)
    Output
      path=3 range=all

---

    Code
      path_to_caption(parsed)
    Output
      [1] 1 3
      attr(,"extra")
      path=1 range=2:5

---

    Code
      find_caption(table)
    Output
      path=1,3 range=all

---

    Code
      path_to_caption(parsed)
    Output
      [1] 1 1 3
      attr(,"extra")
      path=1,1 range=2:4

