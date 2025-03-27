# finders work

    Code
      get_container(table, find_char(table, "6", all = FALSE, path = TRUE))
    Output
      ENVIRONMENT: \begin{tabular}[t]{l|r|r}
      \hline
        & mpg & cyl\\
      \hline
      Mazda RX4 & 21 & 6\\
      \hline
      Mazda RX4 Wag & 21 & 6\\
      \hline
      \end{tabular}

---

    Code
      find_pattern(parsed, "RX4 Wag", fixed = TRUE)
    Output
      path=2 range=39:42

---

    Code
      get_container(table, find_char(table, "6", all = FALSE, path = TRUE))
    Output
      ITEMLIST:  6\\

---

    Code
      find_pattern(parsed, "RX4 Wag", fixed = TRUE)
    Output
      path=2 range=39:42

# setters work

    Code
      table1
    Output
      ENVIRONMENT: \begin{tabular}[t]{l|r|r}
      \hline
        & mpg & cyl\\
      \hline
      Mazda RX4 & 21 & 6\\
      \hline
      Mazda RX4 Wag & 21 & {6}\\
      \hline
      \end{tabular}

---

    Code
      table1
    Output
      ENVIRONMENT: \begin{tabular}[t]{l|r|r}
      \hline
        & mpg & cyl\\
      \hline
      Mazda RX4 & 21 & 6\\
      \hline
      Mazda RX4 Wag & 21 & {6}6\\
      \hline
      \end{tabular}

---

    Code
      parsed1
    Output
      
      \begin{tabular}[t]{l|r|r}
      \hline
        & mpg & cyl\\
      \hline
      Mazda RX4 & 21 & 6\\
      \hline
      Mazda RX4 Wagon & 21 & 6\\
      \hline
      \end{tabular}

---

    Code
      table1
    Output
      ENVIRONMENT: \begin{tabular}[t]{l|r|r}
      \hline
        & mpg & cyl\\
      \hline
      Mazda RX4 & 21 & 6\\
      \hline
      Mazda RX4 Wag & 21 & {6}\\
      \hline
      \end{tabular}

---

    Code
      table1
    Output
      ENVIRONMENT: \begin{tabular}[t]{l|r|r}
      \hline
        & mpg & cyl\\
      \hline
      Mazda RX4 & 21 & 6\\
      \hline
      Mazda RX4 Wag & 21 & {6}6\\
      \hline
      \end{tabular}

---

    Code
      parsed1
    Output
      
      \begin{tabular}[t]{l|r|r}
      \hline
        & mpg & cyl\\
      \hline
      Mazda RX4 & 21 & 6\\
      \hline
      Mazda RX4 Wagon & 21 & 6\\
      \hline
      \end{tabular}

