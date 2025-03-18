
Implement a simple single-file elm application that will allow users to interactively explore when do the congruence equations of the form
ax \equiv b \pmod{n}

have no solutions/unique solution/multiple solutions.

User should be able to set parameters a, b and n to arbitrary values from 1 to 100 using numeric input fields.

For given N, render 100x100 table which has values of a from 1 to 100 represented by rows, values of b from 1 to 100 represented by columns
and cell i,j should contain the solutions of the equation.
If there are no solutions, put empty set symbol in the cell and make the cell have light red background.
If there is unique solution, put just that solution to that cell, and make the cell have light green background.
If there are multiple solution, fill them in as set (like {1,2,3}) and make the cell have light blue  background.