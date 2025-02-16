# Initial promopt

Please implement an Elm application that will help me illustrate weighted sampling with / without replacement.
The application should have 2 radio buttons at the top, that will allow me to select sampling strategy: with replacement / without replacement.

Then there should be 2 sections:
1. configuration of distribution to sample from
2. list of elements sampled so far

## configuration of distribution to sample from

There should be a table representing the set of elements available for sampling.
It should have 4 columns:
1. element - unique number from 1 to the total number of elements (e.g. 1, 2, 3 if we want to sample from the set of 3 elements). This field should not be editable.
2. element's weight. This should be represented by <input type=numeric> field (and be represented as Int in the model), with minimum valid value of 1 and maximum of 100. Users should be able to change this input, and changes should be immediately reflected in the model. When setting the field in the model, make sure to `clamp 1 100 input` to guard agains users trying to use values outside of this range.
3. number of samples - this should represent the number of times that given element was sampled. When sampling without replacement, this will be 0 or 1. When sampling with replacement, this will be >= 0.
4. this cell should contain a button that removes the row. Clicking it, should remove the element from the table, and renumber all the elements below it (to preserve contiguous numbering of all elements). This renumbering should allso be applied to the list of alrady sampled elements (see below). When there is 1 element left in the table, the button to remove it should become disabled, because we always want to have at least 1 element to sample from.

In the initial state, the table should have 2 rows, representing elements 1 and 2, each with weight 1, and "number of samples" 0

It should be possible to add elements to the table using a button below the table.
Clicking this button should add a new row with element n+1 (where n is the current number of elements in the table) and weight 1.
The maximum possible number of elements should be 10. When there's 10 elements added, the button for adding elements should be disabled.

Each element should be assigned a unique color from a nice pastell palette.

Under the above table there should be a visualization of how the weights of the elements contribute to the weighted probability distribution
of elements. The distribution should be represented using a horizontal bar that consists of n sub-bars, ordered based on the order of elements from 1 to n. The entire bar should always span a fixed width, say fixed to 80% of page width. The width of each sub-bar of each element should be in proportion to the sum of weights of all the elements. There should be no gap between the bars.
Example: if you have elements 1, 2 with weights 7, 3 respectively, then 70% of the bar should be occupied by a bar colored with 1's color and
remaining 30% of the bar's width should be occupied by a bar colored by 2's color.

Under this there should be 2 buttons:
1. Sample element. Upon clicking this button, depending on which sampling strategy is selected, a weighted sample is taken (using a Random.weighted function from elm/random package).
    - for sampling without replacement:
        - we should keep track of which elements have already been used and those should not be sampled from
        - when no elements are available for sampling, this button should become disabled
        - when an element is already in the sample, the corresponding row in the table should have greyed out background, indicating that it can't be sampled again.

2. Reset - clicking this should:
    - clear the list of elements sampled so far
    - set "number of times each element has been sampled so far" to 0 for each element in the table
    - while preserving current elements / weights configuration in the table

## list of elements sampled so far

Each element should be represented by a small rectangular box with the element's number in the center
and background color set to that element's color.
The list should be ordered based on the order of sampling - every time an element is sampled, it should be added at the end of the list.
