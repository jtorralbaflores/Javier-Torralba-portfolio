Clustering algorithm:

It may happen that the same book has different ISBN numbers (imagine, the 5th vs 6th edition of a book, hard cover vs soft cover, etc). This may be an issue when analyzing each of the books, as well as giving them subject genres (you would have too much unnecessary information). This is a hard problem to solve, but I made a file that has an algorithm that recudes the number of ISBNs for repeated books. This is what the file "exploring_clustering_algortihms" does. 

This file partially solves this issue in the following steps:
1. select each unique combination of “book_url”, “book_title”, “author_name”, and “isbn13”.
2. Count how many instances of each unique observation of these four variables together there are, make a new column n with the number of combinations there are for these. 
3. For the book title, remove all capitalizations, spaces, and special symbols such as double points, apostrophes, etc. 
4. Group the book title and author name (to have a single entry for each unique title and author name) and make a column which has the mode ISBN for each author name and book title combination. 
5. Now you have a data frame with the most repeated ISBN for each book title and author name combination. 
6. Merge back this dataset into the original dataset with “book_url”, “book_title”, “author_name”, and “isbn13”. Now you have a data frame with the most repeated ISBN13 for each book that may have a different book title due to capitalizations, spaces, and special symbols. 

What are the results of doing this in the original dataset?	
Number of unique book titles
Before it was cleaned: 1,256,810 observations
After being cleaned: 1,209,057 observations
Number of unique ISBNs
Before it was cleaned: 1,292,412 observations
After it was cleaned: 1,252,144 observations
Number of NAs in ISBNs
Before it was cleaned: 490,641 observations
After it was cleaned: 458,562 observations

