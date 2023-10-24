This section includes the API needed to download the information of a book (namely, the genre of the book).
Three files do three different steps to get the information for each ISBN and the subjects of a book. Ahead is the explanation of the steps and each file:

Setp 1: "get_all_isbn.R" 
This script filters all the unique ISBNs for a dataset you give. Selects the unique ISBNs and then downloads 
a CSV file with all the unique ISBNs into your computer (wherever the file is located).

Step 2: "openlibrary_api.R"
For a given sample of ISBNs, use the Open Library API to download the genres in json files that have the ISBN of the book. 
This file uses a fuction to loop over all the ISBNs in the dataset you have. Afterwards, the API downloads a json file with the name
of the 13 number ISBN, in the folder "out/data/". 

Note 1: you can change the speed with which the code loops, so you won't get blocked from using the Open Library API
Note 2: the Open Library API gives the best information for books in English. Other languages are supported, but some languages (for example, portuguese)
return json files with very little info. 
Note 3: the Open Library API sometimes won't have the ISBN you feed it. When this happens, the function simply ignores the ISBN and moves onto the next one. 

Step 3: "saclin_up_genres.R"
The json files for each book contain several information regarding the books. The tab "subjects" includes the subjects for each book. Some books contain several words (around 20 words), some others have little to no subjects. This file selects the subject words for each ISBN number and creates a dataset where there are two columns. The first column is the ISBN for a book and the second column is a subject word for that book. Afterwards, it creates a CSV file with this dataset. 


