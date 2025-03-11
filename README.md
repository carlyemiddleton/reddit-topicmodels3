Welcome!  Below please find the exact steps for reproducing our Latent Dirichlet Allocation topic models for the subreddits r/birthcontrol and r/TwoXChromosomes.
1.	Download the .torrent comment and submission files of r/birthcontrol and r/TwoXChromosomes using the steps outlined at:  https://www.reddit.com/r/pushshift/comments/1akrhg3/separate_dump_files_for_the_top_40k_subreddits/
2.	Save the downloaded files as .txt files
3.	In the Linux terminal, subset the data using the commands in subset-commands.sh
4.	If it doesn’t already exist, make the ‘posts’ folder and empty it
5.	Run create-the-documents_birthcontrol.py
6.	Empty the ‘posts’ folder
7.	Run create-the-documents_TwoXChromosomes.py
8.	Delete the following punctuation from vcorpus_infile_rbirthcontrol.txt and vcorpus_infile_rTwoXChromosomes.txt using find and replace (we used the find and replace function in Notepad++)
a.	’
b.	'
c.	´
d.	"
e.	”
f.	‘
g.	“
h.	[removed]
9.	Again using find and replace, remove, in order, all lines of text listed in welcome-messages.txt from vcorpus_infile_rbirthcontrol.txt
10.	Run run-LDA_birthcontrol.R and run-LDA_TwoXChromosomes.R
11. Create the plots using the scripts in step3-plots
12. Print the qualitative data subset using the script in step4-print-posts
