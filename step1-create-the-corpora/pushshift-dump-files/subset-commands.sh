#To subset the data, run the below commands using the Linux terminal:

awk 'BEGIN{IGNORECASE = 1};/long-acting reversib|long acting reversib|LARC|IUD|Mirena|Liletta|Kyleena|Skyla|Paragard|implant|Nexplanon|intrauterine/' birthcontrol_submissions.txt > birthcontrolsubset_submissions.txt
awk 'BEGIN{IGNORECASE = 1};/long-acting reversib|long acting reversib|LARC|IUD|Mirena|Liletta|Kyleena|Skyla|Paragard|implant|Nexplanon|intrauterine/' birthcontrol_comments.txt > birthcontrolsubset_comments.txt

awk 'BEGIN{IGNORECASE = 1};/long-acting reversib|long acting reversib|LARC|IUD|Mirena|Liletta|Kyleena|Skyla|Paragard|implant|Nexplanon|intrauterine/' TwoXChromosomes_submissions.txt > TwoXChromosomessubset_submissions.txt
awk 'BEGIN{IGNORECASE = 1};/long-acting reversib|long acting reversib|LARC|IUD|Mirena|Liletta|Kyleena|Skyla|Paragard|implant|Nexplanon|intrauterine/' TwoXChromosomes_comments.txt > TwoXChromosomessubset_comments.txt
