#make the documents
import json
infile = open('pushshift-dump-files/TwoXChromosomessubset_submissions.txt','r', encoding='utf-8') 
timestamps = []
id = []
permalinks = []
for line in infile:
    data=json.loads(line)
    utftime = int(data['created_utc'])
    if utftime < 1687564800 and utftime >= 1624492800: #Between June 24, 2021 midnight and June 24, 2023 midnight
        f = open(("posts/submission" + str(data['id']) + ".txt"), "a", encoding='utf-8')
        print(data['title'], file=f)
        print(data['selftext'], file=f)
        timestamps.append(str(data['created_utc']))
        id.append(str(data['id']))
        permalinks.append(str(data['permalink']))

#append the comments to the documents
infile = open('pushshift-dump-files/TwoXChromosomes_comments.txt','r', encoding='utf-8')
for line in infile:
    data=json.loads(line)
    if str(data['link_id'])[3:] in id:
        toppostid = str(data['link_id'])[3:]
        f = open(("posts/submission" + toppostid + ".txt"), "a", encoding='utf-8')
        print(data['body'], file=f)

#make the Vcorpus infile
#text = []
f = open("vcorpus_infile_rTwoXChromosomes.txt", "a", encoding='utf-8')  
for elem in id:
    with open(("posts/submission" + elem + ".txt"), "r", encoding='utf-8') as file:
        str = file.read().replace('\n', ' ')
    print(str, file=f)
    #text.append(str)
f.close()

#make metadata.csv
import csv
import pandas as pd
file = open("metadata_TwoXChromosomes.csv", "w", encoding='utf-8') 
writer = csv.writer(file)
for w in range(len(timestamps)):
    writer.writerow([id[w], timestamps[w], permalinks[w]])
file.close()
import pandas as pd
metadata = pd.read_csv('metadata_TwoXChromosomes.csv') #remove the empty rows
metadata.to_csv('metadata_TwoXChromosomes.csv', index=False)





