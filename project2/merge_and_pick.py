outfile1 = open("/Users/gudazhong/Documents/UCLA_Course/EE232E/Project2/merge.txt",'w',encoding='utf-8')
readfile1 = open("/Users/gudazhong/Documents/UCLA_Course/EE232E/Project2/project_2_data/actor_movies.txt",'r',encoding='utf-8')
readfile2 = open("/Users/gudazhong/Documents/UCLA_Course/EE232E/Project2/project_2_data/actress_movies.txt",'r',encoding='utf-8')

threshold = 5
count = 0
for line in readfile1.readlines():
	
	line = line[:-1]
	tmp=line.split("\t\t")
	
	if len(tmp)< threshold+1:
		continue
	outfile1.write(str(count))
	count +=1
	for i in tmp[1:]:
		m = i.find(")")
		i = i[:m+1]
		i.strip(" ")
		i.strip("\t")
		outfile1.write("\t\t")
		outfile1.write(str(i))
	outfile1.write("\n")

print("Finish actor")
for line in readfile2.readlines():
	
	line = line[:-1]
	tmp=line.split("\t\t")
	
	if len(tmp)<threshold+1:
		continue
	outfile1.write(str(count))
	count +=1
	for i in tmp[1:]:
		m = i.find(")")
		i = i[:m+1]
		i.strip(" ")
		i.strip("\t")
		outfile1.write("\t\t")
		outfile1.write(str(i))
	outfile1.write("\n")
outfile1.close()
print("Finish actress")

