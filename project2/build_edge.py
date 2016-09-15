edge_out_file = open ("/Users/gudazhong/Documents/UCLA_Course/EE232E/Project2/edge_list.txt",'w',encoding='utf-8')
movie_out_file = open ("/Users/gudazhong/Documents/UCLA_Course/EE232E/Project2/movie_list.txt",'w',encoding='utf-8')
readfile1 = open("/Users/gudazhong/Documents/UCLA_Course/EE232E/Project2/merge.txt",'r',encoding='utf-8')
count = 0;
threshold = 5
actor_nummovies = dict()
# (moviename, list() of actors)
movie_actorlist = dict()
movie_id = 0
edge_movienum = dict()
for line in readfile1.readlines():
	line = line[:-1]
	tmp=line.split("\t\t")
	actor_nummovies[tmp[0]] = len(tmp)-1
	for item in tmp[1:]:
		if item in movie_actorlist:
			if tmp[0] not in movie_actorlist[item]:
				movie_actorlist[item].append(tmp[0])
		else:
			movie_actorlist[item] = list()
			movie_actorlist[item].append(movie_id)
			movie_actorlist[item].append(tmp[0])
			movie_id += 1
print("Start to construct Edge list")
#Counstruct the hashtable for edge
for row in movie_actorlist.items():
	actor_list = row[1] #movieid, actorid1,2,3,
	index = len(actor_list)-1
	if index >= threshold:
		movie_out_file.write(row[0])
		for tmp in actor_list:
			movie_out_file.write("\t\t"+str(tmp))
		movie_out_file.write("\n")
	if index > 1:
		for i in range(1,index):
			for j in range(i+1,index+1):
				key1 = (int(actor_list[i]),int(actor_list[j]))				
				key2 = (int(actor_list[j]),int(actor_list[i]))
				if key1 in edge_movienum:
					edge_movienum[key1] += 1
					edge_movienum[key2] += 1
				else:
					edge_movienum[key1] = 1
					edge_movienum[key2] = 1
print("Start to calculate Edge weight")
for i in edge_movienum.items():
	tmp = float(i[1])/int(actor_nummovies[str(i[0][0])]);
	if tmp >= 1:
		continue;
	edge_out_file.write(str(i[0][0]) + "\t\t" + str(i[0][1]) + "\t\t" + str(tmp) + "\n")
edge_out_file.close()
movie_out_file.close()
print("Finish Edge list")









