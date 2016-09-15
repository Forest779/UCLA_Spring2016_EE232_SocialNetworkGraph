movie_edge_out_file = open("/Users/gudazhong/Documents/UCLA_Course/EE232E/Project2/movie_edge_list.txt",'w',encoding='utf-8')
readfile1 = open("/Users/gudazhong/Documents/UCLA_Course/EE232E/Project2/movie_list.txt",'r',encoding='utf-8')
readfile2 = open("/Users/gudazhong/Documents/UCLA_Course/EE232E/Project2/merge.txt",'r',encoding='utf-8')
movie_actorlist = dict() #movie_id, actorlist
movie_name_map = dict()
movie_edge_list = dict()
actor_movielist = dict() #actorid, movie_id_list

print("Construct movie_actorlist")
for line in readfile1.readlines(): #movie_list.txt
	line = line[:-1]
	tmp=line.split("\t\t")
	movie_actorlist[tmp[1]] = tmp[2:]
	movie_name_map[tmp[0]] = tmp[1]
	
readfile1.close()

print("Construct actor_movielist")
for line in readfile2.readlines(): #merge_file.txt
	line = line[:-1]
	tmp=line.split("\t\t")
	tmp_movie_list = list()
	for item in tmp[1:]:
		if item in movie_name_map:
			if movie_name_map[item] in movie_actorlist:
				tmp_movie_list.append(movie_name_map[item])
	actor_movielist[tmp[0]] = tmp_movie_list

readfile2.close()



print("Construct the movie edge")
for row in actor_movielist.items():
	movie_list = row[1]
	index = len(movie_list)
	if index > 1:
		for i in range(0,index-1):
			for j in range(i+1,index):
				key = (int(movie_list[i]),int(movie_list[j]))
				key_rev = (int(movie_list[j]),int(movie_list[i]))
				if key in movie_edge_list:
					movie_edge_list[key] += 1
				elif key_rev in movie_edge_list:
					movie_edge_list[key_rev] += 1
				else:
					movie_edge_list[key] = 1

error = 0
print("Start to calculate Edge weight")
for i in movie_edge_list.items():
	union = len(movie_actorlist[str(i[0][0])]) + len(movie_actorlist[str(i[0][1])])  - int(i[1])
	if union <= 0:
		error+=1
		continue
	weight = float(i[1])/union
	movie_edge_out_file.write(str(i[0][0]) + "\t\t" + str(i[0][1]) + "\t\t" + str(weight) + "\n")
movie_edge_out_file.close()
print("Finish Edge list")
print("error edge weight "+str(error))
