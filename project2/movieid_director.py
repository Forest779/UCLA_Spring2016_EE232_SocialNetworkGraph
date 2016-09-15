read_movielist = open ("/Users/gudazhong/Documents/UCLA_Course/EE232E/Project2/movie_list.txt",'r',encoding='utf-8')
read_directorlist = open ("/Users/gudazhong/Documents/UCLA_Course/EE232E/Project2/project_2_data/director_movies.txt",'r',encoding='utf-8')
out_file = open ("/Users/gudazhong/Documents/UCLA_Course/EE232E/Project2/id_director.txt",'w',encoding='utf-8')

movie_list = dict() #movie name-> movie_id
# director_list = dict()  # director->
movieid_director = dict()

print("read movie list")
for line in read_movielist.readlines():
	line = line[:-1]
	tmp=line.split("\t\t")
	movie_list[tmp[0]] = tmp[1]
read_movielist.close()

print("read director list")
for line in read_directorlist.readlines():
	line = line[:-1]
	tmp=line.split("\t\t")
	for item in tmp[1:]:
		m = item.find(")")
		item = item[:m+1]
		item.strip(" ")
		item.strip("\t")

		if item in movie_list:
			movieid_director[movie_list[item]] = tmp[0]
read_directorlist.close()

print("write to file")
for i in movieid_director.items():
	out_file.write(str(i[0]) + "\t\t" + str(i[1]) + "\n")
out_file.close()