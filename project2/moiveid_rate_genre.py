read_movielist = open ("/Users/gudazhong/Documents/UCLA_Course/EE232E/Project2/movie_list.txt",'r',encoding='utf-8')
genre_list = open ("/Users/gudazhong/Documents/UCLA_Course/EE232E/Project2/movie_genre_new.txt",'r',encoding='utf-8')
rate_list = open ("/Users/gudazhong/Documents/UCLA_Course/EE232E/Project2/movie_rating_new.txt",'r',encoding='utf-8')
out_file_genre = open ("/Users/gudazhong/Documents/UCLA_Course/EE232E/Project2/id_genre.txt",'w',encoding='utf-8')
out_file_rating = open ("/Users/gudazhong/Documents/UCLA_Course/EE232E/Project2/id_rating.txt",'w',encoding='utf-8')

movie_name_dict = dict()
error = 0

for line in read_movielist.readlines():
	line = line[:-1]
	tmp=line.split("\t\t")
	movie_name_dict[tmp[0]] = tmp[1]
read_movielist.close()

print("genre")
for line in genre_list.readlines():
	line = line[:-1]
	tmp=line.split("\t\t")
	item = tmp[0]
	if str(tmp[0]) in movie_name_dict:
		if len(tmp) == 2:
			out_file_genre.write(str(movie_name_dict[str(tmp[0])]) + "\t\t" + str(tmp[1]) + "\n" )
		else:
			error +=1
print(str(error))

print("rating")
error = 0
for line in rate_list.readlines():
	line = line[:-1]
	tmp=line.split("\t\t")
	item = tmp[0]
	if str(tmp[0]) in movie_name_dict:
		if len(tmp) == 2:
			out_file_rating.write(str(movie_name_dict[str(tmp[0])]) + "\t\t" + str(tmp[1]) + "\n" )
		else:
			error +=1
print(str(error))
print("finish")