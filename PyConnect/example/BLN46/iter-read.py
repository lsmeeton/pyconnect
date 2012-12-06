file_name = 'min.data'
i = 1
minima_index = {}
minima_index['Index'] = {}
for line in open(file_name, 'r'):
    minima_data = line.split()
            
    minima_index['Index'][i] =  dict(Energy = float(minima_data[0]))
            
    i += 1
print i
print len(minima_data)
#print minima_data[3]
