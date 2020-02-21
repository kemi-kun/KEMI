file_element = open('element.txt', 'r')
contents = []
elements = []
name = []
if file_element.mode == 'r':
    contents = file_element.readlines()

for i in contents:
    all_contents = i.split()
    name.append(all_contents[0])
    elements.append(all_contents[1])

for j in range(len(elements)):
    print(f'element_name("{elements[j]}", {name[j].lower()}).')