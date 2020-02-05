from itertools import islice


def print_code(parts: list):
    # person,parent,parent,spouse
    if parts[1]:
        print(f'parent({parts[1]}, {parts[0]}).')
    if parts[2]:
        print(f'parent({parts[2]}, {parts[0]}).')
    if parts[3]:
        print(f'spouse({parts[0]}, {parts[3]}).')


if __name__ == '__main__':
    file = open('relationship.csv')
    text = file.read()
    lines = text.splitlines()
    for line in islice(lines, 1, None):
        parts = line.split(',')
        print_code(parts)
