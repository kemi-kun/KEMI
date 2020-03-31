# TODO: fix file not found error
def read_file(file_path) -> dict:
    data = {}
    file = open(file_path)
    lines = file.read().splitlines()
    for line in lines:
        element, en = line.split(',')
        data[element] = en
    return data


def print_en_fact(data: dict):
    for element in data:
        en = data[element]
        print(f'en_fact({element.lower()}, {en}).')


if __name__ == '__main__':
    data = read_file('../assets/element-en.csv')
    print_en_fact(data)
