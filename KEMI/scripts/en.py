# TODO: fix file not found error
def read_file(file_path) -> dict:
    data = {}
    file = open(file_path)
    lines = file.read().splitlines()
    for line in lines:
        symbol, en = line.split(',')
        data[symbol] = en
    return data


def print_en_fact(data: dict):
    for symbol in data:
        en = data[symbol]
        print(f'en("{symbol}", {en}).')


if __name__ == '__main__':
    data = read_file('../assets/element-en.csv')
    print_en_fact(data)
