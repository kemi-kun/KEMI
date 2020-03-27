def read_file(file_path) -> dict:
    data = {}
    file = open(file_path)
    lines = file.read().splitlines()
    for line in lines:
        element, symbol, atomic_mass, num_neutrons, num_protons, num_electrons, period = line.split(',')
        data[symbol] = [element, atomic_mass, num_neutrons, num_protons, num_electrons, period]
    return data


def print_element_name(data: dict):
    for symbol, val in data.items():
        print(f'element_name_fact({val[0].lower()}, "{val[0].lower()}").')


def print_element_symbol(data: dict):
    for symbol, val in data.items():
        print(f'element_symbol_fact({val[0].lower()}, "{symbol}").')


if __name__ == '__main__':
    import sys
    data = read_file('../assets/element.csv')
    print_element_name(data)
    print_element_symbol(data)
