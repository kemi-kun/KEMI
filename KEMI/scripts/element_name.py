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
        print(f'element_name("{symbol}", {val[0].lower()}).')


if __name__ == '__main__':
    data = read_file('../assets/element.csv')
    print_element_name(data)
