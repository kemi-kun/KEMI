def read_file(file_path) -> dict:
    data = {}
    file = open(file_path)
    lines = file.read().splitlines()
    for line in lines:
        element, symbol, atomic_mass, num_neutrons, num_protons, num_electrons, period = line.split(',')
        data[symbol] = [element, atomic_mass, num_neutrons, num_protons, num_electrons, period]
    return data


def print_num_protons(data: dict):
    for symbol, val in data.items():
        print(f'num_protons("{symbol}", {val[3]}).')


if __name__ == '__main__':
    data = read_file('../assets/element.csv')
    print_num_protons(data)
