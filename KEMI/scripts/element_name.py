from operator import itemgetter

def read_file(file_path) -> dict:
    data = {}
    file = open(file_path)
    lines = file.read().splitlines()
    for line in lines:
        element, symbol, atomic_mass, num_neutrons, num_protons, num_electrons, period = line.split(',')
        data[symbol] = [element, atomic_mass, num_neutrons, num_protons, num_electrons, period]
    return data


def print_element_names(data: dict):
    for symbol, val in data.items():
        print(f'element_name_fact({val[0].lower()}, "{val[0].lower()}").')


def print_element_symbols(data: dict):
    for symbol, val in data.items():
        print(f'element_symbol_fact({val[0].lower()}, "{symbol}").')


def _ps(obj):
    return f'"{obj}"'


def add_comma(s):
    return f'{s},'


def print_element_facts(data: dict):
    max_lens = [max(map(len, map(str, map(itemgetter(i), data.values())))) for i in range(6)]
    for symbol, val in data.items():
        element, atomic_mass, num_neutrons, num_protons, num_electrons, period = val
        print((f'element_fact({{:<{max_lens[0]+1}}} {{:<{max_lens[0]+3}}} {{:<{2+3}}} {{:<{max_lens[3]+1}}} {atomic_mass}).')
              .format(*map(add_comma, [element.lower(), _ps(element.lower()), _ps(symbol), num_protons])))


if __name__ == '__main__':
    import sys
    data = read_file('../assets/element.csv')
    print_element_facts(data)
