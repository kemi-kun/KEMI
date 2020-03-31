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
    sdata = {}
    for symbol, val in data.items():
        element, atomic_mass, num_neutrons, num_protons, num_electrons, period = val
        sdata[atomic_mass] = list(map(add_comma, [element.lower(), _ps(element.lower()), _ps(symbol), num_protons]))
    max_lens = [max(map(len, map(str, map(itemgetter(i), sdata.values())))) for i in range(4)]
    for last, row in sdata.items():
        s = ' '.join((f'{{:<{max_lens[i]}}}').format(e) for i, e in enumerate(row))
        print(f'element_fact({s} {last}).')


if __name__ == '__main__':
    data = read_file('../assets/element.csv')
    print_element_facts(data)
