def load_data() -> dict:    
    data = {}
    # TODO: Use module argparse to add + parse cmd options
    with open('../assets/multiplicative-prefixes.csv') as f:
        lines = f.read().splitlines()
        for line in lines:
            number, prefix, ligand_prefix = line.split(',')
            data[number] = [prefix, ligand_prefix]
    return data

def print_multiplicative_prefixes(data: dict):
    for number, val in data.items():
        print(f'multiplicative_prefix({number}, "{val[0]}").')

def print_multiplicative_ligand_prefixes(data: dict):
    for number, val in data.items():
        if val[1]:
            print(f'multiplicative_ligand_prefix({number}, "{val[1]}").')

if __name__ == "__main__":
    data = load_data()
    print_multiplicative_prefixes(data)
    print_multiplicative_ligand_prefixes(data)
