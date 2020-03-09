from itertools import islice

def load_data() -> list:    
    data = []
    # TODO: Use module argparse to add + parse cmd options
    with open('../assets/parent-hydride.csv') as f:
        lines = f.read().splitlines()
        for line in islice(lines, 2, None):
            formula, name = line.split(',')
            data.append((formula, name))
    return data

def print_parent_hydride_names(data: list):
    for formula, name in data:
        print(f'parent_hydride_name("{formula}", "{name}").')

if __name__ == "__main__":
    data = load_data()
    print_parent_hydride_names(data)
