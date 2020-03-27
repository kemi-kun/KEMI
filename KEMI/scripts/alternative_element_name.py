if __name__ == '__main__':
    data = [
        ("antimony", "stibium"),
        ("copper", "cuprum"),
        ("gold", "aurum"),
        ("iron", "ferrum"),
        ("lead", "plumbum"),
        ("mercury", "hydragyrum"),
        ("potassium", "kalium"),
        ("silver", "argentum"),
        ("sodium", "natrium"),
        ("tin", "stannum"),
        ("tungsten", "wolfram"),
        ("aluminium", "aluminum"),
    ]
    for name, alt in data:
        print(f'alternative_element_name_fact({name}, "{alt}").')
