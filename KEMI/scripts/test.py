import re


def get_orbital(n, l) -> str:
    label = {
        0: 's',
        1: 'p',
        2: 'd',
        3: 'f',
        4: 'g',
    }
    return f'{n}{label[l]}'


def get_nth_afbau(n) -> str:
    """
    >>> get_nth_afbau(1) # 1 0
    '1s'
    >>> get_nth_afbau(2) # 2 0
    '2s'
    >>> get_nth_afbau(3) # 2 1
    '2p'
    >>> get_nth_afbau(4) # 3 0
    '3s'
    >>> get_nth_afbau(5) # 3 1
    '3p'
    >>> get_nth_afbau(6) # 4 0
    '4s'
    >>> get_nth_afbau(7) # 3 2
    '3d'
    >>> get_nth_afbau(8) # 4 1
    '4p'
    >>> get_nth_afbau(9) # 5 0
    '5s'
    >>> get_nth_afbau(10) # 4 1
    '4d'
    """
    # 1 1 2 2 3 3 4 4 5 5


def contains(formula: str) -> list:
    pass