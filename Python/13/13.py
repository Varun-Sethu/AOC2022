from typing import List, Union, Tuple
from functools import cmp_to_key
from enum import IntEnum

Packet = List[Union[int, List['Packet']]]
class Equality(IntEnum):
    Eq = 0
    Ordered = -1
    Unordered = 1

# compare_packet_elms determines the ordering of the packet elements
def elms_are_correctly_ordered(left: Union[int, List[int]], right: Union[int, List[int]]) -> Equality:
    match (left, right):
        case (x, y) if isinstance(x, list) and isinstance(y, list): return packets_are_ordered( x, y)
        case (x, y) if isinstance(x, list) and isinstance(y, int): return packets_are_ordered(x, [y])
        case (x, y) if isinstance(x, int) and isinstance(y, list): return packets_are_ordered([x], y)
        case (x, y) if isinstance(x, int) and isinstance(y, int):
            if x == y: return Equality.Eq
            elif x < y: return Equality.Ordered
            else: return Equality.Unordered

# packets_are_ordered determines if a pair of packets are ordered correctly
# if the packets are equal then it returns "on_equality"
def packets_are_ordered(left: Packet, right: Packet) -> Equality:
    for i in range(min(len(left), len(right))):
        sub_comparison = elms_are_correctly_ordered(left=left[i], right=right[i])
        if sub_comparison != Equality.Eq:
            return sub_comparison

    # if we've run out of elements the ordering depends on the length of each of these lists
    if len(left) == len(right): return Equality.Eq
    elif len(left) < len(right): return Equality.Ordered
    else: return Equality.Unordered

# read_pairs read all packet pairs straight from the provided file
def read_pairs_from(filename: str) -> List[Tuple[Packet, Packet]]:
    with open(file=filename) as file:
        lines = file.read().splitlines()
        unparsed_pairs = [(lines[i], lines[i + 1]) for i in range(0, len(lines), 3)]

        parsed_pairs = map(lambda pair: (eval(pair[0]), eval(pair[1])), unparsed_pairs)
        return parsed_pairs


if __name__ == '__main__':
    # part one
    pairs = list(read_pairs_from(filename="day13.txt"))
    part_one = 0

    for pair_num in range(len(pairs)):
        (left, right) = pairs[pair_num]
        if packets_are_ordered(left, right) == Equality.Ordered:
            part_one += pair_num + 1

    print("part one:", part_one)

    # part two
    expanded_pairs = []
    for (packet_a, packet_b) in pairs:
        expanded_pairs.extend([packet_a, packet_b])

    expanded_pairs.extend([ [[2]], [[6]] ])
    sorted_pairs = sorted(expanded_pairs, key=cmp_to_key(lambda pa, pb: int(packets_are_ordered(pa, pb))))

    decoder_key = (sorted_pairs.index([[2]]) + 1) * (sorted_pairs.index([[6]]) + 1)
    print("part two:", decoder_key) 