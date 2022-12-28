from typing import List, Tuple, Dict


HeightMap = List[List[str]]
Coord = Tuple[int, int]

# read_input parses and reads the source input file
def read_input(input_file_name: str) -> HeightMap:
    with open(input_file_name) as file:
        lines = file.read().splitlines()
        rows = list(map(lambda line: [*line], lines))
    
        return rows

# determine_endpoints computes the start and end locations in the height map
def get_cells_with_elevation_char(height_map: HeightMap, elevation_char: str) -> List[Coord]:
    num_rows = len(height_map)
    num_cols = len(height_map[0])

    occurrences = []

    for row in range(num_rows):
        for col in range(num_cols):
            marker = height_map[row][col]
            if marker == elevation_char:
                occurrences.append((row, col))
    
    return occurrences

# determine if a cell exists within the heightmap
def cell_in_height_map(height_map: HeightMap, cell: Tuple[int, int]) -> bool:
    num_rows = len(height_map)
    num_cols = len(height_map[0])

    (row, col) = cell
    return (row >= 0 and row < num_rows and
            col >= 0 and col < num_cols)

# get_height determines the mathematical height for an elevation character
def height(height_map: HeightMap, cell: Tuple[int, int]) -> int:
    height = 0
    (row, col) = cell
    match height_map[row][col]:
        case 'S': height = 1
        case 'E': height = 26
        case x if x >= 'a' and x <= 'z':
            height = 1 + (ord(x) - 97)
    
    return height

# determines if we can climb from (from_cell) to (to_cell) in the heightmap
def can_climb(height_map: HeightMap, from_cell: Coord, to_cell: Coord):
    target_height = height(height_map=height_map, cell=to_cell)
    current_height = height(height_map=height_map, cell=from_cell)

    return (target_height <= current_height 
            or target_height == 1 + current_height)


# get_min_path determines the minimum path from the start to end
def get_min_path(height_map: HeightMap, start: Coord, end: Coord) -> int:
    bfs_queue = [start]
    visited: Dict[Coord, bool] = {}
    predecessors: Dict[Coord, Coord] = {}

    while len(bfs_queue) > 0:
        curr_cell = bfs_queue.pop(0)
        if curr_cell in visited: continue
        if curr_cell == end: break

        (row, col) = curr_cell
        candidate_destinations = [(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)]
        valid_destinations = filter(lambda c: cell_in_height_map(height_map=height_map, cell=c) and can_climb(height_map=height_map, from_cell=curr_cell, to_cell=c), 
                            candidate_destinations)

        # prevent visiting this cell again
        # and register any outgoing predecessors
        visited[(row, col)] = True        
        for dest in valid_destinations:
            if dest not in visited:
                predecessors[dest] = curr_cell
                bfs_queue.append(dest)
    
    # traverse the predecessor array backwards from end to start to get the total path length
    curr_cell = end
    path_length = 0
    while curr_cell in predecessors:
        curr_cell = predecessors[curr_cell]
        path_length += 1

    return path_length


if __name__ == '__main__':
    height_map = read_input(input_file_name="day12.txt")
    (start, end) = (get_cells_with_elevation_char(height_map=height_map, elevation_char='S')[0],
                    get_cells_with_elevation_char(height_map=height_map, elevation_char='E')[0])

    # part one
    print("part one: ", get_min_path(
        height_map=height_map,
        start=start,
        end=end
    ))

    # part two
    potential_starts = get_cells_with_elevation_char(height_map=height_map, elevation_char='a')
    potential_starts.append(start)

    path_distances = filter(
                            lambda x: x != 0, 
                            map(
                                lambda c: get_min_path(height_map, start=c, end=end), 
                                potential_starts
                            )
                        )

    print("part two: ", min(list(path_distances)))