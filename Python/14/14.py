from typing import Callable, Tuple, MutableSet, Optional
from math import inf

Coord = Tuple[int, int]
SurfaceBoundary = Tuple[Tuple[int, int], Tuple[int, int]]
CaveSurface = MutableSet[Coord]

# compute_num_sand_steps determines how many grains of sand we can drip from the source
# till we start dripping into the abyss
def max_stable_particles(
    cave_surface: CaveSurface, 
    cave_boundary: SurfaceBoundary,
    source_stability_condition: Callable[[CaveSurface, SurfaceBoundary, Coord], bool]
) -> int:
    sand_source = (500, 0)
    curr_sand_position = sand_source
    stable_particles = 0 

    while source_stability_condition(cave_surface, cave_boundary, particle_pos=curr_sand_position):
        next_pos = get_next_location(cave_surface, cave_boundary, particle_pos=curr_sand_position)
        # if theres no where to go add it to the cave surface
        if next_pos == None:
            cave_surface.add(curr_sand_position)
            curr_sand_position = sand_source
            stable_particles += 1
        else:
            curr_sand_position = next_pos

    return stable_particles

# get_next_location computes the physical next location of the particle
# at particle_pos within the cave
def get_next_location(cave_surface: CaveSurface, cave_boundary: SurfaceBoundary, particle_pos: Coord) -> Optional[Coord]:
    (pos_x, pos_y) = particle_pos
    _, (min_y, max_y) = cave_boundary
    floor_level = max_y + 2

    # these are the possible directions the particle could fall
    down_step = (pos_x, pos_y + 1)
    diagonally_left_step = (pos_x - 1, pos_y + 1)
    diagonally_right_step = (pos_x + 1, pos_y + 1)

    if down_step not in cave_surface and pos_y + 1 < floor_level: return down_step
    elif diagonally_left_step not in cave_surface and pos_y + 1 < floor_level: return diagonally_left_step
    elif diagonally_right_step not in cave_surface and pos_y + 1 < floor_level: return diagonally_right_step
    else: return None

    
# stability condition for determining if a particle is within the cave boundary
def particle_within_boundary(cave_surface: CaveSurface, cave_boundary: SurfaceBoundary, particle_pos: Coord) -> bool:
    ((min_x, max_x), (min_y, max_y)) = cave_boundary
    (x, y) = particle_pos
    return min_x <= x and x <= max_x and min_y <= y and y <= max_y

# stability condition for determining if a particle is blocking the source
def not_blocking_source(cave_surface: CaveSurface, cave_boundary: SurfaceBoundary, particle_pos: Coord) -> bool:
    return (500, 0) not in cave_surface

# read_init_cave_state reads the initial cave surface state from the provided file
# also reads the corners of the cave surface
def read_init_cave_state(file_name: str) -> Tuple[CaveSurface, SurfaceBoundary]:
    cave_surface = set()
    # for cave surface boundary computations
    # we seed these with the location of the sand source
    (min_x, max_x) = (inf, 500)
    (min_y, max_y) = (0, -inf)

    with open(file_name) as f:
        rock_formations = f.read().splitlines()

        for formation in rock_formations:
            raw_formation_segments = formation.split('->')
            parsed_segments = list(map(parse_coordinate, raw_formation_segments))

            for segment_num in range(len(parsed_segments) - 1):
                (from_x, from_y), (to_x, to_y) = parsed_segments[segment_num], parsed_segments[segment_num + 1]
                
                # compute the delta amounts before drawing a line from -> to
                delta_x = to_x - from_x
                delta_y = to_y - from_y

                # draw a line from from_coord to to_coord
                for delta in range(max(abs(delta_x), abs(delta_y)) + 1):
                    # compute this value and potentially update the cave surface boundaries
                    (x, y) = ((from_x + sign(delta_x) * delta), (from_y + sign(delta_y) * delta))
                    (min_x, max_x) = (min(x, min_x), max(x, max_x))
                    (min_y, max_y) = (min(y, min_y), max(y, max_y))

                    cave_surface.add((x, y))

    return cave_surface, ((min_x, max_x), (min_y, max_y))

# regular mathematical sign function
def sign(n: int) -> int:
    match n:
        case 0: return 0
        case x if x < 0: return -1
        case _: return 1

# parses a raw coordinate pair
def parse_coordinate(coord_str: str) -> Coord:
    [raw_x, raw_y] = coord_str.split(',')
    return (int(raw_x), int(raw_y))

if __name__ == '__main__':
    surface_p1, boundaries = read_init_cave_state(file_name="day14.txt")
    surface_p2 = set(surface_p1)

    # part 1
    num_sand_particles_1 = max_stable_particles(source_stability_condition=particle_within_boundary, cave_surface=surface_p1, cave_boundary=boundaries)
    print("part 1:", num_sand_particles_1)

    # part 2
    num_sand_particles_2 = max_stable_particles(source_stability_condition=not_blocking_source, cave_surface=surface_p2, cave_boundary=boundaries)
    print("part 2:", num_sand_particles_2)
