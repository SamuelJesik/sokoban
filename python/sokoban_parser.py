import os

def parse_map(file_path):
    def parse_line(line, y):
        for x, char in enumerate(line):
            update_map_elements(char, x, y)

    def update_map_elements(char, x, y):
        nonlocal player
        if char == '#':
            walls.add((x, y))
        elif char in ('S', 's'):
            player = (x, y)
            if char == 's':
                storages.add((x, y))
        elif char in ('C', 'c'):
            boxes.append((x, y))
            if char == 'c':
                storages.add((x, y))
        elif char == 'X':
            storages.add((x, y))

    walls, storages, boxes = set(), set(), []
    player = None

    with open(file_path, "r", encoding="utf-8") as f:
        map_data = [line.rstrip("\n") for line in f]

    for y, line in enumerate(map_data):
        parse_line(line, y)

    return build_map_structure(walls, storages, player, boxes, map_data)

def build_map_structure(walls, storages, player, boxes, map_data):
    return {
        "walls": walls,
        "storages": storages,
        "player": player,
        "boxes": boxes,
        "map_data": map_data,
    }

def write_facts(f, key, values):
    for x, y in values:
        f.write(f"{key}({x},{y}).\n")

def write_solution_steps(f, moves):
    if moves:
        f.write("\n% Solution steps\n")
        for i, move in enumerate(moves, start=1):
            f.write(f"step({i}, {move}).\n")

def generate_facts_pl(parsed_map, facts_file, moves=None):
    with open(facts_file, "w", encoding="utf-8") as f:
        write_static_elements(f, parsed_map)
        write_solution_steps(f, moves)

def write_static_elements(f, parsed_map):
    write_facts(f, "wall", parsed_map["walls"])
    write_facts(f, "storage", parsed_map["storages"])
    write_player_position(f, parsed_map["player"])
    write_facts(f, "initial_box", parsed_map["boxes"])

def write_player_position(f, player):
    if player:
        px, py = player
        f.write(f"initial_player({px},{py}).\n")
