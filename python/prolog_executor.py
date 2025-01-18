import os
import subprocess
import re

def format_consults(modules):
    return ", ".join(["consult('{}')".format(os.path.abspath(module).replace("\\", "/")) for module in modules])

def run_prolog_solver(modules):
    cmd = [
        "C:/Program Files/swipl/bin/swipl.exe",
        "-g",
        f"{format_consults(modules)}, solve_bfs, halt."
    ]
    return subprocess.run(cmd, capture_output=True, text=True)

def extract_player(line):
    match = re.search(r"player\((\d+),(\d+)\)", line)
    return (int(match.group(1)), int(match.group(2))) if match else None

def extract_boxes(line):
    matches = re.findall(r"box\((\d+),(\d+)\)", line)
    return [(int(x), int(y)) for x, y in matches]

def extract_player_and_boxes(line):
    player = extract_player(line)
    boxes = extract_boxes(line)
    if player:
        return {"player": player, "boxes": boxes}
    return None

def parse_prolog_output(output):
    states, moves = [], []
    for line in output.strip().split("\n"):
        if line.startswith("state"):
            state = extract_player_and_boxes(line)
            if state:
                states.append(state)
        elif line.startswith("move"):
            moves.append(line.split("(")[1].split(")")[0])
    return states, moves


def calculate_actions(solution):
    directions = {
        (-1, 0): "Vľavo",
        (1, 0): "Vpravo",
        (0, -1): "Hore",
        (0, 1): "Dole"
    }

    actions = []
    for i in range(1, len(solution)):
        prev_state = solution[i - 1]
        current_state = solution[i]

        px_prev, py_prev = prev_state["player"]
        px_curr, py_curr = current_state["player"]

        dx, dy = px_curr - px_prev, py_curr - py_prev
        action = directions.get((dx, dy), "unknown")

        if current_state["boxes"] != prev_state["boxes"]:
            action += " + potlačil krabičku"

        actions.append(action)

    return actions
