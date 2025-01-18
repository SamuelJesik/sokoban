from sokoban_parser import parse_map, generate_facts_pl
from sokoban_renderer import visualize_solution_pygame
from prolog_executor import run_prolog_solver, parse_prolog_output, calculate_actions
import os
import pygame


def load_map_and_generate_facts(map_path, facts_path):
    parsed_map = parse_map(map_path)
    generate_facts_pl(parsed_map, facts_path)
    return parsed_map


def process_prolog_result(result, parsed_map, facts_path):
    if result.returncode == 0 and result.stdout:
        handle_successful_result(result, parsed_map, facts_path)
    else:
        handle_failed_result(result)


def handle_successful_result(result, parsed_map, facts_path):
    print_prolog_output(result.stdout)
    states, moves = parse_prolog_output(result.stdout)
    update_facts_with_solution(parsed_map, facts_path, moves)
    actions = calculate_actions(states)
    print_actions(actions)
    visualize_solution(parsed_map, states)


def handle_failed_result(result):
    print("Prolog execution failed:")
    print(result.stderr)


def print_prolog_output(output):
    print("Prolog output:")
    print(output)


def update_facts_with_solution(parsed_map, facts_path, moves):
    generate_facts_pl(parsed_map, facts_path, moves=moves)


def visualize_solution(parsed_map, states):
    visualize_solution_pygame(
        parsed_map["map_data"], states, parsed_map["storages"], parsed_map["walls"]
    )


def print_actions(actions):
    print("Sekvencia akcií:")
    for action in actions:
        print(action)


def get_prolog_modules():
    return [
        "solver/state.pl",
        "solver/actions.pl",
        "solver/utils.pl",
        "solver/solver.pl",
    ]


def select_map():
    pygame.init()
    screen = pygame.display.set_mode((800, 600))
    pygame.display.set_caption("Vyberte mapu")
    font = pygame.font.Font(None, 50)
    background_color = (30, 30, 30)
    highlight_color = (255, 215, 0)
    text_color = (255, 255, 255)
    dim_text_color = (150, 150, 150)
    box_color = (50, 50, 50)
    border_color = (255, 255, 255)

    map_files = [f for f in os.listdir("maps") if f.endswith(".txt")]
    display_map_files = [
        f"{f} (NEFUNGUJE)" if f in ["map2.txt", "map3.txt"] else f
        for f in map_files
    ]
    selected_index = 0
    running = True

    while running:
        screen.fill(background_color)

        start_y = 50  

        for i, map_file in enumerate(display_map_files):
            if i == selected_index:
                color = highlight_color
                text = font.render(f"> {map_file}", True, color)
            else:
                color = text_color if "NEFUNGUJE" not in map_file else dim_text_color
                text = font.render(map_file, True, color)
            
            text_rect = text.get_rect(center=(400, start_y + i * 60))
            screen.blit(text, text_rect)

        instructions_text = "Použite šípky na výber, Enter na potvrdenie"
        instructions = font.render(instructions_text, True, text_color)
        instructions_rect = instructions.get_rect(center=(400, 550))
        
        box_rect = instructions_rect.inflate(20, 20)
        pygame.draw.rect(screen, box_color, box_rect)
        pygame.draw.rect(screen, border_color, box_rect, 3)
        screen.blit(instructions, instructions_rect)

        pygame.display.flip()

        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                pygame.quit()
                exit()
            elif event.type == pygame.KEYDOWN:
                if event.key == pygame.K_UP:
                    selected_index = (selected_index - 1) % len(display_map_files)
                elif event.key == pygame.K_DOWN:
                    selected_index = (selected_index + 1) % len(display_map_files)
                elif event.key == pygame.K_RETURN:
                    running = False

    pygame.quit()
    return os.path.join("maps", map_files[selected_index])






def main():
    print_start_message()

    map_path = select_map()
    facts_path = "solver/facts.pl"
    modules = get_prolog_modules()

    parsed_map = load_map_and_generate_facts(map_path, facts_path)

    result = run_prolog_solver(modules)

    process_prolog_result(result, parsed_map, facts_path)


def print_start_message():
    print("Python main.py spustený!")


if __name__ == "__main__":
    main()
