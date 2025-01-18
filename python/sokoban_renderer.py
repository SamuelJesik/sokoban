import pygame
import time


def initialize_pygame_window(map_data, cell_size):
    rows = len(map_data)
    cols = len(map_data[0])
    screen = pygame.display.set_mode((cols * cell_size, rows * cell_size))
    pygame.display.set_caption("Sokoban Solver")
    return screen


def load_and_scale_images(cell_size):
    image_paths = {
        "wall": "assets/wall.png",
        "storage": "assets/storage.png",
        "player": "assets/player.png",
        "box": "assets/box.png",
        "floor": "assets/floor.png",
    }
    images = {key: pygame.image.load(path) for key, path in image_paths.items()}
    return {key: pygame.transform.scale(img, (cell_size, cell_size)) for key, img in images.items()}


def draw_grid(screen, map_data, walls, storages, images, cell_size):
    for y, row in enumerate(map_data):
        for x, _ in enumerate(row):
            if (x, y) in walls:
                screen.blit(images["wall"], (x * cell_size, y * cell_size))
            elif (x, y) in storages:
                screen.blit(images["storage"], (x * cell_size, y * cell_size))
            else:
                screen.blit(images["floor"], (x * cell_size, y * cell_size))


def draw_dynamic_objects(screen, player, boxes, images, cell_size):
    screen.blit(images["player"], (player[0] * cell_size, player[1] * cell_size))
    for box in boxes:
        screen.blit(images["box"], (box[0] * cell_size, box[1] * cell_size))


def show_popup_message(screen, message):
    popup_width = 800
    popup_height = 400
    screen = pygame.display.set_mode((popup_width, popup_height))
    font = pygame.font.Font(None, 100)
    text = font.render(message, True, (255, 255, 255))
    text_rect = text.get_rect(center=(popup_width // 2, popup_height // 2))
    screen.fill((0, 0, 0))
    screen.blit(text, text_rect)
    pygame.display.flip()
    wait_for_quit()


def wait_for_quit():
    waiting = True
    while waiting:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                waiting = False


def animate_solution(screen, solution, map_data, storages, walls, images, cell_size):
    running = True
    for step, state in enumerate(solution):
        if not handle_quit_event():
            running = False
            break
        screen.fill((0, 0, 0))
        draw_grid(screen, map_data, walls, storages, images, cell_size)
        draw_dynamic_objects(screen, state['player'], state['boxes'], images, cell_size)
        pygame.display.flip()
        time.sleep(0.5)
    return running


def handle_quit_event():
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            return False
    return True


def visualize_solution_pygame(map_data, solution, storages, walls):
    pygame.init()
    cell_size = 80
    screen = initialize_pygame_window(map_data, cell_size)
    images = load_and_scale_images(cell_size)
    running = animate_solution(screen, solution, map_data, storages, walls, images, cell_size)

    if running:
        show_popup_message(screen, "Mapa je vyriešená! :)")

    pygame.quit()
