# Sokoban Solver

Sokoban Solver je projekt na riešenie Sokoban hádaniek s použitím Prologu a vizualizácie cez Pygame. Tento README popisuje, ako spustiť projekt, aké nástroje a verzie softvéru potrebujete, a vysvetľuje štruktúru projektu.

---

## Požiadavky

### Softvér
- **Python**: Verzia 3.9 alebo novšia
- **SWI-Prolog**: Verzia 8.0 alebo novšia
- **Pygame**: Verzia 2.6.1

### Python balíky
- **pygame**: Ak nie je nainštalovaný, použite príkaz:

    ```bash
    pip install pygame
    ```

## Štruktúra projektu

### Priečinky
- **assets**: Obsahuje obrázky na vizualizáciu (hráč, krabice, steny, podlaha, ciele).
- **maps**: Obsahuje mapy Sokoban hádaniek vo formáte `.txt`. (Projekt správne rieši len mapy 1,4,5,6,7,8)
- **python**: Obsahuje Python skripty.
- **solver**: Obsahuje Prolog skripty na riešenie hádaniek.

### Súbory a ich úlohy

#### Python skripty
- **main.py**

  Hlavný spúšťací bod projektu. Načíta mapu, vygeneruje fakty, spustí Prolog solver, spracuje výsledok a spustí vizualizáciu.

- **sokoban_parser.py**

  Obsahuje logiku na načítanie mapy z `.txt` súboru a generovanie faktov pre Prolog.

- **sokoban_renderer.py**

  Obsahuje vizualizáciu riešenia pomocou Pygame.

- **prolog_executor.py**

  Spúšťa Prolog solver a spracováva jeho výstup.

#### Prolog skripty
- **solver.pl**

  Obsahuje hlavný BFS algoritmus na riešenie Sokoban hádaniek.

- **state.pl**

  Definuje počiatočný stav a cieľový stav.

- **actions.pl**

  Definuje pohyby hráča (hore, dole, vľavo, vpravo) a logiku posúvania krabíc.

- **utils.pl**

  Obsahuje pomocné funkcie, ako napríklad detekciu deadlockov a výpis riešenia.

- **facts.pl**

  Dynamicky generovaný súbor obsahujúci fakty o aktuálnej mape, ako napríklad umiestnenie stien, krabíc, cieľov a hráča.

## Použitie

### Spustenie projektu
1. Uistite sa, že máte nainštalovaný Python a SWI-Prolog.
2. Nainštalujte Pygame:

    ```bash
    pip install pygame
    ```

3. Spustite skript:

    ```bash
    python python/main.py ALEBO iba runnuť main.py v nejakom IDE
    ```

### Riešenie problémov so spustením SWI-Prologu

Ak sa vám nepodarí spustiť Prolog solver, uistite sa, že cesta k `swipl.exe` v súbore `prolog_executor.py` je správna.

Otvorte súbor `prolog_executor.py` a vyhľadajte nasledujúci riadok a nastavte správnu cestu k Vášmu prologu:

```python
cmd = [
    "C:/Program Files/swipl/bin/swipl.exe",
    "-g",
    f"{format_consults(modules)}, solve_bfs, halt."
]
```


4. Po spustení sa zobrazí výber mapy. Šípkami hore/dole vyberte požadovanú mapu a stlačte **Enter**, aby ste ju načítali.

### Vstup a výstup

- **Vstup**: `.txt` súbor mapy v priečinku `maps`.

  **Formát:**
  - `#`: Stena
  - `S`: Hráč
  - `C`: Krabica
  - `X`: Cieľ
  - `s` alebo `c`: Hráč alebo krabica na cieľovom poli

- **Výstup:**
  - Fakty uložené v `solver/facts.pl`.
  - Vizualizácia riešenia v okne Pygame.
  - Sekvencia krokov (`up`, `down`, `left`, `right`) vypísaná v konzole.

## Ukážka

### Príklad mapy

**Súbor `maps/map1.txt`:**

```shell
######
#s   #
#CCCX#
#X   #
######

# Výstup v konzole

## Prolog output:

Goal reached! Boxes: [box(1,3),box(1,1),box(4,2)]
state(player(1,1),[box(1,2),box(2,2),box(3,2)])
state(player(1,2),[box(1,3),box(2,2),box(3,2)])
state(player(1,1),[box(1,3),box(2,2),box(3,2)])
state(player(2,1),[box(1,3),box(2,2),box(3,2)])
state(player(3,1),[box(1,3),box(2,2),box(3,2)])
state(player(4,1),[box(1,3),box(2,2),box(3,2)])
state(player(4,2),[box(1,3),box(2,2),box(3,2)])
state(player(4,3),[box(1,3),box(2,2),box(3,2)])
state(player(3,3),[box(1,3),box(2,2),box(3,2)])
state(player(2,3),[box(1,3),box(2,2),box(3,2)])
state(player(2,2),[box(1,3),box(2,1),box(3,2)])
state(player(3,2),[box(1,3),box(2,1),box(4,2)])
state(player(3,1),[box(1,3),box(2,1),box(4,2)])
state(player(2,1),[box(1,3),box(1,1),box(4,2)])


Sekvencia akcií:
Dole + potlačil krabičku
Hore
Vpravo
Vpravo
Vpravo
Dole
Dole
Vľavo
Vľavo
Hore + potlačil krabičku
Vpravo + potlačil krabičku
Hore
Vľavo + potlačil krabičku

### Vybehne okno s vizualizáciou, ktorá zbehne a na záver pogratuluje k vyriešeniu mapy.