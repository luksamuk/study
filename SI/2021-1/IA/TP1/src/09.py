#!/usr/bin/python
import sys

def slurp(filename):
    slurped_file = []
    try:
        with open(filename, "r") as the_file:
            slurped_file = [line.rstrip('\n') for line in the_file]
    except IsADirectoryError:
        print("Erro ao abrir \"{}\": é um diretório".format(filename))
    except FileNotFoundError:
        print("Erro ao abrir o arquivo \"{}\"".format(filename))
    except OSError:
        print("Impossível ler o arquivo \"{}\"".format(filename))
    return slurped_file

def get_function_name(line):
    return line.split(" ")[1].split("(")[0]

def print_undocumented_function(filename, line, function_name):
    print("{}:{:03d}: {}".format(filename, line, function_name))

def check_file(filename):
    lines = slurp(filename)
    if len(lines) > 0:
        # Caso especial para primeira linha: se for uma
        # função, não tem comentário antes.
        if lines[0].startswith("def "):
            print_undocumented_function(
                filename, 1, get_function_name(lines[0]))
        for idx in range(1, len(lines)):
            prev_line = lines[idx - 1]
            curr_line = lines[idx]
            if curr_line.startswith("def ") and \
               (not prev_line.startswith("#")):
                print_undocumented_function(
                    filename, idx + 1, get_function_name(curr_line))

for arg in sys.argv[1:]:
    check_file(arg)
