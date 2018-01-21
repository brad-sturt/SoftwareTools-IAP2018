import re
import sys


def is_exercise_start(line):
    return re.match("^```{r exercise", line)


def is_exercise_end(line):
    return re.match("```$", line)


if __name__ == '__main__':
    with open(sys.argv[1], 'rb') as f:
        in_exercise_block = False
        for line in f:
            if is_exercise_start(line):
                in_exercise_block = True
            elif in_exercise_block:
                if is_exercise_end(line):
                    in_exercise_block = False
            else:
                sys.stdout.write(line)
