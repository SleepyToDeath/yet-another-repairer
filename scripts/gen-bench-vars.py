import sys
from typing import List


def read_file(filepath: str) -> List[str]:
    with open(filepath, 'r') as f:
        return f.readlines()

def write_file(filepath: str, lines: List[str]) -> None:
    with open(filepath, 'w') as f:
        f.writelines(lines)

def gen_local_vars(num: int) -> List[str]:
    lines = [
        '        DatapathId none = DatapathId.NONE;\n',
        '        OFPort p0 = OFPort.of(0);\n',
        '        OFPort p4 = OFPort.of(4);\n'
    ]
    for i in range(num):
        lines.append('        Link v%d = new Link(none, p0, none, p4);\n' % i)
    return lines

def gen_bench_file(src_path: str, out_path: str, branch_num: int) -> None:
    var_line = 136
    lines = read_file(src_path)
    assert var_line < len(lines)
    new_lines = []
    for i in range(0, var_line):
        new_lines.append(lines[i])
    new_lines += gen_local_vars(branch_num)
    for i in range(var_line, len(lines)):
        new_lines.append(lines[i])
    write_file(out_path, new_lines)

def main(args):
    if not len(args) == 4:
        print('Usage: python3 gen-bench-vars.py <src_path> <out_path> <branch_num>')
        sys.exit(1)
    src_path = args[1]
    out_path = args[2]
    branch_num = int(args[3])
    gen_bench_file(src_path, out_path, branch_num)

if __name__ == '__main__':
    main(sys.argv)
