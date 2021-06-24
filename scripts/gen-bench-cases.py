import sys
from typing import List


def read_file(filepath: str) -> List[str]:
    with open(filepath, 'r') as f:
        return f.readlines()

def write_file(filepath: str, lines: List[str]) -> None:
    with open(filepath, 'w') as f:
        f.writelines(lines)

def gen_static_fields(num: int) -> List[str]:
    lines = []
    for i in range(num):
        lines.append('        private final static OFPort p%d = new OFPort(%d);\n' % (i, i))
    return lines

def gen_cases(num: int) -> List[str]:
    lines = []
    for i in range(num):
        lines.append('            case %d:\n' % i)
        lines.append('                return PrecachedPort.p%d;\n' % i)
    return lines

def gen_bench_file(src_path: str, out_path: str, branch_num: int) -> None:
    fields_start_line = 101
    fields_end_line = 148
    cases_start_line = 169
    cases_end_line = 264
    lines = read_file(src_path)
    assert fields_start_line < fields_end_line
    assert fields_end_line < cases_start_line
    assert cases_start_line < cases_end_line
    assert cases_end_line < len(lines)
    new_lines = []
    for i in range(0, fields_start_line-1):
        new_lines.append(lines[i])
    new_lines += gen_static_fields(branch_num)
    for i in range(fields_end_line, cases_start_line-1):
        new_lines.append(lines[i])
    new_lines += gen_cases(branch_num)
    for i in range(cases_end_line, len(lines)):
        new_lines.append(lines[i])
    write_file(out_path, new_lines)

def main(args):
    if not len(args) == 4:
        print('Usage: python3 gen-bench-cases.py <src_path> <out_path> <branch_num>')
        sys.exit(1)
    src_path = args[1]
    out_path = args[2]
    branch_num = int(args[3])
    gen_bench_file(src_path, out_path, branch_num)

if __name__ == '__main__':
    main(sys.argv)
