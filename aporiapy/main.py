import argparse
import ast
import sys
from pathlib import Path

from aporiapy.compilers.compiler_01_sc import CompilerSc
from aporiapy.compilers.compiler_02_flat import CompilerFlat
from aporiapy.compilers.compiler_03_cfi import CompilerCfi

compilers = [CompilerSc(), CompilerFlat(), CompilerCfi()]


def compile_source(input_path, output_to_stdout, output_path):
    """Compile the source file and handle output based on the mode."""
    with open(input_path, "r") as file:
        original_source = file.read()

    program_ast = ast.parse(original_source)

    compiled_source = original_source
    for compiler in compilers:
        compiled_ast = compiler.compile(program_ast)
        if isinstance(compiled_ast, ast.Module):
            ast.fix_missing_locations(compiled_ast)
            compiled_source = ast.unparse(compiled_ast)
        else:
            compiled_source = str(compiled_ast)

        program_ast = compiled_ast

    if output_to_stdout:
        print(compiled_source)
    else:
        if output_path is None:
            output_path = input_path.with_suffix(".spp")
        with open(output_path, "w") as output_file:
            output_file.write(compiled_source)
        print(f"Compiled file written to: {output_path}")

def cli():
    parser = argparse.ArgumentParser(description="Compiles Python code to Aporia code")

    parser.add_argument(
        "input_file", type=str, help="Path to the input Python file to compile."
    )

    parser.add_argument(
        "-o", "--output", type=str, help="Path to the output file to write the compiled code."
    )

    parser.add_argument(
        "--stdout", action="store_true", help="Output the compiled code to stdout instead of a file."
    )

    args = parser.parse_args()

    input_path = Path(args.input_file)

    if not input_path.exists():
        sys.stderr.write(f"Error: Input file '{input_path}' does not exist.\n")
        sys.exit(1)

    compile_source(input_path, args.stdout, args.output)

if __name__ == "__main__":
    cli()


