import ast
import io
import contextlib
import pytest
from pathlib import Path
import aporia.interpreter as interpreter
from aporiapy.compilers.compiler_01_sc import CompilerSc
from aporiapy.compilers.compiler_02_flat import CompilerFlat
from aporiapy.compilers.compiler_03_cfi import CompilerCfi

compilers = [CompilerSc(), CompilerFlat(), CompilerCfi()]

TEST_DIR = Path(__file__).parent / "test_cases"

def execute_and_capture(code):
    """Executes a Python code string and captures stdout."""
    output = io.StringIO()
    try:
        with contextlib.redirect_stdout(output):
            exec(code)
        # TODO: remove trailing newline, should be fixed differently
        return output.getvalue().rstrip("\n")
    except Exception as e:
        return f"Error: {e}"

# Load all test cases
def load_test_cases():
    return sorted(list(TEST_DIR.glob("*.py")))


@pytest.mark.parametrize("test_file", load_test_cases(), ids=lambda f: f.name)
def test_compiler(test_file):
    """Tests the compiler on multiple programs."""
    original_source = test_file.read_text().strip()
    output_original = execute_and_capture(original_source)
    program_ast = ast.parse(original_source)

    for compiler in compilers:
        program_ast = compiler.compile(program_ast)
        if isinstance(program_ast, ast.Module):
            ast.fix_missing_locations(program_ast)
            compiled_source = ast.unparse(program_ast)
            output_compiled = execute_and_capture(compiled_source)
        else:
            output_compiled = interpreter.InterpLcfi().interp(program_ast)
        assert output_original == output_compiled, f"FAIL ({test_file.name}, {compiler.__class__.__name__}): expected {output_original} but got {output_compiled}"

