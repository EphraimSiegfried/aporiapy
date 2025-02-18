from ast import (
    Module,
    stmt,
    If,
    BoolOp,
    And,
    Constant,
    Name,
    Assign,
)

from aporiapy.compilers.utils import generate_name


class CompilerFlat:

    def compile(self, program_ast: Module) -> Module:
        out_list = []
        self.flatten_statements(program_ast.body, out_list)
        return Module(out_list, [])

    def flatten_statements(self, stmts: list[stmt], out_list: list[stmt], cond=None):

        for statement in stmts:
            self.flatten_statement(statement, out_list, cond)

    def flatten_statement(self, statement: stmt, out_list: list[stmt], cond=None):
        match statement:
            case If(test, body, []):
                if not cond:
                    temp = test
                else:
                    temp = Name(generate_name())
                    new_cond = BoolOp(And(), [cond, test])
                    out_list.append(If(Constant(True), [Assign([temp], new_cond)], []))
                for stmt in body:
                    match stmt:
                        case If():
                            self.flatten_statement(stmt, out_list, temp)
                        case _:
                            out_list.append(If(temp, [stmt], []))
            case _:
                out_list.append(If(Constant(True), [statement], []))
