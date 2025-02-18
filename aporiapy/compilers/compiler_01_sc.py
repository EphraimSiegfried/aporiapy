from ast import *
from aporiapy.compilers.utils import generate_name


class CompilerSc:

    def compile(self, program_ast: Module) -> Module:

        return Module(self.rco_statements(program_ast.body), [])

    def rco_statements(self, stmts):

        new_stmts = []

        for statement in stmts:
            new_stmts.extend(self.rco_statement(statement))
        return new_stmts

    def rco_statement(self, stmt) -> list[stmt]:
        new_stmts = []
        match stmt:
            case If(test, body, orelse):
                new_test, temp = self.rco_expression(test, True)
                new_stmts.extend(temp)
                neg_test = Name(generate_name())
                new_stmts.append(Assign([neg_test], UnaryOp(Not(), new_test)))
                new_body = self.rco_statements(body)
                new_orelse = self.rco_statements(orelse)
                new_stmts.append(If(new_test, new_body, []))
                new_stmts.append(If(neg_test, new_orelse, []))
                return new_stmts
            case Expr(Call(Name("print"), [exp])):
                exp, temps = self.rco_expression(exp, False)
                new_stmts.extend(temps)
                new_stmts.append(Expr(Call(Name("print"), [exp], keywords=[])))
                return new_stmts
            case Expr():
                return []
            case Assign([Name(var)], expr):
                return self.rco_assign(var, expr)
            case _:
                raise Exception(f"Unexpected statement in rco_statement: {stmt}")

    def rco_assign(self, name, expr) -> list[stmt]:
        new_stmts = []
        match expr:
            case IfExp(test, body, orelse):
                return self.rco_if_exp(name, test, body, orelse)

            case _:
                expr, temps = self.rco_expression(expr, False)
                new_stmts.extend(temps)
                new_stmts.append(Assign([Name(name)], expr))
                return new_stmts

    def rco_if_exp(self, name, test, body, orelse) -> list[stmt]:
        new_stmts = []
        new_test, temp = self.rco_expression(test, True)
        new_stmts.extend(temp)
        neg_test = Name(generate_name())
        new_stmts.append(Assign([neg_test], UnaryOp(Not(), new_test)))
        body_expr, body_temps = self.rco_expression(body, False)
        orelse_expr, orelse_temps = self.rco_expression(orelse, False)
        body_assign = [Assign([Name(name)], body_expr)]
        orelse_assign = [Assign([Name(name)], orelse_expr)]
        new_body = body_temps + body_assign
        new_orelse = orelse_temps + orelse_assign
        new_stmts.append(If(new_test, new_body, []))
        new_stmts.append(If(neg_test, new_orelse, []))
        return new_stmts

    def rco_expression(self, expression, need_atomic) -> tuple[expr, list[stmt]]:

        def make_atom_if_needed(exp, exp_temps) -> tuple[expr, list[stmt]]:
            if need_atomic:
                temp_var = Name(generate_name())
                exp_temps.append(Assign([temp_var], exp))
                exp = temp_var
            return exp, exp_temps

        match expression:
            case IfExp(test, body, orelse):
                name = generate_name("temp")
                temps = self.rco_if_exp(name, test, body, orelse)
                return Name(name), temps
            case BinOp(left, op, right):
                left, left_temps = self.rco_expression(left, False)
                right, right_temps = self.rco_expression(right, False)
                exp = BinOp(left, op, right)
                exp_temps = left_temps + right_temps
                return make_atom_if_needed(exp, exp_temps)
            case UnaryOp(op, exp):
                exp, exp_temps = self.rco_expression(exp, False)
                exp = UnaryOp(op, exp)
                return make_atom_if_needed(exp, exp_temps)
            case Constant() | Name():
                return expression, []
            case BoolOp(op, [left, right]):
                left, left_temps = self.rco_expression(left, False)
                right, right_temps = self.rco_expression(right, False)
                exp = BoolOp(op, [left, right])
                exp_temps = left_temps + right_temps
                return make_atom_if_needed(exp, exp_temps)
            case Compare(left, cmp, [right]):
                left, left_temps = self.rco_expression(left, False)
                right, right_temps = self.rco_expression(right, False)
                exp = Compare(left, cmp, [right])
                exp_temps = left_temps + right_temps
                return make_atom_if_needed(exp, exp_temps)
            case _:
                raise Exception(
                    f"Unexpected expression in rco_expression: {expression}"
                )
