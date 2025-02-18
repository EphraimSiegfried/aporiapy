import ast
from ast import *
import aporia.aporia_ast as lcfi_ast
from aporia.aporia_ast import *


class CompilerCfi:

    def compile(self, program_ast: Module) -> L_cfi:
        int_vars = set()
        bool_vars = set()
        ap_stmts = []
        for stmt in program_ast.body:
            if not isinstance(stmt, ast.If):
                raise Exception("Only if statements are allowed in the program")
            ap_stmts.append(self.translate_stmt(stmt, int_vars, bool_vars))

        int_vars = {Var(var) for var in int_vars}
        bool_vars = {Var(var) for var in bool_vars}
        ap_declar = []
        if not len(int_vars) == 0:
            ap_declar.append(Declar(Int(), int_vars))
        if not len(bool_vars) == 0:
            ap_declar.append(Declar(Bool(), bool_vars))
        cfi = L_cfi(ap_declar, ap_stmts)
        return cfi

    def translate_stmt(self, if_stmt, int_vars, bool_vars) -> Stmt:
        test = if_stmt.test
        ap_test = self.select_pred(test)
        body = if_stmt.body
        if not body or len(body) != 1:
            raise Exception("Body of if statement must have exactly one statement")
        ap_body = self.select_instruction(body[0], int_vars, bool_vars)
        return Stmt(None, ap_test, ap_body)

    def select_pred(self, test) -> Pred:
        match test:
            case ast.Name(var):
                return Pred(Var(var))
            case ast.Constant(value):
                return Pred(Bools(value))
            case _:
                raise Exception(f"Unexpected test in select_pred: {test}")

    def select_instruction(self, stmt, int_vars, bool_vars) -> Inst:
        match stmt:
            case ast.Expr(Call(Name('print'), [exp])):
                return PrintInst("", self.select_exp(exp))
            case ast.Assign([Name(var)], exp):
                self.check_type(var, exp, int_vars, bool_vars)
                return AssignInst(lcfi_ast.Assign(Var(var), self.select_exp(exp)))
            case ast.Expr(value):
                return ExpInst(self.select_exp(value))
            case _:
                raise Exception(f"Unexpected statement in select_instruction: {stmt}")


    def check_type(self, new_var, exp, int_vars, bool_vars):
        match exp:
            case ast.Name(var):
                if var in int_vars:
                    int_vars.add(new_var)
                elif var in bool_vars:
                    bool_vars.add(new_var)
                else:
                    raise Exception(f"Variable {var} not declared")
            case ast.BinOp():
                int_vars.add(new_var)
            case ast.BoolOp():
                bool_vars.add(new_var)
            case ast.UnaryOp(ast.USub(), _):
                int_vars.add(new_var)
            case ast.UnaryOp(ast.Not(), _):
                bool_vars.add(new_var)
            case ast.Compare():
                bool_vars.add(new_var)
            case ast.Constant(value):
                if isinstance(value, bool):
                    bool_vars.add(new_var)
                elif isinstance(value, int):
                    int_vars.add(new_var)
            case _:
                raise Exception(f"Unexpected expression in check_type: {exp}")

    def select_exp(self, exp) -> Exp:
        match exp:
            case ast.Name(var):
                return Var(var)
            case ast.Constant(value):
                return lcfi_ast.Bools(value) if isinstance(value,bool) else lcfi_ast.Constant(value)
            case ast.BinOp(left, op, right):
                op = self.select_op(op)
                return lcfi_ast.BinOp(self.select_exp(left), op, self.select_exp(right))
            case ast.UnaryOp(op, operand):
                op = self.select_op(op)
                return lcfi_ast.UnaryOp(op, self.select_exp(operand))
            case ast.BoolOp(op, [left, right]):
                op = self.select_op(op)
                return lcfi_ast.BinOp(self.select_exp(left), op, self.select_exp(right))
            case ast.Compare(left, [cmp], [right]):
                cmp = self.select_cmp(cmp)
                return lcfi_ast.BinOp(self.select_exp(left), cmp, self.select_exp(right))
            case _:
                raise Exception(f"Unexpected expression in select_exp: {exp}")

    def select_op(self, op) -> Operator:
        match op:
            case ast.Add():
                return lcfi_ast.Add()
            case ast.Sub():
                return lcfi_ast.Sub()
            case ast.USub():
                return lcfi_ast.USub()
            case ast.And():
                return lcfi_ast.And()
            case ast.Or():
                return lcfi_ast.Or()
            case ast.Not():
                return lcfi_ast.Not()
            case _:
                raise Exception(f"Unexpected operator in select_op: {op}")

    def select_cmp(self, cmp) -> Comparator:
        match cmp:
            case ast.Eq():
                return lcfi_ast.Eq()
            case ast.NotEq():
                return lcfi_ast.Neq()
            case ast.Lt():
                return lcfi_ast.Lt()
            case ast.LtE():
                return lcfi_ast.Le()
            case ast.Gt():
                return lcfi_ast.Gt()
            case ast.GtE():
                return lcfi_ast.Ge()
            case _:
                raise Exception(f"Unexpected comparator in select_cmp: {cmp}")

