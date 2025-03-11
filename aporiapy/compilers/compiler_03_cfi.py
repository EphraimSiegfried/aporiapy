import ast
from ast import *
from collections import defaultdict

import aporia.aporia_ast as lcfi_ast
from aporia.aporia_ast import *

from aporiapy.compilers.utils import generate_name


class CompilerCfi:

    def compile(self, program_ast: Module) -> L_cfi:
        var_to_type = dict()
        ap_stmts = []
        for stmt in program_ast.body:
            if not isinstance(stmt, ast.If):
                raise Exception("Only if statements are allowed in the program")
            ap_stmts.extend(self.translate_stmt(stmt, var_to_type))

        type_to_var = defaultdict(set)
        for var, var_type in var_to_type.items():
            type_to_var[var_type].add(Var(var))

        ap_declar = [Declar(var_type(), var_set) for var_type, var_set in type_to_var.items()]

        cfi = L_cfi(ap_declar, ap_stmts)
        return cfi

    def translate_stmt(self, if_stmt, var_to_type) -> list[Stmt]:
        pred = self.select_pred(if_stmt.test)
        body = if_stmt.body
        if not body or len(body) != 1:
            raise Exception("Body of if statement must have exactly one statement")
        aux_stmts, ap_body = self.select_instruction(body[0], var_to_type)
        return aux_stmts + [Stmt(None, pred, ap_body)]

    def select_pred(self, test) -> Pred:
        match test:
            case ast.Name(var):
                return Pred(Var(var))
            case ast.Constant(value):
                return Pred(Bools(value))
            case _:
                raise Exception(f"Unexpected test in select_pred: {test}")

    def select_instruction(self, stmt, var_to_type) -> tuple[list[Stmt], Inst]:
        match stmt:
            case ast.Expr(ast.Call(ast.Name("print"), args)):
                if isinstance(string := getattr(args[0], "value", None), str):
                    exp = None
                    aux_stmts = []
                    if len(args) > 1:
                        aux_stmts, exp = self.select_exp(args[1], var_to_type)
                        string += " "
                    return aux_stmts, PrintInst(string, exp)
                else:
                    aux_stmts, exp = self.select_exp(args[0], var_to_type)
                    return aux_stmts, PrintInst("", exp)
            case ast.Assign([Name(var)], exp):
                self.check_type(var, exp, var_to_type)
                aux_stmts, exp = self.select_exp(exp, var_to_type)
                return aux_stmts, AssignInst(lcfi_ast.Assign(Var(var), exp))
            case ast.Expr(value):
                aux_stmts, exp = self.select_exp(value, var_to_type)
                return aux_stmts, ExpInst(exp)
            case _:
                raise Exception(f"Unexpected statement in select_instruction: {stmt}")

    def check_expression_type(self, exp, var_to_type):
        match exp:
            case ast.Constant(bool()):
                return lcfi_ast.Bool
            case ast.Constant(int()):
                return lcfi_ast.Int
            case ast.Constant(float()):
                return lcfi_ast.Float
            case ast.Name(var):
                if not var in var_to_type:
                     raise Exception(f"Undefined variable detected in check_type: {var}")
                return var_to_type[var]
            case ast.BoolOp():
                return lcfi_ast.Bool
            case ast.UnaryOp(ast.Not(), _):
                return lcfi_ast.Bool
            case ast.Compare():
                return lcfi_ast.Bool
            case ast.BinOp(_, ast.Div(), _):
                return lcfi_ast.Float
            case ast.BinOp(_, ast.FloorDiv(), _):
                return lcfi_ast.Int
            case ast.BinOp(left, op, right):
                left_type = self.check_expression_type(left, var_to_type)
                right_type = self.check_expression_type(right, var_to_type)
                operand_types = [left_type, right_type]
                if any(operand_type == lcfi_ast.Float for operand_type in operand_types):
                    return lcfi_ast.Float
                elif all(operand_type == lcfi_ast.Int for operand_type in operand_types):
                    return lcfi_ast.Int
                elif all(operand_type == lcfi_ast.Bool for operand_type in operand_types):
                    return lcfi_ast.Bool
                else:
                    raise Exception(f"Unexpected operand type in check_type: {left} {op} {right}")
            case ast.UnaryOp(ast.USub(), exp):
                return self.check_expression_type(exp, var_to_type)
            case _:
                raise Exception(f"Unexpected expression in check_type: {exp}")

    def check_type(self, new_var, exp, var_to_type):
        exp_type = self.check_expression_type(exp, var_to_type)
        if not new_var in var_to_type:
            var_to_type[new_var] = exp_type
        elif var_to_type[new_var] == lcfi_ast.Int and exp_type == lcfi_ast.Float:
            var_to_type[new_var] = lcfi_ast.Float
        elif exp_type == var_to_type[new_var] or (exp_type == lcfi_ast.Int and var_to_type[new_var] == lcfi_ast.Float):
            return
        else:
            raise Exception(f"Cannot cast {var_to_type[new_var]} to {exp_type} in statement: {new_var} = {exp}")

    def to_float(self, exp, var_to_type):
        new_stmt = None
        if isinstance(exp, lcfi_ast.Constant):
            new_exp = Constant(float(exp.value))
        else:
            name = generate_name("temp")
            new_exp = lcfi_ast.Var(name)
            var_to_type[name] = lcfi_ast.Float
            to_float_cast_instr = lcfi_ast.AssignInst(lcfi_ast.Assign(new_exp, exp))
            new_stmt = lcfi_ast.Stmt(None, lcfi_ast.Pred(Bools(True)), to_float_cast_instr)
        return new_stmt, new_exp

    def select_exp(self, exp, var_to_type) -> tuple[list[Stmt], Exp]:
        match exp:
            case ast.Name(var):
                return [], Var(var)
            case ast.Constant(value):
                return [], (
                    lcfi_ast.Bools(value)
                    if isinstance(value, bool)
                    else lcfi_ast.Constant(value)
                )
            case ast.BinOp(left, op, right):
                new_op = self.select_op(op)
                operand_types = [self.check_expression_type(o, var_to_type) for o in [left, right]]
                aux_left, left = self.select_exp(left, var_to_type)
                aux_right, right = self.select_exp(right, var_to_type)
                new_operands = [left, right]
                aux_stmts = aux_left + aux_right
                if (self.check_expression_type(exp, var_to_type) == lcfi_ast.Float
                        and all(type == lcfi_ast.Int for type in operand_types)):
                    index = next((i for i, o in enumerate(new_operands) if isinstance(o,lcfi_ast.Constant)), 0)
                    aux_stmt, new_operands[index] = self.to_float(new_operands[index], var_to_type)
                    if aux_stmt:
                        aux_stmts.append(aux_stmt)
                return aux_stmts, lcfi_ast.BinOp(new_operands[0], new_op, new_operands[1])
            case ast.UnaryOp(op, operand):
                op = self.select_op(op)
                aux_stmts, operand = self.select_exp(operand, var_to_type)
                return aux_stmts, lcfi_ast.UnaryOp(op, operand)
            case ast.BoolOp(op, [left, right]):
                op = self.select_op(op)
                aux_left, left = self.select_exp(left, var_to_type)
                aux_right, right = self.select_exp(right, var_to_type)
                return aux_left + aux_right, lcfi_ast.BinOp(left, op, right)
            case ast.Compare(left, [cmp], [right]):
                cmp = self.select_cmp(cmp)
                aux_left, left = self.select_exp(left, var_to_type)
                aux_right, right = self.select_exp(right, var_to_type)
                return aux_left + aux_right, lcfi_ast.BinOp(left, cmp, right)
            case _:
                raise Exception(f"Unexpected expression in select_exp: {exp}")

    def select_op(self, op) -> Operator:
        match op:
            case ast.Add():
                return lcfi_ast.Add()
            case ast.Sub():
                return lcfi_ast.Sub()
            case ast.Mult():
                return lcfi_ast.Mult()
            case ast.Mod():
                return lcfi_ast.Mod()
            case ast.USub():
                return lcfi_ast.USub()
            case ast.Div():
                return lcfi_ast.Div()
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
