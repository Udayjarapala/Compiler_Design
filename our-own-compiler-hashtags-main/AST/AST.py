from typing import Optional
from dataclasses import dataclass
from typing import Any, List, Tuple, Union

@dataclass
class Program:
    statements: List[Any]

@dataclass
class Statements:
    statement: Tuple[str, Any]
    statements: List[Tuple[str, Any]]

@dataclass
class Declaration:
    type: str
    assignment: 'Assignment'

@dataclass
class LAT:
    identifier: str
    index: int

@dataclass
class Assignment:
    identifier: str
    assignment_operator: str
    value: Any

@dataclass
class OperationalStatements_ADD:
    identifier: str
    operation: str
    parameters: str

@dataclass
class OperationalStatements_CUT:
    identifier: str
    operation: str
    parameters1: int
    parameters2: int

@dataclass
class OperationalStatements_EDIT:
    identifier: str
    operation: str
    parameters: int
    parameters: int
    string: str

@dataclass
class OperationalStatements_GET:
    identifier: str
    operation: str
    parameters: int
    parameters: int

@dataclass
class OperationalStatements_LEN:
    identifier: str
    operation: str

@dataclass
class OperationalStatements_L_ADD:
    identifier: str
    operation: str
    parameters: any

@dataclass
class OperationalStatements_L_CUT:
    identifier: str
    operation: str
    parameters: int
    parameters: int

@dataclass
class OperationalStatements_SLICE:
    identifier: str
    operation: str
    parameters: int
    parameters: int

@dataclass
class OperationalStatements_AADD:
    identifier: str
    operation: str
    parameters: int    

@dataclass
class Type:
    type: str

@dataclass
class MA:
    condition: any

@dataclass
class IfStatement:
    condition: Any
    statements: List[Any]
    elsif: 'ElsifStatement'
    elses: 'ElseStatement'

@dataclass
class ElsifStatement:
    condition: Any
    statements: List[Any]
    next_elsif: Optional['ElsifStatement']

@dataclass
class ElseStatement:
    statements: Optional[List[Any]]

@dataclass
class WhileStatement:
    condition: Any
    statement_list: List[Any]

@dataclass
class ForStatement:
    condition: Any
    statement_list: List[Any]

@dataclass
class MB:
    assignment_statement: Any
    condition: str
    unary_ops: str
    identifier: Any

@dataclass
class PrintStatement:
    printing_data: Any

@dataclass
class Pexpression:
    value: Union[str, 'Identifiers', List['Pexpression']]

@dataclass
class Identifiers:
    identifier: str
    next_identifiers: Optional['Identifiers']

@dataclass
class FunctionDefinition:
    identifier: str
    parameter_list: Any
    statements: List[Any]
    return_expression: Any

@dataclass
class FunctionCall:
    identifier: str
    arguments: Any


@dataclass
class ParameterList:
    type_: str
    identifier: str
    cp: Optional['Cp']

@dataclass
class Cp:
    parameter_list: Optional[ParameterList]

@dataclass
class MutableVariable:
    Variable: Any

@dataclass
class CatchStatement:
    Identifier1: str
    Identifier2: str
    Statements: List[Any]

@dataclass
class ExceptionHandling:
    try_block: List[Any]
    exception_type: str
    identifier: str
    CatchS: Union['CatchStatement', None]

@dataclass
class LetExpression:
    variable_declaration: Any
    assignment_operator: str
    expression: Any
    Statements: List[Any]
    return_expression: Any

@dataclass
class Condition:
    left_expression: Any
    comparison_operator: 'ComparisonOperator'
    right_expression: Any


@dataclass
class ComparisonOperator:
    operator: str

@dataclass
class Expression:
    left_expression: Union[str, int, tuple]
    binary_operations: str
    right_expression: Union['T']

@dataclass
class T:
    factor: Union['Factor']
    unary_operations: Union[None, Any]

@dataclass
class Factor:
    value: Union[str, int, Tuple]

@dataclass
class BinaryOperations:
    operator: str


@dataclass
class UnaryOperations:
    operator: str

@dataclass
class BooleanConstant:
    value: bool

@dataclass
class Empty:
    pass
