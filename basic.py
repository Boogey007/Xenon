####################################### IMPORTS #######################################
from strings_with_arrows import *
import string
import os
import math

####################################### CONSTANTS #######################################
DIGITS = '0123456789'
LETTERS = string.ascii_letters
LETTERS_DIGITS = LETTERS + DIGITS


####################################### ERRORS #######################################
class Error:
  def __init__(self, starting_postition, ending_postition, error, details):
    self.starting_postition = starting_postition
    self.ending_postition = ending_postition
    self.error = error
    self.details = details

  def as_string(self):
    result  = f'{self.error}: {self.details}\n'
    result += f'File {self.starting_postition.fn}, line {self.starting_postition.ln + 1}'
    result += '\n\n' + string_with_arrows(self.starting_postition.ftxt, self.starting_postition, self.ending_postition)
    return result

class IllegalChar(Error):
  def __init__(self, starting_postition, ending_postition, details):
    super().__init__(starting_postition, ending_postition, 'Illegal Character', details)

class ExpectedChar(Error):
  def __init__(self, starting_postition, ending_postition, details):
    super().__init__(starting_postition, ending_postition, 'Expected Character', details)

class InvalidSyntaxError(Error):
  def __init__(self, starting_postition, ending_postition, details=''):
    super().__init__(starting_postition, ending_postition, 'Invalid Syntax', details)

class RTError(Error):
  def __init__(self, starting_postition, ending_postition, details, context):
    super().__init__(starting_postition, ending_postition, 'Runtime Error', details)
    self.context = context

  def as_string(self):
    result  = self.generate_traceback()
    result += f'{self.error}: {self.details}'
    result += '\n\n' + string_with_arrows(self.starting_postition.ftxt, self.starting_postition, self.ending_postition)
    return result

  def generate_traceback(self):
    result = ''
    pos = self.starting_postition
    ctx = self.context

    while ctx:
      result = f'  File {pos.fn}, line {str(pos.ln + 1)}, in {ctx.display_name}\n' + result
      pos = ctx.parent_entry_pos
      ctx = ctx.parent

    return 'Traceback (most recent call last):\n' + result

####################################### POSITION #######################################
class Position:
  def __init__(self, idx, ln, col, fn, ftxt):
    self.idx = idx
    self.ln = ln
    self.col = col
    self.fn = fn
    self.ftxt = ftxt

  def nextToken(self, curr_character=None):
    self.idx += 1
    self.col += 1

    if curr_character == '\n':
      self.ln += 1
      self.col = 0

    return self

  def copy(self):
    return Position(self.idx, self.ln, self.col, self.fn, self.ftxt)


####################################### TOKENS #######################################
TT_INT				= 'INT'
TT_FLOAT    	= 'FLOAT'
TT_STRING			= 'STRING'
TT_IDENTIFIER	= 'IDENTIFIER'
TT_KEYWORD		= 'KEYWORD'
TT_PLUS     	= 'PLUS'
TT_MINUS    	= 'MINUS'
TT_MUL      	= 'MUL'
TT_DIV      	= 'DIV'
TT_POW				= 'POW'
TT_EQ					= 'EQ'
TT_LPAREN   	= 'LPAREN'
TT_RPAREN   	= 'RPAREN'
TT_LSQUARE    = 'LSQUARE'
TT_RSQUARE    = 'RSQUARE'
TT_EE					= 'EE'
TT_NE					= 'NE'
TT_LT					= 'LT'
TT_GT					= 'GT'
TT_LTE				= 'LTE'
TT_GTE				= 'GTE'
TT_COMMA			= 'COMMA'
TT_ARROW			= 'ARROW'
TT_NEWLINE		= 'NEWLINE'
TT_EOF				= 'EOF'
####################################### TEST 2 TOKENS #######################################
TT_MOD       = 'MOD'
TT_PREINC    = 'PREINC'
TT_PREDEC    = 'PREDEC'
TT_POSTINC   = 'POSTINC'
TT_POSTDEC   = 'POSTDEC'
TT_AND       = 'AND'
TT_OR        = 'OR'
TT_BAND      = 'BAND'
TT_XOR		   = 'XOR'

KEYWORDS = [
  'VAR',
  'AND',
  'OR',
  'NOT',
  'IF',
  'ELIF',
  'ELSE',
  'FOR',
  'TO',
  'STEP',
  'WHILE',
  'FUN',
  'THEN',
  'END',
  'RETURN',
  'CONTINUE',
  'BREAK',
]

class Token:
  def __init__(self, type_, value=None, starting_postition=None, ending_postition=None):
    self.type = type_
    self.value = value

    if starting_postition:
      self.starting_postition = starting_postition.copy()
      self.ending_postition = starting_postition.copy()
      self.ending_postition.nextToken()

    if ending_postition:
      self.ending_postition = ending_postition.copy()

  def matches(self, type_, value):
    return self.type == type_ and self.value == value

  def __repr__(self):
    if self.value: return f'{self.type}:{self.value}'
    return f'{self.type}'


####################################### LEXER #######################################
class Lexer:
  def __init__(self, fn, text):
    self.fn = fn
    self.text = text
    self.pos = Position(-1, 0, -1, fn, text)
    self.curr_character = None
    self.nextToken()

  def nextToken(self):
    self.pos.nextToken(self.curr_character)
    self.curr_character = self.text[self.pos.idx] if self.pos.idx < len(self.text) else None

  def make_tokens(self):
    tokens = []

    while self.curr_character != None:
      if self.curr_character in ' \t':
        self.nextToken()
      elif self.curr_character == '~':
        self.skip_comment()
      elif self.curr_character in ';\n':
        tokens.append(Token(TT_NEWLINE, starting_postition=self.pos))
        self.nextToken()
      elif self.curr_character in DIGITS:
        tokens.append(self.make_number())
      elif self.curr_character in LETTERS:
        tokens.append(self.make_identifier())
      elif self.curr_character == '"':
        tokens.append(self.make_string())
      elif self.curr_character == '+':
        tokens.append(self.make_plus_preinc())
        # tokens.append(Token(TT_PLUS, starting_postition=self.pos))
        # self.nextToken()
      elif self.curr_character == '&':
        tokens.append(self.make_band_or_and())
      elif self.curr_character == '|':
        tokens.append(self.make_xor_or_or())
      elif self.curr_character == '-':
        tokens.append(self.make_minus_or_arrow())
      elif self.curr_character == '*':
        tokens.append(Token(TT_MUL, starting_postition=self.pos))
        self.nextToken()
      elif self.curr_character == '%':
        tokens.append(Token(TT_MOD, starting_postition=self.pos))
        self.nextToken()
      elif self.curr_character == '/':
        tokens.append(Token(TT_DIV, starting_postition=self.pos))
        self.nextToken()
      elif self.curr_character == '^':
        tokens.append(Token(TT_POW, starting_postition=self.pos))
        self.nextToken()
      elif self.curr_character == '(':
        tokens.append(Token(TT_LPAREN, starting_postition=self.pos))
        self.nextToken()
      elif self.curr_character == ')':
        tokens.append(Token(TT_RPAREN, starting_postition=self.pos))
        self.nextToken()
      elif self.curr_character == '[':
        tokens.append(Token(TT_LSQUARE, starting_postition=self.pos))
        self.nextToken()
      elif self.curr_character == ']':
        tokens.append(Token(TT_RSQUARE, starting_postition=self.pos))
        self.nextToken()
      elif self.curr_character == '!':
        token, error = self.make_not_equals()
        if error: return [], error
        tokens.append(token)
      elif self.curr_character == '=':
        tokens.append(self.make_equals())
      elif self.curr_character == '<':
        tokens.append(self.make_less_than())
      elif self.curr_character == '>':
        tokens.append(self.make_greater_than())
      elif self.curr_character == ',':
        tokens.append(Token(TT_COMMA, starting_postition=self.pos))
        self.nextToken()
      else:
        starting_postition = self.pos.copy()
        char = self.curr_character
        self.nextToken()
        return [], IllegalChar(starting_postition, self.pos, "'" + char + "'")

    tokens.append(Token(TT_EOF, starting_postition=self.pos))
    return tokens, None

  def make_number(self):
    num_str = ''
    dot_count = 0
    starting_postition = self.pos.copy()

    while self.curr_character != None and self.curr_character in DIGITS + '.':
      if self.curr_character == '.':
        if dot_count == 1: break
        dot_count += 1
      num_str += self.curr_character
      self.nextToken()

    if dot_count == 0:
      return Token(TT_INT, int(num_str), starting_postition, self.pos)
    else:
      return Token(TT_FLOAT, float(num_str), starting_postition, self.pos)

  def make_string(self):
    string = ''
    starting_postition = self.pos.copy()
    escape_character = False
    self.nextToken()

    escape_characters = {
      'n': '\n',
      't': '\t'
    }

    while self.curr_character != None and (self.curr_character != '"' or escape_character):
      if escape_character:
        string += escape_characters.get(self.curr_character, self.curr_character)
      else:
        if self.curr_character == '\\':
          escape_character = True
        else:
          string += self.curr_character
      self.nextToken()
      escape_character = False

    self.nextToken()
    return Token(TT_STRING, string, starting_postition, self.pos)

  def make_identifier(self):
    id_str = ''
    starting_postition = self.pos.copy()

    while self.curr_character != None and self.curr_character in LETTERS_DIGITS + '_':
      id_str += self.curr_character
      self.nextToken()

    tok_type = TT_KEYWORD if id_str in KEYWORDS else TT_IDENTIFIER
    return Token(tok_type, id_str, starting_postition, self.pos)

  def make_minus_or_arrow(self):
    tok_type = TT_MINUS
    starting_postition = self.pos.copy()
    self.nextToken()

    if self.curr_character == '>':
      self.nextToken()
      tok_type = TT_ARROW

    if self.curr_character == '-':
      self.nextToken()
      tok_type = TT_PREDEC

    return Token(tok_type, starting_postition=starting_postition, ending_postition=self.pos)

  def make_xor_or_or(self):
    tok_type = TT_XOR
    starting_postition = self.pos.copy()
    self.nextToken()

    if self.curr_character == '|':
      self.nextToken()
      tok_type = TT_OR

    return Token(tok_type, starting_postition=starting_postition, ending_postition=self.pos)

  def make_band_or_and(self):
    tok_type = TT_BAND
    starting_postition = self.pos.copy()
    self.nextToken()

    if self.curr_character == '&':
      self.nextToken()
      tok_type = TT_AND
    print(tok_type)
    return Token(tok_type, starting_postition=starting_postition, ending_postition=self.pos)

  def make_plus_preinc(self):
    tok_type = TT_PLUS
    starting_postition = self.pos.copy()
    self.nextToken()

    if self.curr_character == '+':
      self.nextToken()
      tok_type = TT_PREINC

    return Token(tok_type, starting_postition=starting_postition, ending_postition=self.pos)

  def make_not_equals(self):
    starting_postition = self.pos.copy()
    self.nextToken()

    if self.curr_character == '=':
      self.nextToken()
      return Token(TT_NE, starting_postition=starting_postition, ending_postition=self.pos), None

    self.nextToken()
    return None, ExpectedChar(starting_postition, self.pos, "'=' (after '!')")

  def make_equals(self):
    tok_type = TT_EQ
    starting_postition = self.pos.copy()
    self.nextToken()

    if self.curr_character == '=':
      self.nextToken()
      tok_type = TT_EE

    return Token(tok_type, starting_postition=starting_postition, ending_postition=self.pos)

  def make_less_than(self):
    tok_type = TT_LT
    starting_postition = self.pos.copy()
    self.nextToken()

    if self.curr_character == '=':
      self.nextToken()
      tok_type = TT_LTE

    return Token(tok_type, starting_postition=starting_postition, ending_postition=self.pos)

  def make_greater_than(self):
    tok_type = TT_GT
    starting_postition = self.pos.copy()
    self.nextToken()

    if self.curr_character == '=':
      self.nextToken()
      tok_type = TT_GTE

    return Token(tok_type, starting_postition=starting_postition, ending_postition=self.pos)

  def skip_comment(self):
    self.nextToken()

    while self.curr_character != '\n':
      self.nextToken()

    self.nextToken()


####################################### NODES #######################################
class NumberLeaf:
  def __init__(self, tok):
    self.tok = tok

    self.starting_postition = self.tok.starting_postition
    self.ending_postition = self.tok.ending_postition

  def __repr__(self):
    return f'{self.tok}'

class StringLeaf:
  def __init__(self, tok):
    self.tok = tok

    self.starting_postition = self.tok.starting_postition
    self.ending_postition = self.tok.ending_postition

  def __repr__(self):
    return f'{self.tok}'

class ListLeaf:
  def __init__(self, element_leafs, starting_postition, ending_postition):
    self.element_leafs = element_leafs

    self.starting_postition = starting_postition
    self.ending_postition = ending_postition

class VarAccessLeaf:
  def __init__(self, variable_token_name):
    self.variable_token_name = variable_token_name

    self.starting_postition = self.variable_token_name.starting_postition
    self.ending_postition = self.variable_token_name.ending_postition

class VarAssignLeaf:
  def __init__(self, variable_token_name, value_leaf):
    self.variable_token_name = variable_token_name
    self.value_leaf = value_leaf

    self.starting_postition = self.variable_token_name.starting_postition
    self.ending_postition = self.value_leaf.ending_postition

class BinOpLeaf:
  def __init__(self, left_leaf, operator_token, right_leaf):
    self.left_leaf = left_leaf
    self.operator_token = operator_token
    self.right_leaf = right_leaf

    self.starting_postition = self.left_leaf.starting_postition
    self.ending_postition = self.right_leaf.ending_postition

  def __repr__(self):
    return f'({self.left_leaf}, {self.operator_token}, {self.right_leaf})'

class UnaryOpLeaf:
  def __init__(self, operator_token, leaf):
    self.operator_token = operator_token
    self.leaf = leaf

    self.starting_postition = self.operator_token.starting_postition
    self.ending_postition = leaf.ending_postition

  def __repr__(self):
    return f'({self.operator_token}, {self.leaf})'

class IfLeaf:
  def __init__(self, cases, else_case):
    self.cases = cases
    self.else_case = else_case

    self.starting_postition = self.cases[0][0].starting_postition
    self.ending_postition = (self.else_case or self.cases[len(self.cases) - 1])[0].ending_postition

class ForLeaf:
  def __init__(self, variable_token_name, starting_value_leaf, ending_value_leaf, increment_value_leaf, body_leaf, return_null):
    self.variable_token_name = variable_token_name
    self.starting_value_leaf = starting_value_leaf
    self.ending_value_leaf = ending_value_leaf
    self.increment_value_leaf = increment_value_leaf
    self.body_leaf = body_leaf
    self.return_null = return_null

    self.starting_postition = self.variable_token_name.starting_postition
    self.ending_postition = self.body_leaf.ending_postition

class WhileLeaf:
  def __init__(self, conditional_leaf, body_leaf, return_null):
    self.conditional_leaf = conditional_leaf
    self.body_leaf = body_leaf
    self.return_null = return_null

    self.starting_postition = self.conditional_leaf.starting_postition
    self.ending_postition = self.body_leaf.ending_postition

class FuncDefLeaf:
  def __init__(self, variable_token_name, argument_name_toks, body_leaf, should_auto_return):
    self.variable_token_name = variable_token_name
    self.argument_name_toks = argument_name_toks
    self.body_leaf = body_leaf
    self.should_auto_return = should_auto_return

    if self.variable_token_name:
      self.starting_postition = self.variable_token_name.starting_postition
    elif len(self.argument_name_toks) > 0:
      self.starting_postition = self.argument_name_toks[0].starting_postition
    else:
      self.starting_postition = self.body_leaf.starting_postition

    self.ending_postition = self.body_leaf.ending_postition

class CallLeaf:
  def __init__(self, leaf_to_call, argument_leafs):
    self.leaf_to_call = leaf_to_call
    self.argument_leafs = argument_leafs

    self.starting_postition = self.leaf_to_call.starting_postition

    if len(self.argument_leafs) > 0:
      self.ending_postition = self.argument_leafs[len(self.argument_leafs) - 1].ending_postition
    else:
      self.ending_postition = self.leaf_to_call.ending_postition

class ReturnLeaf:
  def __init__(self, leaf_to_return, starting_postition, ending_postition):
    self.leaf_to_return = leaf_to_return

    self.starting_postition = starting_postition
    self.ending_postition = ending_postition

class ContinueLeaf:
  def __init__(self, starting_postition, ending_postition):
    self.starting_postition = starting_postition
    self.ending_postition = ending_postition

class BreakLeaf:
  def __init__(self, starting_postition, ending_postition):
    self.starting_postition = starting_postition
    self.ending_postition = ending_postition

# PARSE RESULT
class ParseResult:
  def __init__(self):
    self.error = None
    self.leaf = None
    self.last_registered_nextToken_count = 0
    self.nextToken_count = 0
    self.to_reverse_count = 0

  def register_nextTokenment(self):
    self.last_registered_nextToken_count = 1
    self.nextToken_count += 1

  def register(self, response):
    self.last_registered_nextToken_count = response.nextToken_count
    self.nextToken_count += response.nextToken_count
    if response.error: self.error = response.error
    return response.leaf

  def try_register(self, response):
    if response.error:
      self.to_reverse_count = response.nextToken_count
      return None
    return self.register(response)

  def success(self, leaf):
    self.leaf = leaf
    return self

  def failure(self, error):
    if not self.error or self.last_registered_nextToken_count == 0:
      self.error = error
    return self

####################################### PARSER #######################################
class Parser:
  def __init__(self, tokens):
    self.tokens = tokens
    self.token_index = -1
    self.nextToken()

  def nextToken(self):
    self.token_index += 1
    self.update_current_token()
    return self.current_token

  def reverse(self, amount=1):
    self.token_index -= amount
    self.update_current_token()
    return self.current_token

  def update_current_token(self):
    if self.token_index >= 0 and self.token_index < len(self.tokens):
      self.current_token = self.tokens[self.token_index]

  def parse(self):
    response= self.statements()
    if not response.error and self.current_token.type != TT_EOF:
      return response.failure(InvalidSyntaxError(
        self.current_token.starting_postition, self.current_token.ending_postition,
        "Token cannot appear after previous tokens"
      ))
    return response

  ###################################

  def statements(self):
    response= ParseResult()
    statements = []
    starting_postition = self.current_token.starting_postition.copy()

    while self.current_token.type == TT_NEWLINE:
      response.register_nextTokenment()
      self.nextToken()

    statement = response.register(self.statement())
    if response.error: return response
    statements.append(statement)

    more_statements = True

    while True:
      newline_count = 0
      while self.current_token.type == TT_NEWLINE:
        response.register_nextTokenment()
        self.nextToken()
        newline_count += 1
      if newline_count == 0:
        more_statements = False

      if not more_statements: break
      statement = response.try_register(self.statement())
      if not statement:
        self.reverse(response.to_reverse_count)
        more_statements = False
        continue
      statements.append(statement)

    return response.success(ListLeaf(
      statements,
      starting_postition,
      self.current_token.ending_postition.copy()
    ))

  def statement(self):
    response= ParseResult()
    starting_postition = self.current_token.starting_postition.copy()

    if self.current_token.matches(TT_KEYWORD, 'RETURN'):
      response.register_nextTokenment()
      self.nextToken()

      expr = response.try_register(self.expr())
      if not expr:
        self.reverse(response.to_reverse_count)
      return response.success(ReturnLeaf(expr, starting_postition, self.current_token.starting_postition.copy()))

    if self.current_token.matches(TT_KEYWORD, 'CONTINUE'):
      response.register_nextTokenment()
      self.nextToken()
      return response.success(ContinueLeaf(starting_postition, self.current_token.starting_postition.copy()))

    if self.current_token.matches(TT_KEYWORD, 'BREAK'):
      response.register_nextTokenment()
      self.nextToken()
      return response.success(BreakLeaf(starting_postition, self.current_token.starting_postition.copy()))

    expr = response.register(self.expr())
    if response.error:
      return response.failure(InvalidSyntaxError(
        self.current_token.starting_postition, self.current_token.ending_postition,
        "Expected 'RETURN', 'CONTINUE', 'BREAK', 'VAR', 'IF', 'FOR', 'WHILE', 'FUN', int, float, identifier, '+', '-', '(', '[' or 'NOT'"
      ))
    return response.success(expr)

  def expr(self):
    response= ParseResult()

    if self.current_token.matches(TT_KEYWORD, 'VAR'):
      response.register_nextTokenment()
      self.nextToken()

      if self.current_token.type != TT_IDENTIFIER:
        return response.failure(InvalidSyntaxError(
          self.current_token.starting_postition, self.current_token.ending_postition,
          "Expected identifier"
        ))

      var_name = self.current_token
      response.register_nextTokenment()
      self.nextToken()

      if self.current_token.type != TT_EQ:
        return response.failure(InvalidSyntaxError(
          self.current_token.starting_postition, self.current_token.ending_postition,
          "Expected '='"
        ))

      response.register_nextTokenment()
      self.nextToken()
      expr = response.register(self.expr())
      if response.error: return response
      return response.success(VarAssignLeaf(var_name, expr))

    leaf = response.register(self.binary_operation(self.comp_expr, ((TT_KEYWORD, 'AND'), (TT_KEYWORD, 'OR'))))

    if response.error:
      return response.failure(InvalidSyntaxError(
        self.current_token.starting_postition, self.current_token.ending_postition,
        "Expected 'VAR', 'IF', 'FOR', 'WHILE', 'FUN', int, float, identifier, '+', '-', '(', '[' or 'NOT'"
      ))

    return response.success(leaf)

  def comp_expr(self):
    response= ParseResult()

    if self.current_token.matches(TT_KEYWORD, 'NOT'):
      operator_token = self.current_token
      response.register_nextTokenment()
      self.nextToken()

      leaf = response.register(self.comp_expr())
      if response.error: return response
      return response.success(UnaryOpLeaf(operator_token, leaf))

    leaf = response.register(self.binary_operation(self.arith_expr, (TT_EE, TT_NE, TT_LT, TT_GT, TT_LTE, TT_GTE)))

    if response.error:
      return response.failure(InvalidSyntaxError(
        self.current_token.starting_postition, self.current_token.ending_postition,
        "(comp_expr) Expected int, float, identifier, '+', '-', '(', '[', 'IF', 'FOR', 'WHILE', 'FUN' or 'NOT'"
      ))

    return response.success(leaf)

  def arith_expr(self):
    return self.binary_operation(self.term, (TT_PLUS, TT_MINUS, TT_PREINC, TT_PREDEC, TT_POSTINC, TT_POSTDEC, TT_AND, TT_OR, TT_BAND, TT_XOR ))

  def term(self):
    return self.binary_operation(self.factor, (TT_MUL, TT_DIV, TT_MINUS, TT_PREINC, TT_PREDEC, TT_POSTINC, TT_POSTDEC, TT_AND, TT_OR, TT_BAND, TT_XOR))

  def factor(self):
    response= ParseResult()
    tok = self.current_token

    if tok.type in (TT_PLUS, TT_MINUS, TT_MINUS, TT_PREINC, TT_PREDEC, TT_POSTINC, TT_POSTDEC, TT_AND, TT_OR, TT_BAND, TT_XOR):
      response.register_nextTokenment()
      self.nextToken()
      factor = response.register(self.factor())
      if response.error: return response
      return response.success(UnaryOpLeaf(tok, factor))

    return self.power()

  def power(self):
    return self.binary_operation(self.call, (TT_POW, ), self.factor)

  def call(self):
    response= ParseResult()
    xenon = response.register(self.xenon())
    if response.error: return response

    if self.current_token.type == TT_LPAREN:
      response.register_nextTokenment()
      self.nextToken()
      argument_leafs = []

      if self.current_token.type == TT_RPAREN:
        response.register_nextTokenment()
        self.nextToken()
      else:
        argument_leafs.append(response.register(self.expr()))
        if response.error:
          return response.failure(InvalidSyntaxError(
            self.current_token.starting_postition, self.current_token.ending_postition,
            "Expected ')', 'VAR', 'IF', 'FOR', 'WHILE', 'FUN', int, float, identifier, '+', '-', '(', '[' or 'NOT'"
          ))

        while self.current_token.type == TT_COMMA:
          response.register_nextTokenment()
          self.nextToken()

          argument_leafs.append(response.register(self.expr()))
          if response.error: return response

        if self.current_token.type != TT_RPAREN:
          return response.failure(InvalidSyntaxError(
            self.current_token.starting_postition, self.current_token.ending_postition,
            f"Expected ',' or ')'"
          ))

        response.register_nextTokenment()
        self.nextToken()
      return response.success(CallLeaf(xenon, argument_leafs))
    return response.success(xenon)

  def xenon(self):
    response= ParseResult()
    tok = self.current_token

    if tok.type in (TT_INT, TT_FLOAT):
      response.register_nextTokenment()
      self.nextToken()
      return response.success(NumberLeaf(tok))

    elif tok.type == TT_PREINC:
      response.register_nextTokenment()
      self.nextToken()
      factor = response.register(self.factor())
      return response.success(UnaryOpLeaf(tok, factor))

    elif tok.type == TT_STRING:
      response.register_nextTokenment()
      self.nextToken()
      return response.success(StringLeaf(tok))

    elif tok.type == TT_IDENTIFIER:
      response.register_nextTokenment()
      self.nextToken()
      return response.success(VarAccessLeaf(tok))

    elif tok.type == TT_LPAREN:
      response.register_nextTokenment()
      self.nextToken()
      expr = response.register(self.expr())
      if response.error: return response
      if self.current_token.type == TT_RPAREN:
        response.register_nextTokenment()
        self.nextToken()
        return response.success(expr)
      else:
        return response.failure(InvalidSyntaxError(
          self.current_token.starting_postition, self.current_token.ending_postition,
          "Expected ')'"
        ))

    elif tok.type == TT_LSQUARE:
      list_expr = response.register(self.list_expr())
      if response.error: return response
      return response.success(list_expr)

    elif tok.matches(TT_KEYWORD, 'IF'):
      if_expr = response.register(self.if_expr())
      if response.error: return response
      return response.success(if_expr)

    elif tok.matches(TT_KEYWORD, 'FOR'):
      for_expr = response.register(self.for_expr())
      if response.error: return response
      return response.success(for_expr)

    elif tok.matches(TT_KEYWORD, 'WHILE'):
      while_expr = response.register(self.while_expr())
      if response.error: return response
      return response.success(while_expr)

    elif tok.matches(TT_KEYWORD, 'FUN'):
      func_def = response.register(self.func_def())
      if response.error: return response
      return response.success(func_def)

    return response.failure(InvalidSyntaxError(
      tok.starting_postition, tok.ending_postition,
      "(xenon) Expected int, float, identifier, '+', '-', '(', '[', IF', 'FOR', 'WHILE', 'FUN'"
    ))

  def list_expr(self):
    response= ParseResult()
    element_leafs = []
    starting_postition = self.current_token.starting_postition.copy()

    if self.current_token.type != TT_LSQUARE:
      return response.failure(InvalidSyntaxError(
        self.current_token.starting_postition, self.current_token.ending_postition,
        f"Expected '['"
      ))

    response.register_nextTokenment()
    self.nextToken()

    if self.current_token.type == TT_RSQUARE:
      response.register_nextTokenment()
      self.nextToken()
    else:
      element_leafs.append(response.register(self.expr()))
      if response.error:
        return response.failure(InvalidSyntaxError(
          self.current_token.starting_postition, self.current_token.ending_postition,
          "Expected ']', 'VAR', 'IF', 'FOR', 'WHILE', 'FUN', int, float, identifier, '+', '-', '(', '[' or 'NOT'"
        ))

      while self.current_token.type == TT_COMMA:
        response.register_nextTokenment()
        self.nextToken()

        element_leafs.append(response.register(self.expr()))
        if response.error: return response

      if self.current_token.type != TT_RSQUARE:
        return response.failure(InvalidSyntaxError(
          self.current_token.starting_postition, self.current_token.ending_postition,
          f"Expected ',' or ']'"
        ))

      response.register_nextTokenment()
      self.nextToken()

    return response.success(ListLeaf(
      element_leafs,
      starting_postition,
      self.current_token.ending_postition.copy()
    ))

  def if_expr(self):
    response= ParseResult()
    all_cases = response.register(self.if_expr_cases('IF'))
    if response.error: return response
    cases, else_case = all_cases
    return response.success(IfLeaf(cases, else_case))

  def if_expr_b(self):
    return self.if_expr_cases('ELIF')

  def if_expr_c(self):
    response= ParseResult()
    else_case = None

    if self.current_token.matches(TT_KEYWORD, 'ELSE'):
      response.register_nextTokenment()
      self.nextToken()

      if self.current_token.type == TT_NEWLINE:
        response.register_nextTokenment()
        self.nextToken()

        statements = response.register(self.statements())
        if response.error: return response
        else_case = (statements, True)

        if self.current_token.matches(TT_KEYWORD, 'END'):
          response.register_nextTokenment()
          self.nextToken()
        else:
          return response.failure(InvalidSyntaxError(
            self.current_token.starting_postition, self.current_token.ending_postition,
            "Expected 'END'"
          ))
      else:
        expr = response.register(self.statement())
        if response.error: return response
        else_case = (expr, False)

    return response.success(else_case)

  def if_expr_b_or_c(self):
    response= ParseResult()
    cases, else_case = [], None

    if self.current_token.matches(TT_KEYWORD, 'ELIF'):
      all_cases = response.register(self.if_expr_b())
      if response.error: return response
      cases, else_case = all_cases
    else:
      else_case = response.register(self.if_expr_c())
      if response.error: return response

    return response.success((cases, else_case))

  def if_expr_cases(self, case_keyword):
    response= ParseResult()
    cases = []
    else_case = None

    if not self.current_token.matches(TT_KEYWORD, case_keyword):
      return response.failure(InvalidSyntaxError(
        self.current_token.starting_postition, self.current_token.ending_postition,
        f"Expected '{case_keyword}'"
      ))

    response.register_nextTokenment()
    self.nextToken()

    condition = response.register(self.expr())
    if response.error: return response

    if not self.current_token.matches(TT_KEYWORD, 'THEN'):
      return response.failure(InvalidSyntaxError(
        self.current_token.starting_postition, self.current_token.ending_postition,
        f"Expected 'THEN'"
      ))

    response.register_nextTokenment()
    self.nextToken()

    if self.current_token.type == TT_NEWLINE:
      response.register_nextTokenment()
      self.nextToken()

      statements = response.register(self.statements())
      if response.error: return response
      cases.append((condition, statements, True))

      if self.current_token.matches(TT_KEYWORD, 'END'):
        response.register_nextTokenment()
        self.nextToken()
      else:
        all_cases = response.register(self.if_expr_b_or_c())
        if response.error: return response
        new_cases, else_case = all_cases
        cases.extend(new_cases)
    else:
      expr = response.register(self.statement())
      if response.error: return response
      cases.append((condition, expr, False))

      all_cases = response.register(self.if_expr_b_or_c())
      if response.error: return response
      new_cases, else_case = all_cases
      cases.extend(new_cases)

    return response.success((cases, else_case))

  def for_expr(self):
    response= ParseResult()

    if not self.current_token.matches(TT_KEYWORD, 'FOR'):
      return response.failure(InvalidSyntaxError(
        self.current_token.starting_postition, self.current_token.ending_postition,
        f"Expected 'FOR'"
      ))

    response.register_nextTokenment()
    self.nextToken()

    if self.current_token.type != TT_IDENTIFIER:
      return response.failure(InvalidSyntaxError(
        self.current_token.starting_postition, self.current_token.ending_postition,
        f"Expected identifier"
      ))

    var_name = self.current_token
    response.register_nextTokenment()
    self.nextToken()

    if self.current_token.type != TT_EQ:
      return response.failure(InvalidSyntaxError(
        self.current_token.starting_postition, self.current_token.ending_postition,
        f"Expected '='"
      ))

    response.register_nextTokenment()
    self.nextToken()

    start_value = response.register(self.expr())
    if response.error: return response

    if not self.current_token.matches(TT_KEYWORD, 'TO'):
      return response.failure(InvalidSyntaxError(
        self.current_token.starting_postition, self.current_token.ending_postition,
        f"Expected 'TO'"
      ))

    response.register_nextTokenment()
    self.nextToken()

    end_value = response.register(self.expr())
    if response.error: return response

    if self.current_token.matches(TT_KEYWORD, 'STEP'):
      response.register_nextTokenment()
      self.nextToken()

      step_value = response.register(self.expr())
      if response.error: return response
    else:
      step_value = None

    if not self.current_token.matches(TT_KEYWORD, 'THEN'):
      return response.failure(InvalidSyntaxError(
        self.current_token.starting_postition, self.current_token.ending_postition,
        f"Expected 'THEN'"
      ))

    response.register_nextTokenment()
    self.nextToken()

    if self.current_token.type == TT_NEWLINE:
      response.register_nextTokenment()
      self.nextToken()

      body = response.register(self.statements())
      if response.error: return response

      if not self.current_token.matches(TT_KEYWORD, 'END'):
        return response.failure(InvalidSyntaxError(
          self.current_token.starting_postition, self.current_token.ending_postition,
          f"Expected 'END'"
        ))

      response.register_nextTokenment()
      self.nextToken()

      return response.success(ForLeaf(var_name, start_value, end_value, step_value, body, True))

    body = response.register(self.statement())
    if response.error: return response

    return response.success(ForLeaf(var_name, start_value, end_value, step_value, body, False))

  def while_expr(self):
    response= ParseResult()

    if not self.current_token.matches(TT_KEYWORD, 'WHILE'):
      return response.failure(InvalidSyntaxError(
        self.current_token.starting_postition, self.current_token.ending_postition,
        f"Expected 'WHILE'"
      ))

    response.register_nextTokenment()
    self.nextToken()

    condition = response.register(self.expr())
    if response.error: return response

    if not self.current_token.matches(TT_KEYWORD, 'THEN'):
      return response.failure(InvalidSyntaxError(
        self.current_token.starting_postition, self.current_token.ending_postition,
        f"Expected 'THEN'"
      ))

    response.register_nextTokenment()
    self.nextToken()

    if self.current_token.type == TT_NEWLINE:
      response.register_nextTokenment()
      self.nextToken()

      body = response.register(self.statements())
      if response.error: return response

      if not self.current_token.matches(TT_KEYWORD, 'END'):
        return response.failure(InvalidSyntaxError(
          self.current_token.starting_postition, self.current_token.ending_postition,
          f"Expected 'END'"
        ))

      response.register_nextTokenment()
      self.nextToken()

      return response.success(WhileLeaf(condition, body, True))

    body = response.register(self.statement())
    if response.error: return response

    return response.success(WhileLeaf(condition, body, False))

  def func_def(self):
    response= ParseResult()

    if not self.current_token.matches(TT_KEYWORD, 'FUN'):
      return response.failure(InvalidSyntaxError(
        self.current_token.starting_postition, self.current_token.ending_postition,
        f"Expected 'FUN'"
      ))

    response.register_nextTokenment()
    self.nextToken()

    if self.current_token.type == TT_IDENTIFIER:
      variable_token_name = self.current_token
      response.register_nextTokenment()
      self.nextToken()
      if self.current_token.type != TT_LPAREN:
        return response.failure(InvalidSyntaxError(
          self.current_token.starting_postition, self.current_token.ending_postition,
          f"Expected '('"
        ))
    else:
      variable_token_name = None
      if self.current_token.type != TT_LPAREN:
        return response.failure(InvalidSyntaxError(
          self.current_token.starting_postition, self.current_token.ending_postition,
          f"Expected identifier or '('"
        ))

    response.register_nextTokenment()
    self.nextToken()
    argument_name_toks = []

    if self.current_token.type == TT_IDENTIFIER:
      argument_name_toks.append(self.current_token)
      response.register_nextTokenment()
      self.nextToken()

      while self.current_token.type == TT_COMMA:
        response.register_nextTokenment()
        self.nextToken()

        if self.current_token.type != TT_IDENTIFIER:
          return response.failure(InvalidSyntaxError(
            self.current_token.starting_postition, self.current_token.ending_postition,
            f"Expected identifier"
          ))

        argument_name_toks.append(self.current_token)
        response.register_nextTokenment()
        self.nextToken()

      if self.current_token.type != TT_RPAREN:
        return response.failure(InvalidSyntaxError(
          self.current_token.starting_postition, self.current_token.ending_postition,
          f"Expected ',' or ')'"
        ))
    else:
      if self.current_token.type != TT_RPAREN:
        return response.failure(InvalidSyntaxError(
          self.current_token.starting_postition, self.current_token.ending_postition,
          f"Expected identifier or ')'"
        ))

    response.register_nextTokenment()
    self.nextToken()

    if self.current_token.type == TT_ARROW:
      response.register_nextTokenment()
      self.nextToken()

      body = response.register(self.expr())
      if response.error: return response

      return response.success(FuncDefLeaf(
        variable_token_name,
        argument_name_toks,
        body,
        True
      ))

    if self.current_token.type == TT_OR:
      response.register_nextTokenment()
      self.nextToken()

      body = response.register(self.expr())
      if response.error: return response

      return response.success(FuncDefLeaf(
        variable_token_name,
        argument_name_toks,
        body,
        True
      ))


    if self.current_token.type != TT_NEWLINE:
      return response.failure(InvalidSyntaxError(
        self.current_token.starting_postition, self.current_token.ending_postition,
        f"Expected '->' or NEWLINE"
      ))

    response.register_nextTokenment()
    self.nextToken()

    body = response.register(self.statements())
    if response.error: return response

    if not self.current_token.matches(TT_KEYWORD, 'END'):
      return response.failure(InvalidSyntaxError(
        self.current_token.starting_postition, self.current_token.ending_postition,
        f"Expected 'END'"
      ))

    response.register_nextTokenment()
    self.nextToken()

    return response.success(FuncDefLeaf(
      variable_token_name,
      argument_name_toks,
      body,
      False
    ))

  ###################################

  def binary_operation(self, func_a, ops, func_b=None):
    if func_b == None:
      func_b = func_a

    response= ParseResult()
    left = response.register(func_a())
    if response.error: return response

    while self.current_token.type in ops or (self.current_token.type, self.current_token.value) in ops:
      operator_token = self.current_token
      response.register_nextTokenment()
      self.nextToken()
      right = response.register(func_b())
      if response.error: return response
      left = BinOpLeaf(left, operator_token, right)

    return response.success(left)

####################################### RUNTIME RESULT #######################################
class RTResult:
  def __init__(self):
    self.redo()

  def redo(self):
    self.value = None
    self.error = None
    self.func_return_value = None
    self.continue_loop = False
    self.loop_should_break = False

  def register(self, response):
    self.error = response.error
    self.func_return_value = response.func_return_value
    self.continue_loop = response.continue_loop
    self.loop_should_break = response.loop_should_break
    return response.value

  def success(self, value):
    self.redo()
    self.value = value
    return self

  def success_return(self, value):
    self.redo()
    self.func_return_value = value
    return self

  def success_continue(self):
    self.redo()
    self.continue_loop = True
    return self

  def success_break(self):
    self.redo()
    self.loop_should_break = True
    return self

  def failure(self, error):
    self.redo()
    self.error = error
    return self

  def should_return(self):
    # Note: this will allow you to continue and break outside the current function
    return (
      self.error or
      self.func_return_value or
      self.continue_loop or
      self.loop_should_break
    )


####################################### VALUES #######################################
class Value:
  def __init__(self):
    self.set_pos()
    self.set_value()

  def set_pos(self, starting_postition=None, ending_postition=None):
    self.starting_postition = starting_postition
    self.ending_postition = ending_postition
    return self

  def set_value(self, context=None):
    self.context = context
    return self

  def number_added_to(self, rightSide):
    return None, self.operation_error(rightSide)

  def list_subbed_by(self, rightSide):
    return None, self.operation_error(rightSide)

  def multed_by(self, rightSide):
    return None, self.operation_error(rightSide)

  def mod_by(self, rightSide):
    return None, self.operation_error(rightSide)

  def xor_with(self, rightSide):
    return None, self.operation_error(rightSide)

  def and_with(self, rightSide):
    return None, self.operation_error(rightSide)

  def band_with(self, rightSide):
    return None, self.operation_error(rightSide)

  def dived_by(self, rightSide):
    return None, self.operation_error(rightSide)

  def powed_by(self, rightSide):
    return None, self.operation_error(rightSide)

  def comparison_eq(self, rightSide):
    return None, self.operation_error(rightSide)

  def comparison_ne(self, rightSide):
    return None, self.operation_error(rightSide)

  def comparison_lt(self, rightSide):
    return None, self.operation_error(rightSide)

  def comparison_gt(self, rightSide):
    return None, self.operation_error(rightSide)

  def comparison_lte(self, rightSide):
    return None, self.operation_error(rightSide)

  def comparison_gte(self, rightSide):
    return None, self.operation_error(rightSide)

  def anded_by(self, rightSide):
    return None, self.operation_error(rightSide)

  def ored_by(self, rightSide):
    return None, self.operation_error(rightSide)

  def notted(self, rightSide):
    return None, self.operation_error(rightSide)

  def execute(self, args):
    return RTResult().failure(self.operation_error())

  def copy(self):
    raise Exception('Copy method defined')

  def is_true(self):
    return False

  def operation_error(self, rightSide=None):
    if not rightSide: rightSide = self
    return RTError(
      self.starting_postition, rightSide.ending_postition,
      'Illegal operation',
      self.context
    )

class Number(Value):
  def __init__(self, value):
    super().__init__()
    self.value = value

  def number_added_to(self, rightSide):
    if isinstance(rightSide, Number):
      return Number(self.value + rightSide.value).set_value(self.context), None
    else:
      return None, Value.operation_error(self, rightSide)

  def list_subbed_by(self, rightSide):
    if isinstance(rightSide, Number):
      return Number(self.value - rightSide.value).set_value(self.context), None
    else:
      return None, Value.operation_error(self, rightSide)

  def multed_by(self, rightSide):
    if isinstance(rightSide, Number):
      return Number(self.value * rightSide.value).set_value(self.context), None
    else:
      return None, Value.operation_error(self, rightSide)

  def mod_by(self, rightSide):
    if isinstance(rightSide, Number):
      return Number(self.value % rightSide.value).set_value(self.context), None
    else:
      return None, Value.operation_error(self, rightSide)

  def xor_with(self, rightSide):
    if isinstance(rightSide, Number):
      return Number(self.value != rightSide.value).set_value(self.context), None
    else:
      return None, Value.operation_error(self, rightSide)

  def and_with(self, rightSide):
    if isinstance(rightSide, Number):
      return Number(self.value and rightSide.value).set_value(self.context), None
    else:
      return None, Value.operation_error(self, rightSide)

  def band_with(self, rightSide):
    if isinstance(rightSide, Number):
      return Number(self.value & rightSide.value).set_value(self.context), None
    else:
      return None, Value.operation_error(self, rightSide)

  def dived_by(self, rightSide):
    if isinstance(rightSide, Number):
      if rightSide.value == 0:
        return None, RTError(
          rightSide.starting_postition, rightSide.ending_postition,
          'Division by zero',
          self.context
        )

      return Number(self.value / rightSide.value).set_value(self.context), None
    else:
      return None, Value.operation_error(self, rightSide)

  def powed_by(self, rightSide):
    if isinstance(rightSide, Number):
      return Number(self.value ** rightSide.value).set_value(self.context), None
    else:
      return None, Value.operation_error(self, rightSide)

  def comparison_eq(self, rightSide):
    if isinstance(rightSide, Number):
      return Number(int(self.value == rightSide.value)).set_value(self.context), None
    else:
      return None, Value.operation_error(self, rightSide)

  def comparison_ne(self, rightSide):
    if isinstance(rightSide, Number):
      return Number(int(self.value != rightSide.value)).set_value(self.context), None
    else:
      return None, Value.operation_error(self, rightSide)

  def comparison_lt(self, rightSide):
    if isinstance(rightSide, Number):
      return Number(int(self.value < rightSide.value)).set_value(self.context), None
    else:
      return None, Value.operation_error(self, rightSide)

  def comparison_gt(self, rightSide):
    if isinstance(rightSide, Number):
      return Number(int(self.value > rightSide.value)).set_value(self.context), None
    else:
      return None, Value.operation_error(self, rightSide)

  def comparison_lte(self, rightSide):
    if isinstance(rightSide, Number):
      return Number(int(self.value <= rightSide.value)).set_value(self.context), None
    else:
      return None, Value.operation_error(self, rightSide)

  def comparison_gte(self, rightSide):
    if isinstance(rightSide, Number):
      return Number(int(self.value >= rightSide.value)).set_value(self.context), None
    else:
      return None, Value.operation_error(self, rightSide)

  def anded_by(self, rightSide):
    if isinstance(rightSide, Number):
      return Number(int(self.value and rightSide.value)).set_value(self.context), None
    else:
      return None, Value.operation_error(self, rightSide)

  def ored_by(self, rightSide):
    if isinstance(rightSide, Number):
      return Number(int(self.value or rightSide.value)).set_value(self.context), None
    else:
      return None, Value.operation_error(self, rightSide)

  def notted(self):
    return Number(1 if self.value == 0 else 0).set_value(self.context), None

  def copy(self):
    copy = Number(self.value)
    copy.set_pos(self.starting_postition, self.ending_postition)
    copy.set_value(self.context)
    return copy

  def is_true(self):
    return self.value != 0

  def __str__(self):
    return str(self.value)

  def __repr__(self):
    return str(self.value)

Number.null = Number(0)
Number.false = Number(0)
Number.true = Number(1)
Number.math_PI = Number(math.pi)
Number.factorial = Number(math.factorial)

class String(Value):
  def __init__(self, value):
    super().__init__()
    self.value = value

  def number_added_to(self, rightSide):
    if isinstance(rightSide, String):
      return String(self.value + rightSide.value).set_value(self.context), None
    else:
      return None, Value.operation_error(self, rightSide)

  def multed_by(self, rightSide):
    if isinstance(rightSide, Number):
      return String(self.value * rightSide.value).set_value(self.context), None
    else:
      return None, Value.operation_error(self, rightSide)

  def is_true(self):
    return len(self.value) > 0

  def copy(self):
    copy = String(self.value)
    copy.set_pos(self.starting_postition, self.ending_postition)
    copy.set_value(self.context)
    return copy

  def __str__(self):
    return self.value

  def __repr__(self):
    return f'"{self.value}"'

class List(Value):
  def __init__(self, elements):
    super().__init__()
    self.elements = elements

  def number_added_to(self, rightSide):
    new_list = self.copy()
    new_list.elements.append(rightSide)
    return new_list, None

  def list_subbed_by(self, rightSide):
    if isinstance(rightSide, Number):
      new_list = self.copy()
      try:
        new_list.elements.pop(rightSide.value)
        return new_list, None
      except:
        return None, RTError(
          rightSide.starting_postition, rightSide.ending_postition,
          'Element at this index could not be removed from list because index is out of bounds',
          self.context
        )
    else:
      return None, Value.operation_error(self, rightSide)

  def multed_by(self, rightSide):
    if isinstance(rightSide, List):
      new_list = self.copy()
      new_list.elements.extend(rightSide.elements)
      return new_list, None
    else:
      return None, Value.operation_error(self, rightSide)

  def dived_by(self, rightSide):
    if isinstance(rightSide, Number):
      try:
        return self.elements[rightSide.value], None
      except:
        return None, RTError(
          rightSide.starting_postition, rightSide.ending_postition,
          'Index is out of bounds',
          self.context
        )
    else:
      return None, Value.operation_error(self, rightSide)

  def copy(self):
    copy = List(self.elements)
    copy.set_pos(self.starting_postition, self.ending_postition)
    copy.set_value(self.context)
    return copy

  def __str__(self):
    return ", ".join([str(x) for x in self.elements])

  def __repr__(self):
    return f'[{", ".join([repr(x) for x in self.elements])}]'

class BaseFunction(Value):
  def __init__(self, name):
    super().__init__()
    self.name = name or "<anonymous>"

  def generate_new_context(self):
    new_context = Context(self.name, self.context, self.starting_postition)
    new_context.symbol_table = SymbolTable(new_context.parent.symbol_table)
    return new_context

  def check_args(self, argument_names, args):
    response= RTResult()

    if len(args) > len(argument_names):
      return response.failure(RTError(
        self.starting_postition, self.ending_postition,
        f"{len(args) - len(argument_names)} too many args in {self}",
        self.context
      ))

    if len(args) < len(argument_names):
      return response.failure(RTError(
        self.starting_postition, self.ending_postition,
        f"{len(argument_names) - len(args)} too few args in {self}",
        self.context
      ))

    return response.success(None)

  def populate_args(self, argument_names, args, exec_ctx):
    for i in range(len(args)):
      argument_name = argument_names[i]
      argument_value = args[i]
      argument_value.set_value(exec_ctx)
      exec_ctx.symbol_table.set(argument_name, argument_value)

  def check_and_populate_args(self, argument_names, args, exec_ctx):
    response= RTResult()
    response.register(self.check_args(argument_names, args))
    if response.should_return(): return response
    self.populate_args(argument_names, args, exec_ctx)
    return response.success(None)

class Function(BaseFunction):
  def __init__(self, name, body_leaf, argument_names, should_auto_return):
    super().__init__(name)
    self.body_leaf = body_leaf
    self.argument_names = argument_names
    self.should_auto_return = should_auto_return

  def execute(self, args):
    response= RTResult()
    interpreter = Interpreter()
    exec_ctx = self.generate_new_context()

    response.register(self.check_and_populate_args(self.argument_names, args, exec_ctx))
    if response.should_return(): return response

    value = response.register(interpreter.visit(self.body_leaf, exec_ctx))
    if response.should_return() and response.func_return_value == None: return response

    ret_value = (value if self.should_auto_return else None) or response.func_return_value or Number.null
    return response.success(ret_value)

  def copy(self):
    copy = Function(self.name, self.body_leaf, self.argument_names, self.should_auto_return)
    copy.set_value(self.context)
    copy.set_pos(self.starting_postition, self.ending_postition)
    return copy

  def __repr__(self):
    return f"<function {self.name}>"

class Common(BaseFunction):
  def __init__(self, name):
    super().__init__(name)

  def execute(self, args):
    response= RTResult()
    exec_ctx = self.generate_new_context()

    method_name = f'execute_{self.name}'
    method = getattr(self, method_name, self.no_visit_method)

    response.register(self.check_and_populate_args(method.argument_names, args, exec_ctx))
    if response.should_return(): return response

    return_value = response.register(method(exec_ctx))
    if response.should_return(): return response
    return response.success(return_value)

  def no_visit_method(self, leaf, context):
    raise Exception(f'No execute_{self.name} method defined')

  def copy(self):
    copy = Common(self.name)
    copy.set_value(self.context)
    copy.set_pos(self.starting_postition, self.ending_postition)
    return copy

  def __repr__(self):
    return f"<common function {self.name}>"

  #####################################

  def execute_print(self, exec_ctx):
    print(str(exec_ctx.symbol_table.get('value')))
    return RTResult().success(Number.null)
  execute_print.argument_names = ['value']

  def execute_print_ret(self, exec_ctx):
    return RTResult().success(String(str(exec_ctx.symbol_table.get('value'))))
  execute_print_ret.argument_names = ['value']

  def execute_input(self, exec_ctx):
    text = input()
    return RTResult().success(String(text))
  execute_input.argument_names = []

  def execute_input_int(self, exec_ctx):
    while True:
      text = input()
      try:
        number = int(text)
        break
      except ValueError:
        print(f"'{text}' must be an integer.")
    return RTResult().success(Number(number))
  execute_input_int.argument_names = []

  def execute_clear(self, exec_ctx):
    os.system('cls' if os.name == 'nt' else 'cls')
    return RTResult().success(Number.null)
  execute_clear.argument_names = []

  def execute_is_number(self, exec_ctx):
    is_number = isinstance(exec_ctx.symbol_table.get("value"), Number)
    return RTResult().success(Number.true if is_number else Number.false)
  execute_is_number.argument_names = ["value"]

  def execute_is_string(self, exec_ctx):
    is_number = isinstance(exec_ctx.symbol_table.get("value"), String)
    return RTResult().success(Number.true if is_number else Number.false)
  execute_is_string.argument_names = ["value"]

  def execute_factorial(self, exec_ctx):
    value = exec_ctx.symbol_table.get("value")
    newNum = int(str(value))

    return RTResult().success(Number(math.factorial(newNum)))
  execute_factorial.argument_names = ["value"]

  def execute_potatosoup(self, exec_ctx):
    print(r"""\

                                   ._ o o
                                   \_`-)|_
                                ,""       \
                              ,"  ## |   ಠ ಠ.
                            ," ##   ,-\__    `.
                          ,"       /     `--._;)
                        ,"     ## /
                      ,"   ##    /


                """)
    return RTResult().success(Number.null)

  execute_potatosoup.argument_names = ["value"]

  def execute_ceil(self, exec_ctx):
    value = exec_ctx.symbol_table.get("value")
    newNum = int(str(value))

    return RTResult().success(Number(math.ceil(newNum)))
  execute_ceil.argument_names = ["value"]

  def execute_fabs(self, exec_ctx):
    value = exec_ctx.symbol_table.get("value")
    newNum = int(str(value))

    return RTResult().success(Number(math.fabs(newNum)))
  execute_fabs.argument_names = ["value"]

  def execute_floor(self, exec_ctx):
    value = exec_ctx.symbol_table.get("value")
    newNum = int(str(value))

    return RTResult().success(Number(math.floor(newNum)))
  execute_floor.argument_names = ["value"]

  def execute_is_finite(self, exec_ctx):
    value = exec_ctx.symbol_table.get("value")
    newNum = int(str(value))

    return RTResult().success(Number(math.isfinite(newNum)))
  execute_is_finite.argument_names = ["value"]

  def execute_sqrt(self, exec_ctx):
    value = exec_ctx.symbol_table.get("value")
    newNum = int(str(value))

    return RTResult().success(Number(math.sqrt(newNum)))
  execute_sqrt.argument_names = ["value"]

  def execute_is_list(self, exec_ctx):
    is_number = isinstance(exec_ctx.symbol_table.get("value"), List)
    return RTResult().success(Number.true if is_number else Number.false)
  execute_is_list.argument_names = ["value"]

  def execute_is_function(self, exec_ctx):
    is_number = isinstance(exec_ctx.symbol_table.get("value"), BaseFunction)
    return RTResult().success(Number.true if is_number else Number.false)
  execute_is_function.argument_names = ["value"]

  def execute_append(self, exec_ctx):
    list_ = exec_ctx.symbol_table.get("list")
    value = exec_ctx.symbol_table.get("value")

    if not isinstance(list_, List):
      return RTResult().failure(RTError(
        self.starting_postition, self.ending_postition,
        "First argument must be list",
        exec_ctx
      ))

    list_.elements.append(value)
    return RTResult().success(Number.null)
  execute_append.argument_names = ["list", "value"]

  def execute_pop(self, exec_ctx):
    list_ = exec_ctx.symbol_table.get("list")
    index = exec_ctx.symbol_table.get("index")

    if not isinstance(list_, List):
      return RTResult().failure(RTError(
        self.starting_postition, self.ending_postition,
        "First argument must be list",
        exec_ctx
      ))

    if not isinstance(index, Number):
      return RTResult().failure(RTError(
        self.starting_postition, self.ending_postition,
        "Second argument must be number",
        exec_ctx
      ))

    try:
      element = list_.elements.pop(index.value)
    except:
      return RTResult().failure(RTError(
        self.starting_postition, self.ending_postition,
        'Element at this index could not be removed from list because index is out of bounds',
        exec_ctx
      ))
    return RTResult().success(element)
  execute_pop.argument_names = ["list", "index"]

  def execute_extend(self, exec_ctx):
    listA = exec_ctx.symbol_table.get("listA")
    listB = exec_ctx.symbol_table.get("listB")

    if not isinstance(listA, List):
      return RTResult().failure(RTError(
        self.starting_postition, self.ending_postition,
        "First argument must be list",
        exec_ctx
      ))

    if not isinstance(listB, List):
      return RTResult().failure(RTError(
        self.starting_postition, self.ending_postition,
        "Second argument must be list",
        exec_ctx
      ))

    listA.elements.extend(listB.elements)
    return RTResult().success(Number.null)
  execute_extend.argument_names = ["listA", "listB"]

  def execute_len(self, exec_ctx):
    list_ = exec_ctx.symbol_table.get("list")

    if not isinstance(list_, List):
      return RTResult().failure(RTError(
        self.starting_postition, self.ending_postition,
        "Argument must be list",
        exec_ctx
      ))

    return RTResult().success(Number(len(list_.elements)))
  execute_len.argument_names = ["list"]

  def execute_run(self, exec_ctx):
    fn = exec_ctx.symbol_table.get("fn")

    if not isinstance(fn, String):
      return RTResult().failure(RTError(
        self.starting_postition, self.ending_postition,
        "Second argument must be string",
        exec_ctx
      ))

    fn = fn.value

    try:
      with open(fn, "r") as f:
        script = f.read()
    except Exception as e:
      return RTResult().failure(RTError(
        self.starting_postition, self.ending_postition,
        f"Failed to load script \"{fn}\"\n" + str(e),
        exec_ctx
      ))

    _, error = run(fn, script)

    if error:
      return RTResult().failure(RTError(
        self.starting_postition, self.ending_postition,
        f"Failed to finish executing script \"{fn}\"\n" +
        error.as_string(),
        exec_ctx
      ))

    return RTResult().success(Number.null)
  execute_run.argument_names = ["fn"]

Common.print       = Common("print")
Common.print_ret   = Common("print_ret")
Common.input       = Common("input")
Common.input_int   = Common("input_int")
Common.clear       = Common("clear")
Common.is_number   = Common("is_number")
Common.is_string   = Common("is_string")
Common.factorial   = Common("factorial")
Common.is_finite   = Common("is_finite")
Common.factorial   = Common("factorial")
Common.ceil        = Common("ceil")
Common.potatosoup  = Common("potatosoup")
Common.fabs        = Common("fabs")
Common.floor       = Common("floor")
Common.sqrt        = Common("sqrt")
Common.is_list     = Common("is_list")
Common.is_function = Common("is_function")
Common.append      = Common("append")
Common.pop         = Common("pop")
Common.extend      = Common("extend")
Common.len			   = Common("len")
Common.run				 = Common("run")

####################################### CONTEXT #######################################
class Context:
  def __init__(self, display_name, parent=None, parent_entry_pos=None):
    self.display_name = display_name
    self.parent = parent
    self.parent_entry_pos = parent_entry_pos
    self.symbol_table = None

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKCYAN = '\033[96m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

####################################### SYMBOL TABLE #######################################
class SymbolTable:
  def __init__(self, parent=None):
    self.symbols = {}
    self.parent = parent

  def get(self, name):
    value = self.symbols.get(name, None)
    if value == None and self.parent:
      return self.parent.get(name)
    return value

  def set(self, name, value):
    self.symbols[name] = value

  def remove(self, name):
    del self.symbols[name]


####################################### INTERPRETER #######################################
class Interpreter:
  def visit(self, leaf, context):
    method_name = f'visit_{type(leaf).__name__}'
    method = getattr(self, method_name, self.no_visit_method)
    return method(leaf, context)

  def no_visit_method(self, leaf, context):
    raise Exception(f'No visit_{type(leaf).__name__} method defined')

  ###################################

  def visit_NumberLeaf(self, leaf, context):
    return RTResult().success(
      Number(leaf.tok.value).set_value(context).set_pos(leaf.starting_postition, leaf.ending_postition)
    )

  def visit_StringLeaf(self, leaf, context):
    return RTResult().success(
      String(leaf.tok.value).set_value(context).set_pos(leaf.starting_postition, leaf.ending_postition)
    )

  def visit_ListLeaf(self, leaf, context):
    response= RTResult()
    elements = []

    for element_leaf in leaf.element_leafs:
      elements.append(response.register(self.visit(element_leaf, context)))
      if response.should_return(): return response

    return response.success(
      List(elements).set_value(context).set_pos(leaf.starting_postition, leaf.ending_postition)
    )

  def visit_VarAccessLeaf(self, leaf, context):
    response= RTResult()
    var_name = leaf.variable_token_name.value
    value = context.symbol_table.get(var_name)

    if not value:
      return response.failure(RTError(
        leaf.starting_postition, leaf.ending_postition,
        f"'{var_name}' is not defined",
        context
      ))

    value = value.copy().set_pos(leaf.starting_postition, leaf.ending_postition).set_value(context)
    return response.success(value)

  def visit_VarAssignLeaf(self, leaf, context):
    response= RTResult()
    var_name = leaf.variable_token_name.value
    value = response.register(self.visit(leaf.value_leaf, context))
    if response.should_return(): return response

    context.symbol_table.set(var_name, value)
    return response.success(value)

  def visit_BinOpLeaf(self, leaf, context):
    response= RTResult()
    left = response.register(self.visit(leaf.left_leaf, context))
    if response.should_return(): return response
    right = response.register(self.visit(leaf.right_leaf, context))
    if response.should_return(): return response

    if leaf.operator_token.type == TT_PLUS:
      result, error = left.number_added_to(right)
    elif leaf.operator_token.type == TT_MINUS:
      result, error = left.list_subbed_by(right)
    elif leaf.operator_token.type == TT_MUL:
      result, error = left.multed_by(right)
    elif leaf.operator_token.type == TT_MOD:
      result, error = left.mod_by(right)
    elif leaf.operator_token.type == TT_AND:
      result, error = left.and_with(right)
    elif leaf.operator_token.type == TT_OR:
      result, error = left.or_with(right)
    elif leaf.operator_token.type == TT_XOR:
      result, error = left.xor_with(right)
    elif leaf.operator_token.type == TT_BAND:
      result, error = left.band_with(right)
    elif leaf.operator_token.type == TT_DIV:
      result, error = left.dived_by(right)
    elif leaf.operator_token.type == TT_POW:
      result, error = left.powed_by(right)
    elif leaf.operator_token.type == TT_EE:
      result, error = left.comparison_eq(right)
    elif leaf.operator_token.type == TT_NE:
      result, error = left.comparison_ne(right)
    elif leaf.operator_token.type == TT_LT:
      result, error = left.comparison_lt(right)
    elif leaf.operator_token.type == TT_GT:
      result, error = left.comparison_gt(right)
    elif leaf.operator_token.type == TT_LTE:
      result, error = left.comparison_lte(right)
    elif leaf.operator_token.type == TT_GTE:
      result, error = left.comparison_gte(right)
    elif leaf.operator_token.matches(TT_KEYWORD, 'AND'):
      result, error = left.anded_by(right)
    elif leaf.operator_token.matches(TT_KEYWORD, 'OR'):
      result, error = left.ored_by(right)

    if error:
      return response.failure(error)
    else:
      return response.success(result.set_pos(leaf.starting_postition, leaf.ending_postition))

  def visit_UnaryOpLeaf(self, leaf, context):
    response= RTResult()
    number = response.register(self.visit(leaf.leaf, context))
    if response.should_return(): return response

    error = None

    if leaf.operator_token.type == TT_MINUS:
      number, error = number.multed_by(Number(-1))
    if leaf.operator_token.type == TT_PREINC:
      number, error = number.number_added_to(Number(+1))
    if leaf.operator_token.type == TT_POSTINC:
      number, error = number.number_added_to(Number(+1))
    if leaf.operator_token.type == TT_PREDEC:
      number, error = number.number_added_to(Number(-1))
    if leaf.operator_token.type == TT_POSTDEC:
      number, error = number.number_added_to(Number(-1))
    elif leaf.operator_token.matches(TT_KEYWORD, 'NOT'):
      number, error = number.notted()

    if error:
      return response.failure(error)
    else:
      return response.success(number.set_pos(leaf.starting_postition, leaf.ending_postition))

  def visit_IfLeaf(self, leaf, context):
    response= RTResult()

    for condition, expr, return_null in leaf.cases:
      condition_value = response.register(self.visit(condition, context))
      if response.should_return(): return response

      if condition_value.is_true():
        expr_value = response.register(self.visit(expr, context))
        if response.should_return(): return response
        return response.success(Number.null if return_null else expr_value)

    if leaf.else_case:
      expr, return_null = leaf.else_case
      expr_value = response.register(self.visit(expr, context))
      if response.should_return(): return response
      return response.success(Number.null if return_null else expr_value)

    return response.success(Number.null)

  def visit_ForLeaf(self, leaf, context):
    response= RTResult()
    elements = []

    start_value = response.register(self.visit(leaf.starting_value_leaf, context))
    if response.should_return(): return response

    end_value = response.register(self.visit(leaf.ending_value_leaf, context))
    if response.should_return(): return response

    if leaf.increment_value_leaf:
      step_value = response.register(self.visit(leaf.increment_value_leaf, context))
      if response.should_return(): return response
    else:
      step_value = Number(1)

    i = start_value.value

    if step_value.value >= 0:
      condition = lambda: i < end_value.value
    else:
      condition = lambda: i > end_value.value

    while condition():
      context.symbol_table.set(leaf.variable_token_name.value, Number(i))
      i += step_value.value

      value = response.register(self.visit(leaf.body_leaf, context))
      if response.should_return() and response.continue_loop == False and response.loop_should_break == False: return response

      if response.continue_loop:
        continue

      if response.loop_should_break:
        break

      elements.append(value)

    return response.success(
      Number.null if leaf.return_null else
      List(elements).set_value(context).set_pos(leaf.starting_postition, leaf.ending_postition)
    )

  def visit_WhileLeaf(self, leaf, context):
    response= RTResult()
    elements = []

    while True:
      condition = response.register(self.visit(leaf.conditional_leaf, context))
      if response.should_return(): return response

      if not condition.is_true():
        break

      value = response.register(self.visit(leaf.body_leaf, context))
      if response.should_return() and response.continue_loop == False and response.loop_should_break == False: return response

      if response.continue_loop:
        continue

      if response.loop_should_break:
        break

      elements.append(value)

    return response.success(
      Number.null if leaf.return_null else
      List(elements).set_value(context).set_pos(leaf.starting_postition, leaf.ending_postition)
    )

  def visit_FuncDefLeaf(self, leaf, context):
    response= RTResult()

    func_name = leaf.variable_token_name.value if leaf.variable_token_name else None
    body_leaf = leaf.body_leaf
    argument_names = [argument_name.value for argument_name in leaf.argument_name_toks]
    func_value = Function(func_name, body_leaf, argument_names, leaf.should_auto_return).set_value(context).set_pos(leaf.starting_postition, leaf.ending_postition)

    if leaf.variable_token_name:
      context.symbol_table.set(func_name, func_value)

    return response.success(func_value)

  def visit_CallLeaf(self, leaf, context):
    response= RTResult()
    args = []

    value_to_call = response.register(self.visit(leaf.leaf_to_call, context))
    if response.should_return(): return response
    value_to_call = value_to_call.copy().set_pos(leaf.starting_postition, leaf.ending_postition)

    for argument_leaf in leaf.argument_leafs:
      args.append(response.register(self.visit(argument_leaf, context)))
      if response.should_return(): return response

    return_value = response.register(value_to_call.execute(args))
    if response.should_return(): return response
    return_value = return_value.copy().set_pos(leaf.starting_postition, leaf.ending_postition).set_value(context)
    return response.success(return_value)

  def visit_ReturnLeaf(self, leaf, context):
    response= RTResult()

    if leaf.leaf_to_return:
      value = response.register(self.visit(leaf.leaf_to_return, context))
      if response.should_return(): return response
    else:
      value = Number.null

    return response.success_return(value)

  def visit_ContinueLeaf(self, leaf, context):
    return RTResult().success_continue()

  def visit_BreakLeaf(self, leaf, context):
    return RTResult().success_break()


####################################### RUN #######################################
CommonX = SymbolTable()
CommonX.set("NULL", Number.null)
CommonX.set("FALSE", Number.false)
CommonX.set("TRUE", Number.true)
CommonX.set("MATH_PI", Number.math_PI)
CommonX.set("FAC", Number.factorial)
CommonX.set("PRINT", Common.print)
CommonX.set("PRINT_RET", Common.print_ret)
CommonX.set("INPUT", Common.input)
CommonX.set("INPUT_INT", Common.input_int)
CommonX.set("CLEAR", Common.clear)
CommonX.set("CLS", Common.clear)
CommonX.set("IS_NUM", Common.is_number)
CommonX.set("IS_STR", Common.is_string)
CommonX.set("IS_FIN", Common.is_finite)
CommonX.set("FAC", Common.factorial)
CommonX.set("CEIL", Common.ceil)
CommonX.set("FABS", Common.fabs)
CommonX.set("FLOOR", Common.floor)
CommonX.set("SQRT", Common.sqrt)
CommonX.set("POTATOSOUP", Common.potatosoup)
CommonX.set("IS_LIST", Common.is_list)
CommonX.set("IS_FUN", Common.is_function)
CommonX.set("APPEND", Common.append)
CommonX.set("POP", Common.pop)
CommonX.set("EXTEND", Common.extend)
CommonX.set("LEN", Common.len)
CommonX.set("RUN", Common.run)

def run(fn, text):
  # tokens
  lexer = Lexer(fn, text)
  tokens, error = lexer.make_tokens()
  if error: return None, error

  # AST
  parser = Parser(tokens)
  ast = parser.parse()
  if ast.error: return None, ast.error

  # Program
  interpreter = Interpreter()
  context = Context('<program>')
  context.symbol_table = CommonX
  result = interpreter.visit(ast.leaf, context)

  return result.value, result.error
