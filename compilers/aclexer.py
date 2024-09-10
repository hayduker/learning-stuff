import ply.lex as lex

class AcLexer:
    def __init__(self):
        self.lexer = self.create_lexer()

    def create_lexer(self):
        tokens = [
            'floatdcl',
            'intdcl',
            'id',
            'assign',
            'print',
            'plus',
            'minus',
            'inum',
            'fnum'
        ]

        t_floatdcl = r'f'
        t_intdcl   = r'i'
        t_id       = r'[a-eghj-oq-z]'
        t_assign   = r'='
        t_print    = r'p'
        t_plus     = r'\+'
        t_minus    = r'-'

        def t_inum(t):
            r'\d+'
            t.value = int(t.value)
            return t

        def t_fnum(t):
            r'\d+.\d+'
            t.value = float(t.value)
            return t

        # Define a rule so we can track line numbers
        def t_newline(t):
            r'\n+'
            t.lexer.lineno += len(t.value)

        t_ignore  = ' \t'

        # Error handling rule
        def t_error(t):
            print("Illegal character '%s'" % t.value[0])
            t.lexer.skip(1)

        return lex.lex()
    
    def tokenize(self, data):
        self.lexer.input(data)

        tokens = []
        while True:
            tok = self.lexer.token()
            if not tok:
                break
            tokens.append(tok)
        
        return tokens