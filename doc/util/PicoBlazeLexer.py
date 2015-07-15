from pygments.lexer import RegexLexer
from pygments.token import *
import re

class PicoBlazeLexer(RegexLexer):
    name = 'PicoBlaze'
    aliases = ['pb']
    filenames = ['*.psm', '*.psm4']
    
    flags = re.IGNORECASE

    tokens = {
        'root': [
            (r'[\t ]+', Text),
            (r'[{}]', Punctuation),
            (r'<.+>', Generic.Emph),
            (r'\w+:', Name.Label),
            (r'pbhex\(', Name.Builtin, 'pbhex'),
            (r'\w+\(', Name.Builtin, 'macro'),
            (r'(if|for|do|while) *\(', Name.Builtin, 'macro'),
            (r'else', Name.Builtin),
            (r'[\w&@]+[#$%]?', Keyword, 'args'),
            (r';[\t ]*pragma.*\n', Comment.Special),
            (r';.*\n', Comment),
            (r'.*\n', Text),
        ],
        'args': [
            (r'[\t ]+', Text),
            (r'\w+\(', Name.Builtin, 'macro'),
            (r'[,()]', Punctuation),
            (r"\[.*\]'d", Number),
            (r"\[.*\]'b", Number.Bin),
            (r"\[.*\]", Number.Hex),
            (r"\d+'d", Number),
            (r"[01]+'b", Number.Bin),
            (r'[0-9a-f]+(?!\w)', Number.Hex),
            (r'"[^"]"', String.Char),
            (r'\w+[#$%]?', Name),
            (r"'(upper|lower)", Operator),
            (r';.*\n', Comment, '#pop'),
            (r'.*\n', Text, '#pop')
        ],
        'macro': [
            (r'\s+', Text),
            (r'pbhex\(', Name.Builtin, 'pbhex'),
            (r'\w+\(', Name.Builtin, 'macro'),
            (r',', Punctuation),
            (r'(:=|=:|<<|>>|!=|[-+*/~<>])', Operator),
            (r'0x[0-9a-f]+', Number.Hex),
            (r'0b[01]+', Number.Bin),
            (r'\d+', Number),
            (r'\w+', String),
            (r'[^ ),]+', String),
            (r'[^;)]*\)', Name.Builtin, '#pop')
        ],
        'pbhex': [
          (r'\s+', Text),
          (r',', Punctuation),
          (r'[^,)]+', Number.Hex),
          (r'\)', Name.Builtin, '#pop')
        ]

    }