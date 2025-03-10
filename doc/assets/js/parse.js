
class Type {};

class Builtin extends Type {
  constructor(name) {
    super();
    this.name = name;
  }

  static ofString(s) {
    const concrete = ['string', 'bool', 'int', 'float', 'void', 'datetime'];
    if (!concrete.includes(s))
      return null;

    return new Builtin(s);
  }
};

class Enum extends Type {
  constructor(name) {
    super();
    this.name = name;
  }
};

class Ctor extends Type {
  constructor(params, name) {
    super();
    this.params = params;
    this.name = name;
  }
};

function lex(str) {
  if (str.indexOf('$') >= 0)
    throw new Error('Not allowed to contain $');

  let ts = str.replaceAll('(', ' ( ');
  ts = ts.replaceAll(')', ' ) ');
  ts = ts.split(' ');
  ts = ts.filter(x => x !== '');
  ts.push('$');
  return ts;
}

class Lexer {
  constructor(tokens) {
    this.tokens = tokens;
    this.pos = 0;
  }

  shift() {
    if (this.pos >= this.tokens.length - 1)
      return '$';

    return this.tokens[this.pos++];
  }

  peek() {
    const prev = this.pos;
    let t = this.shift();
    this.pos = prev;
    return t;
  }

  expect(ts) {
    if (!Array.isArray(ts))
      ts = [ts];

    let l = this.shift();
    for (const t of ts)
      if (l == t) return;

    throw new Error(`Expected ${t}, got ${l}`);
  }
};

function lbp(t) {
  switch (t) {
    case '(':
    case ')':
    case '->':
    case '\u2192':
      return 0;
    case '$':
      return -1;
  }

  return 1;
}

function nud(l, t) {
  switch (t) {
    case 'enum':
      return new Enum(l.shift());

    case '(':
      let left = parseType(l, 0);
      l.expect(['->', '\u2192']);
      let right = parseType(l, 0);
      l.expect(')');
      l.expect('map');
      return new Ctor([left, right], 'map');
  }

  let bty = Builtin.ofString(t);
  if (bty != null)
    return bty;

  const fmt = /^[a-zA-Z_]+$/;
  if (fmt.test(t))
    return new Ctor([], t);

  throw new Error(`No null denotation for ${t}`);
}

function led(l, left, t) {
  const known = ['set', 'ref', 'option', 'record'];
  if (!known.includes(t))
    throw new Error(`Invalid type constructor: ${t}`);

  return new Ctor([left], t);
}

function parseType(l, rbp) {
  let left = nud(l, l.shift());

  while (lbp(l.peek()) > rbp)
    left = led(l, left, l.shift());

  return left;
}

function parseSingleType(input) {
  try {
    let lexer = new Lexer(lex(input));
    let ty = parseType(lexer, 0);
    if (lexer.peek() != '$')
      throw new Error('Did not consume entire input');
    return ty;
  } catch (e) {
  }

  return null;
}

