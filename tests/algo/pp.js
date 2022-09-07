#!/usr/bin/env node

// Reference: Dave Prosser's C Preprocessing Algorithm - Diomidis Spinellis blog
// https://www.spinellis.gr/blog/20060626/

const TK_IDENT = 1
const TK_OTHER = 2
const TK_SPACE = 3
const TK_CONCAT = 4     // ##
const TK_STRINGIFY = 5  // #

class Token {
  constructor(kind, t, hs) {
    this.kind = kind
    this.t = t
    if (this.hs != null)
      this.hs = hs
  }

  static make(ts) {
    return ts.map((t) => new Token(t.kind, t.t))
  }

  dup() {
    return new Token(this.kind, this.t, new Set(this.hs))
  }

  toString() {
    return this.t.toString()
  }
}

function lex(src) {
  const tokens = []
  for (;;) {
    let m
    if (src.match(/^\s*$/)) {
      break
    } else if ((m = src.match(/^\s*(##)(?:\s*)(([^\s].*)|$)/)) != null) {
      tokens.push(new Token(TK_CONCAT, m[1]))
      src = m[2]
    } else if ((m = src.match(/^\s+(.*)/)) != null) {
      tokens.push(new Token(TK_SPACE, ' '))
      src = m[1]
    } else if ((m = src.match(/^\s*(#)(?:\s*)([^\s].*)/)) != null) {
      tokens.push(new Token(TK_STRINGIFY, m[1]))
      src = m[2]
    } else if ((m = src.match(/^\s*(\d+)(.*)/)) != null) {
      tokens.push(new Token(TK_OTHER, m[1]))
      src = m[2]
    } else if ((m = src.match(/^\s*([a-zA-Z_][a-zA-Z0-9_]*)(.*)/)) != null) {
      tokens.push(new Token(TK_IDENT, m[1]))
      src = m[2]
    } else if ((m = src.match(/^\s*([-+*\/\(\)=?:,])(.*)/)) != null) {
      tokens.push(new Token(TK_OTHER, m[1]))
      src = m[2]
    } else {
      throw `Not handled: ${src}`
    }
  }
  return tokens
}

class Macro {
  // Formal parameters, macro body
  constructor(fp, body) {
    this.fp = fp
    this.body = lex(body)
  }
}

function union(setA, setB) {
  if (!setA)
    return setB
  if (!setB)
    return setA
  let u = new Set(setA)
  for (let elem of setB)
    u.add(elem)
  return u
}

function intersection(setA, setB) {
  if (!setA || !setB)
    return null
  let u = new Set()
  for (let elem of setB) {
    if (setA.has(elem))
      u.add(elem)
  }
  return u
}

class MacroExpander {
  constructor(macros) {
    this.macros = macros
  }

  expand(ts) {
    if (ts.length === 0) {
      return []
    } else if (ts[0].kind === TK_IDENT && ts[0].t in this.macros && (ts[0].hs == null || !ts[0].hs.has(ts[0].t))) {
      const t = ts[0].t
      const hs = ts[0].hs
      const macro = this.macros[t]
      if (!macro.fp) {  // "()-less macro"
        return this.expand(this.subst(macro.body, [], [], union(hs, new Set([t])), []).concat(ts.slice(1)))
      } else if (ts.length > 1 && ts[1].t === '(') {  // "()'d macro"
        const [actuals, hs2, ts3] = this.getActuals(ts.slice(2))
        if (macro.fp.length !== actuals.length) {
          throw "illegal parameters for #{ts[0].t}, #{actuals}"
        }
        return this.expand(this.subst(macro.body, macro.fp, actuals, union(intersection(hs, hs2), new Set([t])), []).concat(ts3))
      }
    }
    // note TS must be T^HS・TS'
    return [ts[0]].concat(this.expand(ts.slice(1)))
  }

  subst(is, fp, ap, hs, os) {
    if (is.length === 0) {
      return this.hsadd(hs, os)
    }

    if (is[0].kind === TK_STRINGIFY && is.length > 1) {
      let i
      if (is[1].kind == TK_IDENT && (i = fp.indexOf(is[1].t)) >= 0) {
        const selected = ap[i]  //this.select(i, ap)
        return this.subst(is.slice(2), fp, ap, hs, os.concat([this.stringize(selected)]))
      }
    }

    if (is[0].kind === TK_CONCAT && is.length > 1) {
      let i
      if (is[1].kind == TK_IDENT && (i = fp.indexOf(is[1].t)) >= 0) {
        const selected = ap[i]  //this.select(i, ap)
        if (selected.length === 0)  // only if actuals can be empty
          return this.subst(is.slice(2), fp, ap, hs, os)
        else
          return this.subst(is.slice(2), fp, ap, hs, this.glue(os, selected))
      }
    }

    const i = fp.indexOf(is[0].t)
    if (i >= 0) {
      return this.subst(is.slice(1), fp, ap, hs, os.concat(this.expand(ap[i])))
    }

    return this.subst(is.slice(1), fp, ap, hs, os.concat([is[0].dup()]))
  }

  glue(ls, rs) {  // paste last of left side with first of right side
    if (ls.length === 1) {
      console.assert(ls[0].kind === TK_IDENT)
      console.assert(rs[0].t.match(/^[a-zA-Z0-9_]+$/))
      const combined = ls[0].t + rs[0].t
      const hs = union(ls[0].hs, rs[0].hs)
      return [new Token(TK_IDENT, combined, hs)].concat(rs.slice(1))
    }
    // note LS must be L^HS・LS'
    return [ls[0]].concat(this.glue(ls.slice(1), rs))
  }

  hsadd(hs, ts) {
    if (ts.length === 0) {
      return []
    }

    if (ts[0].kind !== TK_IDENT && (ts[0].kind !== TK_OTHER || ts[0].t !== ')'))
      return [ts[0]].concat(this.hsadd(hs, ts.slice(1)))

    const t = ts[0].dup()
    const hs2 = ts[0].hs
    const ts2 = ts.slice(1)
    t.hs = union(hs, hs2)
    return [t].concat(this.hsadd(hs, ts2))
  }

  stringize(x) {
    const s = `"${x}"`  // TODO: Escape
    return new Token(TK_OTHER, s)
  }

  getActuals(ts) {
    const ap = []
    let i  = 0
    if (ts[0].t !== ')') {
      for (;;) {
        let paren = 0
        while (ts[i].kind === TK_SPACE)
          ++i
        const start = i
        for (;;) {
          const t = ts[i].t
          if (t === '(') {
            paren += 1
          } else if (t === ')') {
            if (paren === 0)
              break
            paren -= 1
          } else {
            if (t === ',' && paren === 0)
              break
          }
          i += 1
        }

        let j = i
        for (j = i; j > start; --j) {
          if (ts[j - 1].kind !== TK_SPACE)
            break
        }

        ap.push(ts.slice(start, j))
        if (ts[i].t == ')') {
          break
        }
        i += 1
      }
    }
    return [ap, ts[i].hs, ts.slice(i+1)]
  }
}

let errorCount = 0

function pptest(expected, input, macros) {
  function print(...args) {
    process.stdout.write(...args)
  }

  print(`${input} => `)

  expander = new MacroExpander(macros)
  result = expander.expand(lex(input))
  actual = result.map((t) => t.t.toString()).join('')
  if (actual === expected) {
    console.log("OK")
  } else {
    console.error(`ERR, expected=[${expected}], actual=[${actual}]`)
    ++errorCount
  }
}

function main() {
  pptest(
      '((void*)0)',
      'NULL',
      {
        'NULL': new Macro(null, '((void*)0)'),
      })

  pptest(
    'foobar',
    'NOPARAM()',
    {
      'NOPARAM': new Macro([], 'foobar'),
    })

  pptest(
      'WITHOUTPAREN',
      'WITHOUTPAREN',
      {
        'WITHOUTPAREN': new Macro([], 'foobar'),
      })

  pptest(
      '1234',
      'ID(1234)',
      {
        'ID': new Macro(['x'], 'x'),
      })

  pptest(
      '(11 11) (25 25)',
      'TWICE(11) TWICE(25)',
      {
        'TWICE': new Macro(['x'], 'G(x)'),
        'G': new Macro(['y'], '(y y)'),
      })

  pptest(
      '((111) * (111))',
      'SQ(111)',
      {
        'SQ': new Macro(['x'], '((x) * (x))'),
      })

  pptest(
      'RECUR(123-1)',
      'RECUR(123)',
      {
        'RECUR': new Macro(['n'], 'RECUR(n-1)'),
      })

  pptest(
      'E(13-1-1)',
      'MUTRECUR(13)',
      {
        'MUTRECUR': new Macro(['n'], 'E(n)'),
        'E': new Macro(['x'], 'O(x-1)'),
        'O': new Macro(['x'], 'E(x-1)'),
      })

  pptest(
      'H(987)',
      'F(987)',
      {
        'F': new Macro(['x'], 'C(G(x))'),
        'G': new Macro(['x'], 'C(H(x))'),
        'C': new Macro(['x'], 'x'),
      })

  {
    const macros = {
      'N': new Macro(null, '1'),
      'CAT': new Macro(['x', 'y'], 'x ## y'),
      'INDIRECT': new Macro(['x', 'y'], 'CAT(x, y)'),
      'FOO1': new Macro(null, 'MATCHED'),
    }

    pptest(
        'ABCXYZ',
        'CAT( ABC , XYZ )',
        macros)

    pptest(
        'FOON',
        'CAT(FOO, N)',
        macros)

    pptest(
        'MATCHED',
        'INDIRECT(FOO, N)',
        macros)

    pptest(
        '"hoge"',
        'STR(hoge)',
        {
          'STR': new Macro(['x'], '# x'),
        })
  }

  process.exit(errorCount === 0 ? 0 : 1)
}

main()
