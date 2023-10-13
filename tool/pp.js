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
    for (let i = 0; i < ts.length; ++i) {
      const tt = ts[i]
      if (tt.kind !== TK_IDENT ||
          !(tt.t in this.macros) ||
          (tt.hs != null && tt.hs.has(tt.t)))
        continue

      const t = tt.t
      const hs = tt.hs
      const macro = this.macros[t]
      if (!macro.fp) {  // "()-less macro"
        const replaced = this.subst(macro.body, [], [], union(hs, new Set([t])))
        ts.splice(i, 1, ...replaced)
        --i
      } else if (ts.length > i + 1 && ts[i + 1].t === '(') {  // "()'d macro"
        const [actuals, closeParenIndex] = this.getActuals(ts, i + 2)
        if (macro.fp.length !== actuals.length) {
          throw "illegal parameters for #{tt.t}, #{actuals}"
        }
        const hs2 = ts[closeParenIndex].hs
        const replaced = this.subst(macro.body, macro.fp, actuals, union(intersection(hs, hs2), new Set([t])))
        ts.splice(i, closeParenIndex + 1 - i, ...replaced)
        --i
      }
    }
    return ts
  }

  subst(is, fp, ap, hs) {
    const expandedAp = ap.map(_ => null)
    const os = []
    for (let i = 0; i < is.length; ++i) {
      const tt = is[i]

      if (tt.kind === TK_STRINGIFY && is.length > i + 1) {
        let j
        if (is[i + 1].kind == TK_IDENT && (j = fp.indexOf(is[i + 1].t)) >= 0) {
          const selected = ap[j]  //this.select(j, ap)
          os.push(this.stringize(selected))
          ++i
          continue
        }
      }

      if (tt.kind === TK_CONCAT && is.length > i + 1) {
        let j
        if (is[i + 1].kind == TK_IDENT && (j = fp.indexOf(is[i + 1].t)) >= 0) {
          const selected = ap[j]  //this.select(i, ap)
          if (selected.length > 0)  // only if actuals can be empty
            this.glue(os, selected)
          ++i
          continue
        }
        this.glue(os, [is[i + 1]])
        ++i
        continue
      }

      if (tt.kind === TK_IDENT) {
        if (is.length > i + 1 && is[i + 1].kind === TK_CONCAT) {
          let j = fp.indexOf(tt.t)
          if (j >= 0) {
            const selected = ap[j]  //this.select(i, ap)
            if (selected.length === 0) {  // only if actuals can be empty
              let k
              if (is.length > i + 2 && is[i + 2].kind == TK_IDENT && (k = fp.indexOf(is[i + 2].t)) >= 0) {
                const selected2 = ap[k]  //this.select(j, ap)
                os.push(...selected2)
              }
              i += 2
            } else {
              os.push(...selected)
              // Handle `##` at next iteration.
            }
            continue
          }
        }

        const j = fp.indexOf(tt.t)
        if (j >= 0) {
          let expanded = expandedAp[j]
          if (expanded == null) {
            expanded = expandedAp[j] = this.expand([...ap[j]])
          }
          os.push(...expanded)
          continue
        }
      }
      os.push(tt)
    }
    this.hsadd(hs, os)
    return os
  }

  glue(ls, rs) {  // paste last of left side with first of right side
    if (ls.length <= 0) {
      ls.push(...rs)
    } else {
      const ll = ls[ls.length - 1]
      const rr = rs[0]
      console.assert(ll.kind === TK_IDENT)
      console.assert(rr.t.match(/^[a-zA-Z0-9_]+$/))
      const combined = ll.t + rr.t
      const hs = union(ll.hs, rr.hs)
      ll.t = combined
      ll.hs = hs
      ls.push(...rs.slice(1))
    }
  }

  hsadd(hs, ts) {
    for (let i = 0; i < ts.length; ++i) {
      const tt = ts[i]
      if (tt.kind !== TK_IDENT && (tt.kind !== TK_OTHER || tt.t !== ')'))
        continue

      ts[i].hs = union(hs, ts[i].hs)
    }
  }

  stringize(x) {
    const actual = x.map((t) => t.t.toString()).join('')
    const s = `"${actual}"`  // TODO: Escape
    return new Token(TK_OTHER, s)
  }

  getActuals(ts, i) {
    const ap = []
    if (ts[i].t !== ')') {
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
    return [ap, i]
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
  pptest(
      'SELF',
      'SELF',
      {
        'SELF': new Macro(null, 'I(SELF)'),
        'I': new Macro(['v'], 'v'),
      })

  {
    const macros = {
      'CAT': new Macro(['x', 'y'], 'x ## y'),
      'INDIRECT': new Macro(['x', 'y'], 'CAT(x, y)'),
      'N': new Macro(null, '1'),
      'FOO1': new Macro(null, 'MATCHED'),
      'X': new Macro(null, 'Y'),
      '_': new Macro(null, ''),
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
        'MATCHED',
        'CAT(FOO, 1)',
        macros)

    pptest(
        'XN',
        'CAT(X, N)',
        macros)

    pptest(
        'X_',
        'CAT(X, _)',
        macros)

    pptest(
        '_X',
        'CAT(_, X)',
        macros)

    pptest(
        'R',
        'CAT(, R)',
        macros)

    pptest(
        'L',
        'CAT(L, )',
        macros)
  }

  pptest(
      '"hoge"',
      'STR(hoge)',
      {
        'STR': new Macro(['x'], '# x'),
      })

  pptest(
      'a(m_z, "M(z)")',
      'MinM(M(z))',
      {
        'MinM': new Macro(['x'], 'a(x, # x)'),
        'M': new Macro(['x'], 'm_ ## x'),
      })

  process.exit(errorCount === 0 ? 0 : 1)
}

main()
