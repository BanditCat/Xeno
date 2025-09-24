const $ = (id) => (document.getElementById(id));

export function init() {
  const run = () => {
    try {
      $('out').style.color = ''; // reset color
      const dict = parse($('src').value);
      $('out').textContent = showReconstructed(dict);
      // after parse():

      // lift the 'out' symbol (or any you choose)
      const packed = liftFromDict(dict);

      // quick eyeball
      console.log('nodes:', packed.A.length, 'root:', packed.rootIdx);
      console.log(debugDump(packed, 50));
    } catch (e) {
      $('out').style.color = 'red';
      $('out').textContent = e.message;
    }
  };
  const loadExample = () => {
    $('src').value = String.raw`cons = [\a d f.f [a d] id];
id = [\a.a];
out = [cons id id];`
  };
  $('run').addEventListener('click', run );
  $('loadExample').addEventListener('click', loadExample );
  window.addEventListener('keydown', (e) => {
	  if ((e.ctrlKey) && e.key === 'Enter') run();
  });

}

function parse(input) {
  // ----- tokenize -----
  const tokens = [];
  let i = 0, line = 1, col = 1;
  const globals = new Set();
  function push(tok) {
	  tok.line = line;
	  tok.col = col;
	  tokens.push(tok);
  }
  while (i < input.length) {
	  const c = input[i];
	  if (c === '/' && input[i + 1] === '/') {
	    i += 2; col += 2;
	    while (i < input.length && input[i] !== '\n') { i++; col++; }
	    continue;
	  }
	  if (c === '\n') { i++; line++; col = 1; continue; }
	  if (/\s/.test(c)) { i++; col++; continue; }

	  if (c === '[' || c === ']' || c === '.' || c === '=' || c === ';' ) {
	    push({ t: c });
	    i++; col++;
	    continue;
	  }

	  if (c === '\\') {
	    push({ t: '\\' });
	    i++; col++;
	    continue;
	  }

    if (/[A-Za-z0-9_]/.test(c)) {
      const start = i, startCol = col;
      while (i < input.length && /[A-Za-z0-9_]/.test(input[i])) { i++; col++; }
      tokens.push({ t: 'ID', v: input.slice(start, i), line, col: startCol });
      continue;
    }

	  throw new SyntaxError(`Unexpected character '${c}' at ${line}:${col}`);
  }
  for (let p = 0; p < tokens.length; ) {
    if (tokens[p].t === 'ID' && tokens[p+1]?.t === '=') {
      globals.add(tokens[p].v);
      p += 2;
      while (p < tokens.length && tokens[p].t !== ';') p++;
      if (tokens[p]?.t === ';') p++;
    } else p++;
  }

  // ----- recursive descent over tokens -----
  let k = 0;
  let lamDepth = 0;
  const atEnd = () => k >= tokens.length;
  const peek = () => tokens[k];
  const eat = (t) => {
	  const tok = peek();
	  const where = tok ? `${tok.line}:${tok.col}` : "end of input";
	  if (!tok || tok.t !== t) throw new SyntaxError(`expected '${t}' at ${where}`);
	  k++;
	  return tok;
  };

  // Expr := Atom (Atom)*     ; left-assoc application
  function parseExpr() {
	  let node = parseAtom();
	  while (!atEnd() && (peek().t === 'ID' || peek().t === '[')) {
	    const arg = parseAtom();
	    node = { type: 'APP', func: node, arg, line:arg.line, col:arg.col };
	  }
	  return node;
  }

  // Atom := ID | List
  function parseAtom() {
    const tok = peek();
    const where = tok ? `${tok.line}:${tok.col}` : "end of input";
    if (!tok) throw new SyntaxError(`expected atom at ${where}`);

    if (tok.t === 'ID') {
      k++;
      // Do NOT inline here. Treat as a variable occurrence;
      // we'll turn free ones that match globals into inlined defs later.
      return { type:'VAR', name: tok.v, line: tok.line, col: tok.col };
    }
    if (tok.t === '[') return parseList();
    throw new SyntaxError(`expected atom at ${where}`);
  }
  // List := '[' ( Lambda | Expr ) ']'
  // Lambda := '\' ID+ '.' Expr     ; desugar \a b c. body -> \a.(\b.(\c. body))
  function parseList() {
    eat('[');
    let node;
    const tok = peek();
    if (tok && tok.t === '\\') {
      eat('\\');
      const params = [];
      if (!(peek() && peek().t === 'ID')) throw new SyntaxError(`expected identifier after '\\' at ${tok.line}:${tok.col}`);
      while (peek() && peek().t === 'ID') params.push(eat('ID').v);
      for (const p of params) {
        if (globals.has(p)) throw new SyntaxError(`param '${p}' shadows global at ${tok.line}:${tok.col}`);
      }
      eat('.');
      lamDepth++;
      const body = parseExpr();
      lamDepth--;
      node = params.reverse().reduce((acc, p) => ({ type:'LAM', param:p, body:acc }), body);
    } else {
      node = parseExpr();
    }
    eat(']');
    return node;
  }
  
  function parseID() {
	  let ret = {}
	  const tok = peek();
	  const where = tok ? `${tok.line}:${tok.col}` : "end of input";
	  if (!tok || tok.t !== 'ID') {
	    throw new SyntaxError(`expected identifier at ${where}`);
	  }
	  k++;
	  ret.v = tok.v;
	  ret.line = tok.line;
	  ret.col = tok.col;
	  return ret
  }
  const defs = []; // <-- collect first

  while (!atEnd()) {
    const id = parseID();
    eat('=');
    const ast = parseExpr();

    if (!atEnd()) {
      const t = peek();
      if (t.t === ';') {
        do { k++; } while (!atEnd() && peek().t === ';');
      } else {
        throw new SyntaxError(`expected ';' after definition of '${id.v}' before ${t.line}:${t.col}`);
      }
    }
    defs.push({ name: id.v, ast, line: id.line, col: id.col });
  }
  
  return buildDictTopo(defs, globals);
}


function getFreeVars(node, env = new Set()) {
  switch (node.type) {
  case "VAR":
	  return env.has(node.name) ? new Set() : new Set([node.name]);

  case "LAM": {
	  const env2 = new Set(env);
	  env2.add(node.param);         // bind param in the body
	  return getFreeVars(node.body, env2);
  }

  case "APP": {
	  const left = getFreeVars(node.func, env);
	  const right = getFreeVars(node.arg, env);
	  // union
	  for (const v of right) left.add(v);
	  return left;
  }

  default:
	  throw new TypeError("Unknown AST node type: " + node.type);
  }
}

function canonAlpha(node, env = {}, counter = {n:0}) {
  switch (node.type) {
  case 'VAR':
	  return env[node.name]; // prefix free vars so they don't collide
  case 'LAM': {
	  const n = counter.n++;
	  const env2 = {...env, [node.param]: n};
	  return `L${n}${canonAlpha(node.body, env2, counter)}`;
  }
  case 'APP':
	  return `A${canonAlpha(node.func, env, counter)}${canonAlpha(node.arg, env, counter)}`;
  }
}

function liftAST(root) {
  const tags = [];
  const left = [];
  const right = [];

  function walk(node, parentIdx = -1) {
    const idx = tags.length;
    switch (node.type) {
    case "VAR":
      tags.push(0);
      left.push(parentIdx);
      right.push(node.name); 
      break;
    case "LAM": {
      tags.push(1);
      left.push(parentIdx);
      right.push(null);
      const bodyIdx = walk(node.body, idx);
      right[idx] = bodyIdx;
      break;
    }
    case "APP": {
      tags.push(2);
      left.push(null); 
      right.push(null);
      const funcIdx = walk(node.func, idx);
      const argIdx  = walk(node.arg, idx);
      left[idx]  = funcIdx;
      right[idx] = argIdx;
      break;
    }
    }
    return idx;
  }

  const rootIdx = walk(root);
  return {
    tags: Uint32Array.from(tags),
    left: Uint32Array.from(left.map(x => x ?? 0)),
    right: Uint32Array.from(right.map(x => x ?? 0)),
    rootIdx
  };
}
function makeCanonIndex(dict) {
  const idx = new Map();
  for (const [name, ast] of dict) {
    const key = canonAlpha(ast);
    idx.set(key, name);
  }
  return idx;
}
function ppFromDict(node, canonIdx, top = false, wrapApps = true) {
  const key = canonAlpha(node);
  if (!top) {
    const hit = canonIdx.get(key);
    if (hit) return hit;
  }
  switch (node.type) {
  case 'VAR':
    return node.name;

  case 'LAM': {
    const params = [];
    let body = node;
    while (body.type === 'LAM') { params.push(body.param); body = body.body; }
    // ?? don't wrap applications for the lambda body
    return `[\\${params.join(' ')}.${ppFromDict(body, canonIdx, false, false)}]`;
  }

  case 'APP': {
    const parts = [];
    let cur = node;
    while (cur.type === 'APP') { parts.unshift(ppFromDict(cur.arg, canonIdx)); cur = cur.func; }
    parts.unshift(ppFromDict(cur, canonIdx));
    const s = parts.join(' ');
    return wrapApps ? `[${s}]` : s;
  }
  }
}
function showReconstructed(dict) {
  const canonIdx = makeCanonIndex(dict);
  let ret = ""
  for (const [name, ast] of dict) {
    ret += `${name} = ${ppFromDict(ast, canonIdx, true)}\n`;
  }
  return ret;
}



// ---- LIFT: AST -> packed (A,B) with 16 tags ----

// Tag constants (0..15); reusing yours:
const TAG_APP = 0, TAG_VAR = 1, TAG_LAM = 2;
const NOIDX = 0x3FFFFFFF >>> 0; // 30-bit "none"
const A_MASK = 0x3FFFFFFF >>> 0, B_MASK = 0x3FFFFFFF >>> 0;

// (reused) pack/unpack helpers
function packWords(tag, aIdx, bIdx) {
  const lo2 = (tag & 0b11) >>> 0;
  const hi2 = (tag >>> 2) & 0b11;
  const A = ((lo2 << 30) >>> 0) | (aIdx & A_MASK);
  const B = ((hi2 << 30) >>> 0) | (bIdx & B_MASK);
  return [A >>> 0, B >>> 0];
}
function getTag(A, B) {
  const lo2 = (A >>> 30) & 0b11;
  const hi2 = (B >>> 30) & 0b11;
  return ((hi2 << 2) | lo2) >>> 0;
}
const getA = (A) => (A & A_MASK) >>> 0;
const getB = (B) => (B & B_MASK) >>> 0;
function setAWord(Aword, newA) { return (((Aword >>> 30) << 30) >>> 0) | (newA & A_MASK); }
function setBWord(Bword, newB) { return (((Bword >>> 30) << 30) >>> 0) | (newB & B_MASK); }

// Main lifter for a single AST root
function lift(root) {
  const A = [], B = [];

  // env: name -> stack of lambda indices (nearest binder at top)
  const env = new Map();
  const pushBind = (name, lamIdx) => {
    let s = env.get(name);
    if (!s) env.set(name, (s = []));
    s.push(lamIdx);
  };
  const popBind = (name) => env.get(name).pop();
  const boundIdx = (name) => {
    const s = env.get(name);
    return (s && s.length) ? s[s.length - 1] : NOIDX;
  };

  // local emitter (packed)
  const emit = (tag, aIdx = NOIDX, bIdx = NOIDX) => {
    const [aw, bw] = packWords(tag, aIdx, bIdx);
    const idx = A.length;
    A.push(aw); B.push(bw);
    return idx >>> 0;
  };

  function walk(node, parentIdx = NOIDX) {
    switch (node.type) {
    case 'APP': {
      // create, then fill children indices
      const self = emit(TAG_APP, NOIDX, NOIDX);
      const f = walk(node.func, self);
      const x = walk(node.arg,  self);
      A[self] = setAWord(A[self], f);
      B[self] = setBWord(B[self], x);
      return self;
    }
    case 'LAM': {
      const self = emit(TAG_LAM, parentIdx, NOIDX);
      pushBind(node.param, self);
      const body = walk(node.body, self);
      B[self] = setBWord(B[self], body);
      popBind(node.param);
      return self;
    }
    case 'VAR': {
      // parent = parentIdx, lambdaBoundTo = nearest binder (or NOIDX)
      return emit(TAG_VAR, parentIdx, boundIdx(node.name));
    }
    default:
      throw new Error('Unknown node.type ' + node.type);
    }
  }

  const rootIdx = walk(root, NOIDX);
  return { A: Uint32Array.from(A), B: Uint32Array.from(B), rootIdx, NOIDX };
}

// Convenience: pick an entry from your dict (default 'out'), lift it
function liftFromDict(dict, entry = 'out') {
  const ast = dict.get(entry);
  if (!ast) throw new Error(`No entry named '${entry}'`);
  return lift(ast);
}

// Optional: tiny dumper to verify shapes quickly
function debugDump(packed, max = 20) {
  const { A, B } = packed;
  const n = Math.min(A.length, max);
  let s = '';
  for (let i = 0; i < n; i++) {
    const tag = getTag(A[i], B[i]);
    const a = getA(A[i]);
    const b = getB(B[i]);
    if (tag === TAG_APP) s += `${i}: APP  f=${a} arg=${b}\n`;
    else if (tag === TAG_VAR) s += `${i}: VAR  parent=${a} lambda=${b}\n`;
    else if (tag === TAG_LAM) s += `${i}: LAM  parent=${a} body=${b}\n`;
    else s += `${i}: TAG${tag} A=${a} B=${b}\n`;
  }
  return s;
}


function depsFrom(ast, globals) {
  // Free vars of this AST that are names of globals = edges
  const fv = getFreeVars(ast);
  const out = new Set();
  for (const v of fv) if (globals.has(v)) out.add(v);
  return out;
}

function topoSort(adj) {
  // Kahn's algorithm
  const indeg = new Map();
  const nodes = Array.from(adj.keys());
  for (const n of nodes) indeg.set(n, 0);
  for (const [n, nbrs] of adj) {
    for (const m of nbrs) indeg.set(m, (indeg.get(m) ?? 0) + 1);
  }
  const q = [];
  for (const [n, d] of indeg) if (d === 0) q.push(n);

  const order = [];
  for (let i = 0; i < q.length; i++) {
    const n = q[i];
    order.push(n);
    for (const m of adj.get(n) || []) {
      indeg.set(m, indeg.get(m) - 1);
      if (indeg.get(m) === 0) q.push(m);
    }
  }
  if (order.length !== nodes.length) {
    // Cycle detection (quick 'who remains' message)
    const remaining = nodes.filter(n => indeg.get(n) > 0);
    throw new SyntaxError(`cyclic definitions among: ${remaining.join(', ')}`);
  }
  return order; 
}

// Expand global references by *copying in* already-built defs.
// We treat a VAR as bound iff it's in 'bound'. Otherwise, if its name is in 'built', inline it.
function inlineGlobals(node, built, bound = new Set()) {
  switch (node.type) {
  case 'VAR': {
    if (bound.has(node.name)) return { type:'VAR', name: node.name };
    const hit = built.get(node.name);
    if (hit) return cloneAST(hit); // copy-on-use
    // leave as free var (unknown) for now; will be caught earlier by checks
    return { type:'VAR', name: node.name };
  }
  case 'LAM': {
    const b2 = new Set(bound); b2.add(node.param);
    return { type:'LAM', param: node.param, body: inlineGlobals(node.body, built, b2) };
  }
  case 'APP':
    return { type:'APP', func: inlineGlobals(node.func, built, bound),
             arg:  inlineGlobals(node.arg,  built, bound) };
  default:
    throw new Error('Unknown node.type in inlineGlobals: ' + node.type);
  }
}

function buildDictTopo(defs, globals) {
  // name -> raw AST
  const raw = new Map();
  for (const d of defs) {
    if (raw.has(d.name)) throw new SyntaxError(`duplicate definition '${d.name}' at ${d.line}:${d.col}`);
    raw.set(d.name, d.ast);
  }

  // unknown-name check
  for (const {name, ast, line, col} of defs) {
    const fv = getFreeVars(ast);
    for (const v of fv) if (!globals.has(v)) {
      throw new SyntaxError(`'${name}' uses unknown '${v}' at ${line}:${col}`);
    }
  }

  // ---- FIX: build graph as dep -> user ----
  const adj = new Map();                 // Map<string, Set<string>>
  for (const {name} of defs) adj.set(name, new Set());
  for (const {name, ast} of defs) {
    for (const dep of depsFrom(ast, globals)) {
      if (!adj.has(dep)) adj.set(dep, new Set());
      adj.get(dep).add(name);            // edge: dep ---> name (user)
    }
  }

  const order = topoSort(adj);           // now yields ["id","cons","out"]

  // build final dict with copy-on-use + alpha-eq dup check
  const final = new Map();
  for (const name of order) {
    const expanded = inlineGlobals(raw.get(name), final);
    expanded.canon = canonAlpha(expanded);
    for (const [otherName, otherAst] of final) {
      if (otherAst.canon === expanded.canon) {
        throw new SyntaxError(`'${name}' duplicates value of '${otherName}'`);
      }
    }
    final.set(name, expanded);
  }
  return final;
}
function cloneAST(node) {
  switch (node.type) {
  case 'VAR': 
    return { type:'VAR', name: node.name, canon: node.canon };
  case 'LAM': 
    return { type:'LAM', param: node.param, body: cloneAST(node.body), canon: node.canon };
  case 'APP': 
    return { type:'APP', func: cloneAST(node.func), arg: cloneAST(node.arg), canon: node.canon };
  }
}
