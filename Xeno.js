const $ = (id) => (document.getElementById(id));

export function init() {
  let st = null;
  const dump = () => { if (st) console.log(debugDump(st)); };
  const run = () => {
    try {
      $('out').style.color = ''; // reset color
      const dict = parse($('src').value);
      st   = liftToArrays(dict.get('out'));   // DAG with de-Bruijn VARs
      const ast  = dropFromArrays(st);                         // pretty dict-AST (x0,x1,foo)

      // You already have:
      st.canonIdx = makeCanonIndex(dict);
      $('out').textContent =
        showReconstructed(dict) +
        "\n\n-- reconstructed (debug view) --\n" +
        ppAstWithDict(ast, st.canonIdx);
    } catch (e) {
      $('out').style.color = 'red';
      $('out').textContent = e.message;
    }
    renderState(st);
  };


  
  function renderState(st){
  
    $('out').textContent = ppAstWithDict(dropFromArrays(st), st.canonIdx);
  }

  const tfunc = () => {
    if( !st )
      run();
    tick(st);
    renderState(st);
  };
  const loadExample = () => {
    $('src').value = String.raw`// pairs & booleans
cons  = [\a b f.f a b];
true  = [\a b.a];
false = [\a b.b];

// pair selectors
head = [\p.p [\a b.a]];
tail = [\p.p [\a b.b]];

// empty?  (nil -> true, cons -> false)
isnil = [\x.x [\a b.false] true];

// fixpoint (normal-order Y)
Y = [\f.[ [\x.f [x x]] [\x.f [x x]] ]];

// increment LSB-first binary (e.g., [0,1,1] -> [1,1,1], [1,1] -> [0,0,1])
inc =
  [Y [\self n.
    [[isnil n]
      [cons true false]                                  // [] -> [1]
      [[head n]
        [cons false [self [tail n]]]                   // 1 :: t -> 0 :: inc t
        [cons true  [tail n]]]                         // 0 :: t -> 1 :: t
  ]]];

// --- examples ---
// six = 110 (LSB-first: [0,1,1]) => inc -> 1112
six = [cons false [cons true [cons true false]]];
out = [inc six];`
  };
  $('run').addEventListener('click', run );
  $('loadExample').addEventListener('click', loadExample );
  window.addEventListener('keydown', (e) => {
    if ((e.ctrlKey) && e.key === 'Enter') run();
    if ((e.ctrlKey) && e.key === '.') tfunc();
  });
  $('tick').addEventListener('click', tfunc);
}

/* ============================== PARSER ============================== */

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
  const atEnd = () => k >= tokens.length;
  const peek = () => tokens[k];
  const eat = (t) => {
    const tok = peek();
    const where = tok ? `${tok.line}:${tok.col}` : "end of input";
    if (!tok || tok.t !== t) throw new SyntaxError(`expected '${t}' at ${where}`);
    k++;
    return tok;
  };

  function parseExpr() {
    let node = parseAtom();
    while (!atEnd() && (peek().t === 'ID' || peek().t === '[')) {
      const arg = parseAtom();
      node = { type: 'APP', func: node, arg, line:arg.line, col:arg.col };
    }
    return node;
  }

  function parseAtom() {
    const tok = peek();
    const where = tok ? `${tok.line}:${tok.col}` : "end of input";
    if (!tok) throw new SyntaxError(`expected atom at ${where}`);

    if (tok.t === 'ID') { k++; return { type:'VAR', name: tok.v, line: tok.line, col: tok.col }; }
    if (tok.t === '[') return parseList();
    throw new SyntaxError(`expected atom at ${where}`);
  }
  // List := '[' ( Lambda | Expr ) ']'
  // Lambda := '\' ID+ '.' Expr
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
      const body = parseExpr();
      node = params.reverse().reduce((acc, p) => ({ type:'LAM', param:p, body:acc }), body);
    } else {
      node = parseExpr();
    }
    eat(']');
    return node;
  }

  const defs = [];
  while (!atEnd()) {
    const id = eat('ID');
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
  case "VAR": return env.has(node.name) ? new Set() : new Set([node.name]);
  case "LAM": {
    const env2 = new Set(env);
    env2.add(node.param);
    return getFreeVars(node.body, env2);
  }
  case "APP": {
    const left = getFreeVars(node.func, env);
    const right = getFreeVars(node.arg, env);
    for (const v of right) left.add(v);
    return left;
  }
  default: throw new TypeError("Unknown AST node type: " + node.type);
  }
}

function canonAlpha(node, env = {}, counter = {n:0}) {
  switch (node.type) {
  case 'VAR': return env[node.name];
  case 'LAM': {
    const n = counter.n++;
    const env2 = {...env, [node.param]: n};
    return `L${n}${canonAlpha(node.body, env2, counter)}`;
  }
  case 'APP': return `A${canonAlpha(node.func, env, counter)}${canonAlpha(node.arg, env, counter)}`;
  }
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
function ppAstWithDict(ast, canonIdx){
  return ppFromDict(ast, canonIdx, /*top=*/true);
}
function showReconstructed(dict) {
  const canonIdx = makeCanonIndex(dict);
  let ret = "";
  for (const [name, ast] of dict) {
    ret += `${name} = ${ppFromDict(ast, canonIdx, true)}\n`;
  }
  return ret;
}

/* =========================== TOPO / INLINE ========================= */

function topoSort(adj) {
  const indeg = new Map();
  const nodes = Array.from(adj.keys());
  for (const n of nodes) indeg.set(n, 0);
  for (const [n, nbrs] of adj) for (const m of nbrs) indeg.set(m, (indeg.get(m) ?? 0) + 1);
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
    const remaining = nodes.filter(n => indeg.get(n) > 0);
    throw new SyntaxError(`cyclic definitions among: ${remaining.join(', ')}`);
  }
  return order;
}

function inlineGlobals(node, built, bound = new Set()) {
  switch (node.type) {
  case 'VAR': {
    if (bound.has(node.name)) return { type:'VAR', name: node.name };
    const hit = built.get(node.name);
    if (hit) return cloneAST(hit);
    return { type:'VAR', name: node.name };
  }
  case 'LAM': {
    const b2 = new Set(bound); b2.add(node.param);
    return { type:'LAM', param: node.param, body: inlineGlobals(node.body, built, b2) };
  }
  case 'APP':
    return { type:'APP',
             func: inlineGlobals(node.func, built, bound),
             arg:  inlineGlobals(node.arg,  built, bound) };
  default:
    throw new Error('Unknown node.type in inlineGlobals: ' + node.type);
  }
}

function buildDictTopo(defs, globals) {
  const raw = new Map();
  for (const d of defs) {
    if (raw.has(d.name)) throw new SyntaxError(`duplicate definition '${d.name}' at ${d.line}:${d.col}`);
    raw.set(d.name, d.ast);
  }

  for (const {name, ast, line, col} of defs) {
    const fv = getFreeVars(ast);
    for (const v of fv) if (!globals.has(v)) {
      throw new SyntaxError(`'${name}' uses unknown '${v}' at ${line}:${col}`);
    }
  }

  const adj = new Map();
  for (const {name} of defs) adj.set(name, new Set());
  for (const {name, ast} of defs) {
    const fv = getFreeVars(ast);
    for (const v of fv) if (globals.has(v)) adj.get(v).add(name);
  }
  const order = topoSort(adj);

  const final = new Map();
  const seen = new Map(); // canon -> name
  for (const name of order) {
    const expanded = inlineGlobals(raw.get(name), final);
    const canon = canonAlpha(expanded);
    const dup = seen.get(canon);
    if (dup) throw new SyntaxError(`'${name}' duplicates value of '${dup}'`);
    seen.set(canon, name);
    final.set(name, expanded);
  }
  return final;
}

function cloneAST(node) {
  switch (node.type) {
  case 'VAR': return { type:'VAR', name: node.name, canon: node.canon };
  case 'LAM': return { type:'LAM', param: node.param, body: cloneAST(node.body), canon: node.canon };
  case 'APP': return { type:'APP', func: cloneAST(node.func), arg: cloneAST(node.arg), canon: node.canon };
  }
}

























/* ====================== DAG packing helpers ======================= */
const TAG_APP = 0, TAG_VAR = 1, TAG_LAM = 2;
const NOIDX   = 0x3FFFFFFF >>> 0;          // 30-bit "none"
const A_MASK  = 0x3FFFFFFF >>> 0;
const B_MASK  = 0x3FFFFFFF >>> 0;

function packWords(tag, aIdx, bIdx) {
  const lo2 = (tag & 0b11) >>> 0;
  const hi2 = ((tag >>> 2) & 0b11) >>> 0;
  const A = ((lo2 << 30) >>> 0) | (aIdx & A_MASK);
  const B = ((hi2 << 30) >>> 0) | (bIdx & B_MASK);
  return [A >>> 0, B >>> 0];
}
function getTag(A, B) { return (((B >>> 30) << 2) | (A >>> 30)) >>> 0; }
function getA(A)      { return (A & A_MASK) >>> 0; }
function getB(B)      { return (B & B_MASK) >>> 0; }

/* ============================ LIFT ================================ */
/** Lift a dict-AST (already inlined, no free globals) to a hash-consed DAG. 
 *  Returns { A, B, rootIdx } where:
 *   - APP: A=func, B=arg
 *   - LAM: A=body, B=NOIDX
 *   - VAR: A=NOIDX, B=deBruijnIndex (0 = nearest binder)
 */
export function liftToArrays(rootAst) {
  const A = [], B = [];
  const cons = new Map(); // "tag|a|b" -> id

  function mk(tag, a, b) {
    const key = `${tag}|${a}|${b}`;
    let id = cons.get(key);
    if (id !== undefined) return id;
    id = A.length;
    const [wa, wb] = packWords(tag, a >>> 0, b >>> 0);
    A.push(wa); B.push(wb);
    cons.set(key, id);
    return id;
  }

  // bound stack: [innermost, ..., outermost] of param names
  function go(node, bound) {
    switch (node.type) {
    case 'VAR': {
      const k = bound.indexOf(node.name);
      if (k < 0) throw new Error(`free variable '${node.name}' in lift`);
      return mk(TAG_VAR, NOIDX, k >>> 0);
    }
    case 'LAM': {
      // push new binder at the *front* so VAR 0 refers to it
      const bodyId = go(node.body, [node.param, ...bound]);
      return mk(TAG_LAM, bodyId >>> 0, NOIDX);
    }
    case 'APP': {
      const f = go(node.func, bound);
      const a = go(node.arg,  bound);
      return mk(TAG_APP, f >>> 0, a >>> 0);
    }
    default:
      throw new TypeError(`lift: unknown node.type ${node.type}`);
    }
  }

  const rootIdx = go(rootAst, []);
  return { A, B, rootIdx };
}

/* ============================= DROP =============================== */
/** Reconstruct a dict-AST (pretty, with synthetic names x0,x1,foo) from DAG. */
export function dropFromArrays(st) {
  const { A, B, rootIdx } = st;

  // Build a tree for pretty printing; we don't attempt to preserve DAG sharing
  function go(id, bound) {
    const tag = getTag(A[id], B[id]);
    if (tag === TAG_VAR) {
      const k = getB(B[id]) | 0; // 0..N
      const name = bound[k];
      if (name === undefined) throw new Error(`drop: VAR ${k} out of scope at node ${id}`);
      return { type: 'VAR', name };
    }
    if (tag === TAG_LAM) {
      const bodyId = getA(A[id]) | 0;
      // synthesize a fresh param name; innermost is at index 0
      const param = `x${bound.length}`;
      const body  = go(bodyId, [param, ...bound]);
      return { type: 'LAM', param, body };
    }
    if (tag === TAG_APP) {
      const f = go(getA(A[id]) | 0, bound);
      const a = go(getB(B[id]) | 0, bound);
      return { type: 'APP', func: f, arg: a };
    }
    throw new Error(`drop: unknown tag ${tag} at node ${id}`);
  }

  return go(rootIdx, []);
}



