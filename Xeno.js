const $ = (id) => (document.getElementById(id));

export function init() {
  let st = null;
  let dict = null;
  const dump = () => { if (st) console.log(debugDump(st)); };
  const run = () => {
    try {
      $('out').style.color = ''; // reset color
      dict = parse($('src').value);
      st = lift(dict.get('out'));
      st.canonIdx = makeCanonIndex(dict);
      $('out').textContent =
        showReconstructed(dict);
    } catch (e) {
      $('out').style.color = 'red';
      $('out').textContent = e.message;
    }
    renderState();
  };


  
  
  function renderState(){
    if(st)
      $('out').textContent = printDag(st);
    else
      $('out').textContent = showReconstructed(dict);
  }

  const tfunc = () => {
    if( !st )
      run();
    tickDAG(st);
    renderState();
  };
  const tfuncNF = () => {
    if( !st )
      run();
    tickNF(dict);
    renderState();
  };
  const loadExample = () => {
    $('src').value = String.raw`id = [\a.a]

// pairs & booleans
cons  = [\a b f.f a b];
true  = [\a b.a];
false = [\a b.b];

// pair selectors
head = [\p.p [\a b.a]];
tail = [\p.p [\a b.b]];

// empty?  (nil -> true, cons -> false)
isnil = [\x. x [\a b. [\z. false]]  true];

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
    if ((e.ctrlKey) && e.key === ',') tfunc();
    if ((e.ctrlKey) && e.key === '.') tfuncNF();
  });
  $('tick').addEventListener('click', tfunc);
  $('tickNF').addEventListener('click', tfuncNF);
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






function tick(st){
  const t = st.get('out');
  const r = step(t);
  if (r) st.set('out', r);
  return !!r;
}

// Leftmost-outermost (normal order). Do not reduce under lambdas.
function step(t){
  if (t.type === 'APP'){
    const f = t.func;
    if (f.type === 'LAM') return beta(f.param, f.body, t.arg); // (?x.M) N  ?  M[x:=N]
    const f2 = step(f);
    if (f2) return { type:'APP', func:f2, arg:t.arg };
    return null; // don't touch the argument (normal order)
  }
  return null; // VAR or LAM: no step
}

// B (capture-avoiding): substitute v for free x in m.
function beta(x, m, v){
  return subst(m, x, v);
}

// Capture-avoiding substitution M[x:=V]
function subst(m, x, v){
  switch(m.type){
  case 'VAR':
    return (m.name === x) ? clone(v) : m;

  case 'APP':
    return { type:'APP',
             func: subst(m.func, x, v),
             arg:  subst(m.arg,  x, v) };

  case 'LAM': {
    if (m.param === x) return m; // x is bound here; stop
    const fvV = freeVars(v);
    if (fvV.has(m.param)) {
      // avoid capture: a-rename binder first
      const freshName = fresh(m.param, unionSets(fvV, allNames(m.body), new Set([x])));
      const bodyRenamed = alphaRenameBound(m.body, m.param, freshName);
      return { type:'LAM',
               param: freshName,
               body:  subst(bodyRenamed, x, v) };
    } else {
      return { type:'LAM', param: m.param, body: subst(m.body, x, v) };
    }
  }
  }
}

// --- helpers ---

function clone(t){
  switch(t.type){
  case 'VAR': return { type:'VAR', name:t.name };
  case 'LAM': return { type:'LAM', param:t.param, body: clone(t.body) };
  case 'APP': return { type:'APP', func: clone(t.func), arg: clone(t.arg) };
  }
}

function freeVars(t, bound = new Set()){
  switch(t.type){
  case 'VAR': return bound.has(t.name) ? new Set() : new Set([t.name]);
  case 'LAM': { const b = new Set(bound); b.add(t.param); return freeVars(t.body, b); }
  case 'APP': {
    const L = freeVars(t.func, bound), R = freeVars(t.arg, bound);
    for (const x of R) L.add(x); return L;
  }
  }
}

function allNames(t){
  const s = new Set();
  (function go(n){
    if (!n) return;
    if (n.type === 'VAR') s.add(n.name);
    else if (n.type === 'LAM'){ s.add(n.param); go(n.body); }
    else if (n.type === 'APP'){ go(n.func); go(n.arg); }
  })(t);
  return s;
}

function unionSets(...sets){
  const u = new Set();
  for (const S of sets) for (const x of S) u.add(x);
  return u;
}

function fresh(base, avoid){
  let i = 0, name = base;
  while (avoid.has(name)) name = base + (++i);
  return name;
}

// a-rename exactly the occurrences bound by the *nearest* binder named oldName
function alphaRenameBound(t, oldName, newName, shadow = 0){
  switch(t.type){
  case 'VAR':
    return (shadow === 0 && t.name === oldName)
      ? { type:'VAR', name:newName }
    : t;

  case 'LAM': {
    if (t.param === oldName){
      // inner binder shadows the one we're renaming; increase shadow depth
      return { type:'LAM', param: oldName,
               body: alphaRenameBound(t.body, oldName, newName, shadow+1) };
    }
    if (shadow === 0){
      // still under the binder we renamed; keep walking
      return { type:'LAM', param: t.param,
               body: alphaRenameBound(t.body, oldName, newName, shadow) };
    } else {
      // under a shadowing binder; do not rename inside
      return { type:'LAM', param: t.param, body: t.body };
    }
  }

  case 'APP':
    return { type:'APP',
             func: alphaRenameBound(t.func, oldName, newName, shadow),
             arg:  alphaRenameBound(t.arg,  oldName, newName, shadow) };
  }
}


function stepNF(t){
  if (t.type === 'APP'){
    if (t.func.type === 'LAM') return beta(t.func.param, t.func.body, t.arg);
    const f2 = stepNF(t.func); if (f2) return { type:'APP', func:f2, arg:t.arg };
    const a2 = stepNF(t.arg);  if (a2) return { type:'APP', func:t.func, arg:a2 };
    return null;
  }
  if (t.type === 'LAM'){
    const b2 = stepNF(t.body); if (b2) return { type:'LAM', param:t.param, body:b2 };
    return null;
  }
  return null; // VAR
}





function tickNF(st){
  const t = st.get('out');
  const r = stepNF(t);
  if (r) st.set('out', r);
  return !!r;
}

























/* ======================= DAG core ======================= */
const TAG_APP = 0, TAG_VAR = 1, TAG_LAM = 2;
function tagLabel(t){ return t===TAG_APP ? 'APP' : t===TAG_VAR ? 'VAR' : t===TAG_LAM ? 'LAM' : `???(${t})`; }
const NOIDX   = 0x3FFFFFFF >>> 0;          // 30-bit "none"
const A_MASK  = 0x3FFFFFFF >>> 0;
const B_MASK  = 0x3FFFFFFF >>> 0;

function packWords(tag, aIdx, bIdx){
  const lo2 = (tag & 0b11) >>> 0;
  const hi2 = ((tag >>> 2) & 0b11) >>> 0;
  const A = ((lo2 << 30) >>> 0) | (aIdx & A_MASK);
  const B = ((hi2 << 30) >>> 0) | (bIdx & B_MASK);
  return [A >>> 0, B >>> 0];
}
function getTag(A, B){ return (((B >>> 30) << 2) | (A >>> 30)) >>> 0; }
function getA(A){ return (A & A_MASK) >>> 0; }
function getB(B){ return (B & B_MASK) >>> 0; }

const keyAB = (A,B) => (BigInt(A) << 32n) | BigInt(B);

/** Hash-consed constructor: returns existing id if present, else allocates. */
function mk(st, tag, a, b){
  const [Aw, Bw] = packWords(tag, a >>> 0, b >>> 0);
  const k = keyAB(Aw, Bw);
  const hit = st.cons.get(k);
  if (hit !== undefined) return hit;
  const id = st.A.length;
  st.A.push(Aw); st.B.push(Bw);
  st.cons.set(k, id);
  return id;
}

/* =================== LIFT: AST -> DAG (DB) =================== */
/** Named AST -> DAG with de Bruijn indices. Produces st = {A,B,rootIdx,cons}. */
function lift(ast){
  const st = { A: [], B: [], rootIdx: 0, cons: new Map(), fwd: [] };

  function go(node, env /* array of binder names, [innermost,...] */){
    switch(node.type){
    case 'VAR': {
      const k = env.indexOf(node.name);
      if (k < 0) throw new Error("lift: free variable " + node.name);
      return mk(st, TAG_VAR, NOIDX, k >>> 0);
    }
    case 'LAM': {
      const body = go(node.body, [node.param, ...env]);
      return mk(st, TAG_LAM, body, NOIDX);
    }
    case 'APP': {
      const f = go(node.func, env);
      const a = go(node.arg,  env);
      return mk(st, TAG_APP, f, a);
    }
    default:
      throw new Error("lift: unknown node.type " + node.type);
    }
  }

  st.rootIdx = go(ast, []);
  return st;
}
function find(st, i){
  let j = i;
  while (st.fwd[j] !== undefined) j = st.fwd[j];
  // path compression
  while (i !== j) { const n = st.fwd[i]; st.fwd[i] = j; i = n; }
  return j;
}
function redirect(st, from, to){
  from = find(st, from);
  to   = find(st, to);
  if (from === to) return false;   // <- no-op
  st.fwd[from] = to;
  return true;
}

// Always canonicalize children before hash-consing:
function mkF(st, tag, a, b){
  a = find(st, a); b = find(st, b);
  return mk(st, tag, a, b); // your mk() from the code
}
// tiny ctors
const mkVAR = (st,k)     => mkF(st, TAG_VAR, NOIDX, k>>>0);
const mkLAM = (st,body)  => mkF(st, TAG_LAM, body, NOIDX);
const mkAPP = (st,f,a)   => mkF(st, TAG_APP, f, a);

// Safe accessors (always through find)
function tagOf(st, i){ i = find(st, i); return getTag(st.A[i], st.B[i]); }
function aOf  (st, i){ i = find(st, i); return getA  (st.A[i]); }
function bOf  (st, i){ i = find(st, i); return getB  (st.B[i]); }




function printDag(st){
  const out = [];
  for (let i = 0; i < st.A.length; i++) {
    const A = st.A[i], B = st.B[i];
    const t = getTag(A, B), a = getA(A), b = getB(B);
    let line = `${i}: ${tagLabel(t)} `;
    if (t === TAG_APP)      line += `f=${a} a=${b}`;
    else if (t === TAG_LAM) line += `body=${a}`;
    else if (t === TAG_VAR) line += `k=${b}`;
    else                    line += `a=${a} b=${b}`;
    if (st.fwd[i] !== undefined) line += `  fwd=${st.fwd[i]}`;
    out.push(line);
  }
  return out.join('\n');
}






/* =================== DAG REDUCTION =================== */

/**
 * Single step of normal-order reduction on the DAG.
 * Finds leftmost-outermost redex and performs in-place beta reduction.
 * Returns true if a step was taken, false if in normal form.
 */
function tickDAG(st) {
  st.rootIdx = find(st, st.rootIdx);
  const result = stepDAG(st, st.rootIdx);
  if (result !== null) {
    redirect(st, st.rootIdx, result);
    st.rootIdx = result;
    return true;
  }
  return false;
}

/**
 * Normal-order step: leftmost-outermost, no reduction under lambdas.
 * Returns new node index if step taken, null otherwise.
 */
function stepDAG(st, idx) {
  idx = find(st, idx);
  const tag = tagOf(st, idx);
  
  if (tag === TAG_APP) {
    const funcIdx = aOf(st, idx);
    const argIdx = bOf(st, idx);
    const funcTag = tagOf(st, funcIdx);
    
    // Beta redex: (?.body) arg
    if (funcTag === TAG_LAM) {
      const bodyIdx = aOf(st, funcIdx);
      return betaDAG(st, bodyIdx, argIdx);
    }
    
    // Try to reduce function position
    const funcReduced = stepDAG(st, funcIdx);
    if (funcReduced !== null) {
      return mkAPP(st, funcReduced, argIdx);
    }
    
    // Normal order: don't reduce argument
    return null;
  }
  
  // VAR or LAM: no reduction
  return null;
}

/**
 * Beta reduction: substitute arg for de Bruijn index 0 in body.
 * body uses indices [0,1,2,...] where 0 is the bound variable.
 * After substitution, all free indices must be decremented.
 */
function betaDAG(st, bodyIdx, argIdx) {
  return substDAG(st, bodyIdx, 0, argIdx, 0);
}

/**
 * Capture-avoiding substitution in DAG with de Bruijn indices.
 * 
 * @param {Object} st - The DAG state
 * @param {number} termIdx - The term to substitute into
 * @param {number} target - The de Bruijn level to replace (relative to depth)
 * @param {number} valueIdx - The value to substitute
 * @param {number} depth - Current binding depth (how many ?s deep we are)
 * @returns {number} New node index after substitution
 */
function substDAG(st, termIdx, target, valueIdx, depth) {
  termIdx = find(st, termIdx);
  const tag = tagOf(st, termIdx);
  
  if (tag === TAG_VAR) {
    const k = bOf(st, termIdx);
    
    if (k === target + depth) {
      // This is the variable we're substituting
      // Shift the value up by 'depth' to account for the lambdas we're under
      return shiftDAG(st, valueIdx, depth, 0);
    } else if (k > target + depth) {
      // Free variable that needs to be decremented (since we're removing a binder)
      return mkVAR(st, k - 1);
    } else {
      // Variable bound by a lambda we're inside - unchanged
      return termIdx;
    }
  }
  
  if (tag === TAG_LAM) {
    const bodyIdx = aOf(st, termIdx);
    // Under a lambda, increment depth
    const newBody = substDAG(st, bodyIdx, target, valueIdx, depth + 1);
    return mkLAM(st, newBody);
  }
  
  if (tag === TAG_APP) {
    const funcIdx = aOf(st, termIdx);
    const argIdx = bOf(st, termIdx);
    const newFunc = substDAG(st, funcIdx, target, valueIdx, depth);
    const newArg = substDAG(st, argIdx, target, valueIdx, depth);
    return mkAPP(st, newFunc, newArg);
  }
  
  return termIdx;
}

/**
 * Shift de Bruijn indices in a term.
 * Adds 'amount' to all indices >= cutoff.
 * Used when moving a term under additional lambda binders.
 * 
 * @param {Object} st - The DAG state
 * @param {number} termIdx - The term to shift
 * @param {number} amount - How much to shift by
 * @param {number} cutoff - Only shift indices >= this value
 * @returns {number} New node index with shifted indices
 */
function shiftDAG(st, termIdx, amount, cutoff) {
  if (amount === 0) return termIdx;
  
  termIdx = find(st, termIdx);
  const tag = tagOf(st, termIdx);
  
  if (tag === TAG_VAR) {
    const k = bOf(st, termIdx);
    if (k >= cutoff) {
      return mkVAR(st, k + amount);
    }
    return termIdx;
  }
  
  if (tag === TAG_LAM) {
    const bodyIdx = aOf(st, termIdx);
    // Under a lambda, increment the cutoff
    const newBody = shiftDAG(st, bodyIdx, amount, cutoff + 1);
    return mkLAM(st, newBody);
  }
  
  if (tag === TAG_APP) {
    const funcIdx = aOf(st, termIdx);
    const argIdx = bOf(st, termIdx);
    const newFunc = shiftDAG(st, funcIdx, amount, cutoff);
    const newArg = shiftDAG(st, argIdx, amount, cutoff);
    return mkAPP(st, newFunc, newArg);
  }
  
  return termIdx;
}

function debugDump(st) {
  return printDag(st) + `\nroot=${st.rootIdx} (canon: ${find(st, st.rootIdx)})`;
}
