const $ = (id) => (document.getElementById(id));

export function init() {
  let st = null;
  const dump = () => { if (st) console.log(debugDump(st)); };
  const run = () => {
    try {
      $('out').style.color = ''; // reset color
      const dict = parse($('src').value);
      $('out').textContent = showReconstructed(dict);
      // after parse():

      // lift the 'out' symbol (or any you choose)
      const packed = liftFromDict(dict);
      initFreeList(packed, Math.max(16, packed.A.length)); 
      st = packed;
      initQueues(st);
      $('out').textContent = ppAst(drop(st));
      console.log('nodes:', st.A.length, 'root:', st.rootIdx);
      console.log(debugDump(st));
      
      
    } catch (e) {
      $('out').style.color = 'red';
      $('out').textContent = e.message;
    }
  };
  const loadExample = () => {
    $('src').value = String.raw`cons = [\a b f.f a b];
true = [\a b.a];
false = [\a b.b];
out = [cons true false];`
  };
  $('run').addEventListener('click', run );
  $('loadExample').addEventListener('click', loadExample );
  window.addEventListener('keydown', (e) => {
	  if ((e.ctrlKey) && e.key === 'Enter') run();
  });
  $('tick').addEventListener('click', () => {
    if (!st) return;
    tick(st);
    $('out').textContent = ppAst(drop(st)) + "\n\n" + debugDump(st); 
    dump();
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

// cons = [\a b f.f a b];
// true = [\a b.a];
// false = [\a b.b];
// out = [cons true false];

//   ===>

// 0: APP  parent=0 func=1 arg=13
// 1: APP  parent=0 func=2 arg=10
// 2: LAM  parent=1 body=3
// 3: LAM  parent=2 body=4
// 4: LAM  parent=3 body=5
// 5: APP  parent=4 func=6 arg=9
// 6: APP  parent=5 func=7 arg=8
// 7: VAR  parent=6 lambda=4
// 8: VAR  parent=6 lambda=2
// 9: VAR  parent=5 lambda=3
// 10: LAM  parent=1 body=11
// 11: LAM  parent=10 body=12
// 12: VAR  parent=11 lambda=10
// 13: LAM  parent=0 body=14
// 14: LAM  parent=13 body=15
// 15: VAR  parent=14 lambda=14

// ---- LIFT: AST -> packed (A,B) with 16 tags ----


// Tag constants (0..15); reusing yours:
const TAG_APP = 0, TAG_VAR = 1, TAG_LAM = 2, TAG_COPY = 3;
const TAG_FREE = 15; // tombstone / freelist node
const NOIDX = 0x3FFFFFFF >>> 0; // 30-bit "none"
const A_MASK = 0x3FFFFFFF >>> 0, B_MASK = 0x3FFFFFFF >>> 0;

// (reused) pack/unpack helpers
function packWords(tag, aIdx, bIdx) {
  // A: parent (low 30) + tag lo2 ; B: main field (low 30) + tag hi2
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
const getA = (A) => (A & A_MASK) >>> 0; // parent
const getB = (B) => (B & B_MASK) >>> 0; // func/body/binding-lambda
function setAWord(Aword, newA) { return (((Aword >>> 30) << 30) >>> 0) | (newA & A_MASK); }
function setBWord(Bword, newB) { return (((Bword >>> 30) << 30) >>> 0) | (newB & B_MASK); }

// ---- LIFT: AST -> packed (A,B,C) ----
function lift(root) {
  const A = [], B = [], C = []; // C holds APP.arg (NOIDX for others)

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

  const emit = (tag, parentIdx = NOIDX, mainIdx = NOIDX, argIdx = NOIDX) => {
    const [aw, bw] = packWords(tag, parentIdx, mainIdx);
    const idx = A.length;
    A.push(aw); B.push(bw); C.push(argIdx >>> 0);
    return idx >>> 0;
  };

  function walk(node, parentIdx = NOIDX) {
    switch (node.type) {
    case 'APP': {
      // APP: A=parent, B=func, C=arg
      const self = emit(TAG_APP, parentIdx, NOIDX, NOIDX);
      if (parentIdx === NOIDX) A[self] = setAWord(A[self], self);   // parent=self for root
      const f = walk(node.func, self);
      const x = walk(node.arg,  self);
      B[self] = setBWord(B[self], f);
      C[self] = x >>> 0;
      return self;
    }
    case 'LAM': {
      // LAM: A=parent, B=body, C=NOIDX
      const self = emit(TAG_LAM, parentIdx, NOIDX, NOIDX);
      if (parentIdx === NOIDX) A[self] = setAWord(A[self], self);   // parent=self for root
      pushBind(node.param, self);
      const body = walk(node.body, self);
      B[self] = setBWord(B[self], body);
      popBind(node.param);
      return self;
    }
    case 'VAR': {
      // VAR: A=parent, B=binding lambda, C=NOIDX
      const self = emit(TAG_VAR, parentIdx, boundIdx(node.name), NOIDX);
      if (parentIdx === NOIDX) A[self] = setAWord(A[self], self);   // parent=self for root
      return self;
    }
    default:
      throw new Error('Unknown node.type ' + node.type);
    }
  }

  const rootIdx = walk(root, NOIDX);
  // return plain Arrays so we can grow them later if needed
  return { A, B, C, rootIdx, NOIDX };
}

// Convenience: pick an entry from your dict (default 'out'), lift it
function liftFromDict(dict, entry = 'out') {
  const ast = dict.get(entry);
  if (!ast) throw new Error(`No entry named '${entry}'`);
  return lift(ast);
}

function debugDump(packed, max = 100) {
  const { A, B, C } = packed;
  const n = Math.min(A.length, max);
  let s = '';
  for (let i = 0; i < n; i++) {
    const t = getTag(A[i], B[i]);
    const a = getA(A[i]); // parent
    const b = getB(B[i]); // func/body/lambda
    const c = C[i];
    if (t === TAG_APP) s += `${i}: APP  parent=${a} func=${b} arg=${(C && C[i]!==undefined)?C[i]:NOIDX}\n`;
    else if (t === TAG_VAR) s += `${i}: VAR  parent=${a} lambda=${b}\n`;
    else if (t === TAG_LAM) s += `${i}: LAM  parent=${a} body=${b}\n`;
    else if (t === TAG_COPY) s += `${i}: CPY  parent=${a} copy=${b}\n`;
    else if (t === TAG_FREE) s += `${i}: FRE  next${c}\n`;
    else s += `${i}: TAG${t} A=${a} B=${b}\n`;
  }
  return s;
}

function initQueues(st){
  st.Q_beta = [];
  st.Q_copy = [];
  st.Q_var  = [];
  
  
  const N = st.A.length;
  for (let i=0;i<N;i++){
    const t = getTag(st.A[i], st.B[i]);
    if (t === TAG_LAM){
      // If parent is APP and this lambda is its func child => that APP is a redex
      const par = getA(st.A[i]);
      if (par !== NOIDX && getTag(st.A[par], st.B[par]) === TAG_APP && getB(st.B[par]) === i){
        st.Q_beta.push(i);
      }
    } else if (t === TAG_VAR){
      // VARs whose binding lambda is currently the func of some APP (for substitution)
      const lam = getB(st.B[i]);
      if (lam !== NOIDX){
        const par = getA(st.A[lam]);
        if (par !== NOIDX && getTag(st.A[par], st.B[par]) === TAG_APP && getB(st.B[par]) === lam){
          st.Q_var.push(i);
        }
      }
    }
  }
}


function allocNode(st, tag, parentIdx, mainIdx, argIdx = NOIDX){
  const NO = NOIDX;

  // ensure freelist exists
  if (st.freeHead === undefined){ st.freeHead = NO; st.freeTail = NO; }

  // empty? grow by ~doubling current capacity
  if (st.freeHead === NO){
    const toAdd = Math.max(1, st.A.length || 1); // "double"
    growArena(st, toAdd);
  }

  // pop from head (contiguous ascending indices if the chunk was linked in order)
  const idx = st.freeHead >>> 0;
  st.freeHead = st.C[idx];
  if (st.freeHead === NO) st.freeTail = NO; // list became empty

  // place the node
  [st.A[idx], st.B[idx]] = packWords(tag, parentIdx, mainIdx);
  st.C[idx] = argIdx >>> 0;
  return idx;
}

function initFreeList(st, extra = 64){
  st.freeHead = NOIDX; st.freeTail = NOIDX;
  growArena(st, Math.max(1, extra));
}

function growArena(st, addCount){
  const NO = NOIDX;
  const start = st.A.length >>> 0;
  // append `addCount` FREE nodes, linking C[i] -> next
  for (let i = 0; i < addCount; i++){
    const idx = start + i;
    const [aw, bw] = packWords(TAG_FREE, NO, NO);
    st.A.push(aw); st.B.push(bw); st.C.push(NO);
  }
  // link the newly appended range
  for (let i = start; i < start + addCount - 1; i++) st.C[i] = (i + 1) >>> 0;
  const first = start, last = (start + addCount - 1) >>> 0;

  // splice into existing queue
  if (st.freeTail !== NO){
    st.C[st.freeTail] = first;
    st.freeTail = last;
  } else {
    st.freeHead = first;
    st.freeTail = last;
  }
}


// keep for later GC:
function freeNode(st, idx){
  const NO = NOIDX;
  [st.A[idx], st.B[idx]] = packWords(TAG_FREE, NO, NO);
  st.C[idx] = NO;
  if (st.freeTail !== NO){
    st.C[st.freeTail] = idx;
    st.freeTail = idx;
  } else {
    st.freeHead = st.freeTail = idx;
  }
}

function tick(st){
  let newQ_beta = [];
  let newQ_copy = [];
  let newQ_var = [];
  //First eliminate vars, so we don't have to worry about moving them later.
  if (!st.varOcc) st.varOcc = Object.create(null);     // lamIdx -> next occurrence #
  for (let i=0;i<st.Q_var.length;++i){
    const ind = st.Q_var[i];

    // ind is a VAR; its binder is a LAM whose parent is an APP (redex)
    const lam = getB(st.B[ind]);
    const app = getA(st.A[lam]);
    const arg = st.C[app];

    // Stamp this var's COPY with an occurrence number k for its binder ?.
    // k = 0 means "use the most recent ? copy" (alias-list head),
    // k = 1 means "one step down", etc.
    const k = (st.varOcc[lam] | 0);
    st.varOcc[lam] = k + 1;

    [st.A[ind], st.B[ind]] = packWords(TAG_COPY, getA(st.A[ind]), arg);
    st.C[ind] = k >>> 0;                                // <- store occurrence in VAR.C (now COPY.C)
    newQ_copy.push(ind);
    // console.log("var:", ind, "lam:", lam, "occ:", k);
  }
  for (let i = 0; i < st.Q_beta.length; ++i) {
    const fnc = st.Q_beta[i];
    const app = getA(st.A[fnc]);
    const bdy = getB(st.B[fnc]);
    st.C[app] = NOIDX;                              // <-- clear k slot
    [st.A[app], st.B[app]] = packWords(TAG_COPY, getA(st.A[app]), bdy);
    newQ_copy.push(app);
  }
  for(let i=0;i<st.Q_copy.length;++i){
    let ind = st.Q_copy[i];                         
    const src    = getB(st.B[ind]);
    const t      = getTag(st.A[src], st.B[src]);
    const parent = getA(st.A[ind]);

    if (t === TAG_APP){
      const f = getB(st.B[src]), x = st.C[src];
      const kprop = st.C[ind];                        // <-- capture before clearing

      [st.A[ind], st.B[ind]] = packWords(TAG_APP, parent, 0);
      st.C[ind] = NOIDX;                              // this C will become the APP.arg field

      const fcpy = allocNode(st, TAG_COPY, ind, f, NOIDX);
      const xcpy = allocNode(st, TAG_COPY, ind, x, NOIDX);

      st.C[fcpy] = kprop;                             // <-- propagate k
      st.C[xcpy] = kprop;                             // <-- propagate k

      st.B[ind] = setBWord(st.B[ind], fcpy);
      st.C[ind] = xcpy;

      newQ_copy.push(fcpy, xcpy);
    }
    else if (t === TAG_LAM){
      const body = getB(st.B[src]);
      [st.A[ind], st.B[ind]] = packWords(TAG_LAM, parent, 0);
      if( st.C[src] === NOIDX )
        st.C[src] = ind;                               // use LAM.C as redirection
      else{
        st.C[ind] = st.C[src];
        st.C[src] = ind;
      }

      // body := COPY(bodySrc)
      const bcpy = allocNode(st, TAG_COPY, ind, body, NOIDX);
      st.C[bcpy] = NOIDX;
      st.B[ind] = setBWord(st.B[ind], bcpy);
      newQ_copy.push(bcpy);

      // if this LAM sits in APP.func, it's a fresh redex
      if (getTag(st.A[parent], st.B[parent]) === TAG_APP && getB(st.B[parent]) === ind){
        newQ_beta.push(ind);
      }
    }
    else if (t === TAG_VAR){
      const srcLam = getB(st.B[src]);    // original binder ?
      let mapped = srcLam;

      // how far down the alias chain to go; default 0 (head)
      let k = st.C[ind];
      if (k === NOIDX) k = 0;

      // alias chain: C[srcLam] -> ?copy0 (newest) -> ?copy1 -> ...
      let a = st.C[srcLam];
      while (k > 0 && a !== NOIDX){ a = st.C[a]; k--; }
      if (a !== NOIDX) mapped = a;       // bind to that ? copy; else fall back to original

      [st.A[ind], st.B[ind]] = packWords(TAG_VAR, parent, mapped);
      st.C[ind] = NOIDX;                 // VAR.C no longer needed
    }
    else if (t === TAG_COPY){
      const next = getB(st.B[src]);
      if (st.C[ind] === NOIDX) st.C[ind] = st.C[src]; // preserve k
      st.B[ind] = setBWord(st.B[ind], next);
      newQ_copy.push(ind);
    }
    console.log("copy: " + ind);
  }
  st.Q_beta = newQ_beta;
  st.Q_copy = newQ_copy;
  st.Q_var = newQ_var;
}
function buildLamAliasRep(st){
  const NO = NOIDX;
  const n = st.A.length;

  // mark LAMs
  const isLam = new Uint8Array(n);
  for (let i = 0; i < n; i++) if (getTag(st.A[i], st.B[i]) === TAG_LAM) isLam[i] = 1;

  // tiny union-find over LAM indices connected by C-edges
  const parent = new Uint32Array(n);
  for (let i = 0; i < n; i++) parent[i] = i;
  const find  = (x) => { while (parent[x] !== x) { parent[x] = parent[parent[x]]; x = parent[x]; } return x; };
  const union = (a,b) => { const ra = find(a), rb = find(b); if (ra !== rb) parent[rb] = ra; };

  for (let i = 0; i < n; i++){
    if (!isLam[i]) continue;
    const j = st.C[i];
    if (j !== NO && isLam[j]) union(i, j); // undirected: i <-> j
  }

  const rep = new Map();
  for (let i = 0; i < n; i++) if (isLam[i]) rep.set(i, find(i));
  return rep;
}


function drop(st, idx = st.rootIdx){
  const NO = NOIDX;
  let gensym = 0;
  const fresh = () => 'x' + (gensym++);

  const aliasRep   = buildLamAliasRep(st);     // lamIdx -> component representative
  const nameByRep  = new Map();                // rep -> 'xN'
  const nameForLam = (lamIdx) => {
    if (lamIdx === NO) return '--';
    const rep = aliasRep.get(lamIdx) ?? lamIdx;
    let nm = nameByRep.get(rep);
    if (!nm) { nm = fresh(); nameByRep.set(rep, nm); }
    return nm;
  };

  function toAst(i){
    const tag = getTag(st.A[i], st.B[i]);

    if (tag === TAG_COPY) {
      return toAst(getB(st.B[i]));             // peel COPY
    }
    if (tag === TAG_APP) {
      const f = getB(st.B[i]), x = st.C[i];
      return { type:'APP', func: toAst(f), arg: toAst(x) };
    }
    if (tag === TAG_LAM) {
      const body = getB(st.B[i]);
      const name = nameForLam(i);
      return { type:'LAM', param: name, body: toAst(body) };
    }
    if (tag === TAG_VAR) {
      const lam = getB(st.B[i]);               // binder (original or any alias)
      return { type:'VAR', name: nameForLam(lam) };
    }
    if (tag === TAG_FREE) return { type:'VAR', name:'?' };
    return { type:'VAR', name:'?' };
  }

  return toAst(idx);
}
function ppAst(node, wrapApps = true){
  switch(node.type){
  case 'VAR': return node.name;

  case 'LAM': {
    const params = [];
    let cur = node;
    while (cur.type === 'LAM'){ params.push(cur.param); cur = cur.body; }
    return `[\\${params.join(' ')}.${ppAst(cur, false)}]`;
  }

  case 'APP': {
    const parts = [];
    let cur = node;
    while (cur.type === 'APP'){ parts.unshift(ppAst(cur.arg)); cur = cur.func; }
    parts.unshift(ppAst(cur));
    const s = parts.join(' ');
    return wrapApps ? `[${s}]` : s;
  }
  }
}
