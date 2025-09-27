const $ = (id) => (document.getElementById(id));

export function init() {
  let st = null;
  const dump = () => { if (st) console.log(debugDump(st)); };
  const run = () => {
    try {
      $('out').style.color = ''; // reset color
      const dict = parse($('src').value);
      const lift = liftFromDict(dict);
      // NEW: build once for pretty-printing during ticks
      const canonIdx = makeCanonIndex(dict);

      // keep runtime state + canon index
      st = initState(lift);
      st.canonIdx = canonIdx;

      $('out').textContent =
        showReconstructed(dict) +
        "\n\n-- Lifted (ASCII) --\n" +
        ppLifted(lift) +
        `\n\n(betaQ=${st.Q_beta.length}, copyQ=${st.Q_copy.length})`;
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
    const did = tick(st);
    const astNow = drop(st);
    const pretty = ppAstWithDict(astNow, st.canonIdx || makeCanonIndex(new Map()));
    $('out').textContent =
      pretty + 
      `\n\n(betaQ=${st.Q_beta.length}, copyQ=${st.Q_copy.length})` +
      (did ? '' : '\n\n<done>');
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
      const body = parseExpr();
      node = params.reverse().reduce((acc, p) => ({ type:'LAM', param:p, body:acc }), body);
    } else {
      node = parseExpr();
    }
    eat(']');
    return node;
  }
  
  const defs = []; // <-- collect first

  while (!atEnd()) {
    const id = eat('ID'); // {t:'ID', v, line, col}
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
    const fv = getFreeVars(ast);
    for (const v of fv) if (globals.has(v)) adj.get(v).add(name); // dep -> user
  }
  const order = topoSort(adj);           // now yields ["id","cons","out"]

  // build final dict with copy-on-use + alpha-eq dup check
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
  case 'VAR': 
    return { type:'VAR', name: node.name, canon: node.canon };
  case 'LAM': 
    return { type:'LAM', param: node.param, body: cloneAST(node.body), canon: node.canon };
  case 'APP': 
    return { type:'APP', func: cloneAST(node.func), arg: cloneAST(node.arg), canon: node.canon };
  }
}
























// -------- LIFTER to {APP, LAM, LEAF, COPY} --------
const N = {
  APP()                    { return { type:'APP', parent:null, slot:null, func:null, arg:null }; },
  LAM(param)               { return { type:'LAM', parent:null, slot:null, param, body:null,
                                      oneLeaf:null, copyRoot:null, useCount:0 }; },
  LEAF(parent,slot,lam)    { return { type:'LEAF', parent, slot, lam }; }, // NEW: lam backlink
  COPY(l, r)               { return { type:'COPY', left:l, right:r }; },
};
// Build a balanced fan over the given leaves; return the root (COPY or LEAF if k=1)
function buildCopyFan(leaves){
  if (leaves.length === 1) return leaves[0];
  let layer = leaves.slice();
  while (layer.length > 1){
    const next = [];
    for (let i=0;i<layer.length;i+=2){
      if (i+1 < layer.length) next.push(N.COPY(layer[i], layer[i+1]));
      else next.push(layer[i]);
    }
    layer = next;
  }
  return layer[0];
}

// Replace all free uses of `name` with LEAFs bound to `boundLam`.
function replaceUsesWithLeavesInPlace(node, name, outLeaves, parent, slot, boundLam){
  switch (node.type){
  case 'VAR':
    if (node.name === name){
      const leaf = N.LEAF(parent, slot, boundLam); // pass lam
      outLeaves.push(leaf);
      parent[slot] = leaf;
    }
    return;
  case 'APP':
    replaceUsesWithLeavesInPlace(node.func, name, outLeaves, node, 'func', boundLam);
    replaceUsesWithLeavesInPlace(node.arg,  name, outLeaves, node, 'arg',  boundLam);
    return;
  case 'LAM':
    if (node.param === name) return; // shadowed
    replaceUsesWithLeavesInPlace(node.body, name, outLeaves, node, 'body', boundLam);
    return;
  default: return;
  }
}

function liftFan(node){
  switch (node.type){
  case 'APP': {
    const app = N.APP();
    app.func = liftFan(node.func); app.func.parent = app; app.func.slot = 'func';
    app.arg  = liftFan(node.arg);  app.arg.parent  = app; app.arg.slot  = 'arg';
    return app;
  }
  case 'LAM': {
    const lam = N.LAM(node.param);
    lam.body = liftFan(node.body); lam.body.parent = lam; lam.body.slot = 'body';
    const leaves = [];
    replaceUsesWithLeavesInPlace(lam.body, lam.param, leaves, lam, 'body', lam); // pass lam
    lam.useCount = leaves.length;

    if (leaves.length === 1){
      lam.oneLeaf = leaves[0];
    } else if (leaves.length >= 2){
      const root = buildCopyFan(leaves);
      lam.copyRoot = (root.type === 'COPY') ? root : null;
      if (!lam.copyRoot) lam.oneLeaf = root;
    }
    return lam;
  }
  case 'VAR': return { ...node };
  default:    return node;
  }
}

function liftFromDict(dict, entry='out'){
  const ast = dict.get(entry);
  if (!ast) throw new Error(`No definition named '${entry}'`);
  const lifted = liftFan(ast);

  // sanity: ensure no VARs remain
  (function check(n){
    switch (n.type){
    case 'VAR': throw new Error(`Unlifted VAR '${n.name}' remains`);
    case 'APP': check(n.func); check(n.arg); break;
    case 'LAM': check(n.body); break;
    case 'LEAF':
    case 'COPY': break;
    }
  })(lifted);

  return lifted;
}

// simple pretty-print for the lifted form (* = LEAF)
function ppLifted(node){
  function pp(n){
    switch(n.type){
    case 'APP': {
      const parts = [];
      let cur = n;
      while (cur.type === 'APP'){ parts.unshift(pp(cur.arg)); cur = cur.func; }
      parts.unshift(pp(cur));
      return `[${parts.join(' ')}]`;
    }
    case 'LAM': {
      const ann = n.useCount===0 ? ' {k=0}' :
            n.copyRoot ? ` {k=${n.useCount}}` :
            n.oneLeaf ? ' {k=1}' : '';
      return `[\\${n.param}.${pp(n.body)}]${ann}`;
    }
    case 'LEAF': return '*';
    default: return n.type;
    }
  }
  return pp(node);
}



function setChild(parent, slot, child){
  if (parent){
    parent[slot] = child;
    if (child) { child.parent = parent; child.slot = slot; }
  }
}

function isBetaRedex(app){
  return app && app.type === 'APP' && app.func && app.func.type === 'LAM';
}

function maybePushBeta(st, node){
  if (node && node.type === 'APP' && node.func && node.func.type === 'LAM'){
    st.Q_beta.push(node);
  }
}

// Collect only the leaves that belong to this lam (thanks to leaf.lam)
function collectLeavesForLam(lam){
  const out = [];
  (function walk(n){
    switch(n.type){
    case 'LEAF': if (n.lam === lam) out.push(n); return;
    case 'APP':  walk(n.func); walk(n.arg); return;
    case 'LAM':  walk(n.body); return;
    default: return;
    }
  })(lam.body);
  return out;
}

function deliver(st, dst, node){
  if (!dst) return;

  if (dst.type === 'LEAF'){
    const p = dst.parent, s = dst.slot;
    if (p) {
      setChild(p, s, node);
      maybePushBeta(st, p);
    } else {
      // LEAF is the root: replace the whole root
      st.root = node;
      if (node) { node.parent = null; node.slot = null; }
      if (isBetaRedex(st.root)) st.Q_beta.push(st.root);
    }
    return;
  }

  if (dst.type === 'COPY'){
    st.Q_copy.push({ c: dst, v: node });
    return;
  }

  // (defensive) if dst is something unexpected, just tee
  if (dst.left)  st.Q_copy.push({ c: dst.left,  v: node });
  if (dst.right) st.Q_copy.push({ c: dst.right, v: node });
}

function betaStep(st, app){
  if (!isBetaRedex(app)) return;
  const lam = app.func, arg = app.arg;
  const parent = app.parent, slot = app.slot;

  // splice lam.body in place of app
  if (parent) {
    setChild(parent, slot, lam.body);
    maybePushBeta(st, parent);
  } else {
    st.root = lam.body;
    st.root.parent = null; st.root.slot = null;
    if (isBetaRedex(st.root)) st.Q_beta.push(st.root);
  }

  // feed argument via prebuilt fan
  const k = lam.useCount|0;
  if (k === 0) {
    /* drop arg */
  } else if (k === 1) {
    deliver(st, lam.oneLeaf, arg);       // works even if the leaf is now the root
  } else {
    st.Q_copy.push({ c: lam.copyRoot, v: arg });
  }

  app.type = 'FREE'; lam.type = 'FREE';
}

function copyStep(st, task){
  const { c, v } = task;
  if (!c || c.type !== 'COPY') return;

  if (v.type === 'APP'){
    // Build two APP shells with LEAF sinks for func/arg
    const aL = N.APP(), aR = N.APP();

    const lfL = N.LEAF(aL, 'func', null), lfR = N.LEAF(aR, 'func', null);
    const laL = N.LEAF(aL, 'arg',  null), laR = N.LEAF(aR, 'arg',  null);

    const cf = N.COPY(lfL, lfR);
    const ca = N.COPY(laL, laR);

    // Enqueue duplication of the subparts
    st.Q_copy.push({ c: cf, v: v.func });
    st.Q_copy.push({ c: ca, v: v.arg  });

    // Deliver the two APPs to the copy's outputs
    deliver(st, c.left,  aL);
    deliver(st, c.right, aR);

  } else if (v.type === 'LAM'){
    // Two LAM shells; their bodies will be filled via a COPY
    const lL = N.LAM(v.param), lR = N.LAM(v.param);

    const lbL = N.LEAF(lL, 'body', lL); // leaves belong to their respective lam
    const lbR = N.LEAF(lR, 'body', lR);
    const cb  = N.COPY(lbL, lbR);

    st.Q_copy.push({ c: cb, v: v.body });

    // Deliver the two lambdas
    deliver(st, c.left,  lL);
    deliver(st, c.right, lR);

  } else {
    // For atoms/others, just tee (safe in this object model)
    deliver(st, c.left,  v);
    deliver(st, c.right, v);
  }
}

function initState(root){
  const st = { root, Q_beta: [], Q_copy: [], initialized: true };
  // Seed initial B redexes
  (function seed(n){
    switch(n.type){
    case 'APP':
      if (n.func) seed(n.func);
      if (n.arg)  seed(n.arg);
      if (isBetaRedex(n)) st.Q_beta.push(n);
      return;
    case 'LAM': seed(n.body); return;
    default: return;
    }
  })(root);
  return st;
}

// One rewrite per tick: prefer B, else COPY. Return true if something happened.
function tick(st){
  while (st.Q_beta.length && st.Q_beta[st.Q_beta.length-1].type !== 'APP') st.Q_beta.pop(); // clean
  if (st.Q_beta.length){
    const app = st.Q_beta.pop();
    betaStep(st, app);
    return true;
  }
  if (st.Q_copy.length){
    const task = st.Q_copy.pop();
    copyStep(st, task);
    return true;
  }
  return false; // quiescent
}


function drop(nodeOrState){
  const root = nodeOrState && nodeOrState.root ? nodeOrState.root : nodeOrState;
  return toAST(root);
}

function toAST(n){
  switch (n.type){
  case 'APP':
    return { type:'APP', func: toAST(n.func), arg: toAST(n.arg) };
  case 'LAM':
    return { type:'LAM', param: n.param, body: toAST(n.body) };
  case 'LEAF':
    // Real var-uses keep their binder via leaf.lam; wiring leaves get '_'
    return { type:'VAR', name: (n.lam && n.lam.param) ? n.lam.param : '_' };
    // COPY/FREE/etc should not be reachable from root; show a marker if they are.
  default:
    return { type:'VAR', name:'?' };
  }
}


function ppAstWithDict(ast, canonIdx){
  // top=true so the whole expr isn't replaced by a name, but inner matches are
  return ppFromDict(ast, canonIdx, /*top=*/true);
}
