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
