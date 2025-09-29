
// Tasteful 3D cube favicon @ 32x32 with shading & outline.
// Usage:
//   import { startCubeFavicon, stopCubeFavicon } from './favicon-cube.js';
//   startCubeFavicon();  // stopCubeFavicon() to pause

const SIZE = 32;
const DPR  = Math.max(1, window.devicePixelRatio || 1);
const PX   = SIZE * DPR;

let link, cvs, ctx, raf = null, t = 0;

// Unit cube vertices and faces (triangles for correct painter's sort)
const V = [
  [-1,-1,-1], [ 1,-1,-1], [ 1, 1,-1], [-1, 1,-1], // back  (z=-1)
  [-1,-1, 1], [ 1,-1, 1], [ 1, 1, 1], [-1, 1, 1], // front (z=+1)
];
// Faces as quads -> two tris each, with per-face base color
const FACES = [
  { idx:[0,1,2,3], color:[  0,255,220] }, // back
  { idx:[4,5,6,7], color:[255, 60,220] }, // front
  { idx:[0,1,5,4], color:[ 80,255,  0] }, // bottom
  { idx:[2,3,7,6], color:[255,255,  0] }, // top
  { idx:[1,2,6,5], color:[  0,170,255] }, // right
  { idx:[0,3,7,4], color:[255,120,  0] }, // left
];

function ensureCanvas() {
  if (!cvs) {
    cvs = document.createElement('canvas');
    cvs.width = PX; cvs.height = PX;
    ctx = cvs.getContext('2d', { alpha: true, desynchronized: true });
  }
  if (!link) {
    link = document.querySelector("link[rel~='icon']") || document.createElement('link');
    link.rel = 'icon';
    link.sizes = `${SIZE}x${SIZE}`;
    if (!link.parentNode) document.head.appendChild(link);
  }
}

function project([x,y,z], s, f, cx, cy) {
  // Simple perspective
  const w = f / (f - z);
  return [cx + x * s * w, cy + y * s * w, w];
}

function rotate([x,y,z], ax, ay, az) {
  // Rz * Ry * Rx
  const sx = Math.sin(ax), cx = Math.cos(ax);
  const sy = Math.sin(ay), cy = Math.cos(ay);
  const sz = Math.sin(az), cz = Math.cos(az);

  // Rx
  let y1 = y*cx - z*sx;
  let z1 = y*sx + z*cx;
  let x1 = x;

  // Ry
  let x2 = x1*cy + z1*sy;
  let z2 = -x1*sy + z1*cy;
  let y2 = y1;

  // Rz
  let x3 = x2*cz - y2*sz;
  let y3 = x2*sz + y2*cz;
  let z3 = z2;
  return [x3, y3, z3];
}

// visible if normal points toward camera
function frontFacing(n, v0) {
  // use > 0 (not < 0)
  return (n[0]*v0[0] + n[1]*v0[1] + n[2]*v0[2]) > 0;
}

function faceNormal(v0, v1, v2) {
  const ax = v1[0]-v0[0], ay = v1[1]-v0[1], az = v1[2]-v0[2];
  const bx = v2[0]-v0[0], by = v2[1]-v0[1], bz = v2[2]-v0[2];
  // cross(a,b)
  const nx = ay*bz - az*by;
  const ny = az*bx - ax*bz;
  const nz = ax*by - ay*bx;
  const len = Math.hypot(nx, ny, nz) || 1;
  return [nx/len, ny/len, nz/len];
}

function drawFrame() {
  ensureCanvas();
  const sctx = ctx;
  sctx.clearRect(0,0,PX,PX);

  // Draw in CSS pixel space
  sctx.save();
  sctx.scale(DPR, DPR);

  const cx = SIZE/2, cy = SIZE/2;
  const scale = SIZE * 0.33;   // cube extent
  const fov   = 5.0;           // perspective focal length

  // Rotation
  const ax = t*0.013, ay = t*0.021, az = t*0.008;

  // Transform verts into view space (camera at origin, looking +Z in our math)
  const TV = V.map(v => rotate(v, ax, ay, az));

  // Light direction (normalized)
  const L = [0.35, -0.5, 0.8];
  { const len = Math.hypot(...L) || 1; L[0]/=len; L[1]/=len; L[2]/=len; }

  // Build all triangles (no culling), with depth & shaded color
  const tris = [];
  for (const f of FACES) {
    const [i0,i1,i2,i3] = f.idx;
    const v0 = TV[i0], v1 = TV[i1], v2 = TV[i2], v3 = TV[i3];

    // Face normal in view space (for shading)
    const n = faceNormal(v0, v1, v2);

    // Lambert shading
    const ndotl = Math.max(0, n[0]*L[0] + n[1]*L[1] + n[2]*L[2]);
    const [r,g,b] = f.color;
    const shade = 0.25 + 0.75*ndotl; // keep visible at 32px
    const fill  = `rgb(${(r*shade)|0}, ${(g*shade)|0}, ${(b*shade)|0})`;

    // Project quad verts
    const p0 = project(v0, scale, fov, cx, cy);
    const p1 = project(v1, scale, fov, cx, cy);
    const p2 = project(v2, scale, fov, cx, cy);
    const p3 = project(v3, scale, fov, cx, cy);

    // Two triangles with average depth in view space
    const zavg1 = (v0[2] + v1[2] + v2[2]) / 3;
    const zavg2 = (v0[2] + v2[2] + v3[2]) / 3;

    tris.push({ pts:[p0,p1,p2], z:zavg1, fill });
    tris.push({ pts:[p0,p2,p3], z:zavg2, fill });
  }

  // Painter's algorithm: far -> near
  tris.sort((a,b) => a.z - b.z);

  // Fill only (no outlines)
  for (const tri of tris) {
    const [a,b,c] = tri.pts;
    sctx.beginPath();
    sctx.moveTo(a[0],a[1]);
    sctx.lineTo(b[0],b[1]);
    sctx.lineTo(c[0],c[1]);
    sctx.closePath();
    sctx.fillStyle = tri.fill;
    sctx.fill();
  }

  sctx.restore();

  // Install as favicon
  link.href = cvs.toDataURL('image/png');
}

export function startCubeFavicon() {
  ensureCanvas();
  if (matchMedia?.('(prefers-reduced-motion: reduce)')?.matches) {
    t = 0; drawFrame(); return;
  }
  stopCubeFavicon();
  alert('foo');
  const step = (ts) => {
    if (!raf) return;
    t += 0.3;           // phase increment; small for subtle motion
    drawFrame();
    raf = requestAnimationFrame(step);
  };
  raf = requestAnimationFrame(step);
}

export function stopCubeFavicon() {
  if (raf) cancelAnimationFrame(raf);
  raf = null;
}
